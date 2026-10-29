package histos.jdk

import histos.native.{NativeOrigin, NativeToolchain}
import histos.toolchain.{Architecture, HostPlatform, LocalOverride, LocalOverrideError, RuntimeFiles, RuntimeIdentity}
import histos.workspace.IoFailure

import java.nio.file.Path

import zio.{Config, ConfigProvider, IO, ZIO, ZLayer}

enum Os:
  case Mac, Linux, Windows, Other

object Os:
  def from(name: String): Os =
    val lower = name.toLowerCase(java.util.Locale.ROOT)
    if lower.contains("mac") || lower == "darwin" then Os.Mac
    else if lower.contains("win") then Os.Windows
    else if lower.contains("linux") || lower.contains("nix") then Os.Linux
    else Os.Other

final case class Machine(
    env: Map[String, String],
    userHome: String,
    os: Os,
    architecture: Architecture,
    cache: Path,
    repo: Path,
    running: Option[Path],
    native: NativeOrigin,
):
  def host: HostPlatform = HostPlatform.of(os, architecture)

object Machine:
  val native: Config[NativeOrigin] =
    Config
      .string("HISTOS_NATIVE")
      .optional
      .map:
        case None      => NativeOrigin.Central
        case Some(raw) =>
          NativeToolchain.parse(raw) match
            case Left(error)   => NativeOrigin.Refused(error)
            case Right(source) => NativeOrigin.Chosen(source)

  /** One `envs` and one `properties`. `HISTOS_NATIVE` is config over that map. The environment stays empty: a test
    * swaps the fiber's `System` instead of providing a layer.
    */
  def load(repo: Path): IO[ToolchainError, Machine] =
    ZIO.systemWith: system =>
      for
        env        <- system.envs.mapError(_ => ToolchainError.Denied)
        properties <- system.properties.mapError(_ => ToolchainError.Denied)
        origin     <- ZIO
          .config(native)
          .provideLayer(zio.Runtime.setConfigProvider(ConfigProvider.fromMap(env)))
          .mapError(error => ToolchainError.Config(error))
        machine <- ZIO.fromEither(read(repo, env, properties, origin))
      yield machine

  private def read(
      repo: Path,
      env: Map[String, String],
      properties: Map[String, String],
      origin: NativeOrigin,
  ): Either[ToolchainError, Machine] =
    val home = properties.get("user.home").filter(_.nonEmpty)
    val arch = properties.get("os.arch").filter(_.strip().nonEmpty)
    (home, arch) match
      case (None, _)                => Left(ToolchainError.NoUserHome)
      case (_, None)                => Left(ToolchainError.NoArchitecture)
      case (Some(home), Some(arch)) =>
        val os = properties.get("os.name").filter(_.nonEmpty).fold(Os.Other)(Os.from)
        Right(
          Machine(
            env,
            home,
            os,
            Architecture.fromMachine(arch),
            histos.cache.ContentStore.home(env.get("XDG_CACHE_HOME"), home),
            repo,
            properties.get("java.home").filter(_.nonEmpty).map(Path.of(_)),
            origin,
          )
        )

  def layer(repo: Path): ZLayer[Any, ToolchainError, Machine] =
    ZLayer.fromZIO(load(repo))
end Machine

enum OverrideError:
  case Empty
  case Multiple
  case Relative(raw: String)
  case Invalid(raw: String)

  def message: String = this match
    case Empty         => "toolchain.local is empty"
    case Multiple      => "toolchain.local has more than one path"
    case Relative(raw) => s"toolchain.local path is not absolute: $raw"
    case Invalid(raw)  => s"toolchain.local path is invalid: $raw"
end OverrideError

enum ToolchainError:
  case Probe(error: JdkError)
  case Override(error: OverrideError)
  case TooOld(required: JdkVersion, found: JdkVersion, home: Path)
  case Absent(required: JdkVersion)
  case NoUserHome
  case NoArchitecture
  case Denied
  case Config(error: zio.Config.Error)
  case Io(failure: IoFailure)
  case Rejected(path: Path, reason: Reject)

  def message: String = this match
    case Probe(error)                  => error.message
    case Override(error)               => error.message
    case TooOld(required, found, home) =>
      s"JDK ${found.value} at $home is older than ${required.value}"
    case Absent(required) =>
      s"no JDK found; Histos needs ${required.value} or newer"
    case NoArchitecture         => "os.arch is not set"
    case NoUserHome             => "user.home is not set"
    case Denied                 => "the process environment could not be read"
    case Config(error)          => s"configuration: ${configText(error)}"
    case Io(failure)            => failure.message
    case Rejected(path, reason) =>
      s"JDK at $path was rejected: ${reason.message}"

  private def configText(error: zio.Config.Error): String =
    error.fold(
      (path, message) => s"${place(path)} $message",
      (path, message) => s"${place(path)} $message",
      (path, message, _) => s"${place(path)} $message",
      (path, message) => s"${place(path)} $message",
    )((left, right) => s"$left; $right", (left, right) => s"$left or $right")

  private def place(path: zio.Chunk[String]): String =
    if path.isEmpty then "config" else path.mkString(".")
end ToolchainError

/** The JDK this command is running on. */
final case class JdkSurvey(
    home: Path,
    version: JdkVersion,
    identity: RuntimeIdentity,
)

final case class JdkRuntime(
    home: Path,
    version: JdkVersion,
    identity: RuntimeIdentity,
)

object Toolchain:
  /** Oldest JDK this process will accept. The Loom executor needs 21. */
  val Floor: JdkVersion = JdkVersion.trusted("21", JdkMajor.trusted(21))

  /** JDK 25 refuses unnamed native access without this flag. The manifest attribute is the jar form.
    */
  val NativeAccess: String         = "--enable-native-access=ALL-UNNAMED"
  val NativeAccessManifest: String = "Enable-Native-Access: ALL-UNNAMED"

  def explain(machine: Machine): IO[ToolchainError, JdkSurvey] =
    resolve(machine).map(runtime => JdkSurvey(runtime.home, runtime.version, runtime.identity))

  def text(survey: JdkSurvey): String =
    s"jdk ${survey.version.value} ${survey.home}"

  /** Fail the command when the running JVM, or `toolchain.local`, is missing or older than [[Floor]]. Histos does not
    * fetch a JDK.
    */
  def prepare(machine: Machine): IO[ToolchainError, Unit] =
    resolve(machine).unit

  def resolve(machine: Machine): IO[ToolchainError, JdkRuntime] =
    readOverride(machine.repo).flatMap:
      case Some(path) => accept(path)
      case None       =>
        machine.running match
          case Some(home) => accept(home)
          case None       => ZIO.fail(ToolchainError.Absent(Floor))

  def command(javaBin: Path, jar: Path, args: List[String]): List[String] =
    javaBin.toString :: NativeAccess :: "-jar" :: jar.toString :: args

  private def accept(path: Path): IO[ToolchainError, JdkRuntime] =
    for
      home <- Jdk.home(path).mapError(ToolchainError.Probe(_))
      _    <- ZIO
        .fail(ToolchainError.Rejected(path, Reject.NoJavac))
        .unless(home.hasJavac)
      released <- ZIO
        .fromEither(home.release)
        .mapError(error => ToolchainError.Rejected(path, Reject.UnreadableVersion(error)))
      _ <- ZIO
        .fail(ToolchainError.TooOld(Floor, released.version, path))
        .unless(released.version.atLeast(Floor))
      identity <- RuntimeFiles
        .identity(path, released.build.value)
        .mapError(ToolchainError.Io(_))
    yield JdkRuntime(path, released.version, identity)

  private def readOverride(repo: Path): IO[ToolchainError, Option[Path]] =
    LocalOverride
      .read(repo.resolve(".histos/toolchain.local"))
      .mapError:
        case failure: IoFailure       => ToolchainError.Io(failure)
        case LocalOverrideError.Empty =>
          ToolchainError.Override(OverrideError.Empty)
        case LocalOverrideError.Multiple =>
          ToolchainError.Override(OverrideError.Multiple)
        case LocalOverrideError.Relative(raw) =>
          ToolchainError.Override(OverrideError.Relative(raw))
        case LocalOverrideError.Invalid(raw) =>
          ToolchainError.Override(OverrideError.Invalid(raw))
end Toolchain
