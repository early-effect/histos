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

  /** One read of the fiber's `zio.System`. `HISTOS_NATIVE` is `zio.Config` over that map. ZIO keeps `System` on the
    * default-services fiber ref, so this effect's environment stays empty: tests swap the ref, they do not provide a
    * layer.
    */
  def load(repo: Path): IO[ToolchainError, Machine] =
    ZIO.systemWith: system =>
      for
        env  <- system.envs.mapError(_ => ToolchainError.Denied)
        home <- system
          .property("user.home")
          .mapError(_ => ToolchainError.Denied)
          .flatMap:
            case Some(value) if value.nonEmpty => ZIO.succeed(value)
            case _                             => ZIO.fail(ToolchainError.NoUserHome)
        os <- system
          .property("os.name")
          .mapError(_ => ToolchainError.Denied)
          .map:
            case Some(name) if name.nonEmpty => Os.from(name)
            case _                           => Os.Other
        architecture <- system
          .property("os.arch")
          .mapError(_ => ToolchainError.Denied)
          .flatMap:
            case Some(raw) if raw.strip().nonEmpty =>
              ZIO.succeed(Architecture.fromMachine(raw))
            case _ => ZIO.fail(ToolchainError.NoArchitecture)
        running <- system
          .property("java.home")
          .mapError(_ => ToolchainError.Denied)
          .map: value =>
            value.filter(_.nonEmpty).map(Path.of(_))
        origin <- ZIO
          .config(native)
          .provideLayer(
            zio.Runtime.setConfigProvider(ConfigProvider.fromMap(env))
          )
          .mapError(error => ToolchainError.Config(error))
      yield Machine(
        env,
        home,
        os,
        architecture,
        histos.cache.ContentStore.home(env.get("XDG_CACHE_HOME"), home),
        repo,
        running,
        origin,
      )

  def layer(repo: Path): ZLayer[Any, ToolchainError, Machine] =
    ZLayer.fromZIO(load(repo))
end Machine

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
        .unless(
          histos.toolchain.VersionOrder.atLeast(released.version.value, Floor.value, stripJavaOne = true)
        )
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
