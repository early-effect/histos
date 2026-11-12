package histos.jdk

import histos.native.{NativeOrigin, NativeToolchain}
import histos.toolchain.{Architecture, HostPlatform, LocalOverride, LocalOverrideError, RuntimeFiles, RuntimeIdentity}
import histos.workspace.IoFailure

import java.nio.file.Path

import zio.{Config, ConfigProvider, IO, UIO, ZIO, ZLayer}

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

/** Where the JDK home was named. */
enum JdkSource:
  case Local
  case Running

  def label: String = this match
    case Local   => ".histos/toolchain.local"
    case Running => "java.home"
end JdkSource

/** A JDK home, or the reason none could be used, with the source that was tried. */
enum JdkRead:
  case Chosen(runtime: JdkRuntime, source: JdkSource)
  case Failed(error: ToolchainError, source: Option[JdkSource])

object Toolchain:
  /** Oldest JDK this process will accept. The Loom executor needs 21. */
  /** The child-process bootstrap compiles `java.lang.foreign`, which JDK 21 marks preview. */
  val Floor: JdkVersion = JdkVersion.trusted("25", JdkMajor.trusted(25))

  /** JDK 25 refuses unnamed native access without this flag. The manifest attribute is the jar form.
    */
  val NativeAccess: String         = "--enable-native-access=ALL-UNNAMED"
  val NativeAccessManifest: String = "Enable-Native-Access: ALL-UNNAMED"

  def explain(machine: Machine): IO[ToolchainError, JdkSurvey] =
    resolve(machine).map(runtime => JdkSurvey(runtime.home, runtime.version, runtime.identity))

  def text(survey: JdkSurvey): String =
    s"jdk ${survey.version.value} ${survey.home}"

  /** Names `.histos/toolchain.local` and `java.home`, and how to install a JDK that meets [[Floor]]. */
  def failure(error: ToolchainError, source: Option[JdkSource]): String =
    val from = source match
      case Some(found) => s" The JDK came from ${found.label}."
      case None        => ""
    s"${error.message}.$from Install a JDK ${Floor.value} or newer and write its home to .histos/toolchain.local, or remove that file to use java.home. Histos does not fetch a JDK."

  /** Fail the command when the running JVM, or `toolchain.local`, is missing or older than [[Floor]]. Histos does not
    * fetch a JDK.
    */
  def prepare(machine: Machine): IO[ToolchainError, Unit] =
    resolve(machine).unit

  /** JDK from `.histos/toolchain.local`, or else `java.home`. A failure does not hide the source that was tried. */
  def read(machine: Machine): UIO[JdkRead] =
    readOverride(machine.repo).foldZIO(
      error => ZIO.succeed(JdkRead.Failed(error, Some(JdkSource.Local))),
      {
        case Some(path) => finish(accept(path), JdkSource.Local)
        case None       =>
          machine.running match
            case None       => ZIO.succeed(JdkRead.Failed(ToolchainError.Absent(Floor), Some(JdkSource.Running)))
            case Some(home) => finish(accept(home), JdkSource.Running)
      },
    )

  def resolve(machine: Machine): IO[ToolchainError, JdkRuntime] =
    read(machine).flatMap:
      case JdkRead.Chosen(runtime, _) => ZIO.succeed(runtime)
      case JdkRead.Failed(error, _)   => ZIO.fail(error)

  def command(javaBin: Path, jar: Path, args: List[String]): List[String] =
    javaBin.toString :: NativeAccess :: "-jar" :: jar.toString :: args

  private def finish(accepted: IO[ToolchainError, JdkRuntime], source: JdkSource): UIO[JdkRead] =
    accepted.fold(
      error => JdkRead.Failed(error, Some(source)),
      runtime => JdkRead.Chosen(runtime, source),
    )

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
