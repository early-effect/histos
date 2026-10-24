package histos.jdk

import histos.archive.ArchivePin
import histos.native.{NativeOrigin, NativeToolchain}
import histos.process.ChildProcess
import histos.toolchain.{
  Architecture,
  HostPlatform,
  ToolArchiveError,
  LocalOverride,
  LocalOverrideError,
  RuntimeFiles,
  RuntimeIdentity
}
import histos.workspace.IoFailure

import java.nio.file.{Files, Path}

import zio.{Config, ConfigProvider, Duration, IO, ZIO, ZLayer, durationInt}

enum Os:
  case Mac, Linux, Windows, Other

object Os:
  def from(name: String): Os =
    val lower = name.toLowerCase(java.util.Locale.ROOT)
    if lower.contains("mac") || lower == "darwin" then Os.Mac
    else if lower.contains("win") then Os.Windows
    else if lower.contains("linux") || lower.contains("nix") then Os.Linux
    else Os.Other

/** Set by a re-exec so the child does not re-exec again. */
enum Settled:
  case No, Yes

object Settled:
  def from(env: Map[String, String]): Settled =
    env.get("HISTOS_JDK_SETTLED") match
      case Some("1") => Settled.Yes
      case _         => Settled.No

final case class Machine(
    env: Map[String, String],
    userHome: String,
    os: Os,
    architecture: Architecture,
    cache: Path,
    repo: Path,
    running: Option[Path],
    settled: Settled,
    native: NativeOrigin
):
  def host: HostPlatform = HostPlatform.of(os, architecture)

object Machine:
  /** `HISTOS_NATIVE` and `HISTOS_JDK_SETTLED` from one environment map. */
  final case class Settings(native: NativeOrigin, settled: Settled)

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

  val settled: Config[Settled] =
    Config
      .string("HISTOS_JDK_SETTLED")
      .optional
      .map:
        case Some("1") => Settled.Yes
        case _         => Settled.No

  val config: Config[Settings] =
    (native ++ settled).map { case (origin, flag) => Settings(origin, flag) }

  /** One read of the fiber's `zio.System`. `HISTOS_*` is `zio.Config` over that
    * map. ZIO keeps `System` on the default-services fiber ref, so this
    * effect's environment stays empty: tests swap the ref, they do not provide
    * a layer.
    */
  def load(repo: Path): IO[ToolchainError, Machine] =
    ZIO.systemWith: system =>
      for
        env <- system.envs.mapError(_ => ToolchainError.Denied)
        home <- system
          .property("user.home")
          .mapError(_ => ToolchainError.Denied)
          .flatMap:
            case Some(value) if value.nonEmpty => ZIO.succeed(value)
            case _ => ZIO.fail(ToolchainError.NoUserHome)
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
        settings <- ZIO
          .config(config)
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
        settings.settled,
        settings.native
      )

  def layer(repo: Path): ZLayer[Any, ToolchainError, Machine] =
    ZLayer.fromZIO(load(repo))

enum Launch:
  case Stay(chosen: Candidate.Match)
  case Reexec(javaBin: Path, chosen: Candidate.Match)
  case Halt(resolution: Resolution)

object Launch:
  def decide(
      running: Option[Path],
      home: Path,
      resolution: Resolution,
      os: Os
  ): Launch =
    resolution match
      case Resolution.Selected(chosen, _) =>
        if running.exists(_.normalize == home.normalize) then
          Launch.Stay(chosen)
        else
          Launch.Reexec(
            home
              .resolve("bin")
              .resolve(if os == Os.Windows then "java.exe" else "java"),
            chosen
          )
      case other => Launch.Halt(other)

enum Startup:
  case Continue
  case Replace(javaBin: Path)
  case Refuse(report: String)

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

enum ToolchainError:
  case Probe(error: JdkError)
  case Lock(error: LockError)
  case Stage(error: StageError)
  case Override(error: OverrideError)
  case Unreadable(path: Path)
  case NoLock(path: Path)
  case NoRuntime
  case NoUserHome
  case NoArchitecture
  case Archive(error: ToolArchiveError)
  case Denied
  case Config(error: zio.Config.Error)
  case Reexec
  case Io(failure: IoFailure)
  case Rejected(path: Path, reason: Reject)

  def message: String = this match
    case Probe(error)           => error.message
    case Lock(error)            => error.message
    case Stage(error)           => error.message
    case Override(error)        => error.message
    case Unreadable(path)       => s"could not read $path"
    case NoLock(path)           => s"no jdk lock at $path"
    case NoRuntime              => "no running JDK is available without a lock"
    case NoArchitecture         => "os.arch is not set"
    case Archive(error)         => error.message
    case NoUserHome             => "user.home is not set"
    case Denied                 => "the process environment could not be read"
    case Config(error)          => s"configuration: ${configText(error)}"
    case Reexec                 => "could not re-exec onto the chosen jdk"
    case Io(failure)            => failure.message
    case Rejected(path, reason) => s"JDK override $path was rejected: $reason"

  private def configText(error: zio.Config.Error): String =
    error.fold(
      (path, message) => s"${place(path)} $message",
      (path, message) => s"${place(path)} $message",
      (path, message, _) => s"${place(path)} $message",
      (path, message) => s"${place(path)} $message"
    )((left, right) => s"$left; $right", (left, right) => s"$left or $right")

  private def place(path: zio.Chunk[String]): String =
    if path.isEmpty then "config" else path.mkString(".")

object JdkLocations:
  def coursierCache(userHome: String, os: Os, env: Map[String, String]): Path =
    env.get("COURSIER_CACHE").filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None      =>
        os match
          case Os.Mac     => Path.of(userHome, "Library", "Caches", "Coursier")
          case Os.Windows =>
            Path.of(userHome, "AppData", "Local", "Coursier", "Cache")
          case Os.Linux | Os.Other => Path.of(userHome, ".cache", "coursier")

  def sdkmanRoot(userHome: String, env: Map[String, String]): Path =
    env.get("SDKMAN_DIR").filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None      => Path.of(userHome, ".sdkman")

  def sdkmanJava(userHome: String, env: Map[String, String]): Path =
    sdkmanRoot(userHome, env).resolve("candidates").resolve("java")

  def probe(machine: Machine, explicit: Option[Path]): IO[JdkError, ScanInput] =
    val coursier =
      JdkLocations.coursierCache(machine.userHome, machine.os, machine.env)
    val sdkRoot = JdkLocations.sdkmanRoot(machine.userHome, machine.env)
    for
      csOnPath <- toolOnPath(machine.env.get("PATH"), "cs")
      sdkDir <- isDir(sdkRoot)
      java <- firstJdk(machine.env.get("PATH"), machine.os)
    yield
      // Histos's own Coursier fetches create the cache directory, so the directory alone is not an install.
      val cs =
        if csOnPath then ManagerProbe.Installed(Some(coursier.resolve("arc")))
        else ManagerProbe.Cached(coursier.resolve("arc"))
      val sdk =
        if sdkDir then
          ManagerProbe.Installed(
            Some(sdkRoot.resolve("candidates").resolve("java"))
          )
        else ManagerProbe.Absent
      ScanInput(
        explicit,
        machine.env.get("JAVA_HOME").filter(_.nonEmpty).map(Path.of(_)),
        java,
        cs,
        sdk,
        machine.cache.resolve("jdks")
      )

  private def isDir(path: Path): IO[JdkError, Boolean] =
    ZIO
      .attemptBlocking(Files.isDirectory(path))
      .mapError(_ =>
        JdkError.Io(IoFailure(path, histos.workspace.IoFault.Stat))
      )

  private def toolOnPath(
      pathEnv: Option[String],
      name: String
  ): IO[JdkError, Boolean] =
    RuntimeFiles
      .firstOnPath(pathEnv, List(name, name + ".exe", name + ".bat"))
      .mapError(JdkError.Locations(_))
      .map(_.nonEmpty)

  private def firstJdk(
      pathEnv: Option[String],
      os: Os
  ): IO[JdkError, Option[Path]] =
    val name = if os == Os.Windows then "java.exe" else "java"
    RuntimeFiles
      .firstOnPath(pathEnv, List(name))
      .mapError(JdkError.Locations(_))
      .map(_.flatMap(RuntimeFiles.homeOf))

end JdkLocations

/** The lock that was read and the decision it produced. Printing is
  * [[Toolchain.text]].
  */
final case class JdkSurvey(
    lock: JdkLock,
    resolution: Resolution,
    identity: Option[RuntimeIdentity],
    archive: Option[ArchivePin]
)

final case class JdkRuntime(
    home: Path,
    version: JdkVersion,
    identity: RuntimeIdentity
)

final case class Toolchain(stager: JdkStage):
  def prepare(machine: Machine): IO[ToolchainError, Startup] =
    Toolchain.prepareWith(machine, stager.stage)
  def resolve(machine: Machine): IO[ToolchainError, JdkRuntime] =
    Toolchain.resolveWith(machine, stager.stage)

object Toolchain:
  val layer: ZLayer[JdkStage, Nothing, Toolchain] =
    ZLayer.fromFunction(Toolchain.apply)

  /** JDK 25 refuses unnamed native access without this flag. The manifest
    * attribute is the jar form.
    */
  val NativeAccess: String = "--enable-native-access=ALL-UNNAMED"
  val NativeAccessManifest: String = "Enable-Native-Access: ALL-UNNAMED"

  def explain(machine: Machine): IO[ToolchainError, JdkSurvey] =
    readLock(machine.repo).flatMap:
      case None       => ZIO.fail(ToolchainError.NoLock(lockPath(machine.repo)))
      case Some(lock) =>
        survey(machine, lock)

  private def survey(
      machine: Machine,
      lock: JdkLock
  ): IO[ToolchainError, JdkSurvey] =
    for
      explicit <- readOverride(machine.repo)
      archive <-
        if explicit.isEmpty then
          ZIO
            .fromEither(lock.archive(machine.host))
            .mapError(ToolchainError.Archive(_))
            .map(Some(_))
        else ZIO.none
      found <- resolution(lock, explicit)
      identity <- found match
        case Resolution.Selected(chosen, _) =>
          RuntimeFiles
            .identity(chosen.home)
            .mapError(ToolchainError.Io(_))
            .map(Some(_))
        case Resolution.OverrideRejected(_, _) => ZIO.none
        case _                                 =>
          ZIO.succeed(archive.map(pin => RuntimeIdentity.Locked(pin.sha256)))
    yield JdkSurvey(lock, found, identity, archive)

  def text(survey: JdkSurvey): String =
    render(survey.lock, survey.resolution, survey.archive)

  def resolve(machine: Machine): IO[ToolchainError, JdkRuntime] =
    resolveWith(machine, JdkStage.stage)

  private def resolveWith(
      machine: Machine,
      stage: (JdkLock, Machine) => IO[StageError, JdkHome]
  ): IO[ToolchainError, JdkRuntime] =
    readLock(machine.repo).flatMap:
      case None       => bootstrap(machine)
      case Some(lock) =>
        for
          survey <- Toolchain.survey(machine, lock)
          runtime <- survey.resolution match
            case Resolution.Selected(chosen, _) =>
              survey.identity
                .fold[IO[ToolchainError, RuntimeIdentity]](
                  RuntimeFiles
                    .identity(chosen.home)
                    .mapError(ToolchainError.Io(_))
                )(ZIO.succeed(_))
                .map(identity =>
                  JdkRuntime(chosen.home, chosen.version, identity)
                )
            case Resolution.OverrideRejected(reason, _) =>
              ZIO.fail(
                ToolchainError.Rejected(
                  machine.repo.resolve(".histos/toolchain.local"),
                  reason
                )
              )
            case _ =>
              ZIO
                .fromOption(survey.archive)
                .orElseFail(
                  ToolchainError.Archive(
                    ToolArchiveError.NoArchive(machine.host)
                  )
                )
                .flatMap(archive =>
                  stage(survey.lock, machine)
                    .mapError(ToolchainError.Stage(_))
                    .flatMap: home =>
                      ZIO
                        .fromEither(home.version)
                        .mapError(error =>
                          ToolchainError.Stage(
                            StageError
                              .Version(home.path.resolve("release"), error)
                          )
                        )
                        .map(version =>
                          JdkRuntime(
                            home.path,
                            version,
                            RuntimeIdentity.Locked(archive.sha256)
                          )
                        )
                )
        yield runtime

  def prepare(machine: Machine): IO[ToolchainError, Startup] =
    prepareWith(machine, JdkStage.stage)

  private def prepareWith(
      machine: Machine,
      stage: (JdkLock, Machine) => IO[StageError, JdkHome]
  ): IO[ToolchainError, Startup] =
    if machine.settled == Settled.Yes then ZIO.succeed(Startup.Continue)
    else
      readLock(machine.repo).flatMap:
        case None =>
          readOverride(machine.repo).flatMap:
            case None    => ZIO.succeed(Startup.Continue)
            case Some(_) =>
              bootstrap(machine).flatMap(runtime =>
                align(machine.running, runtime.home).map((running, home) =>
                  if running.contains(home) then Startup.Continue
                  else
                    Startup.Replace(
                      home
                        .resolve("bin")
                        .resolve(if machine.os == Os.Windows then "java.exe"
                        else "java")
                    )
                )
              )
        case Some(lock) =>
          readOverride(machine.repo)
            .flatMap(explicit => resolution(lock, explicit))
            .flatMap:
              case Resolution.Provision(_, trace) =>
                stage(lock, machine)
                  .mapError(ToolchainError.Stage(_))
                  .flatMap: home =>
                    ZIO
                      .fromEither(home.version)
                      .mapError(error =>
                        ToolchainError.Stage(
                          StageError
                            .Version(home.path.resolve("release"), error)
                        )
                      )
                      .flatMap: version =>
                        align(machine.running, home.path).map:
                          (running, canonicalHome) =>
                            val matched = new Candidate.Match(
                              Origin.Cache,
                              canonicalHome,
                              version
                            )
                            startup(
                              lock,
                              running,
                              canonicalHome,
                              Resolution.Selected(matched, trace :+ matched),
                              machine.os
                            )
              case found @ Resolution.Selected(chosen, _) =>
                align(machine.running, chosen.home)
                  .map((running, home) =>
                    startup(lock, running, home, found, machine.os)
                  )
              case Resolution.OverrideRejected(reason, _) =>
                ZIO.fail(
                  ToolchainError.Rejected(
                    machine.repo.resolve(".histos/toolchain.local"),
                    reason
                  )
                )
              case found =>
                ZIO.succeed(Startup.Refuse(render(lock, found, None)))

  private def bootstrap(machine: Machine): IO[ToolchainError, JdkRuntime] =
    for
      explicit <- readOverride(machine.repo)
      path <- ZIO
        .fromOption(explicit.orElse(machine.running))
        .orElseFail(ToolchainError.NoRuntime)
      home <- Jdk.home(path).mapError(ToolchainError.Probe(_))
      _ <- ZIO
        .fail(ToolchainError.Rejected(path, Reject.NoJavac))
        .unless(home.hasJavac)
      version <- ZIO
        .fromEither(home.version)
        .mapError(error =>
          ToolchainError.Rejected(path, Reject.UnreadableVersion(error))
        )
      identity <- RuntimeFiles.identity(path).mapError(ToolchainError.Io(_))
    yield JdkRuntime(path, version, identity)

  def command(javaBin: Path, jar: Path, args: List[String]): List[String] =
    javaBin.toString :: NativeAccess :: "-jar" :: jar.toString :: args

  /** The child shares this terminal, and its status is the result. Interrupt
    * stops it and everything it started.
    */
  def replace(
      javaBin: Path,
      jar: Path,
      args: List[String]
  ): IO[ToolchainError, Int] =
    ChildProcess
      .attached(
        command(javaBin, jar, args),
        Map("HISTOS_JDK_SETTLED" -> "1"),
        HandOffGrace
      )
      .mapError(_ => ToolchainError.Reexec)

  /** Time for the child's own SIGTERM handling, which stops its test children,
    * before it is force-killed.
    */
  private val HandOffGrace: Duration = 2.seconds

  private def startup(
      lock: JdkLock,
      running: Option[Path],
      home: Path,
      resolution: Resolution,
      os: Os
  ): Startup =
    Launch.decide(running, home, resolution, os) match
      case Launch.Stay(_)            => Startup.Continue
      case Launch.Reexec(javaBin, _) => Startup.Replace(javaBin)
      case Launch.Halt(_) => Startup.Refuse(render(lock, resolution, None))

  private def align(
      running: Option[Path],
      home: Path
  ): IO[ToolchainError, (Option[Path], Path)] =
    for
      canonicalHome <- canonical(home)
      canonicalRun <- ZIO.foreach(running)(canonical)
    yield (canonicalRun, canonicalHome)

  private def canonical(path: Path): IO[ToolchainError, Path] =
    ZIO
      .attemptBlocking(path.toRealPath().normalize)
      .mapError(_ =>
        ToolchainError.Io(IoFailure(path, histos.workspace.IoFault.Stat))
      )

  private def resolution(
      lock: JdkLock,
      explicit: Option[Path]
  ): IO[ToolchainError, Resolution] =
    explicit match
      case None       => ZIO.succeed(Resolution.Provision(lock.major, Nil))
      case Some(path) =>
        Jdk
          .locate(Some(path))
          .mapError(ToolchainError.Probe(_))
          .map(location =>
            Jdk.resolve(
              lock.major,
              ProbeView(
                location,
                Located.Unset,
                Located.Unset,
                ManagerSlot.Absent,
                ManagerSlot.Absent,
                Nil
              )
            )
          )

  def render(
      lock: JdkLock,
      resolution: Resolution,
      archive: Option[ArchivePin]
  ): String =
    val tail = resolution match
      case Resolution.Provision(_, _) =>
        archive.fold("explicit local runtime")(pin =>
          s"archive ${pin.source.render}"
        )
      case _ => s"lock ${lock.version.value}"
    Jdk.report(resolution) + "\n" + tail

  private def readLock(repo: Path): IO[ToolchainError, Option[JdkLock]] =
    RuntimeFiles
      .readLock(lockPath(repo))(JdkLock.parse)
      .mapError:
        case failure: IoFailure => ToolchainError.Io(failure)
        case error: LockError   => ToolchainError.Lock(error)

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

  private def lockPath(repo: Path): Path = repo.resolve(".histos/jdk.lock")
end Toolchain
