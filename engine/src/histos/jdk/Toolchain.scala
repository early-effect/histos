package histos.jdk

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

enum Os:
  case Mac, Linux, Windows, Other

object Os:
  def from(name: String): Os =
    val lower = name.toLowerCase(java.util.Locale.ROOT)
    if lower.contains("mac") then Os.Mac
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
    cache: Path,
    repo: Path,
    running: Option[Path],
    settled: Settled,
)

enum Launch:
  case Stay(chosen: Candidate.Match)
  case Reexec(javaBin: Path, chosen: Candidate.Match)
  case Halt(resolution: Resolution)

object Launch:
  def decide(running: Option[Path], home: Path, resolution: Resolution): Launch =
    resolution match
      case Resolution.Selected(chosen, _) =>
        if running.exists(_.normalize == home.normalize) then Launch.Stay(chosen)
        else Launch.Reexec(home.resolve("bin").resolve("java"), chosen)
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
    case Empty           => "toolchain.local is empty"
    case Multiple        => "toolchain.local has more than one path"
    case Relative(raw)   => s"toolchain.local path is not absolute: $raw"
    case Invalid(raw)    => s"toolchain.local path is invalid: $raw"

enum ToolchainError:
  case Probe(error: JdkError)
  case Lock(error: LockError)
  case Stage(error: StageError)
  case Override(error: OverrideError)
  case Unreadable(path: Path)
  case NoLock(path: Path)
  case NoUserHome
  case Reexec

  def message: String = this match
    case Probe(error)     => error.message
    case Lock(error)      => error.message
    case Stage(error)     => error.message
    case Override(error)  => error.message
    case Unreadable(path) => s"could not read $path"
    case NoLock(path)     => s"no jdk lock at $path"
    case NoUserHome       => "user.home is not set"
    case Reexec           => "could not re-exec onto the chosen jdk"

object JdkLocations:
  def coursierCache(userHome: String, os: Os, env: Map[String, String]): Path =
    env.get("COURSIER_CACHE").filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None =>
        os match
          case Os.Mac     => Path.of(userHome, "Library", "Caches", "Coursier")
          case Os.Windows => Path.of(userHome, "AppData", "Local", "Coursier", "Cache")
          case Os.Linux | Os.Other => Path.of(userHome, ".cache", "coursier")

  def sdkmanRoot(userHome: String, env: Map[String, String]): Path =
    env.get("SDKMAN_DIR").filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None      => Path.of(userHome, ".sdkman")

  def sdkmanJava(userHome: String, env: Map[String, String]): Path =
    sdkmanRoot(userHome, env).resolve("candidates").resolve("java")

  def probe(machine: Machine, explicit: Option[Path]): IO[JdkError, ScanInput] =
    val coursier = JdkLocations.coursierCache(machine.userHome, machine.os, machine.env)
    val sdkRoot  = JdkLocations.sdkmanRoot(machine.userHome, machine.env)
    for
      cacheDir <- isDir(coursier)
      csOnPath <- toolOnPath(machine.env.get("PATH"), "cs")
      sdkDir   <- isDir(sdkRoot)
      java     <- firstJdk(machine.env.get("PATH"), machine.os)
    yield
      val cs =
        if cacheDir || csOnPath then ManagerProbe.Installed(Some(coursier.resolve("arc")))
        else ManagerProbe.Absent
      val sdk =
        if sdkDir then ManagerProbe.Installed(Some(sdkRoot.resolve("candidates").resolve("java")))
        else ManagerProbe.Absent
      ScanInput(
        explicit,
        machine.env.get("JAVA_HOME").filter(_.nonEmpty).map(Path.of(_)),
        java,
        cs,
        sdk,
        machine.cache.resolve("jdks"),
      )

  private def isDir(path: Path): IO[JdkError, Boolean] =
    ZIO.attempt(Files.isDirectory(path)).mapError(_ => JdkError.Unreadable(path))

  private def toolOnPath(pathEnv: Option[String], name: String): IO[JdkError, Boolean] =
    ZIO.attempt {
      entries(pathEnv).exists: dir =>
        Files.isRegularFile(dir.resolve(name)) ||
          Files.isRegularFile(dir.resolve(name + ".exe")) ||
          Files.isRegularFile(dir.resolve(name + ".bat"))
    }.mapError(_ => JdkError.PathScan)

  private def firstJdk(pathEnv: Option[String], os: Os): IO[JdkError, Option[Path]] =
    val names = os match
      case Os.Windows => List("java.exe")
      case _          => List("java")
    ZIO.attempt {
      entries(pathEnv).flatMap { dir =>
        names.flatMap { name =>
          val bin = dir.resolve(name)
          if Files.isRegularFile(bin) then jdkHome(bin).toList else Nil
        }
      }.headOption
    }.mapError(_ => JdkError.PathScan)

  private def entries(pathEnv: Option[String]): List[Path] =
    pathEnv.filter(_.nonEmpty).toList.flatMap(_.split(java.io.File.pathSeparator).toList.filter(_.nonEmpty)).map(Path.of(_))

  private def jdkHome(javaBin: Path): Option[Path] =
    Option(javaBin.getParent).flatMap(parent => Option(parent.getParent))
end JdkLocations

/** The lock that was read and the decision it produced. Printing is [[Toolchain.text]]. */
final case class JdkSurvey(lock: JdkLock, resolution: Resolution)

object Toolchain:
  /** JDK 25 refuses unnamed native access without this flag. The manifest attribute is the jar form. */
  val NativeAccess: String = "--enable-native-access=ALL-UNNAMED"
  val NativeAccessManifest: String = "Enable-Native-Access: ALL-UNNAMED"

  def explain(machine: Machine): IO[ToolchainError, JdkSurvey] =
    readLock(machine.repo).flatMap:
      case None       => ZIO.fail(ToolchainError.NoLock(lockPath(machine.repo)))
      case Some(lock) => resolution(machine, lock).map(found => JdkSurvey(lock, found))

  def text(survey: JdkSurvey): String =
    render(survey.lock, survey.resolution)

  def prepare(machine: Machine): IO[ToolchainError, Startup] =
    if machine.settled == Settled.Yes then ZIO.succeed(Startup.Continue)
    else
      readLock(machine.repo).flatMap:
        case None       => ZIO.succeed(Startup.Continue)
        case Some(lock) =>
          resolution(machine, lock).flatMap:
            case found @ Resolution.Provision(_, trace) =>
              lock.source match
                case ArchiveSource.Remote(_) => ZIO.succeed(Startup.Refuse(render(lock, found)))
                case ArchiveSource.Local(_) =>
                  JdkStage.stage(lock, machine.cache.resolve("jdks")).mapError(ToolchainError.Stage(_)).flatMap: home =>
                    home.version match
                      case Left(_) => ZIO.succeed(Startup.Refuse(render(lock, found)))
                      case Right(version) =>
                        align(machine.running, home.path).map: (running, canonicalHome) =>
                          val matched: Candidate.Match = Candidate.Match(Origin.Cache, canonicalHome, version)
                          val selected = Resolution.Selected(matched, trace :+ matched)
                          startup(lock, running, canonicalHome, selected)
            case found @ Resolution.Selected(chosen, _) =>
              align(machine.running, chosen.home).map: (running, home) =>
                startup(lock, running, home, found)
            case found => ZIO.succeed(Startup.Refuse(render(lock, found)))

  def command(javaBin: Path, jar: Path, args: List[String]): List[String] =
    javaBin.toString :: NativeAccess :: "-jar" :: jar.toString :: args

  def replace(javaBin: Path, jar: Path, args: List[String]): IO[ToolchainError, Int] =
    ZIO.attemptBlockingInterrupt {
      val process = new ProcessBuilder(command(javaBin, jar, args).asJava).inheritIO()
      process.environment().put("HISTOS_JDK_SETTLED", "1")
      process.start().waitFor()
    }.mapError(_ => ToolchainError.Reexec)

  private def startup(lock: JdkLock, running: Option[Path], home: Path, resolution: Resolution): Startup =
    Launch.decide(running, home, resolution) match
      case Launch.Stay(_)            => Startup.Continue
      case Launch.Reexec(javaBin, _) => Startup.Replace(javaBin)
      case Launch.Halt(_)            => Startup.Refuse(render(lock, resolution))

  private def align(running: Option[Path], home: Path): IO[Nothing, (Option[Path], Path)] =
    for
      canonicalHome <- canonical(home)
      canonicalRun  <- ZIO.foreach(running)(canonical)
    yield (canonicalRun, canonicalHome)

  private def canonical(path: Path): IO[Nothing, Path] =
    ZIO.attempt(path.toRealPath().normalize).orElseSucceed(path.normalize)

  private def resolution(machine: Machine, lock: JdkLock): IO[ToolchainError, Resolution] =
    for
      explicit <- readOverride(machine.repo)
      input    <- JdkLocations.probe(machine, explicit).mapError(ToolchainError.Probe(_))
      view     <- Jdk.scan(input).mapError(ToolchainError.Probe(_))
    yield Jdk.resolve(lock.major, view)

  def render(lock: JdkLock, resolution: Resolution): String =
    val tail = resolution match
      case Resolution.Provision(_, _) => s"archive ${JdkLock.url(lock.source)}"
      case _                          => s"lock ${lock.version.value}"
    Jdk.report(resolution) + "\n" + tail

  private def readLock(repo: Path): IO[ToolchainError, Option[JdkLock]] =
    val file = lockPath(repo)
    ZIO.attempt(Files.isRegularFile(file)).mapError(_ => ToolchainError.Unreadable(file)).flatMap:
      case false => ZIO.none
      case true =>
        ZIO.attempt(Files.readString(file)).mapError(_ => ToolchainError.Unreadable(file)).flatMap: text =>
          ZIO.fromEither(JdkLock.parse(text)).mapBoth(ToolchainError.Lock(_), Some(_))

  private def readOverride(repo: Path): IO[ToolchainError, Option[Path]] =
    val file = repo.resolve(".histos").resolve("toolchain.local")
    ZIO.attempt(Files.isRegularFile(file)).mapError(_ => ToolchainError.Unreadable(file)).flatMap:
      case false => ZIO.none
      case true =>
        ZIO.attempt(Files.readString(file)).mapError(_ => ToolchainError.Unreadable(file)).flatMap: text =>
          ZIO.fromEither(parseOverride(text)).mapBoth(ToolchainError.Override(_), Some(_))

  private def parseOverride(text: String): Either[OverrideError, Path] =
    val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    lines match
      case Nil => Left(OverrideError.Empty)
      case raw :: Nil =>
        try
          val path = Path.of(raw)
          if path.isAbsolute then Right(path) else Left(OverrideError.Relative(raw))
        catch case _: java.nio.file.InvalidPathException => Left(OverrideError.Invalid(raw))
      case _ => Left(OverrideError.Multiple)

  private def lockPath(repo: Path): Path =
    repo.resolve(".histos").resolve("jdk.lock")
end Toolchain
