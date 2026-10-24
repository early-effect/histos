package histos.jdk

import java.nio.file.{Files, Path}

import histos.toolchain.{RuntimeFiles, RuntimeLocationError, ToolSelection}

import zio.{IO, ZIO}

opaque type JdkMajor = Int

object JdkMajor:
  given CanEqual[JdkMajor, JdkMajor] = CanEqual.derived

  def from(raw: Int): Either[JdkError, JdkMajor] =
    if raw > 0 then Right(raw) else Left(JdkError.BadMajor(raw))

  /** A positive major the caller already checked. */
  private[histos] def trusted(raw: Int): JdkMajor = raw

  extension (major: JdkMajor) def value: Int = major

/** A release-file version. The major is part of the value, so reading it cannot
  * fail.
  */
final case class JdkVersion private (value: String, major: JdkMajor)

object JdkVersion:
  def from(raw: String): Either[VersionError, JdkVersion] =
    majorOf(raw) match
      case Some(major) if charset(raw) => Right(JdkVersion(raw, major))
      case _                           => Left(VersionError.Bad(raw))

  def fromRelease(text: String): Either[ReleaseError, JdkVersion] =
    val key = "JAVA_VERSION=\""
    val at = text.indexOf(key)
    if at < 0 then Left(ReleaseError.MissingKey)
    else
      val rest = text.substring(at + key.length)
      val end = rest.indexOf('"')
      if end < 0 then Left(ReleaseError.Unclosed)
      else
        val raw = rest.substring(0, end)
        from(raw).left.map(_ => ReleaseError.BadVersion(raw))

  /** A version the caller already checked. The major has to be that version's
    * major.
    */
  private[histos] def trusted(text: String, major: JdkMajor): JdkVersion =
    JdkVersion(text, major)

  private def majorOf(raw: String): Option[JdkMajor] =
    val body = if raw.startsWith("1.") then raw.drop(2) else raw
    val digits = body.takeWhile(_.isDigit)
    digits.toIntOption.flatMap(n => JdkMajor.from(n).toOption)

  private def charset(raw: String): Boolean =
    raw.matches("[0-9][A-Za-z0-9.+_-]*")

enum VersionError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"not a JDK version: $raw"

enum ReleaseError:
  case NoFile
  case MissingKey
  case Unclosed
  case BadVersion(raw: String)

  def message: String = this match
    case NoFile          => "no release file"
    case MissingKey      => "release file has no JAVA_VERSION"
    case Unclosed        => "release file has an unclosed JAVA_VERSION"
    case BadVersion(raw) => s"release file has a bad JAVA_VERSION: $raw"

enum Origin:
  case Override, JavaHome, Path, Coursier, Sdkman, Cache

enum Reject:
  case Absent
  case NoJavac
  case WrongMajor(found: JdkMajor)
  case UnreadableVersion(error: ReleaseError)

enum Candidate:
  case Match(origin: Origin, home: Path, version: JdkVersion)
  case Rejected(origin: Origin, reason: Reject)
  case NotPresent(origin: Origin)
  case Empty(origin: Origin)

enum InstallHint:
  case Coursier(major: JdkMajor)
  case Sdkman(major: JdkMajor)

  def command: String = this match
    case Coursier(major) => s"cs java-home --jvm temurin:${major.value}"
    case Sdkman(_)       => "sdk list java"

enum Resolution:
  case Selected(chosen: Candidate.Match, trace: List[Candidate])
  case OverrideRejected(reason: Reject, trace: List[Candidate])
  case Install(hint: InstallHint, trace: List[Candidate])
  case Provision(major: JdkMajor, trace: List[Candidate])

final case class JdkHome(
    path: Path,
    hasJavac: Boolean,
    version: Either[ReleaseError, JdkVersion]
)

/** `Unset` was not configured. `Missing` was configured and is not a directory.
  */
enum Located:
  case Unset
  case Missing(path: Path)
  case Found(home: JdkHome)

/** `Absent` means the manager is not installed. `Present` means it is, even
  * with no JDKs.
  */
enum ManagerSlot:
  case Absent
  case Present(homes: List[JdkHome])

/** `Cached` is a manager whose binary is not on `PATH`. It counts only when its
  * cache holds a JDK.
  */
enum ManagerProbe:
  case Absent
  case Installed(homesRoot: Option[Path])
  case Cached(homesRoot: Path)

final case class ProbeView(
    explicit: Located,
    javaHome: Located,
    onPath: Located,
    coursier: ManagerSlot,
    sdkman: ManagerSlot,
    cached: List[JdkHome]
)

final case class ScanInput(
    explicit: Option[Path],
    javaHome: Option[Path],
    onPath: Option[Path],
    coursier: ManagerProbe,
    sdkman: ManagerProbe,
    cache: Path
)

enum JdkError:
  case BadMajor(raw: Int)
  case Unreadable(path: Path)
  case PathScan
  case Locations(error: RuntimeLocationError)
  case Io(failure: histos.workspace.IoFailure)

  def message: String = this match
    case BadMajor(raw)    => s"$raw is not a JDK major"
    case Unreadable(path) => s"could not read $path"
    case PathScan         => "could not read PATH"
    case Locations(error) => error.message
    case Io(failure)      => failure.message

object Jdk:
  private val MaxDepth: Int = 12

  def resolve(major: JdkMajor, view: ProbeView): Resolution =
    val trace =
      located(Origin.Override, view.explicit, major) ++
        located(Origin.JavaHome, view.javaHome, major) ++
        located(Origin.Path, view.onPath, major) ++
        manager(Origin.Coursier, view.coursier, major) ++
        manager(Origin.Sdkman, view.sdkman, major) ++
        homes(Origin.Cache, view.cached, major)
    ToolSelection.fold(trace):
      case chosen @ Candidate.Match(Origin.Override, _, _) =>
        ToolSelection.Selected(chosen)
      case Candidate.Rejected(Origin.Override, reason) =>
        ToolSelection.Rejected(reason)
      case _ => ToolSelection.Locked
    match
      case ToolSelection.Selected(chosen) => Resolution.Selected(chosen, trace)
      case ToolSelection.Rejected(reason) =>
        Resolution.OverrideRejected(reason, trace)
      case ToolSelection.Locked => Resolution.Provision(major, trace)

  def scan(input: ScanInput): IO[JdkError, ProbeView] =
    for
      explicit <- locate(input.explicit)
      javaHome <- locate(input.javaHome)
      onPath <- locate(input.onPath)
      coursier <- managers(input.coursier)
      sdkman <- managers(input.sdkman)
      cached <- homes(input.cache)
    yield ProbeView(explicit, javaHome, onPath, coursier, sdkman, cached)

  def homes(root: Path): IO[JdkError, List[JdkHome]] = listHomes(root)

  def report(resolution: Resolution): String =
    val lines = resolution match
      case Resolution.Selected(chosen, trace) =>
        trace.map(
          show
        ) :+ s"chosen ${chosen.origin} ${chosen.version.value} ${chosen.home}"
      case Resolution.OverrideRejected(reason, trace) =>
        trace.map(show) :+ s"override rejected ${showReason(reason)}"
      case Resolution.Install(hint, trace) =>
        trace.map(show) :+ s"install ${hint.command}"
      case Resolution.Provision(major, trace) =>
        trace.map(show) :+ s"provision temurin ${major.value}"
    lines.mkString("\n")

  private def located(
      origin: Origin,
      location: Located,
      wanted: JdkMajor
  ): List[Candidate] =
    location match
      case Located.Unset      => List(Candidate.NotPresent(origin))
      case Located.Missing(_) => List(Candidate.Rejected(origin, Reject.Absent))
      case Located.Found(home) => List(judge(origin, wanted, home))

  private def manager(
      origin: Origin,
      slot: ManagerSlot,
      wanted: JdkMajor
  ): List[Candidate] =
    slot match
      case ManagerSlot.Absent         => List(Candidate.NotPresent(origin))
      case ManagerSlot.Present(Nil)   => List(Candidate.Empty(origin))
      case ManagerSlot.Present(found) => homes(origin, found, wanted)

  private def homes(
      origin: Origin,
      installed: List[JdkHome],
      wanted: JdkMajor
  ): List[Candidate] =
    if installed.isEmpty then List(Candidate.NotPresent(origin))
    else installed.map(judge(origin, wanted, _))

  private def judge(
      origin: Origin,
      wanted: JdkMajor,
      home: JdkHome
  ): Candidate =
    if !home.hasJavac then Candidate.Rejected(origin, Reject.NoJavac)
    else
      home.version match
        case Left(error) =>
          Candidate.Rejected(origin, Reject.UnreadableVersion(error))
        case Right(version) if version.major == wanted =>
          Candidate.Match(origin, home.path, version)
        case Right(version) =>
          Candidate.Rejected(origin, Reject.WrongMajor(version.major))

  private def show(candidate: Candidate): String =
    candidate match
      case Candidate.Match(origin, home, version) =>
        s"match $origin ${version.value} $home"
      case Candidate.Rejected(origin, reason) =>
        s"reject $origin ${showReason(reason)}"
      case Candidate.NotPresent(origin) => s"absent $origin"
      case Candidate.Empty(origin)      => s"empty $origin"

  private def showReason(reason: Reject): String =
    reason match
      case Reject.Absent                   => "absent"
      case Reject.NoJavac                  => "no javac"
      case Reject.WrongMajor(found)        => s"major ${found.value}"
      case Reject.UnreadableVersion(error) => error.message

  def locate(path: Option[Path]): IO[JdkError, Located] =
    path match
      case None      => ZIO.succeed(Located.Unset)
      case Some(dir) =>
        ZIO
          .attemptBlocking(Files.isDirectory(dir))
          .mapError(_ =>
            JdkError.Io(
              histos.workspace.IoFailure(dir, histos.workspace.IoFault.Stat)
            )
          )
          .flatMap:
            case false => ZIO.succeed(Located.Missing(dir))
            case true  => home(dir).map(Located.Found(_))

  private def managers(probe: ManagerProbe): IO[JdkError, ManagerSlot] =
    probe match
      case ManagerProbe.Absent          => ZIO.succeed(ManagerSlot.Absent)
      case ManagerProbe.Installed(None) => ZIO.succeed(ManagerSlot.Present(Nil))
      case ManagerProbe.Installed(Some(dir)) =>
        listHomes(dir).map(ManagerSlot.Present(_))
      case ManagerProbe.Cached(dir) =>
        listHomes(dir).map:
          case Nil   => ManagerSlot.Absent
          case found => ManagerSlot.Present(found)

  private def listHomes(root: Path): IO[JdkError, List[JdkHome]] =
    RuntimeFiles
      .homes(root, MaxDepth)(dir =>
        Files.isRegularFile(dir.resolve("bin/javac")) || Files.isRegularFile(
          dir.resolve("bin/javac.exe")
        )
      )
      .mapError(JdkError.Io(_))
      .flatMap(dirs => ZIO.foreach(dirs)(home))

  def home(dir: Path): IO[JdkError, JdkHome] =
    for
      javac <- ZIO
        .attemptBlocking(
          Files.isRegularFile(dir.resolve("bin/javac")) || Files.isRegularFile(
            dir.resolve("bin/javac.exe")
          )
        )
        .mapError(_ =>
          JdkError.Io(
            histos.workspace.IoFailure(dir, histos.workspace.IoFault.Stat)
          )
        )
      text <- RuntimeFiles
        .readIfPresent(dir.resolve("release"))
        .mapError(JdkError.Io(_))
    yield JdkHome(
      dir,
      javac,
      text.fold[Either[ReleaseError, JdkVersion]](Left(ReleaseError.NoFile))(
        JdkVersion.fromRelease
      )
    )
end Jdk
