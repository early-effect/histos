package histos.jdk

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

opaque type JdkMajor = Int

object JdkMajor:
  given CanEqual[JdkMajor, JdkMajor] = CanEqual.derived

  def from(raw: Int): Either[JdkError, JdkMajor] =
    if raw > 0 then Right(raw) else Left(JdkError.BadMajor(raw))

  /** A positive major the caller already checked. */
  private[histos] def trusted(raw: Int): JdkMajor = raw

  extension (major: JdkMajor) def value: Int = major

/** A release-file version. The major is part of the value, so reading it cannot fail. */
final case class JdkVersion private (value: String, major: JdkMajor)

object JdkVersion:
  def from(raw: String): Either[VersionError, JdkVersion] =
    majorOf(raw) match
      case Some(major) if charset(raw) => Right(JdkVersion(raw, major))
      case _                            => Left(VersionError.Bad(raw))

  def fromRelease(text: String): Either[ReleaseError, JdkVersion] =
    val key = "JAVA_VERSION=\""
    val at  = text.indexOf(key)
    if at < 0 then Left(ReleaseError.MissingKey)
    else
      val rest = text.substring(at + key.length)
      val end  = rest.indexOf('"')
      if end < 0 then Left(ReleaseError.Unclosed)
      else
        val raw = rest.substring(0, end)
        from(raw).left.map(_ => ReleaseError.BadVersion(raw))

  /** A version the caller already checked. The major has to be that version's major. */
  private[histos] def trusted(text: String, major: JdkMajor): JdkVersion = JdkVersion(text, major)

  private def majorOf(raw: String): Option[JdkMajor] =
    val body   = if raw.startsWith("1.") then raw.drop(2) else raw
    val digits = body.takeWhile(_.isDigit)
    digits.toIntOption.flatMap(n => JdkMajor.from(n).toOption)

  private def charset(raw: String): Boolean =
    raw.nonEmpty && raw.forall(c => c.isLetterOrDigit || c == '.' || c == '+' || c == '-' || c == '_')

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
    case NoFile           => "no release file"
    case MissingKey       => "release file has no JAVA_VERSION"
    case Unclosed         => "release file has an unclosed JAVA_VERSION"
    case BadVersion(raw)  => s"release file has a bad JAVA_VERSION: $raw"

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

final case class JdkHome(path: Path, hasJavac: Boolean, version: Either[ReleaseError, JdkVersion])

/** `Unset` was not configured. `Missing` was configured and is not a directory. */
enum Located:
  case Unset
  case Missing(path: Path)
  case Found(home: JdkHome)

/** `Absent` means the manager is not installed. `Present` means it is, even with no JDKs. */
enum ManagerSlot:
  case Absent
  case Present(homes: List[JdkHome])

/** `Cached` is a manager whose binary is not on `PATH`. It counts only when its cache holds a JDK. */
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
    cached: List[JdkHome],
)

final case class ScanInput(
    explicit: Option[Path],
    javaHome: Option[Path],
    onPath: Option[Path],
    coursier: ManagerProbe,
    sdkman: ManagerProbe,
    cache: Path,
)

enum JdkError:
  case BadMajor(raw: Int)
  case Unreadable(path: Path)
  case PathScan

  def message: String = this match
    case BadMajor(raw)    => s"$raw is not a JDK major"
    case Unreadable(path) => s"could not read $path"
    case PathScan         => "could not read PATH"

object Jdk:
  private val MaxDepth: Int = 12

  def resolve(major: JdkMajor, view: ProbeView): Resolution =
    slot(Origin.Override, view.explicit, major, Nil, stop = true) match
      case Left(done) => done
      case Right(afterOverride) =>
        val continued =
          for
            afterHome <- slot(Origin.JavaHome, view.javaHome, major, afterOverride, stop = false)
            afterPath <- slot(Origin.Path, view.onPath, major, afterHome, stop = false)
            afterCs   <- manager(Origin.Coursier, view.coursier, major, afterPath)
            afterSdk  <- manager(Origin.Sdkman, view.sdkman, major, afterCs)
          yield afterSdk
        continued match
          case Left(done) => done
          case Right(seen) =>
            if managerPresent(view) then hint(view, major, seen)
            else
              homes(Origin.Cache, view.cached, major, seen) match
                case Left(done)  => done
                case Right(done) => Resolution.Provision(major, done)

  def scan(input: ScanInput): IO[JdkError, ProbeView] =
    for
      explicit <- locate(input.explicit)
      javaHome <- locate(input.javaHome)
      onPath   <- locate(input.onPath)
      coursier <- managers(input.coursier)
      sdkman   <- managers(input.sdkman)
      cached   <- homes(input.cache)
    yield ProbeView(explicit, javaHome, onPath, coursier, sdkman, cached)

  def homes(root: Path): IO[JdkError, List[JdkHome]] = listHomes(root)

  def report(resolution: Resolution): String =
    val lines = resolution match
      case Resolution.Selected(chosen, trace) =>
        trace.map(show) :+ s"chosen ${chosen.origin} ${chosen.version.value} ${chosen.home}"
      case Resolution.OverrideRejected(reason, trace) =>
        trace.map(show) :+ s"override rejected ${showReason(reason)}"
      case Resolution.Install(hint, trace) =>
        trace.map(show) :+ s"install ${hint.command}"
      case Resolution.Provision(major, trace) =>
        trace.map(show) :+ s"provision temurin ${major.value}"
    lines.mkString("\n")

  /** `Left` is a finished resolution. `Right` is the trace so far. */
  private def slot(
      origin: Origin,
      located: Located,
      wanted: JdkMajor,
      trace: List[Candidate],
      stop: Boolean,
  ): Either[Resolution, List[Candidate]] =
    located match
      case Located.Unset => Right(trace :+ Candidate.NotPresent(origin))
      case Located.Missing(_) =>
        val next = trace :+ Candidate.Rejected(origin, Reject.Absent)
        if stop then Left(Resolution.OverrideRejected(Reject.Absent, next)) else Right(next)
      case Located.Found(home) =>
        judge(origin, wanted, home) match
          case chosen @ Candidate.Match(_, _, _) => Left(Resolution.Selected(chosen, trace :+ chosen))
          case Candidate.Rejected(_, reason) if stop =>
            Left(Resolution.OverrideRejected(reason, trace :+ Candidate.Rejected(origin, reason)))
          case other => Right(trace :+ other)

  private def manager(
      origin: Origin,
      slot: ManagerSlot,
      wanted: JdkMajor,
      trace: List[Candidate],
  ): Either[Resolution, List[Candidate]] =
    slot match
      case ManagerSlot.Absent         => Right(trace :+ Candidate.NotPresent(origin))
      case ManagerSlot.Present(Nil)   => Right(trace :+ Candidate.Empty(origin))
      case ManagerSlot.Present(found) => homes(origin, found, wanted, trace)

  private def homes(
      origin: Origin,
      installed: List[JdkHome],
      wanted: JdkMajor,
      trace: List[Candidate],
  ): Either[Resolution, List[Candidate]] =
    if installed.isEmpty then Right(trace :+ Candidate.NotPresent(origin))
    else
      val judged = installed.map(home => judge(origin, wanted, home))
      judged.collectFirst { case chosen @ Candidate.Match(_, _, _) => chosen } match
        case Some(chosen) => Left(Resolution.Selected(chosen, trace ++ judged))
        case None         => Right(trace ++ judged)

  private def managerPresent(view: ProbeView): Boolean =
    view.coursier match
      case ManagerSlot.Present(_) => true
      case ManagerSlot.Absent =>
        view.sdkman match
          case ManagerSlot.Present(_) => true
          case ManagerSlot.Absent     => false

  private def hint(view: ProbeView, major: JdkMajor, trace: List[Candidate]): Resolution =
    view.coursier match
      case ManagerSlot.Present(_) => Resolution.Install(InstallHint.Coursier(major), trace)
      case ManagerSlot.Absent     => Resolution.Install(InstallHint.Sdkman(major), trace)

  private def judge(origin: Origin, wanted: JdkMajor, home: JdkHome): Candidate =
    if !home.hasJavac then Candidate.Rejected(origin, Reject.NoJavac)
    else
      home.version match
        case Left(error)                                    => Candidate.Rejected(origin, Reject.UnreadableVersion(error))
        case Right(version) if version.major == wanted      => Candidate.Match(origin, home.path, version)
        case Right(version)                                 => Candidate.Rejected(origin, Reject.WrongMajor(version.major))

  private def show(candidate: Candidate): String =
    candidate match
      case Candidate.Match(origin, home, version) => s"match $origin ${version.value} $home"
      case Candidate.Rejected(origin, reason)     => s"reject $origin ${showReason(reason)}"
      case Candidate.NotPresent(origin)           => s"absent $origin"
      case Candidate.Empty(origin)                => s"empty $origin"

  private def showReason(reason: Reject): String =
    reason match
      case Reject.Absent                    => "absent"
      case Reject.NoJavac                   => "no javac"
      case Reject.WrongMajor(found)         => s"major ${found.value}"
      case Reject.UnreadableVersion(error)  => error.message

  private def locate(path: Option[Path]): IO[JdkError, Located] =
    path match
      case None => ZIO.succeed(Located.Unset)
      case Some(dir) =>
        ZIO.attempt(Files.isDirectory(dir)).mapError(_ => JdkError.Unreadable(dir)).flatMap:
          case false => ZIO.succeed(Located.Missing(dir))
          case true  => readHome(dir).map(Located.Found(_))

  private def managers(probe: ManagerProbe): IO[JdkError, ManagerSlot] =
    probe match
      case ManagerProbe.Absent              => ZIO.succeed(ManagerSlot.Absent)
      case ManagerProbe.Installed(None)     => ZIO.succeed(ManagerSlot.Present(Nil))
      case ManagerProbe.Installed(Some(dir)) => listHomes(dir).map(ManagerSlot.Present(_))
      case ManagerProbe.Cached(dir) =>
        listHomes(dir).map:
          case Nil   => ManagerSlot.Absent
          case found => ManagerSlot.Present(found)

  private def listHomes(root: Path): IO[JdkError, List[JdkHome]] =
    ZIO.attempt {
      if !Files.isDirectory(root) then Nil
      else
        val walk = Files.walk(root, MaxDepth)
        try
          val found = walk.iterator.asScala.filter(dir => Files.isDirectory(dir) && isHome(dir)).toList
          shallow(distinct(found))
        finally walk.close()
    }.mapError(_ => JdkError.Unreadable(root)).flatMap(dirs => ZIO.foreach(dirs)(readHome))

  private def isHome(dir: Path): Boolean =
    Files.isRegularFile(dir.resolve("bin").resolve("javac"))

  private def distinct(dirs: List[Path]): List[Path] =
    dirs.foldLeft((List.empty[Path], Set.empty[String])):
      case ((kept, seen), dir) =>
        val key =
          try dir.toRealPath().normalize.toString
          catch case _: java.io.IOException => dir.normalize.toString
        if seen.contains(key) then (kept, seen) else (kept :+ dir, seen + key)
    ._1

  private def shallow(dirs: List[Path]): List[Path] =
    dirs.sortBy(path => (path.getNameCount, path.toString)).foldLeft(List.empty[Path]): (kept, dir) =>
      val normal = dir.normalize
      if kept.exists(parent => normal.startsWith(parent.normalize)) then kept else kept :+ dir

  private def readHome(dir: Path): IO[JdkError, JdkHome] =
    ZIO.attempt {
      val javac   = Files.isRegularFile(dir.resolve("bin").resolve("javac"))
      val release = dir.resolve("release")
      val version =
        if Files.isRegularFile(release) then JdkVersion.fromRelease(Files.readString(release))
        else Left(ReleaseError.NoFile)
      JdkHome(dir, javac, version)
    }.mapError(_ => JdkError.Unreadable(dir))
end Jdk
