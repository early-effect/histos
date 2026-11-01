package histos.publish

import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, LinkOption, Path}
import java.nio.file.attribute.BasicFileAttributes

import scala.concurrent.duration.*

import zio.{IO, ZIO}

/** The version the next boot runs. A pointer follows the snapshot. A revision is one timestamp. */
final class LauncherPin private (val choice: LauncherChoice):
  def version: String = choice match
    case LauncherChoice.Pointer(channel)     => channel
    case LauncherChoice.Revision(_, revision) => revision

  def channel: String = choice match
    case LauncherChoice.Pointer(channel)    => channel
    case LauncherChoice.Revision(channel, _) => channel

  def shown: String = s"${LauncherPin.Key}=${version}"

enum LauncherChoice:
  case Pointer(channel: String)
  case Revision(channel: String, revision: String)

object LauncherPin:
  val file: Path = Path.of(".histos", "build.properties")
  val Key        = "histos.version"

  val Snapshots: String = "https://central.sonatype.com/repository/maven-snapshots"

  def parse(text: String): Either[LauncherError, LauncherPin] =
    val lines = text.linesIterator
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .toList
    lines
      .foldLeft[Either[LauncherError, Option[String]]](Right(None)): (acc, line) =>
        acc.flatMap: found =>
          field(line) match
            case None                    => Left(LauncherError.Line(line))
            case Some((key, _)) if key != Key =>
              Left(LauncherError.Unknown(key))
            case Some((_, _)) if found.nonEmpty => Left(LauncherError.Duplicate)
            case Some((_, value))               => Right(Some(value))
      .flatMap:
        case None        => Left(LauncherError.Missing)
        case Some(value) => from(value)

  def from(raw: String): Either[LauncherError, LauncherPin] =
    if validPointer(raw) then Right(new LauncherPin(LauncherChoice.Pointer(raw)))
    else
      channelOf(raw) match
        case Some(channel) => Right(new LauncherPin(LauncherChoice.Revision(channel, raw)))
        case None          => Left(LauncherError.Version(raw))

  /** The snapshot channel a pointer or a timestamp belongs to. */
  def channel(raw: String): Option[String] =
    if validPointer(raw) then Some(raw) else channelOf(raw)

  def render(pin: LauncherPin): String = s"${pin.shown}\n"

  def read(repo: Path): IO[LauncherError, LauncherPin] =
    readString(repo.resolve(file)).flatMap(text => ZIO.fromEither(parse(text)))

  /** Resolve the channel and write the timestamp Coursier selected. */
  def update(repo: Path): IO[LauncherError, LauncherPin] =
    ZIO
      .attempt(coursier.cache.CacheDefaults.location.toPath)
      .orElseFail(LauncherError.Io(IoFailure(Path.of("coursier-cache"), IoFault.Stat)))
      .flatMap(cache => updateAt(repo, cache))

  /** Write the snapshot pointer for the channel already in the file. Does not fetch. */
  def latest(repo: Path): IO[LauncherError, LauncherPin] =
    for
      current <- read(repo)
      pin     <- ZIO.fromEither(from(current.channel))
      _       <- write(repo, pin)
    yield pin

  private[publish] def updateAt(repo: Path, cache: Path): IO[LauncherError, LauncherPin] =
    for
      current  <- read(repo)
      base     <- repository
      selected <- fetch(base, current.channel, cache)
      pin      <- ZIO.fromEither(from(selected))
      _        <- write(repo, pin)
    yield pin

  def renderBase(raw: String): Either[LauncherError, String] =
    val supported =
      raw.startsWith("https://") ||
        raw.startsWith("http://127.0.0.1") ||
        raw.startsWith("http://localhost") ||
        raw.startsWith("file:///")
    if supported && raw.nonEmpty && !raw.exists(_.isWhitespace) then Right(raw)
    else Left(LauncherError.Repository(raw))

  private[publish] def validPointer(raw: String): Boolean =
    "[0-9]+\\.[0-9]+\\.[0-9]+(-[A-Za-z0-9]+([.-][A-Za-z0-9]+)*)?-SNAPSHOT".r.matches(raw)

  private[publish] def validRevision(version: String, raw: String): Boolean =
    val prefix = version.stripSuffix("-SNAPSHOT") + "-"
    val tail   = raw.stripPrefix(prefix)
    raw.startsWith(prefix) && "[0-9]{8}\\.[0-9]{6}-[1-9][0-9]*".r.matches(tail) &&
    tail.drop(16).toIntOption.exists(_ > 0) && validStamp(tail.take(15))

  private def channelOf(raw: String): Option[String] =
    raw match
      case Stamp(ship, stamp, build) =>
        val channel  = s"$ship-SNAPSHOT"
        val revision = s"$ship-$stamp-$build"
        if validPointer(channel) && validRevision(channel, revision) then Some(channel) else None
      case _ => None

  private val Stamp = "^(.+)-([0-9]{8}\\.[0-9]{6})-([1-9][0-9]*)$".r

  private def validStamp(raw: String): Boolean =
    try
      val format = java.time.format.DateTimeFormatter
        .ofPattern("uuuuMMdd.HHmmss")
        .withResolverStyle(java.time.format.ResolverStyle.STRICT)
      java.time.LocalDateTime.parse(raw, format).getYear > 0
    catch case _: java.time.format.DateTimeParseException => false

  private def field(line: String): Option[(String, String)] =
    val split = line.indexOf('=')
    if split <= 0 then None
    else
      val key   = line.take(split).trim
      val value = line.drop(split + 1).trim
      if key.nonEmpty && value.nonEmpty then Some((key, value)) else None

  private def repository: IO[LauncherError, String] =
    ZIO
      .systemWith(_.env("HISTOS_LAUNCHER_REPO"))
      .mapError(_ => LauncherError.Io(IoFailure(Path.of("HISTOS_LAUNCHER_REPO"), IoFault.Read)))
      .flatMap:
        case Some(raw) if raw.nonEmpty => ZIO.fromEither(renderBase(raw))
        case _                         => ZIO.succeed(Snapshots)

  /** `Fetch.run` is the documented call. `withRepositories` drops Central and ivy2 local. TTL 0 rechecks the snapshot. */
  private def fetch(repository: String, channel: String, cache: Path): IO[LauncherError, String] =
    ZIO
      .attemptBlockingInterrupt:
        val files = coursier.cache.FileCache()
          .withLocation(cache.toString)
          .withTtl(0.seconds)
        val dependency = coursier
          .Dependency(
            coursier.Module(
              coursier.Organization("rocks.earlyeffect"),
              coursier.ModuleName("histos"),
            ),
            coursier.version.VersionConstraint(channel),
          )
          .copy(transitive = false)
        coursier.Fetch()
          .addDependencies(dependency)
          .withRepositories(Seq(coursier.MavenRepository(repository)))
          .withCache(files)
          .run()
      .mapError(error => LauncherError.Resolve(shownFailure(error)))
      .flatMap(files => ZIO.fromEither(selected(files.toList, channel)))

  private def selected(files: List[java.io.File], channel: String): Either[LauncherError, String] =
    val jars = files.map(_.getName).filter(launcherJar)
    jars match
      case name :: Nil =>
        val revision = name.stripPrefix("histos-").stripSuffix(".jar")
        if validRevision(channel, revision) then Right(revision)
        else Left(LauncherError.Resolve(s"launcher artifact is not $channel: $name"))
      case _ => Left(LauncherError.Resolve("launcher fetch did not return one jar"))

  private def launcherJar(name: String): Boolean =
    name.startsWith("histos-") && name.endsWith(".jar") && !name.contains("-sources") && !name.contains("-javadoc")

  private def shownFailure(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(error.toString)

  private def write(repo: Path, pin: LauncherPin): IO[LauncherError, Unit] =
    val path = repo.resolve(file)
    ZIO
      .attemptBlocking:
        Option(path.getParent).foreach(Files.createDirectories(_))
        Files.writeString(path, render(pin))
      .mapError(_ => LauncherError.Io(IoFailure(path, IoFault.Write)))
      .unit

  private def readString(path: Path): IO[LauncherError, String] =
    val noFollow = LinkOption.NOFOLLOW_LINKS
    ZIO
      .attemptBlocking:
        if !Files.readAttributes(path, classOf[BasicFileAttributes], noFollow).isRegularFile &&
          !Files.isSymbolicLink(path)
        then Left(LauncherError.InvalidPath(path))
        else if !Files.isRegularFile(path) || !Files.isReadable(path) then Left(LauncherError.InvalidPath(path))
        else Right(Files.readString(path))
      .mapError {
        case _: java.nio.file.NoSuchFileException => LauncherError.Absent(path)
        case _                                    => LauncherError.Io(IoFailure(path, IoFault.Read))
      }
      .flatMap(ZIO.fromEither)
end LauncherPin

enum LauncherError:
  case Line(raw: String)
  case Unknown(raw: String)
  case Duplicate
  case Missing
  case Version(raw: String)
  case Repository(raw: String)
  case Resolve(detail: String)
  case Io(error: IoFailure)
  case InvalidPath(path: Path)
  case Absent(path: Path)

  def message: String = this match
    case Line(raw)        => s"launcher properties line is not a field: $raw"
    case Unknown(raw)     => s"launcher properties field is unknown: $raw"
    case Duplicate        => s"launcher properties repeat ${LauncherPin.Key}"
    case Missing          => s"launcher properties are missing ${LauncherPin.Key}"
    case Version(raw)     =>
      s"launcher version is not a snapshot pointer or a timestamped revision: $raw"
    case Repository(raw)  => s"launcher repository is not a supported base: $raw"
    case Resolve(detail)  => detail
    case Io(error)        => error.message
    case InvalidPath(path) =>
      s"launcher path is not a readable regular file: $path"
    case Absent(path) => s"launcher pin is absent: $path"
end LauncherError
