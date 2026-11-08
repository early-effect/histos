package histos.publish

import histos.version.{PinLag, Revision, ShipNumber, SnapshotCoordinate, SnapshotPointer}
import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, LinkOption, Path}
import java.nio.file.attribute.BasicFileAttributes

import scala.concurrent.duration.*

import zio.{IO, ZIO}

/** The version the next boot runs. A release is a ship number. A pointer follows the snapshot. A revision is one
  * timestamp.
  */
final class LauncherPin private (val choice: LauncherChoice):
  def version: String = choice match
    case LauncherChoice.Release(number)       => number.render
    case LauncherChoice.Pointer(channel)      => channel.render
    case LauncherChoice.Revision(_, revision) => revision

  def shown: String = s"${LauncherPin.Key}=${version}"
end LauncherPin

/** The snapshot a pin follows: the line's pointer, or one commit's stored coordinate. */
enum LauncherChannel:
  case Line(pointer: SnapshotPointer)
  case Commit(coordinate: SnapshotCoordinate)

  def line: ShipNumber = this match
    case Line(pointer)      => pointer.ship
    case Commit(coordinate) => coordinate.ship

  def render: String = this match
    case Line(pointer)      => pointer.render
    case Commit(coordinate) => coordinate.render
end LauncherChannel

object LauncherChannel:
  def from(raw: String): Option[LauncherChannel] =
    Revision.parse(raw) match
      case Right(Revision.Pointer(pointer))     => Some(Line(pointer))
      case Right(Revision.Snapshot(coordinate)) => Some(Commit(coordinate))
      case _                                    => None

/** `revision` is the timestamp the shim reads: `<channel without -SNAPSHOT>-<yyyyMMdd.HHmmss>-<build>`. */
enum LauncherChoice:
  case Release(number: ShipNumber)
  case Pointer(channel: LauncherChannel)
  case Revision(channel: LauncherChannel, revision: String)

  /** The release line this pin runs. A snapshot's line is the release it leads to. */
  def line: ShipNumber = this match
    case Release(number)      => number
    case Pointer(channel)     => channel.line
    case Revision(channel, _) => channel.line

  /** A release pin trails a newer release. A snapshot pin trails a release of its line or a later one. */
  def lag(published: ShipNumber): Option[PinLag] =
    val order = Ordering[ShipNumber]
    this match
      case Release(number) =>
        Option.when(order.gt(published, number))(PinLag.NewerRelease(number, published))
      case Pointer(_) | Revision(_, _) =>
        Option.when(order.gteq(published, line))(PinLag.LineReleased(line, published))
end LauncherChoice

object LauncherPin:
  val file: Path = Path.of("build.properties")
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
            case None                         => Left(LauncherError.Line(line))
            case Some((key, _)) if key != Key =>
              Left(LauncherError.Unknown(key))
            case Some((_, _)) if found.nonEmpty => Left(LauncherError.Duplicate)
            case Some((_, value))               => Right(Some(value))
      .flatMap:
        case None        => Left(LauncherError.Missing)
        case Some(value) => from(value)

  def from(raw: String): Either[LauncherError, LauncherPin] =
    LauncherChannel
      .from(raw)
      .map(LauncherChoice.Pointer(_))
      .orElse(revisionOf(raw))
      .orElse(ShipNumber.from(raw).toOption.map(LauncherChoice.Release(_)))
      .map(new LauncherPin(_))
      .toRight(LauncherError.Version(raw))

  def render(pin: LauncherPin): String = s"${pin.shown}\n"

  def read(repo: Path): IO[LauncherError, LauncherPin] =
    readString(repo.resolve(file)).flatMap(text => ZIO.fromEither(parse(text)))

  /** Resolve the channel and write the timestamp Coursier selected. */
  def update(repo: Path): IO[LauncherError, LauncherPin] =
    ZIO
      .attempt(coursier.cache.CacheDefaults.location.toPath)
      .orElseFail(LauncherError.Io(IoFailure(Path.of("coursier-cache"), IoFault.Stat)))
      .flatMap(cache => updateAt(repo, cache))

  /** Write the snapshot pointer for the channel already in the file. A release pin stays. Does not fetch. */
  def latest(repo: Path): IO[LauncherError, LauncherPin] =
    for
      current <- read(repo)
      pin = current.choice match
        case LauncherChoice.Revision(channel, _)                   => new LauncherPin(LauncherChoice.Pointer(channel))
        case LauncherChoice.Release(_) | LauncherChoice.Pointer(_) => current
      _ <- write(repo, pin)
    yield pin

  private[publish] def updateAt(repo: Path, cache: Path): IO[LauncherError, LauncherPin] =
    read(repo).flatMap: current =>
      current.choice match
        case LauncherChoice.Release(number)      => ZIO.fail(LauncherError.Release(number.render))
        case LauncherChoice.Pointer(channel)     => refresh(repo, channel, cache)
        case LauncherChoice.Revision(channel, _) => refresh(repo, channel, cache)

  private def refresh(repo: Path, channel: LauncherChannel, cache: Path): IO[LauncherError, LauncherPin] =
    for
      base     <- repository
      selected <- fetch(base, channel.render, cache)
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

  private[publish] def validRevision(version: String, raw: String): Boolean =
    val prefix = version.stripSuffix("-SNAPSHOT") + "-"
    val tail   = raw.stripPrefix(prefix)
    raw.startsWith(prefix) && "[0-9]{8}\\.[0-9]{6}-[1-9][0-9]*".r.matches(tail) &&
    tail.drop(16).toIntOption.exists(_ > 0) && validStamp(tail.take(15))

  private def revisionOf(raw: String): Option[LauncherChoice] =
    raw match
      case Stamp(ship, _, _) =>
        LauncherChannel
          .from(s"$ship-SNAPSHOT")
          .filter(channel => validRevision(channel.render, raw))
          .map(LauncherChoice.Revision(_, raw))
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

  /** `Fetch.run` is the documented call. `withRepositories` drops Central and ivy2 local. TTL 0 rechecks the snapshot.
    */
  private def fetch(repository: String, channel: String, cache: Path): IO[LauncherError, String] =
    ZIO
      .attemptBlockingInterrupt:
        val files = coursier.cache
          .FileCache()
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
        coursier
          .Fetch()
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
