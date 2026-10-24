package histos.publish

import histos.central.{SnapshotRepository, TransportFault}
import histos.jdk.{Sha256, Sha256Error}
import histos.archive.{RuntimeCache, RuntimeCacheError}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.io.StringReader
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{
  Files,
  LinkOption,
  Path,
  StandardCopyOption,
  StandardOpenOption
}
import java.nio.channels.{FileChannel, OverlappingFileLockException}
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.{Element, Node}
import org.xml.sax.{InputSource, SAXParseException}
import org.xml.sax.helpers.DefaultHandler

import heddle.client.Client
import heddle.http.{Request, Url}
import zio.{Chunk, IO, Scope, ZIO, ZLayer, durationInt}
import zio.stream.ZStream
import heddle.server.BodyBytes

/** The assembly digest is independent of this immutable remote launcher. */
final class LauncherPin private (
    val repository: SnapshotRepository,
    val version: String,
    val revision: String,
    val sha256: Sha256
):
  def directory: Url = List("rocks", "earlyeffect", "histos", version).foldLeft(
    repository.root
  )(_ / _)
  def artifact: Url = directory / s"histos-$revision.jar"

object LauncherPin:
  val file: Path = Path.of(".histos", "launcher.lock")

  def parse(text: String): Either[LauncherError, LauncherPin] =
    val lines = text.linesIterator
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .toList
    for
      fields <- lines
        .foldLeft[Either[LauncherError, Map[LauncherField, String]]](
          Right(Map.empty)
        ): (acc, line) =>
          acc.flatMap: found =>
            line.split("\\s+", 2).toList match
              case name :: value :: Nil if value.nonEmpty =>
                LauncherField
                  .named(name)
                  .flatMap: field =>
                    if found.contains(field) then
                      Left(LauncherError.Duplicate(field))
                    else Right(found.updated(field, value))
              case _ => Left(LauncherError.Line(line))
      repository <- fields
        .get(LauncherField.Repository)
        .toRight(LauncherError.Missing(LauncherField.Repository))
      version <- fields
        .get(LauncherField.Version)
        .toRight(LauncherError.Missing(LauncherField.Version))
      revision <- fields
        .get(LauncherField.Revision)
        .toRight(LauncherError.Missing(LauncherField.Revision))
      checksum <- fields
        .get(LauncherField.Sha256)
        .toRight(LauncherError.Missing(LauncherField.Sha256))
      pin <- from(repository, version, revision, checksum)
    yield pin

  def from(
      repository: String,
      version: String,
      revision: String,
      checksum: String
  ): Either[LauncherError, LauncherPin] =
    for
      repo <- endpoint(repository)
      _ <- Either.cond(
        validVersion(version),
        (),
        LauncherError.Version(version)
      )
      _ <- Either.cond(
        validRevision(version, revision),
        (),
        LauncherError.Revision(revision)
      )
      sha <-
        if "[0-9a-fA-F]{64}".r.matches(checksum.trim) then
          Sha256.from(checksum).left.map(LauncherError.Checksum(_))
        else Left(LauncherError.Checksum(Sha256Error.Bad(checksum)))
    yield new LauncherPin(repo, version, revision, sha)

  def render(pin: LauncherPin): String =
    s"""repository ${pin.repository.root.render}
       |version ${pin.version}
       |revision ${pin.revision}
       |sha256 ${pin.sha256.value}
       |""".stripMargin

  def read(repo: Path): IO[LauncherError, LauncherPin] =
    LauncherFiles
      .read(repo.resolve(file))
      .flatMap(text => ZIO.fromEither(parse(text)))

  /** Only this operation reads latest metadata. Script generation and startup
    * do not.
    */
  def update(repo: Path): IO[LauncherError, LauncherPin] =
    ZIO
      .serviceWithZIO[LauncherUpdater](_.update(repo))
      .provide(LauncherUpdater.layer)

  private def endpoint(raw: String): Either[LauncherError, SnapshotRepository] =
    LauncherUrl
      .repository(raw)
      .flatMap(_ =>
        SnapshotRepository
          .from(raw)
          .left
          .map(_ => LauncherError.Repository(raw))
      )

  private[publish] def validVersion(raw: String): Boolean =
    "[0-9]+\\.[0-9]+\\.[0-9]+(-[A-Za-z0-9]+([.-][A-Za-z0-9]+)*)?-SNAPSHOT".r
      .matches(raw)

  private[publish] def validRevision(version: String, raw: String): Boolean =
    val prefix = version.stripSuffix("-SNAPSHOT") + "-"
    val tail = raw.stripPrefix(prefix)
    raw.startsWith(prefix) && "[0-9]{8}\\.[0-9]{6}-[1-9][0-9]*".r.matches(
      tail
    ) &&
    tail.drop(16).toIntOption.exists(_ > 0) && validStamp(tail.take(15))

  private def validStamp(raw: String): Boolean =
    try
      val format = java.time.format.DateTimeFormatter
        .ofPattern("uuuuMMdd.HHmmss")
        .withResolverStyle(java.time.format.ResolverStyle.STRICT)
      java.time.LocalDateTime.parse(raw, format).getYear > 0
    catch case _: java.time.format.DateTimeParseException => false

enum LauncherField:
  case Repository, Version, Revision, Sha256

  def name: String = this match
    case Repository => "repository"
    case Version    => "version"
    case Revision   => "revision"
    case Sha256     => "sha256"

object LauncherField:
  def named(raw: String): Either[LauncherError, LauncherField] = raw match
    case "repository" => Right(LauncherField.Repository)
    case "version"    => Right(LauncherField.Version)
    case "revision"   => Right(LauncherField.Revision)
    case "sha256"     => Right(LauncherField.Sha256)
    case _            => Left(LauncherError.Unknown(raw))

enum LauncherMetadataError:
  case Xml, Coordinates, Snapshot, Jar
  case Missing(name: String)
  case Duplicate(name: String)

enum LauncherError:
  case Line(raw: String)
  case Unknown(raw: String)
  case Duplicate(field: LauncherField)
  case Missing(field: LauncherField)
  case Repository(raw: String)
  case Version(raw: String)
  case Revision(raw: String)
  case Checksum(error: Sha256Error)
  case Mismatch(expected: Sha256, actual: Sha256)
  case Metadata(error: LauncherMetadataError)
  case Status(url: String, code: Int)
  case Transport(error: TransportFault)
  case Io(error: IoFailure)
  case InvalidPath(path: Path)
  case Absent(path: Path)
  case Size(kind: LauncherPayload, limit: Long)
  case Conflict(original: String, current: String)
  case Cache(path: Path)

  def message: String = this match
    case Line(raw)        => s"launcher lock line is not a field: $raw"
    case Unknown(raw)     => s"launcher lock field is unknown: $raw"
    case Duplicate(field) => s"launcher lock repeats ${field.name}"
    case Missing(field)   => s"launcher lock is missing ${field.name}"
    case Repository(raw)  =>
      s"launcher repository is not https or loopback http: $raw"
    case Version(raw)  => s"launcher version is not a snapshot coordinate: $raw"
    case Revision(raw) =>
      s"launcher revision is not an exact timestamped snapshot: $raw"
    case Checksum(error)            => error.message
    case Mismatch(expected, actual) =>
      s"launcher checksum mismatch: expected ${expected.value}, found ${actual.value}"
    case Metadata(error)   => s"invalid launcher snapshot metadata: $error"
    case Status(url, code) => s"launcher download failed with HTTP $code: $url"
    case Transport(error)  => error.message
    case Io(error)         => error.message
    case InvalidPath(path) =>
      s"launcher path is not a readable regular file: $path"
    case Absent(path)                => s"launcher pin is absent: $path"
    case Size(kind, limit)           => s"launcher $kind exceeds $limit bytes"
    case Conflict(original, current) =>
      s"launcher pin changed during update: $original -> $current"
    case Cache(path) =>
      s"launcher cache is not private to the current user: $path"

enum LauncherPayload(val limit: Long):
  case Metadata extends LauncherPayload(256L * 1024)
  case Checksum extends LauncherPayload(256)
  case Artifact extends LauncherPayload(128L * 1024 * 1024)

/** Bounded writes and atomic renames at the filesystem adapter boundary. */
trait LauncherCommit:
  def write(path: Path, bytes: Chunk[Byte]): IO[LauncherError, Unit]
  def move(from: Path, to: Path): IO[LauncherError, Unit]

object LauncherCommit:
  val live: LauncherCommit = new LauncherCommit:
    def write(path: Path, bytes: Chunk[Byte]): IO[LauncherError, Unit] =
      LauncherFiles.write(path, bytes)
    def move(from: Path, to: Path): IO[LauncherError, Unit] =
      LauncherFiles.move(from, to)

final case class LauncherUpdater(
    client: Client,
    commit: LauncherCommit = LauncherCommit.live
):
  def update(repo: Path): IO[LauncherError, LauncherPin] =
    ZIO.scoped:
      for
        old <- LauncherPin.read(repo)
        xml <- small(
          old.directory / "maven-metadata.xml",
          LauncherPayload.Metadata
        )
        revision <- LauncherMetadata.revision(
          new String(xml.toArray, StandardCharsets.UTF_8),
          old.version
        )
        candidate <- ZIO.fromEither(
          LauncherPin.from(
            old.repository.root.render,
            old.version,
            revision,
            old.sha256.value
          )
        )
        checksum <- small(
          candidate.directory / s"histos-$revision.jar.sha256",
          LauncherPayload.Checksum
        )
        pin <- ZIO.fromEither(
          LauncherPin.from(
            old.repository.root.render,
            old.version,
            revision,
            new String(checksum.toArray, StandardCharsets.UTF_8)
          )
        )
        cache = repo.resolve(".histos/launcher-cache")
        _ <- LauncherFiles.privateDirectory(cache)
        directory = cache.resolve(pin.sha256.value)
        _ <- LauncherFiles.privateDirectory(directory)
        staged <- LauncherFiles.temp(directory)
        actual <- download(pin.artifact, staged)
        _ <- ZIO
          .fail(LauncherError.Mismatch(pin.sha256, actual))
          .unless(pin.sha256 == actual)
        text <- LauncherFiles.temp(repo.resolve(".histos"))
        _ <- commit
          .write(
            text,
            Chunk.fromArray(
              LauncherPin.render(pin).getBytes(StandardCharsets.UTF_8)
            )
          )
          .uninterruptible
        _ <- LauncherFiles.locked(repo):
          for
            current <- LauncherPin.read(repo)
            _ <- ZIO
              .fail(LauncherError.Conflict(old.revision, current.revision))
              .unless(LauncherPin.render(old) == LauncherPin.render(current))
            _ <- (commit.move(staged, directory.resolve("histos.jar")) *>
              commit.move(
                text,
                repo.resolve(LauncherPin.file)
              )).uninterruptible
          yield ()
      yield pin

  private def response(
      url: Url,
      kind: LauncherPayload
  ): ZIO[Scope, LauncherError, heddle.http.Response] =
    client
      .streaming(Request.get(url))
      .mapError(error => LauncherError.Transport(TransportFault.from(error)))
      .flatMap(response =>
        if response.status.code != 200 then
          ZIO.fail(LauncherError.Status(url.render, response.status.code))
        else if response.body.length.exists(_ > kind.limit) then
          ZIO.fail(LauncherError.Size(kind, kind.limit))
        else ZIO.succeed(response)
      )

  private def limited(
      stream: ZStream[Any, Throwable, Byte],
      kind: LauncherPayload
  ): ZStream[Any, LauncherError, Byte] =
    ZStream.unwrap:
      zio.Ref
        .make(0L)
        .map: count =>
          stream
            .mapError(_ => LauncherError.Transport(TransportFault.Body))
            .mapChunksZIO(bytes =>
              count
                .modify: previous =>
                  val next = previous + bytes.length
                  (next <= kind.limit, next)
                .flatMap(allowed =>
                  if allowed then ZIO.succeed(bytes)
                  else ZIO.fail(LauncherError.Size(kind, kind.limit))
                )
            )
            .rechunk(64 * 1024)

  private def small(
      url: Url,
      kind: LauncherPayload
  ): IO[LauncherError, Chunk[Byte]] =
    ZIO.scoped(
      response(url, kind).flatMap(found =>
        limited(found.body.toStream, kind).runCollect
      )
    )

  private def download(url: Url, path: Path): IO[LauncherError, Sha256] =
    ZIO.scoped:
      for
        found <- response(url, LauncherPayload.Artifact)
        output <- ZIO.acquireRelease(
          ZIO
            .attemptBlocking(Files.newOutputStream(path))
            .mapError(_ => LauncherError.Io(IoFailure(path, IoFault.Write)))
        )(stream => ZIO.attemptBlocking(stream.close()).ignore)
        digest <- ZIO
          .attempt(MessageDigest.getInstance("SHA-256"))
          .mapError(_ => LauncherError.Io(IoFailure(path, IoFault.Write)))
        _ <- limited(found.body.toStream, LauncherPayload.Artifact).chunks
          .runForeach: bytes =>
            ZIO
              .attemptBlocking:
                val buffer = bytes.toArray
                output.write(buffer)
                digest.update(buffer)
              .mapError(_ => LauncherError.Io(IoFailure(path, IoFault.Write)))
              .uninterruptible
        actual <- ZIO.succeed(Sha256.fromDigest(digest.digest()))
      yield actual

object LauncherUpdater:
  val layer: ZLayer[Any, LauncherError, LauncherUpdater] =
    (ZLayer.succeed(
      Client.Config.default.copy(maxBodyBytes = BodyBytes(256L * 1024 * 1024))
    ) >>> Client.layer)
      .mapError(error =>
        LauncherError.Transport(TransportFault.from(error))
      ) >>> ZLayer.fromZIO(
      ZIO.serviceWith[Client](client => LauncherUpdater(client))
    )

private[publish] object LauncherFiles:
  private val noFollow = LinkOption.NOFOLLOW_LINKS

  def read(path: Path): IO[LauncherError, String] =
    ZIO
      .attemptBlocking:
        if !Files
            .readAttributes(path, classOf[BasicFileAttributes], noFollow)
            .isRegularFile && !Files.isSymbolicLink(path)
        then Left(LauncherError.InvalidPath(path))
        else if !Files.isRegularFile(path) || !Files.isReadable(path) then
          Left(LauncherError.InvalidPath(path))
        else Right(Files.readString(path))
      .mapError {
        case _: java.nio.file.NoSuchFileException => LauncherError.Absent(path)
        case _ => LauncherError.Io(IoFailure(path, IoFault.Read))
      }
      .flatMap(ZIO.fromEither)

  def privateDirectory(path: Path): IO[LauncherError, Unit] =
    for
      link <- ZIO
        .attemptBlocking(Files.isSymbolicLink(path))
        .mapError(_ => LauncherError.Io(IoFailure(path, IoFault.Stat)))
      _ <- ZIO.fail(LauncherError.Cache(path)).when(link)
      _ <- RuntimeCache
        .prepare(path)
        .mapError:
          case RuntimeCacheError.Io(failure) => LauncherError.Io(failure)
          case _                             => LauncherError.Cache(path)
    yield ()

  def temp(parent: Path): ZIO[Scope, LauncherError, Path] =
    ZIO.acquireRelease(
      ZIO
        .attemptBlocking(Files.createTempFile(parent, ".launcher-", ".tmp"))
        .mapError(_ => LauncherError.Io(IoFailure(parent, IoFault.Temp)))
    )(path => Workspace.delete(path).ignore)

  def write(path: Path, bytes: Chunk[Byte]): IO[LauncherError, Unit] =
    ZIO
      .attemptBlocking(Files.write(path, bytes.toArray))
      .mapError(_ => LauncherError.Io(IoFailure(path, IoFault.Write)))
      .uninterruptible
      .unit

  def move(from: Path, to: Path): IO[LauncherError, Unit] =
    ZIO
      .attemptBlocking(
        Files.move(
          from,
          to,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
      )
      .mapError(_ => LauncherError.Io(IoFailure(to, IoFault.Write)))
      .unit

  def locked[A](repo: Path)(
      effect: IO[LauncherError, A]
  ): IO[LauncherError, A] =
    ZIO.scoped:
      val path = repo.resolve(".histos/launcher-update.lock")
      for
        _ <- ZIO
          .fail(LauncherError.InvalidPath(path))
          .whenZIO(
            ZIO
              .attemptBlocking(Files.isSymbolicLink(path))
              .mapError(_ => LauncherError.Io(IoFailure(path, IoFault.Stat)))
          )
        channel <- ZIO.acquireRelease(
          ZIO
            .attemptBlocking(
              FileChannel.open(
                path,
                StandardOpenOption.CREATE,
                StandardOpenOption.WRITE,
                noFollow
              )
            )
            .mapError(_ => LauncherError.Io(IoFailure(path, IoFault.Write)))
        )(channel => ZIO.attemptBlocking(channel.close()).ignore)
        result <- ZIO.uninterruptibleMask: restore =>
          def waitForLock: IO[LauncherError, java.nio.channels.FileLock] =
            ZIO
              .attemptBlocking:
                try Option(channel.tryLock())
                catch case _: OverlappingFileLockException => None
              .mapError(_ => LauncherError.Io(IoFailure(path, IoFault.Write)))
              .uninterruptible
              .flatMap:
                case Some(lock) => ZIO.succeed(lock)
                case None       => ZIO.sleep(10.millis) *> waitForLock
          for
            lock <- restore(waitForLock)
            result <- restore(effect).ensuring(
              ZIO.attemptBlocking(lock.close()).ignore
            )
          yield result
      yield result

object LauncherUrl:
  enum Kind:
    case Repository, Archive

  def repository(raw: String): Either[LauncherError, Unit] =
    validate(raw, Kind.Repository)
  def archive(raw: String): Either[LauncherError, Unit] =
    validate(raw, Kind.Archive)

  private def validate(raw: String, kind: Kind): Either[LauncherError, Unit] =
    try
      val uri = URI(raw)
      val ascii = raw.nonEmpty && raw.forall(c =>
        c >= '!' && c <= '~' && !"\"<>\\^`{|}".contains(c)
      )
      val file = kind == Kind.Archive && Option(uri.getScheme).contains(
        "file"
      ) && Option(uri.getRawAuthority).isEmpty &&
        Option(uri.getRawQuery).isEmpty && Option(
          uri.getRawFragment
        ).isEmpty && Option(uri.getRawPath).exists(_.startsWith("/")) &&
        !"(?i)%(00|0[1-9a-f]|1[0-9a-f]|7f)".r.findFirstIn(raw).nonEmpty
      val host = Option(uri.getHost).getOrElse("")
      val authority = Option(uri.getRawAuthority).getOrElse("")
      val authorityValid = !authority.endsWith(":") && !authority.contains('%')
      val fileShape = !raw.startsWith("file:////")
      val brackets = Option(uri.getRawPath).exists(path =>
        path.contains('[') || path.contains(']')
      ) ||
        Option(uri.getRawQuery).exists(query =>
          query.contains('[') || query.contains(']')
        )
      val remote = Option(uri.getRawUserInfo).isEmpty && Option(
        uri.getRawFragment
      ).isEmpty && host.nonEmpty &&
        (uri.getPort == -1 || (uri.getPort > 0 && uri.getPort <= 65535)) &&
        (Option(uri.getScheme).contains(
          "https"
        ) || (kind == Kind.Repository && Option(uri.getScheme).contains(
          "http"
        ) &&
          Set("localhost", "127.0.0.1", "[::1]").contains(host)))
      val repository = kind != Kind.Repository || (Option(
        uri.getRawQuery
      ).isEmpty && !raw.contains('%') &&
        !"(^|/)\\.\\.(/|$)".r
          .findFirstIn(Option(uri.getRawPath).getOrElse(""))
          .nonEmpty)
      Either.cond(
        ascii && repository && !brackets && ((file && fileShape) || (remote && authorityValid)),
        (),
        LauncherError.Repository(raw)
      )
    catch
      case _: java.net.URISyntaxException => Left(LauncherError.Repository(raw))

private[publish] object LauncherMetadata:
  def revision(xml: String, version: String): IO[LauncherError, String] =
    ZIO
      .attemptBlocking:
        val factory = DocumentBuilderFactory.newInstance()
        factory.setFeature(
          "http://apache.org/xml/features/disallow-doctype-decl",
          true
        )
        factory.setFeature(
          "http://xml.org/sax/features/external-general-entities",
          false
        )
        factory.setFeature(
          "http://xml.org/sax/features/external-parameter-entities",
          false
        )
        factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "")
        factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "")
        val builder = factory.newDocumentBuilder()
        builder.setErrorHandler(new DefaultHandler:
          override def error(error: SAXParseException): Unit = ()
          override def fatalError(error: SAXParseException): Unit = ())
        val root =
          builder.parse(InputSource(StringReader(xml))).getDocumentElement
        for
          _ <- Either.cond(
            root.getTagName == "metadata",
            (),
            LauncherMetadataError.Xml
          )
          group <- text(root, "groupId")
          artifact <- text(root, "artifactId")
          found <- text(root, "version")
          _ <- Either.cond(
            group == "rocks.earlyeffect" && artifact == "histos" && found == version,
            (),
            LauncherMetadataError.Coordinates
          )
          versioning <- one(root, "versioning")
          snapshot <- one(versioning, "snapshot")
          stamp <- text(snapshot, "timestamp")
          build <- text(snapshot, "buildNumber")
          revision = s"${version.stripSuffix("-SNAPSHOT")}-$stamp-$build"
          _ <- Either.cond(
            LauncherPin.validRevision(version, revision),
            (),
            LauncherMetadataError.Snapshot
          )
          versions <- one(versioning, "snapshotVersions")
          jars = children(versions, "snapshotVersion").filter(row =>
            text(row, "extension")
              .contains("jar") && children(row, "classifier").isEmpty
          )
          jar <- jars match
            case item :: Nil => Right(item)
            case _           => Left(LauncherMetadataError.Jar)
          value <- text(jar, "value")
          _ <- Either.cond(value == revision, (), LauncherMetadataError.Jar)
        yield revision
      .mapError(_ => LauncherError.Metadata(LauncherMetadataError.Xml))
      .flatMap(result =>
        ZIO.fromEither(result).mapError(LauncherError.Metadata(_))
      )

  private def children(parent: Element, name: String): List[Element] =
    val nodes = parent.getChildNodes
    (0 until nodes.getLength).toList.flatMap: index =>
      nodes.item(index) match
        case element: Element if element.getTagName == name => List(element)
        case _                                              => Nil

  private def one(
      parent: Element,
      name: String
  ): Either[LauncherMetadataError, Element] =
    children(parent, name) match
      case element :: Nil => Right(element)
      case Nil            => Left(LauncherMetadataError.Missing(name))
      case _              => Left(LauncherMetadataError.Duplicate(name))

  private def text(
      parent: Element,
      name: String
  ): Either[LauncherMetadataError, String] =
    one(parent, name).flatMap: element =>
      val nodes = element.getChildNodes
      val plain = (0 until nodes.getLength).forall(index =>
        nodes.item(index).getNodeType == Node.TEXT_NODE
      )
      if plain && element.getTextContent.trim.nonEmpty then
        Right(element.getTextContent.trim)
      else Left(LauncherMetadataError.Xml)
