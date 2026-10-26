package histos.central

import histos.Organization

import java.nio.charset.StandardCharsets
import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter

import heddle.client.{Authority, Client, ClientError}
import heddle.http.{Body, Method, Request, Response, Scheme, Url}
import heddle.http.header.{BasicCredentials, Headers}

import zio.{Chunk, Config, IO, ZIO}

/** Where a snapshot deploy goes. `https` anywhere, `http` only on loopback. */
opaque type SnapshotRepository = Url

object SnapshotRepository:
  given CanEqual[SnapshotRepository, SnapshotRepository] = CanEqual.derived

  enum Error:
    case Malformed(raw: String)
    case Insecure(host: String)

    def message: String = this match
      case Malformed(raw) => s"'$raw' is not a snapshot repository"
      case Insecure(host) => s"http snapshot repository '$host' is not loopback"
  end Error

  val central: SnapshotRepository =
    trusted(
      Url(
        heddle.http.Path(Chunk("repository", "maven-snapshots")),
        scheme = Some(Scheme.Https),
        host = Some("central.sonatype.com"),
      )
    )

  def from(raw: String): Either[Error, SnapshotRepository] =
    val trimmed =
      raw.trim match
        case text if text.endsWith("/") => text.dropRight(1)
        case text                       => text
    Url.decode(trimmed) match
      case Left(_)    => Left(Error.Malformed(raw))
      case Right(url) =>
        (url.scheme, url.host) match
          case (Some(Scheme.Https), Some(_))                     => Right(url)
          case (Some(Scheme.Http), Some(host)) if loopback(host) => Right(url)
          case (Some(Scheme.Http), Some(host))                   => Left(Error.Insecure(host))
          case _                                                 => Left(Error.Malformed(raw))

  extension (repo: SnapshotRepository) def root: Url = repo

  private[central] def trusted(url: Url): SnapshotRepository = url

  private def loopback(host: String): Boolean =
    host == "localhost" || host == "127.0.0.1" || host == "[::1]" || host == "::1"
end SnapshotRepository

/** A repository username. A colon would split the Basic credential. */
opaque type RepositoryUser = String

object RepositoryUser:
  given CanEqual[RepositoryUser, RepositoryUser] = CanEqual.derived

  enum Error:
    case Empty
    case Colon

    def message: String = this match
      case Empty => "a repository user is empty"
      case Colon => "a repository user contains ':'"
  end Error

  def from(raw: String): Either[Error, RepositoryUser] =
    val name = raw.trim
    if name.isEmpty then Left(Error.Empty)
    else if name.contains(':') then Left(Error.Colon)
    else Right(name)

  extension (user: RepositoryUser) def value: String = user
end RepositoryUser

/** Basic credentials. `password` renders as `Secret(<redacted>)`. */
final case class RepositorySecret(user: RepositoryUser, password: Config.Secret)

/** Why no HTTP response arrived. The case carries the target, never a throwable or its message. */
enum TransportFault:
  case Target(raw: String)
  case Connect(host: String, port: Int)
  case ConnectTimeout(host: String, port: Int)
  case Tls(host: String, port: Int)
  case ReadTimeout(host: String, port: Int)
  case Io(host: String, port: Int)
  case Protocol(host: String, port: Int)
  case Pool(host: String, port: Int)
  case Trust
  case Body

  def message: String = this match
    case Target(raw)                => s"could not reach the snapshot repository: '$raw' is not a target"
    case Connect(host, port)        => s"could not connect to the snapshot repository at $host:$port"
    case ConnectTimeout(host, port) => s"the snapshot repository at $host:$port did not accept a connection"
    case Tls(host, port)            => s"tls failed with the snapshot repository at $host:$port"
    case ReadTimeout(host, port)    => s"the snapshot repository at $host:$port stopped answering"
    case Io(host, port)             => s"the snapshot repository at $host:$port closed the exchange"
    case Protocol(host, port)       => s"the snapshot repository at $host:$port sent an invalid response"
    case Pool(host, port)           => s"no free connection to the snapshot repository at $host:$port"
    case Trust                      => "the snapshot repository trust material is unusable"
    case Body                       => "the snapshot repository response body failed"
end TransportFault

object TransportFault:
  def from(error: ClientError): TransportFault = error match
    case ClientError.InvalidTarget(target, _)       => Target(target)
    case ClientError.Connect(authority, _)          => at(authority, Connect.apply)
    case ClientError.ConnectTimeout(authority)      => at(authority, ConnectTimeout.apply)
    case ClientError.Tls(authority, _)              => at(authority, Tls.apply)
    case ClientError.ReadTimeout(authority)         => at(authority, ReadTimeout.apply)
    case ClientError.Io(authority, _)               => at(authority, Io.apply)
    case ClientError.Protocol(authority, _)         => at(authority, Protocol.apply)
    case ClientError.PoolExhausted(authority)       => at(authority, Pool.apply)
    case ClientError.NotAnEventStream(authority, _) => at(authority, Protocol.apply)
    case ClientError.InvalidTrust(_)                => Trust
    case ClientError.BodyFailed(_)                  => Body

  private def at(authority: Authority, make: (String, Int) => TransportFault): TransportFault =
    make(authority.host, authority.port)
end TransportFault

/** Why snapshot metadata could not name the next build. */
enum MetadataFault:
  case MissingBuild
  case BadBuild
  case Saturated
  case Unread

  def message: String = this match
    case MissingBuild => "snapshot metadata has no build number"
    case BadBuild     => "snapshot metadata build number is not a positive int"
    case Saturated    => "snapshot metadata build number cannot increase"
    case Unread       => "snapshot metadata was not a complete body"
end MetadataFault

/** A revision that is not a `-SNAPSHOT`. */
enum SnapshotRevisionError:
  case NotSnapshot(revision: String)

  def message: String = this match
    case NotSnapshot(revision) => s"'$revision' is not a snapshot"
end SnapshotRevisionError

/** The text before `-SNAPSHOT`. */
opaque type SnapshotBase = String

object SnapshotBase:
  private[central] def trusted(raw: String): SnapshotBase = raw

  extension (base: SnapshotBase) def value: String = base
end SnapshotBase

/** The `<version>` a snapshot directory is named with, including `-SNAPSHOT`. */
opaque type SnapshotRevision = String

object SnapshotRevision:
  private[central] def trusted(raw: String): SnapshotRevision = raw

  extension (revision: SnapshotRevision) def value: String = revision
end SnapshotRevision

final case class SnapshotName(base: SnapshotBase, revision: SnapshotRevision)

/** `yyyyMMdd.HHmmss` in UTC. */
opaque type SnapshotStamp = String

object SnapshotStamp:
  def from(instant: Instant): SnapshotStamp = stamp.format(instant)

  extension (value: SnapshotStamp) def value: String = value

  private val stamp = DateTimeFormatter.ofPattern("yyyyMMdd.HHmmss").withZone(ZoneOffset.UTC)
end SnapshotStamp

/** `yyyyMMddHHmmss` in UTC. */
opaque type SnapshotUpdated = String

object SnapshotUpdated:
  def from(instant: Instant): SnapshotUpdated = compact.format(instant)

  extension (value: SnapshotUpdated) def value: String = value

  private val compact = DateTimeFormatter.ofPattern("yyyyMMddHHmmss").withZone(ZoneOffset.UTC)
end SnapshotUpdated

/** `<base>-<stamp>-<build>`, the file name Maven resolves a snapshot to. */
opaque type SnapshotVersion = String

object SnapshotVersion:
  def apply(base: SnapshotBase, stamp: SnapshotStamp, build: Int): SnapshotVersion =
    s"${base.value}-${stamp.value}-$build"

  extension (version: SnapshotVersion) def value: String = version
end SnapshotVersion

/** One `maven-metadata.xml`. */
final case class SnapshotDocument(
    group: Organization,
    name: ArtifactId,
    revision: SnapshotRevision,
    stamp: SnapshotStamp,
    build: Int,
    unique: SnapshotVersion,
    updated: SnapshotUpdated,
)

object SnapshotMetadata:
  def parse(revision: String): Either[SnapshotRevisionError, SnapshotName] =
    val stripped = revision.stripSuffix("-SNAPSHOT")
    if stripped.nonEmpty && stripped != revision && !stripped.contains("..") then
      Right(SnapshotName(SnapshotBase.trusted(stripped), SnapshotRevision.trusted(revision)))
    else Left(SnapshotRevisionError.NotSnapshot(revision))

  def buildNumber(xml: String): Either[MetadataFault, Int] =
    val open  = "<buildNumber>"
    val close = "</buildNumber>"
    val start = xml.indexOf(open)
    val end   = if start < 0 then -1 else xml.indexOf(close, start + open.length)
    if start < 0 || end < 0 then Left(MetadataFault.MissingBuild)
    else
      xml.substring(start + open.length, end).trim.toIntOption match
        case Some(number) if number > 0 => Right(number)
        case _                          => Left(MetadataFault.BadBuild)

  def render(document: SnapshotDocument): String =
    val groupText    = xml(document.group.value)
    val artifactText = xml(document.name.value)
    val revisionText = xml(document.revision.value)
    val uniqueText   = xml(document.unique.value)
    val stamp        = document.stamp.value
    val updated      = document.updated.value
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<metadata modelVersion="1.1.0">
       |  <groupId>$groupText</groupId>
       |  <artifactId>$artifactText</artifactId>
       |  <version>$revisionText</version>
       |  <versioning>
       |    <snapshot>
       |      <timestamp>$stamp</timestamp>
       |      <buildNumber>${document.build}</buildNumber>
       |    </snapshot>
       |    <lastUpdated>$updated</lastUpdated>
       |    <snapshotVersions>
       |      <snapshotVersion>
       |        <extension>jar</extension>
       |        <value>$uniqueText</value>
       |        <updated>$updated</updated>
       |      </snapshotVersion>
       |      <snapshotVersion>
       |        <extension>pom</extension>
       |        <value>$uniqueText</value>
       |        <updated>$updated</updated>
       |      </snapshotVersion>
       |    </snapshotVersions>
       |  </versioning>
       |</metadata>
       |""".stripMargin

  private def xml(text: String): String =
    text.flatMap:
      case '&'  => "&amp;"
      case '<'  => "&lt;"
      case '>'  => "&gt;"
      case char => String.valueOf(char)
end SnapshotMetadata

private[central] final class SnapshotPublisher(
    repository: SnapshotRepository,
    credentials: RepositorySecret,
    client: Client,
) extends Publisher:
  private val token =
    "Basic " + BasicCredentials.render(credentials.user.value, new String(credentials.password.value.toArray))

  def publish(artifact: Artifact): IO[PublishError, Published] =
    ZIO
      .fromEither(plan(artifact))
      .flatMap: planned =>
        for
          previous <- metadata(planned.metadata)
          build    <- ZIO.fromEither(nextBuild(previous)).mapError(PublishError.Metadata(_))
          instant  <- ZIO.clockWith(_.instant)
          stamp  = SnapshotStamp.from(instant)
          unique = SnapshotVersion(planned.base, stamp, build)
          _ <- ZIO.foreachDiscard(files(planned, unique))(item => put(planned.directory / item.name, item.bytes))
          body = SnapshotMetadata.render(
            SnapshotDocument(
              planned.group,
              planned.name,
              planned.revision,
              stamp,
              build,
              unique,
              SnapshotUpdated.from(instant),
            )
          )
          xml = Chunk.fromArray(body.getBytes(StandardCharsets.UTF_8))
          _ <- ZIO
            .foreachDiscard(sidecars("maven-metadata.xml", xml))(item => put(planned.directory / item.name, item.bytes))
        yield Published.Snapshot(unique, planned.sha256)

  private def nextBuild(previous: Option[String]): Either[MetadataFault, Int] =
    previous match
      case None      => Right(1)
      case Some(xml) =>
        SnapshotMetadata
          .buildNumber(xml)
          .flatMap: current =>
            if current == Int.MaxValue then Left(MetadataFault.Saturated) else Right(current + 1)

  private def plan(artifact: Artifact): Either[PublishError, Planned] =
    for
      jar   <- JarCheck.accept(artifact.jar)
      named <- SnapshotMetadata.parse(artifact.revision.render).left.map(PublishError.NotSnapshot(_))
    yield
      val tail      = artifact.group.segments :+ artifact.name.value :+ named.revision.value
      val directory = tail.foldLeft(repository.root)(_ / _)
      Planned(
        artifact.group,
        artifact.name,
        named.revision,
        named.base,
        directory / "maven-metadata.xml",
        directory,
        Layout.pom(artifact),
        jar,
        Checksum.sha256(jar),
      )

  private def files(planned: Planned, unique: SnapshotVersion): List[Upload] =
    val stem = s"${planned.name.value}-${unique.value}"
    val pom  = Chunk.fromArray(planned.pom.getBytes(StandardCharsets.UTF_8))
    sidecars(s"$stem.jar", planned.jar) ++ sidecars(s"$stem.pom", pom)

  private def sidecars(name: String, bytes: Chunk[Byte]): List[Upload] =
    List(
      Upload(name, bytes),
      Upload(s"$name.md5", text(Digest.md5(bytes))),
      Upload(s"$name.sha1", text(Digest.sha1(bytes))),
      Upload(s"$name.sha256", text(Digest.sha256(bytes))),
    )

  private def text(hex: String): Chunk[Byte] =
    Chunk.fromArray(s"$hex\n".getBytes(StandardCharsets.UTF_8))

  private def metadata(url: Url): IO[PublishError, Option[String]] =
    exchange(Method.GET, url, None).flatMap: response =>
      response.status.code match
        case 404 => ZIO.succeed(None)
        case 200 =>
          response.body.strict match
            case Some(bytes) => ZIO.succeed(Some(new String(bytes.toArray, StandardCharsets.UTF_8)))
            case None        => ZIO.fail(PublishError.Metadata(MetadataFault.Unread))
        case 401 | 403 => ZIO.fail(PublishError.Unauthorized)
        case code      => ZIO.fail(PublishError.Status(code))

  private def put(url: Url, bytes: Chunk[Byte]): IO[PublishError, Unit] =
    exchange(Method.PUT, url, Some(bytes)).flatMap: response =>
      response.status.code match
        case 200 | 201 | 204 => ZIO.unit
        case 401 | 403       => ZIO.fail(PublishError.Unauthorized)
        case code            => ZIO.fail(PublishError.Status(code))

  private def exchange(method: Method, url: Url, body: Option[Chunk[Byte]]): IO[PublishError, Response] =
    val headers =
      body match
        case None    => Headers.empty.add("Authorization", token).add("User-Agent", "histos")
        case Some(_) =>
          Headers.empty
            .add("Authorization", token)
            .add("User-Agent", "histos")
            .add("Content-Type", "application/octet-stream")
    val request = Request(method, url, headers, body.fold(Body.empty)(Body.fromBytes(_)))
    client.batched(request).mapError(error => PublishError.Transport(TransportFault.from(error)))

  private final case class Planned(
      group: Organization,
      name: ArtifactId,
      revision: SnapshotRevision,
      base: SnapshotBase,
      metadata: Url,
      directory: Url,
      pom: String,
      jar: Chunk[Byte],
      sha256: Checksum,
  )

  private final case class Upload(name: String, bytes: Chunk[Byte])
end SnapshotPublisher
