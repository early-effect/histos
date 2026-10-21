package histos.central

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.{Clock, Duration, ZoneOffset}
import java.time.format.DateTimeFormatter
import java.util.Base64

import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

/** The Central snapshot repository. A release does not go here. */
object Snapshots:
  val central: String = "https://central.sonatype.com/repository/maven-snapshots/"

/** The `maven-metadata.xml` a snapshot deploy writes and the next build reads. */
object SnapshotMetadata:
  def timestamp(instant: java.time.Instant): String =
    stamp.format(instant)

  def updated(instant: java.time.Instant): String =
    compact.format(instant)

  def base(revision: String): Either[PublishError, String] =
    val stripped = revision.stripSuffix("-SNAPSHOT")
    if stripped.nonEmpty && stripped != revision && !stripped.contains("..") then Right(stripped)
    else Left(PublishError.NotSnapshot(revision))

  def buildNumber(xml: String): Either[PublishError, Int] =
    val open = "<buildNumber>"
    val close = "</buildNumber>"
    val start = xml.indexOf(open)
    val end = if start < 0 then -1 else xml.indexOf(close, start + open.length)
    if start < 0 || end < 0 then Left(PublishError.Metadata("snapshot metadata has no build number"))
    else
      val raw = xml.substring(start + open.length, end).trim
      raw.toIntOption match
        case Some(number) if number > 0 => Right(number)
        case _                           => Left(PublishError.Metadata("snapshot metadata has no build number"))

  def render(
      group: String,
      artifact: String,
      revision: String,
      timestamp: String,
      build: Int,
      unique: String,
      updatedAt: String,
  ): String =
    val groupText = xml(group)
    val artifactText = xml(artifact)
    val revisionText = xml(revision)
    val uniqueText = xml(unique)
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<metadata modelVersion="1.1.0">
       |  <groupId>$groupText</groupId>
       |  <artifactId>$artifactText</artifactId>
       |  <version>$revisionText</version>
       |  <versioning>
       |    <snapshot>
       |      <timestamp>$timestamp</timestamp>
       |      <buildNumber>$build</buildNumber>
       |    </snapshot>
       |    <lastUpdated>$updatedAt</lastUpdated>
       |    <snapshotVersions>
       |      <snapshotVersion>
       |        <extension>jar</extension>
       |        <value>$uniqueText</value>
       |        <updated>$updatedAt</updated>
       |      </snapshotVersion>
       |      <snapshotVersion>
       |        <extension>pom</extension>
       |        <value>$uniqueText</value>
       |        <updated>$updatedAt</updated>
       |      </snapshotVersion>
       |    </snapshotVersions>
       |  </versioning>
       |</metadata>
       |""".stripMargin

  private val stamp = DateTimeFormatter.ofPattern("yyyyMMdd.HHmmss").withZone(ZoneOffset.UTC)
  private val compact = DateTimeFormatter.ofPattern("yyyyMMddHHmmss").withZone(ZoneOffset.UTC)

  private def xml(text: String): String =
    text.flatMap:
      case '&' => "&amp;"
      case '<' => "&lt;"
      case '>' => "&gt;"
      case char => String.valueOf(char)

private[central] final class SnapshotPublisher(
    base: String,
    user: String,
    password: String,
    clock: Clock,
) extends Publisher:
  private val client = HttpClient.newBuilder()
    .followRedirects(HttpClient.Redirect.NEVER)
    .connectTimeout(Duration.ofSeconds(20))
    .build()

  private val authorization =
    val token = Base64.getEncoder.encodeToString(s"$user:$password".getBytes(StandardCharsets.UTF_8))
    s"Basic $token"

  def publish(artifact: Artifact): IO[PublishError, Published] =
    ZIO.fromEither(plan(artifact)).flatMap: planned =>
      for
        previous <- metadata(planned.metadataUrl)
        build <- ZIO.fromEither(nextBuild(previous))
        instant = clock.instant()
        stamped = SnapshotMetadata.timestamp(instant)
        unique = s"${planned.base}-$stamped-$build"
        _ <- ZIO.foreachDiscard(files(planned, unique))(item => put(planned.fileUrl(item.name), item.bytes))
        body = SnapshotMetadata.render(
          planned.group,
          planned.artifact,
          planned.revision,
          stamped,
          build,
          unique,
          SnapshotMetadata.updated(instant),
        )
        _ <- ZIO.foreachDiscard(sidecars("maven-metadata.xml", Chunk.fromArray(body.getBytes(StandardCharsets.UTF_8)))): item =>
          put(planned.fileUrl(item.name), item.bytes)
      yield Published(
        java.nio.file.Path.of(s"${planned.artifact}-$unique.jar"),
        java.nio.file.Path.of(s"${planned.artifact}-$unique.pom"),
        planned.jarSha,
        Some(unique),
      )

  override def toString: String = "SnapshotPublisher"

  private def nextBuild(previous: Option[String]): Either[PublishError, Int] =
    previous match
      case None => Right(1)
      case Some(xml) =>
        SnapshotMetadata.buildNumber(xml).flatMap: current =>
          if current == Int.MaxValue then Left(PublishError.Metadata("snapshot metadata build number cannot increase"))
          else Right(current + 1)

  private def plan(artifact: Artifact): Either[PublishError, Planned] =
    for
      jar <- JarCheck.accept(artifact.jar)
      base <- SnapshotMetadata.base(artifact.revision.render)
      root <- repository
    yield
      val revision = artifact.revision.render
      val tail = artifact.group.segments :+ artifact.name.value :+ revision
      val directory = (root :: tail).mkString("/")
      val pom = Layout.pom(artifact)
      Planned(
        artifact.group.value,
        artifact.name.value,
        revision,
        base,
        directory + "/maven-metadata.xml",
        directory,
        pom,
        jar,
        Digest.sha256(jar),
      )

  private def repository: Either[PublishError, String] =
    val trimmed = if base.endsWith("/") then base.dropRight(1) else base
    if trimmed.startsWith("https://") || trimmed.startsWith("http://") then Right(trimmed)
    else Left(PublishError.Transport(trimmed))

  private def files(planned: Planned, unique: String): List[Upload] =
    val stem = s"${planned.artifact}-$unique"
    val pom = Chunk.fromArray(planned.pom.getBytes(StandardCharsets.UTF_8))
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

  private def metadata(url: String): IO[PublishError, Option[String]] =
    send(HttpRequest.newBuilder(URI.create(url)).timeout(Duration.ofMinutes(2)).header("Authorization", authorization).header("User-Agent", "histos").GET().build()).flatMap: response =>
      response.statusCode match
        case 404 => ZIO.succeed(None)
        case 200 => ZIO.succeed(Some(new String(response.body, StandardCharsets.UTF_8)))
        case 401 | 403 => ZIO.fail(PublishError.Unauthorized)
        case code => ZIO.fail(PublishError.Status(code, detail(response.body)))

  private def put(url: String, bytes: Chunk[Byte]): IO[PublishError, Unit] =
    val request = HttpRequest.newBuilder(URI.create(url))
      .timeout(Duration.ofMinutes(5))
      .header("Authorization", authorization)
      .header("User-Agent", "histos")
      .header("Content-Type", "application/octet-stream")
      .PUT(HttpRequest.BodyPublishers.ofByteArray(bytes.toArray))
      .build()
    send(request).flatMap: response =>
      response.statusCode match
        case 200 | 201 | 204 => ZIO.unit
        case 401 | 403       => ZIO.fail(PublishError.Unauthorized)
        case code            => ZIO.fail(PublishError.Status(code, detail(response.body)))

  private def send(request: HttpRequest): IO[PublishError, HttpResponse[Array[Byte]]] =
    ZIO.attemptBlocking(client.send(request, HttpResponse.BodyHandlers.ofByteArray())).mapError: error =>
      PublishError.Transport(Option(error.getMessage).getOrElse("").take(180).replace('\n', ' ').trim)

  private def detail(body: Array[Byte]): String =
    new String(body, StandardCharsets.UTF_8).take(180).replace('\n', ' ').trim

  private final case class Planned(
      group: String,
      artifact: String,
      revision: String,
      base: String,
      metadataUrl: String,
      directory: String,
      pom: String,
      jar: Chunk[Byte],
      jarSha: String,
  ):
    def fileUrl(name: String): String = s"$directory/$name"

  private final case class Upload(name: String, bytes: Chunk[Byte])

object SnapshotLayer:
  def apply(base: String, user: String, password: String, clock: Clock = Clock.systemUTC()): ULayer[Publisher] =
    ZLayer.succeed(SnapshotPublisher(base, user, password, clock))
