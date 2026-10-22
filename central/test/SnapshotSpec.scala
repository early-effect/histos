package histos.central

import histos.version.{ManualVersion, ReleaseVersion, ShipNumber}
import histos.Organization

import com.sun.net.httpserver.{HttpExchange, HttpServer}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.time.{Clock, Instant, ZoneOffset}
import java.util.Base64
import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object SnapshotSpec extends ZIOSpecDefault:
  private val bytes = Chunk[Byte](0x50, 0x4b, 0x03, 0x04) ++ Chunk.fromArray("histos".getBytes(StandardCharsets.UTF_8))
  private val clock = Clock.fixed(Instant.parse("2026-10-21T14:01:02Z"), ZoneOffset.UTC)

  private val publish = suite("snapshot repository")(
    test("metadata names the build it was given"):
      val xml = SnapshotMetadata.render(
        "rocks.earlyeffect",
        "histos",
        "0.0.1-SNAPSHOT",
        "20261021.140102",
        4,
        "0.0.1-20261021.140102-4",
        "20261021140102",
      )
      assertTrue(
        SnapshotMetadata.buildNumber(xml) == Right(4),
        SnapshotMetadata.base("0.0.1-SNAPSHOT") == Right("0.0.1"),
        SnapshotMetadata.base("1.0.0") == Left(PublishError.NotSnapshot("1.0.0")),
        SnapshotMetadata.timestamp(clock.instant()) == "20261021.140102",
      )
    ,
    test("a release is refused before a request"):
      ZIO.scoped:
        for
          open <- depot("maven", "secret")
          (http, store) = open
          number = ShipNumber("1.4.2")
          group = Organization("rocks.earlyeffect")
          name <- ZIO.fromEither(ArtifactId.from("histos"))
          refused <- ZIO.serviceWithZIO[Publisher](
                       _.publish(Artifact(group, name, Revision.Release(ReleaseVersion(number)), bytes))
                     ).provide(publisher(http)).either
        yield assertTrue(refused == Left(PublishError.NotSnapshot("1.4.2")) && store.keys.isEmpty)
    ,
    test("two deploys increment the build and keep the checksum"):
      ZIO.scoped:
        for
          open <- depot("maven", "secret")
          (http, store) = open
          group = Organization("rocks.earlyeffect")
          name <- ZIO.fromEither(ArtifactId.from("histos"))
          first <- ZIO.serviceWithZIO[Publisher](_.publish(artifact(group, name))).provide(publisher(http))
          second <- ZIO.serviceWithZIO[Publisher](_.publish(artifact(group, name))).provide(publisher(http))
          jar = store.get("/repo/rocks/earlyeffect/histos/0.0.1-SNAPSHOT/histos-0.0.1-20261021.140102-1.jar").map(Chunk.fromArray)
          sha = store.text("/repo/rocks/earlyeffect/histos/0.0.1-SNAPSHOT/histos-0.0.1-20261021.140102-1.jar.sha256")
          pom = store.text("/repo/rocks/earlyeffect/histos/0.0.1-SNAPSHOT/histos-0.0.1-20261021.140102-1.pom")
          meta = store.text("/repo/rocks/earlyeffect/histos/0.0.1-SNAPSHOT/maven-metadata.xml")
        yield assertTrue(
          first.unique == Some("0.0.1-20261021.140102-1"),
          second.unique == Some("0.0.1-20261021.140102-2"),
          jar == Some(bytes),
          sha.stripLineEnd == Digest.sha256(bytes),
          pom.contains("<artifactId>histos</artifactId>"),
          pom.contains("<version>0.0.1-SNAPSHOT</version>"),
          !pom.contains("<dependencies>"),
          SnapshotMetadata.buildNumber(meta) == Right(2),
          store.keys.exists(_.endsWith(".jar.sha1")),
          store.keys.exists(_.endsWith("maven-metadata.xml.sha256")),
        )
    ,
    test("a wrong password is rejected and is not in the error"):
      ZIO.scoped:
        for
          open <- depot("maven", "secret")
          (http, _) = open
          group = Organization("rocks.earlyeffect")
          name <- ZIO.fromEither(ArtifactId.from("histos"))
          refused <- ZIO.serviceWithZIO[Publisher](_.publish(artifact(group, name))).provide(Publisher.snapshots(base(http), "maven", "nope", clock)).either
        yield assertTrue(
          refused == Left(PublishError.Unauthorized),
          !refused.is(_.left).message.contains("nope"),
          !refused.is(_.left).message.contains("secret"),
        )
    ,
  )

  private def artifact(group: Organization, name: ArtifactId): Artifact =
    Artifact(group, name, Revision.Manual(ManualVersion("0.0.1-SNAPSHOT")), bytes)

  private def publisher(http: HttpServer) =
    Publisher.snapshots(base(http), "maven", "secret", clock)

  private def base(http: HttpServer): String =
    s"http://127.0.0.1:${http.getAddress.getPort}/repo"

  private final class Depot:
    private val files = new ConcurrentHashMap[String, Array[Byte]]
    def put(path: String, body: Array[Byte]): Unit =
      files.put(path, body)
      ()
    def get(path: String): Option[Array[Byte]] = Option(files.get(path))
    def text(path: String): String =
      get(path).map(found => new String(found, StandardCharsets.UTF_8)).getOrElse("")
    def keys: List[String] = files.keys.asScala.toList

  private def depot(user: String, password: String): ZIO[Scope, Throwable, (HttpServer, Depot)] =
    ZIO.acquireRelease(ZIO.attempt {
      val store = new Depot
      val http = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
      val token = "Basic " + Base64.getEncoder.encodeToString(s"$user:$password".getBytes(StandardCharsets.UTF_8))
      http.createContext("/", (exchange: HttpExchange) => handle(exchange, store, token))
      http.start()
      (http, store)
    })(open => ZIO.attempt(open._1.stop(0)).ignore)

  private def handle(exchange: HttpExchange, store: Depot, token: String): Unit =
    val auth = Option(exchange.getRequestHeaders.getFirst("Authorization"))
    if auth != Some(token) then
      exchange.sendResponseHeaders(401, -1)
      exchange.close()
    else
      val path = exchange.getRequestURI.getPath
      exchange.getRequestMethod match
        case "GET" =>
          val found = store.get(path)
          found match
            case None =>
              exchange.sendResponseHeaders(404, -1)
              exchange.close()
            case Some(body) =>
              exchange.sendResponseHeaders(200, body.length)
              val out = exchange.getResponseBody
              out.write(body)
              out.close()
              exchange.close()
        case "PUT" =>
          val body = exchange.getRequestBody.readAllBytes()
          store.put(path, body)
          exchange.sendResponseHeaders(201, -1)
          exchange.close()
        case _ =>
          exchange.sendResponseHeaders(405, -1)
          exchange.close()

  def spec = publish
