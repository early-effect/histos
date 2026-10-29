package histos.central

import histos.version.{ManualVersion, ReleaseVersion, ShipNumber}
import histos.Organization

import java.nio.charset.StandardCharsets
import java.time.Instant

import heddle.{BasicCredentials, Body, Handler, Method, Request, Response, Routes, Server, Status}
import heddle.client.{Authority, ClientError}
import heddle.error.ServerError
import heddle.server.Port

import zio.{Chunk, Config, Ref, Scope, ZIO}
import zio.test.*

object SnapshotSpec extends ZIOSpecDefault:
  private val bytes = Chunk[Byte](0x50, 0x4b, 0x03, 0x04) ++ Chunk.fromArray("histos".getBytes(StandardCharsets.UTF_8))
  private val instant = Instant.parse("2026-10-21T14:01:02Z")

  private val publish = suite("snapshot repository")(
    test("metadata names the build it was given"):
      val built =
        for
          name  <- ArtifactId.from("histos")
          named <- SnapshotMetadata.parse("0.0.1-SNAPSHOT")
        yield
          val unique = SnapshotVersion(named.base, SnapshotStamp.from(instant), 4)
          val xml    = SnapshotMetadata.render(
            SnapshotDocument(
              Organization("rocks.earlyeffect"),
              name,
              named.revision,
              SnapshotStamp.from(instant),
              4,
              unique,
              SnapshotUpdated.from(instant),
            )
          )
          (SnapshotMetadata.buildNumber(xml), unique.value, SnapshotStamp.from(instant).value)
      assertTrue(
        built == Right((Right(4), "0.0.1-20261021.140102-4", "20261021.140102")),
        SnapshotMetadata.parse("0.0.1-SNAPSHOT").map(_.base.value) == Right("0.0.1"),
        SnapshotMetadata.parse("1.0.0") == Left(SnapshotRevisionError.NotSnapshot("1.0.0")),
        SnapshotMetadata.buildNumber("<metadata></metadata>") == Left(MetadataFault.MissingBuild),
        SnapshotMetadata.buildNumber("<buildNumber>nope</buildNumber>") == Left(MetadataFault.BadBuild),
      )
    ,
    test("http is only a repository on loopback, and a bad url fails before a publish"):
      val central = "https://central.sonatype.com/repository/maven-snapshots"
      val trust   = TransportFault.from(ClientError.InvalidTrust("s3cret"))
      assertTrue(
        SnapshotRepository.from(central) == Right(SnapshotRepository.central),
        SnapshotRepository.from(s"$central/") == Right(SnapshotRepository.central),
        SnapshotRepository.from("http://127.0.0.1:9/repo").isRight,
        SnapshotRepository.from("http://example.com/maven") == Left(SnapshotRepository.Error.Insecure("example.com")),
        SnapshotRepository.from("not a url") == Left(SnapshotRepository.Error.Malformed("not a url")),
        TransportFault.from(ClientError.ConnectTimeout(Authority("central.sonatype.com", 443))) ==
          TransportFault.ConnectTimeout("central.sonatype.com", 443),
        trust == TransportFault.Trust,
        !trust.message.contains("s3cret"),
      )
    ,
    test("a secret does not render its value"):
      val secret = Config.Secret(Chunk.fromArray("s3cret".toCharArray))
      val creds  = RepositoryUser.from("maven").map(RepositorySecret(_, secret))
      assertTrue(
        !secret.toString.contains("s3cret"),
        creds.forall(shown => !shown.toString.contains("s3cret")),
      )
    ,
    test("a release is refused before a request"):
      ZIO.scoped:
        for
          open <- depot("maven", "secret")
          (base, store) = open
          number        = ShipNumber("1.4.2")
          group         = Organization("rocks.earlyeffect")
          name      <- ZIO.fromEither(ArtifactId.from("histos"))
          publisher <- layer(base, "maven", "secret")
          refused   <- ZIO
            .serviceWithZIO[Publisher](
              _.publish(Artifact(group, name, Revision.Release(ReleaseVersion(number)), bytes))
            )
            .provide(publisher)
            .either
          files <- store.get
        yield assertTrue(
          refused == Left(PublishError.NotSnapshot(SnapshotRevisionError.NotSnapshot("1.4.2"))),
          files.isEmpty,
        )
    ,
    test("two deploys increment the build and keep the checksum"):
      ZIO.scoped:
        for
          _    <- TestClock.setTime(instant)
          open <- depot("maven", "secret")
          (base, store) = open
          group         = Organization("rocks.earlyeffect")
          name      <- ZIO.fromEither(ArtifactId.from("histos"))
          publisher <- layer(base, "maven", "secret")
          first     <- ZIO.serviceWithZIO[Publisher](_.publish(artifact(group, name))).provide(publisher)
          second    <- ZIO.serviceWithZIO[Publisher](_.publish(artifact(group, name))).provide(publisher)
          files     <- store.get
          jar  = files.get(path("histos-0.0.1-20261021.140102-1.jar"))
          sha  = text(files, path("histos-0.0.1-20261021.140102-1.jar.sha256"))
          pom  = text(files, path("histos-0.0.1-20261021.140102-1.pom"))
          meta = text(files, path("maven-metadata.xml"))
        yield assertTrue(
          first == Published
            .Snapshot(SnapshotVersion(baseOf("0.0.1"), SnapshotStamp.from(instant), 1), Checksum.sha256(bytes)),
          second == Published
            .Snapshot(SnapshotVersion(baseOf("0.0.1"), SnapshotStamp.from(instant), 2), Checksum.sha256(bytes)),
          jar == Some(bytes),
          sha.stripLineEnd == Checksum.sha256(bytes).value,
          pom.contains("<artifactId>histos</artifactId>"),
          pom.contains("<version>0.0.1-SNAPSHOT</version>"),
          !pom.contains("<dependencies>"),
          SnapshotMetadata.buildNumber(meta) == Right(2),
          files.keys.exists(_.endsWith(".jar.sha1")),
          files.keys.exists(_.endsWith("maven-metadata.xml.sha256")),
        )
    ,
    test("a saturated build number is refused and uploads nothing"):
      ZIO.scoped:
        for
          _    <- TestClock.setTime(instant)
          open <- depot("maven", "secret")
          (base, store) = open
          named <- ZIO.fromEither(SnapshotMetadata.parse("0.0.1-SNAPSHOT"))
          name  <- ZIO.fromEither(ArtifactId.from("histos"))
          xml = SnapshotMetadata.render(
            SnapshotDocument(
              Organization("rocks.earlyeffect"),
              name,
              named.revision,
              SnapshotStamp.from(instant),
              Int.MaxValue,
              SnapshotVersion(named.base, SnapshotStamp.from(instant), Int.MaxValue),
              SnapshotUpdated.from(instant),
            )
          )
          _ <- store.set(Map(path("maven-metadata.xml") -> Chunk.fromArray(xml.getBytes(StandardCharsets.UTF_8))))
          publisher <- layer(base, "maven", "secret")
          group = Organization("rocks.earlyeffect")
          refused <- ZIO.serviceWithZIO[Publisher](_.publish(artifact(group, name))).provide(publisher).either
          files   <- store.get
        yield assertTrue(
          refused == Left(PublishError.Metadata(MetadataFault.Saturated)),
          !files.keys.exists(_.endsWith(".jar")),
        )
    ,
    test("a wrong password is rejected and is not in the error"):
      ZIO.scoped:
        for
          _    <- TestClock.setTime(instant)
          open <- depot("maven", "secret")
          (base, _) = open
          group     = Organization("rocks.earlyeffect")
          name      <- ZIO.fromEither(ArtifactId.from("histos"))
          publisher <- layer(base, "maven", "nope")
          refused   <- ZIO.serviceWithZIO[Publisher](_.publish(artifact(group, name))).provide(publisher).either
        yield
          val hidden = refused match
            case Left(error) => !error.message.contains("nope") && !error.message.contains("secret")
            case Right(_)    => false
          assertTrue(refused == Left(PublishError.Unauthorized), hidden)
    ,
    test("an error status carries the code and not the body"):
      ZIO.scoped:
        val routes = Routes.fromHandler(Handler { (_: Request) =>
          ZIO.succeed(Response.text("s3cret-body", Status.InternalServerError))
        })
        for
          server <- listen(routes)
          port   <- server.port
          group = Organization("rocks.earlyeffect")
          name      <- ZIO.fromEither(ArtifactId.from("histos"))
          publisher <- layer(s"http://127.0.0.1:$port/repo", "maven", "secret")
          refused   <- ZIO.serviceWithZIO[Publisher](_.publish(artifact(group, name))).provide(publisher).either
        yield
          val hidden = refused match
            case Left(error) => !error.message.contains("s3cret-body")
            case Right(_)    => false
          assertTrue(refused == Left(PublishError.Status(500)), hidden),
  )

  private def artifact(group: Organization, name: ArtifactId): Artifact =
    Artifact(group, name, Revision.Manual(ManualVersion("0.0.1-SNAPSHOT")), bytes)

  private def baseOf(raw: String): SnapshotBase =
    SnapshotMetadata.parse(s"$raw-SNAPSHOT") match
      case Right(named) => named.base
      case Left(_)      => SnapshotBase.trusted(raw)

  private def path(name: String): String =
    s"/repo/rocks/earlyeffect/histos/0.0.1-SNAPSHOT/$name"

  private def text(files: Map[String, Chunk[Byte]], key: String): String =
    files.get(key) match
      case Some(found) => new String(found.toArray, StandardCharsets.UTF_8)
      case None        => ""

  private def layer(base: String, user: String, password: String) =
    for
      repo <- ZIO.fromEither(SnapshotRepository.from(base))
      name <- ZIO.fromEither(RepositoryUser.from(user))
    yield Publisher.snapshots(repo, RepositorySecret(name, Config.Secret(Chunk.fromArray(password.toCharArray))))

  /** OriginDate sleeps on `Clock`. TestClock does not move, so the server runs on the live clock. */
  private def listen[R](routes: Routes[R, Response]): ZIO[R & Scope, ServerError, Server] =
    live(Server.install(routes, Server.Config.default.copy(host = "127.0.0.1", port = Port(0))))

  private def depot(user: String, password: String): ZIO[Scope, ServerError, (String, Ref[Map[String, Chunk[Byte]]])] =
    val expected = "Basic " + BasicCredentials.render(user, password)
    Ref.make(Map.empty[String, Chunk[Byte]]).flatMap { store =>
      val routes = Routes.fromHandler(Handler { (req: Request) =>
        if req.header("Authorization") != Some(expected) then ZIO.succeed(Response.unauthorized())
        else
          val key = req.path.render
          req.method match
            case Method.GET =>
              store.get.map: files =>
                files.get(key) match
                  case Some(body) => Response(Status.Ok, Body.fromBytes(body))
                  case None       => Response.notFound()
            case Method.PUT =>
              req.body.collect.foldZIO(
                _ => ZIO.succeed(Response.internalServerError()),
                found => store.update(_ + (key -> found)).as(Response.empty(Status.Created)),
              )
            case _ =>
              ZIO.succeed(Response.methodNotAllowed("GET, PUT"))
      })
      listen(routes).flatMap(server => server.port.map(port => (s"http://127.0.0.1:$port/repo", store)))
    }

  def spec = publish
end SnapshotSpec
