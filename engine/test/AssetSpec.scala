package histos.publish

import histos.central.{DeploymentId, DeploymentState, Finding, Gav, PortalServer, PortalStore}
import histos.version.{ManualVersion, ShipName}
import histos.{Jvm, MainClass, Projects, ScalaVersion, asset, module}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Ref, Scope, ZIO}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}
import zio.durationInt

object AssetSpec extends histos.SuiteClock:
  private val scala3 = ScalaVersion("3.10.0")
  private val jar    = utf("histos-jar")
  private val other  = utf("site-jar")

  private val one =
    val api = module("api", Jvm)(scala3)
    val app = module("app", Jvm)(scala3).assembly(MainClass("demo.App")).asset(asset"dist/histos.jar")
    Projects(histos.ships(histos.manual("histos", ManualVersion("1.0.0"))(api, app)), api, app)

  private val pair =
    val api  = module("api", Jvm)(scala3)
    val app  = module("app", Jvm)(scala3).assembly(MainClass("demo.App")).asset(asset"dist/histos.jar")
    val site = module("site", Jvm)(scala3).assembly(MainClass("demo.Site")).asset(asset"dist/site.jar")
    Projects(
      histos.ships(
        histos.manual("histos", ManualVersion("1.0.0"))(api, app),
        histos.manual("pages", ManualVersion("1.0.0"))(site),
      ),
      api,
      app,
      site,
    )

  private val libs =
    val core = module("core", Jvm)(scala3)
    Projects(histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(core)), core)

  private val doubled =
    val app  = module("app", Jvm)(scala3).assembly(MainClass("demo.App")).asset(asset"dist/histos.jar")
    val tool = module("tool", Jvm)(scala3).assembly(MainClass("demo.Tool")).asset(asset"out/histos.jar")
    Projects(histos.ships(histos.manual("histos", ManualVersion("1.0.0"))(app, tool)), app, tool)

  private val validated =
    test("a validated deployment uploads the jar and leaves the record"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          _     <- plant(root, "dist/histos.jar", jar)
          _     <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          path = DeploymentFile.location(root, ShipName("histos"))
          before <- ZIO.attempt(Files.readString(path))
          ran    <- AssetRun.publish(root, graph, List("histos"), env(stand.base)).either
          seen   <- stand.seen
          after  <- ZIO.attempt(Files.readString(path))
        yield assertTrue(
          ran == Right(AssetReport(Nil, List("histos histos.jar"))),
          seen.requests == 2,
          seen.tags == List("v1.0.0"),
          seen.uploads == List(
            AssetStand.Upload("early-effect", "histos", "histos.jar", jar, "application/octet-stream")
          ),
          before == after,
          after.contains("state VALIDATED"),
        )

  private val missingRecord =
    test("a named ship with no deployment uploads nothing and does not need a token"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          _     <- plant(root, "dist/histos.jar", jar)
          ran   <- AssetRun.publish(root, graph, List("histos"), Map.empty).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Right(AssetReport(List("histos has no recorded deployment"), Nil)),
          seen.requests == 0,
          seen.uploads.isEmpty,
        )

  private val published =
    test("a published deployment is not an upload"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          _     <- plant(root, "dist/histos.jar", jar)
          _     <- note(root, "histos", DeploymentState.Published, "1.0.0")
          ran   <- AssetRun.publish(root, graph, List("histos"), env(stand.base)).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Right(AssetReport(List("histos is PUBLISHED, not VALIDATED"), Nil)),
          seen.requests == 0,
          seen.uploads.isEmpty,
        )

  private val missingFile =
    test("a validated ship whose jar is missing uploads nothing"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          _     <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          ran   <- AssetRun.publish(root, graph, List("histos"), env(stand.base)).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Right(AssetReport(List("histos dist/histos.jar is not a file"), Nil)),
          seen.requests == 0,
          seen.uploads.isEmpty,
        )

  private val missingToken =
    test("a validated ship without a token fails before a socket"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          _     <- plant(root, "dist/histos.jar", jar)
          _     <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          ran   <- AssetRun.publish(root, graph, List("histos"), Map("HISTOS_GITHUB" -> stand.base)).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Left(AssetError.Credentials),
          ran.fold(_.message, _ => "") == "GITHUB_TOKEN and GITHUB_REPOSITORY are required",
          seen.requests == 0,
        )

  private val insecure =
    test("an http release host that is not loopback fails before a socket"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          _     <- plant(root, "dist/histos.jar", jar)
          _     <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          ran   <- AssetRun
            .publish(root, graph, List("histos"), env("http://example.com", token = "s3cret"))
            .either
          seen <- stand.seen
          text = shown(ran)
        yield assertTrue(
          ran == Left(AssetError.Host("http release host 'example.com' is not loopback")),
          text.contains("s3cret") == false,
          seen.requests == 0,
        )

  private val quiet =
    test("no name uploads only the validated ship, and the tag carries the ship"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(pair)
          _     <- plant(root, "dist/histos.jar", jar)
          _     <- plant(root, "dist/site.jar", other)
          _     <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          ran   <- AssetRun.publish(root, graph, Nil, env(stand.base)).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Right(AssetReport(Nil, List("histos histos.jar"))),
          seen.tags == List("histos/v1.0.0"),
          seen.uploads == List(
            AssetStand.Upload("early-effect", "histos", "histos.jar", jar, "application/octet-stream")
          ),
          seen.uploads.exists(_.bytes == other) == false,
          seen.requests == 2,
        )

  private val absent =
    test("a missing release does not post the jar"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token", present = false)
          graph <- ZIO.fromEither(one)
          _     <- plant(root, "dist/histos.jar", jar)
          _     <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          ran   <- AssetRun.publish(root, graph, List("histos"), env(stand.base)).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Right(AssetReport(List("histos the release v1.0.0 was not found"), Nil)),
          seen.requests == 1,
          seen.uploads.isEmpty,
        )

  private val library =
    test("a library ship has no asset and uploads nothing"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(libs)
          ran   <- AssetRun.publish(root, graph, List("libs"), env(stand.base)).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Right(AssetReport(List("libs has no asset"), Nil)),
          seen.requests == 0,
          seen.uploads.isEmpty,
        )

  private val unknown =
    test("an unknown ship fails before a socket"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          ran   <- AssetRun.publish(root, graph, List("nope"), Map.empty).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Left(AssetError.UnknownShip("nope")),
          seen.requests == 0,
        )

  private val duplicate =
    test("a ship named twice fails before a socket"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          ran   <- AssetRun.publish(root, graph, List("histos", "histos"), Map.empty).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Left(AssetError.Duplicate("histos")),
          seen.requests == 0,
        )

  private val repository =
    test("a repository that is not owner/name fails before a socket"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          _     <- plant(root, "dist/histos.jar", jar)
          _     <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          ran   <- AssetRun
            .publish(root, graph, List("histos"), env(stand.base, repository = "owner/name/extra"))
            .either
          seen <- stand.seen
        yield assertTrue(
          ran == Left(AssetError.Repository("owner/name/extra")),
          seen.requests == 0,
        )

  private val rejected =
    test("a rejected token uploads nothing and stays out of the failure"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          _     <- plant(root, "dist/histos.jar", jar)
          _     <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          ran   <- AssetRun.publish(root, graph, List("histos"), env(stand.base, token = "s3cret")).either
          seen  <- stand.seen
          text = shown(ran)
        yield assertTrue(
          ran == Right(AssetReport(List("histos the release host rejected the credentials"), Nil)),
          text.contains("s3cret") == false,
          seen.requests == 1,
          seen.uploads.isEmpty,
        )

  private val mismatch =
    test("a deployment for another version is not an upload"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          _     <- plant(root, "dist/histos.jar", jar)
          _     <- note(root, "histos", DeploymentState.Validated, "9.9.9")
          ran   <- AssetRun.publish(root, graph, List("histos"), env(stand.base)).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Right(AssetReport(List("histos deployment records 9.9.9, not 1.0.0"), Nil)),
          seen.requests == 0,
        )

  private val clash =
    test("two assets with one file name upload neither"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(doubled)
          _     <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          ran   <- AssetRun.publish(root, graph, List("histos"), env(stand.base)).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Right(AssetReport(List("histos names two assets called histos.jar"), Nil)),
          seen.requests == 0,
          seen.uploads.isEmpty,
        )

  private val removed =
    test("drop deletes the uploaded asset after the deployment is gone"):
      ZIO.scoped:
        for
          root   <- temp
          portal <- temp
          graph  <- ZIO.fromEither(pair)
          secret <- ZIO.fromEither(PortalServer.credentials(sonatype))
          bound  <- PortalServer.open(portal, secret, ZIO.unit)
          histos  = DeploymentFile.location(root, ShipName("histos"))
          pages   = DeploymentFile.location(root, ShipName("pages"))
          dropped = portal.resolve("deployments").resolve("dep-1")
          kept    = portal.resolve("deployments").resolve("dep-2")
          first <- Ref.make(false)
          stand <- AssetStand.open(
            "asset-token",
            onDelete = ZIO
              .attemptBlocking(!Files.exists(histos) && !Files.exists(dropped))
              .orElseSucceed(false)
              .flatMap(first.set),
          )
          github = portalEnv(bound.url, env(stand.base))
          _          <- plant(root, "dist/histos.jar", jar)
          _          <- plant(root, "dist/site.jar", other)
          _          <- note(root, "histos", DeploymentState.Validated, "1.0.0", "dep-1")
          _          <- note(root, "pages", DeploymentState.Validated, "1.0.0", "dep-2")
          _          <- hold(portal, "dep-1")
          _          <- hold(portal, "dep-2")
          sent       <- AssetRun.publish(root, graph, Nil, github).either
          before     <- stand.seen
          ran        <- PublishRun.drop(root, graph, github, "dep-1").either
          after      <- stand.seen
          order      <- first.get
          histosFile <- ZIO.attempt(Files.exists(histos))
          pagesFile  <- ZIO.attempt(Files.readString(pages))
          portalGone <- ZIO.attempt(Files.exists(dropped))
          portalKept <- ZIO.attempt(Files.isDirectory(kept))
        yield assertTrue(
          sent == Right(AssetReport(Nil, List("histos histos.jar", "pages site.jar"))),
          before.uploads.map(_.name) == List("histos.jar", "site.jar"),
          ran == Right(()),
          order,
          !histosFile,
          pagesFile.contains("state VALIDATED"),
          !portalGone,
          portalKept,
          after.uploads == List(
            AssetStand.Upload("early-effect", "histos", "site.jar", other, "application/octet-stream")
          ),
          after.tags.lastOption.contains("histos/v1.0.0"),
        )

  private val assetFailed =
    test("a failed asset delete leaves the deployment dropped"):
      ZIO.scoped:
        for
          root   <- temp
          portal <- temp
          graph  <- ZIO.fromEither(one)
          secret <- ZIO.fromEither(PortalServer.credentials(sonatype))
          bound  <- PortalServer.open(portal, secret, ZIO.unit)
          stand  <- AssetStand.open("asset-token")
          _      <- plant(root, "dist/histos.jar", jar)
          _      <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          _      <- hold(portal, "dep-1")
          _      <- AssetRun.publish(root, graph, List("histos"), env(stand.base))
          ran    <- PublishRun
            .drop(root, graph, portalEnv(bound.url, env(stand.base, token = "s3cret")), "dep-1")
            .either
          seen <- stand.seen
          file <- ZIO.attempt(Files.exists(DeploymentFile.location(root, ShipName("histos"))))
          gone <- ZIO.attempt(Files.exists(portal.resolve("deployments").resolve("dep-1")))
          text = ran.fold(_.message, _ => "")
        yield assertTrue(
          ran == Left(ReleaseError.Asset(AssetError.Unauthorized)),
          !text.contains("s3cret"),
          !file,
          !gone,
          seen.uploads.map(_.name) == List("histos.jar"),
          seen.requests == 3,
        )

  private val libraryDrop =
    test("a ship with no asset drops without a release socket or a token"):
      ZIO.scoped:
        for
          root   <- temp
          portal <- temp
          graph  <- ZIO.fromEither(libs)
          secret <- ZIO.fromEither(PortalServer.credentials(sonatype))
          bound  <- PortalServer.open(portal, secret, ZIO.unit)
          stand  <- AssetStand.open("asset-token")
          _      <- note(root, "libs", DeploymentState.Validated, "1.0.0")
          _      <- hold(portal, "dep-1")
          ran    <- PublishRun
            .drop(root, graph, portalEnv(bound.url, Map("HISTOS_GITHUB" -> stand.base)), "dep-1")
            .either
          seen <- stand.seen
          file <- ZIO.attempt(Files.exists(DeploymentFile.location(root, ShipName("libs"))))
          gone <- ZIO.attempt(Files.exists(portal.resolve("deployments").resolve("dep-1")))
        yield assertTrue(
          ran == Right(()),
          seen.requests == 0,
          seen.uploads.isEmpty,
          !file,
          !gone,
        )

  private val noRecord =
    test("a missing deployment file drops the portal and does not open a release socket"):
      ZIO.scoped:
        for
          root   <- temp
          portal <- temp
          graph  <- ZIO.fromEither(one)
          secret <- ZIO.fromEither(PortalServer.credentials(sonatype))
          bound  <- PortalServer.open(portal, secret, ZIO.unit)
          stand  <- AssetStand.open("asset-token")
          _      <- hold(portal, "dep-1")
          ran    <- PublishRun.drop(root, graph, portalEnv(bound.url, env(stand.base)), "dep-1").either
          seen   <- stand.seen
          gone   <- ZIO.attempt(Files.exists(portal.resolve("deployments").resolve("dep-1")))
        yield assertTrue(
          ran == Right(()),
          seen.requests == 0,
          seen.uploads.isEmpty,
          !gone,
        )

  private val unreleased =
    test("a missing release is not an asset and the deployment still drops"):
      ZIO.scoped:
        for
          root   <- temp
          portal <- temp
          graph  <- ZIO.fromEither(one)
          secret <- ZIO.fromEither(PortalServer.credentials(sonatype))
          bound  <- PortalServer.open(portal, secret, ZIO.unit)
          stand  <- AssetStand.open("asset-token", present = false)
          _      <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          _      <- hold(portal, "dep-1")
          ran    <- PublishRun.drop(root, graph, portalEnv(bound.url, env(stand.base)), "dep-1").either
          seen   <- stand.seen
          file   <- ZIO.attempt(Files.exists(DeploymentFile.location(root, ShipName("histos"))))
          gone   <- ZIO.attempt(Files.exists(portal.resolve("deployments").resolve("dep-1")))
        yield assertTrue(
          ran == Right(()),
          seen.requests == 1,
          seen.tags.isEmpty,
          seen.uploads.isEmpty,
          !file,
          !gone,
        )

  private val unnamed =
    test("a release that has no such asset still drops"):
      ZIO.scoped:
        for
          root   <- temp
          portal <- temp
          graph  <- ZIO.fromEither(one)
          secret <- ZIO.fromEither(PortalServer.credentials(sonatype))
          bound  <- PortalServer.open(portal, secret, ZIO.unit)
          stand  <- AssetStand.open("asset-token")
          _      <- note(root, "histos", DeploymentState.Validated, "1.0.0")
          _      <- hold(portal, "dep-1")
          ran    <- PublishRun.drop(root, graph, portalEnv(bound.url, env(stand.base)), "dep-1").either
          seen   <- stand.seen
          file   <- ZIO.attempt(Files.exists(DeploymentFile.location(root, ShipName("histos"))))
          gone   <- ZIO.attempt(Files.exists(portal.resolve("deployments").resolve("dep-1")))
        yield assertTrue(
          ran == Right(()),
          seen.requests == 2,
          seen.tags == List("v1.0.0"),
          seen.uploads.isEmpty,
          !file,
          !gone,
        )

  private val idle =
    test("no name with nothing validated uploads nothing"):
      ZIO.scoped:
        for
          root  <- temp
          stand <- AssetStand.open("asset-token")
          graph <- ZIO.fromEither(one)
          _     <- plant(root, "dist/histos.jar", jar)
          ran   <- AssetRun.publish(root, graph, Nil, env(stand.base)).either
          seen  <- stand.seen
        yield assertTrue(
          ran == Right(AssetReport(Nil, Nil)),
          seen.requests == 0,
          seen.uploads.isEmpty,
        )

  def spec =
    suite("assets")(
      validated,
      missingRecord,
      published,
      missingFile,
      missingToken,
      insecure,
      quiet,
      absent,
      library,
      unknown,
      duplicate,
      repository,
      rejected,
      mismatch,
      clash,
      idle,
      removed,
      assetFailed,
      libraryDrop,
      noRecord,
      unreleased,
      unnamed,
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)

  private def env(
      base: String,
      token: String = "asset-token",
      repository: String = "early-effect/histos",
  ): Map[String, String] =
    Map(
      "HISTOS_GITHUB"     -> base,
      "GITHUB_TOKEN"      -> token,
      "GITHUB_REPOSITORY" -> repository,
    )

  private def shown(ran: Either[AssetError, AssetReport]): String =
    ran.fold(_.message, report => report.lines.mkString("\n"))

  private val sonatype =
    Map(
      "SONATYPE_USERNAME" -> "maven",
      "SONATYPE_PASSWORD" -> "secret",
    )

  private def portalEnv(base: String, github: Map[String, String]): Map[String, String] =
    sonatype.updated("HISTOS_PORTAL", base) ++ github

  private def hold(root: Path, id: String) =
    ZIO.fromEither(DeploymentId.from(id)).flatMap { parsed =>
      PortalStore.create(
        root,
        parsed,
        PortalStore.Record(DeploymentState.Validated, Chunk.empty[Finding], Chunk.empty[Gav]),
      )
    }

  private def note(root: Path, ship: String, state: DeploymentState, version: String, id: String = "dep-1") =
    ZIO.fromEither(ShipName.from(ship)).flatMap { name =>
      ZIO.fromEither(DeploymentId.from(id)).flatMap { parsed =>
        DeploymentFile.write(root, name, parsed, state, version)
      }
    }

  private def plant(root: Path, relative: String, bytes: Chunk[Byte]) =
    val path = root.resolve(relative)
    ZIO.attemptBlocking {
      val parent = Option(path.getParent).getOrElse(root)
      Files.createDirectories(parent)
      Files.write(path, bytes.toArray)
    }

  private def utf(text: String): Chunk[Byte] =
    Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-asset"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore
end AssetSpec
