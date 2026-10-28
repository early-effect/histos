package histos.publish

import histos.central.{DeploymentId, DeploymentState}
import histos.version.{ManualVersion, ShipName}
import histos.{Jvm, MainClass, Projects, ScalaVersion, asset, module}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}
import zio.durationInt

object AssetSpec extends ZIOSpecDefault:
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
    ) @@ TestAspect.timeout(30.seconds)

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

  private def note(root: Path, ship: String, state: DeploymentState, version: String) =
    ZIO.fromEither(ShipName.from(ship)).flatMap { name =>
      ZIO.fromEither(DeploymentId.from("dep-1")).flatMap { id =>
        DeploymentFile.write(root, name, id, state, version)
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
