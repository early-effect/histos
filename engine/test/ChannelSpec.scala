package histos.publish

import histos.jdk.Sha256
import histos.version.ShipNumber

import histos.{Jvm, MainClass, Organization, Projects, ScalaVersion, asset, module}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO}
import zio.durationInt
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue}

object ChannelSpec extends ZIOSpecDefault:
  private val scala3   = ScalaVersion("3.10.0")
  private val revision = "0.2.0-20261105.135856-2"
  private val pointer  = "0.2.0-SNAPSHOT"
  private val stored   = "0.2.0-0123456789ab-SNAPSHOT"
  private val jar      = Chunk.fromArray("toolchain".getBytes(StandardCharsets.UTF_8))

  private val one =
    val api = module("api", Jvm)(scala3)
    val app = module("app", Jvm)(scala3).assembly(MainClass("demo.App")).asset(asset"dist/histos.jar")
    Projects(histos.ships(histos.ship("histos", ShipNumber("0.2.0"))(api, app)), api, app)

  private val doubled =
    val app  = module("app", Jvm)(scala3).assembly(MainClass("demo.App")).asset(asset"dist/histos.jar")
    val tool = module("tool", Jvm)(scala3).assembly(MainClass("demo.Tool")).asset(asset"dist/tool.jar")
    Projects(histos.ships(histos.ship("histos", ShipNumber("0.2.0"))(app, tool)), app, tool)

  private val plain =
    val core = module("core", Jvm)(scala3)
    Projects(histos.ships(histos.ship("libs", ShipNumber("1.0.0"))(core)), core)

  def spec =
    suite("dist channel")(
      test("a pointer of the asset module names channel, manifest, and jar"):
        val files    = DistChannel.files(pointer, revision, jar).toMap
        val manifest = text(files(DistLayout.manifest(revision)))
        assertTrue(
          text(files(DistLayout.channel(pointer))) == s"$revision\n",
          files(DistLayout.jar(revision)) == jar,
          manifest.contains(s""""version": "$revision""""),
          manifest.contains(s""""jar": "${DistLayout.jar(revision)}""""),
          manifest.contains(s""""jarSha256": "${DistLayout.sha256(jar.toArray)}""""),
          manifest.contains(""""clients": {"""),
          !manifest.contains("linux-x86_64"),
          !manifest.contains("sourcesSha256"),
        )
      ,
      test("a stored coordinate and a library ship do not plan a channel"):
        for
          graph <- must(one)
          libs  <- must(plain)
          planned <- must(
            DistChannel.plan(
              graph,
              List(
                row(graph, moduleName(graph, "api"), "histos-api", pointer),
                row(graph, moduleName(graph, "app"), "histos", stored),
              ),
            )
          )
          quiet <- must(DistChannel.plan(libs, List(row(libs, moduleName(libs, "core"), "core", pointer))))
        yield assertTrue(planned.isEmpty, quiet.isEmpty)
      ,
      test("the asset pointer is the channel, and two assets are refused"):
        for
          graph <- must(one)
          both  <- must(doubled)
          planned <- must(DistChannel.plan(graph, List(row(graph, moduleName(graph, "app"), "histos", pointer))))
          refused = DistChannel
            .plan(both, List(row(both, moduleName(both, "app"), "histos", pointer)))
            .left
            .map(_.message)
        yield assertTrue(
          planned.map(_.pointer) == List(pointer),
          planned.map(_.artifact) == List("histos"),
          refused == Left("histos names two assets"),
        )
      ,
      test("the jar and the token are required before a pointer upload"):
        ZIO.scoped:
          for
            root  <- temp
            graph <- must(one)
            plans <- must(DistChannel.plan(graph, List(row(graph, moduleName(graph, "app"), "histos", pointer))))
            missing  = DistChannel.ready(root, env("http://127.0.0.1:9"), plans).left.map(_.message)
            _       <- plant(root, "dist/histos.jar", jar)
            token    = DistChannel.ready(root, Map.empty, plans).left.map(_.message)
            insecure = DistChannel.ready(root, env("http://example.com"), plans).left.map(_.message)
            ready   <- must(DistChannel.ready(root, env("http://127.0.0.1:9"), plans))
          yield assertTrue(
            missing == Left("dist/histos.jar is not a file. ./histos assembly writes the install jar."),
            token == Left("GITHUB_TOKEN and GITHUB_REPOSITORY are required"),
            insecure == Left("http release host 'example.com' is not loopback"),
            ready == (),
          )
      ,
      test("upload writes the channel body and the timestamped jar"):
        ZIO.scoped:
          for
            root  <- temp
            stand <- AssetStand.open("asset-token").mapError(error => new IllegalStateException(error.toString))
            graph <- must(one)
            _     <- plant(root, "dist/histos.jar", jar)
            plans <- must(DistChannel.plan(graph, List(row(graph, moduleName(graph, "app"), "histos", pointer))))
            line = row(graph, moduleName(graph, "app"), "histos", pointer).copy(unique = Some(revision)).rendered(None)
            noted <- DistChannel.upload(root, env(stand.base), plans, List(line, s"rocks.earlyeffect histos $stored x"))
            seen  <- stand.seen
            files = seen.uploads.map(item => item.name -> item.bytes).toMap
          yield assertTrue(
            noted == List(s"histos dist ${DistLayout.channel(pointer)} $revision"),
            line == s"rocks.earlyeffect histos $pointer $revision",
            seen.tags.nonEmpty && seen.tags.forall(_ == DistChannel.Tag),
            text(files(DistLayout.channel(pointer))) == s"$revision\n",
            files(DistLayout.jar(revision)) == jar,
            text(files(DistLayout.manifest(revision))).contains(s""""version": "$revision""""),
          )
      ,
      test("a pointer without a timestamp is not uploaded"):
        ZIO.scoped:
          for
            root  <- temp
            stand <- AssetStand.open("asset-token").mapError(error => new IllegalStateException(error.toString))
            graph <- must(one)
            _     <- plant(root, "dist/histos.jar", jar)
            plans <- must(DistChannel.plan(graph, List(row(graph, moduleName(graph, "app"), "histos", pointer))))
            ran  <- DistChannel.upload(root, env(stand.base), plans, Nil).either
            seen <- stand.seen
          yield assertTrue(
            ran == Left(LocalPublishError.Stock(s"histos $pointer did not publish a timestamp")),
            seen.requests == 0,
            seen.uploads.isEmpty,
          )
      ,
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)

  private def must[A, E](value: Either[E, A]): ZIO[Any, Throwable, A] =
    ZIO.fromEither(value).mapError(error => new IllegalStateException(error.toString))

  private def row(graph: histos.ProjectGraph, module: histos.ProjectName, artifact: String, revision: String) =
    PublishedModule(
      module,
      Organization("rocks.earlyeffect"),
      artifact,
      revision,
      None,
      None,
      None,
      None,
      graph.catalog.ships.head.name,
      Sha256.of(Chunk.empty),
      None,
      Nil,
    )

  private def moduleName(graph: histos.ProjectGraph, name: String): histos.ProjectName =
    graph.projects.collectFirst { case found: histos.Module if found.name.value == name => found.name }.get

  private def env(base: String): Map[String, String] =
    Map(
      "HISTOS_GITHUB"     -> base,
      "GITHUB_TOKEN"      -> "asset-token",
      "GITHUB_REPOSITORY" -> "early-effect/histos",
    )

  private def text(bytes: Chunk[Byte]): String =
    new String(bytes.toArray, StandardCharsets.UTF_8)

  private def plant(root: Path, relative: String, bytes: Chunk[Byte]) =
    val path = root.resolve(relative)
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      Files.write(path, bytes.toArray)
    }

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-channel"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          try walk.iterator().asScala.toList.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists(_))
          finally walk.close()
      }.ignore
end ChannelSpec
