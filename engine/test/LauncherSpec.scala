package histos.publish

import histos.cache.ContentStore
import histos.load.{ApiJar, Loader}
import histos.resolve.{ArtifactName, Resolve}
import histos.version.ManualVersion
import histos.{
  ArtifactBase,
  Jvm,
  MainClass,
  Module,
  Organization,
  Platform,
  Projects,
  Publication,
  ScalaVersion,
  module,
}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, System, ZIO, ZLayer}
import zio.test.*

object LauncherSpec extends ZIOSpecDefault:
  private val checks = suite("launcher publish")(
    test("a release ship boots the snapshot channel of that number"):
      val posix = LauncherScript.posix("0.0.1")
      ZIO.succeed(
        assertTrue(
          LauncherScript.boots("0.0.1-SNAPSHOT", "0.0.1-SNAPSHOT"),
          LauncherScript.boots("0.0.1-SNAPSHOT", "0.0.1-20261101.030257-1"),
          LauncherScript.boots("0.0.1", "0.0.1-SNAPSHOT"),
          LauncherScript.boots("0.0.1", "0.0.1-20261101.030257-1"),
          !LauncherScript.boots("0.0.1", "0.0.2-SNAPSHOT"),
          !LauncherScript.boots("0.0.1", "0.0.2"),
          !LauncherScript.boots("0.0.2-SNAPSHOT", "0.0.1-SNAPSHOT"),
          posix.contains("bin/histos"),
          posix.contains("assembly.sha256"),
          posix.contains("histos.version"),
          posix.contains("On Windows, use WSL"),
          !posix.contains("cs fetch"),
          !posix.contains("HISTOS_VERSION=\"histos $version\""),
        )
      )
    ,
    test("the committed scripts are the manual ship"):
      ZIO.scoped {
        for
          root <- repository
          work <- ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-launcher"))): path =>
            ZIO.attempt {
              val stream = Files.walk(path)
              try stream.iterator.asScala.toList.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists(_))
              finally stream.close()
            }.ignore
          _ <- ZIO.attempt {
            Files.createDirectories(work.resolve("cache"))
            Files.createDirectories(work.resolve("store"))
          }
          api    <- ApiJar.locate.provideSomeLayer[Scope](ZLayer.succeed(System.SystemLive))
          loaded <- ZIO
            .serviceWithZIO[Loader](_.load(root, api))
            .provideSome[Scope](
              Resolve.live(work.resolve("cache")),
              ZLayer.succeed(ContentStore.in(work.resolve("store"))),
              Loader.live,
            )
          checked <- LauncherScript.check(root, loaded.graph).either
        yield assertTrue(checked == Right(()))
      }
    ,
    test("the api name keeps the binary suffix and the assembly name does not"):
      ZIO
        .fromEither(opened)
        .map: graph =>
          val libraries = graph.projects.collect:
            case item: Module if item.publication == Publication.Library =>
              val base = item.published.fold(item.name.value)(_.value)
              ArtifactName.published(base, ScalaVersion("3.10.0"), Platform.Jvm)
          val assemblies = graph.projects.flatMap:
            case item: Module =>
              item.publication match
                case Publication.Assembly(main) =>
                  List(item.published.fold(item.name.value)(_.value) -> main.value)
                case _ => Nil
            case _ => Nil
          assertTrue(
            libraries == List("histos-api_3"),
            assemblies == List("histos" -> "histos.cli.Main"),
            graph.organization.map(_.value) == Some("rocks.earlyeffect"),
          )
    ,
    test("snapshots need both sonatype variables"):
      val missing = PublishWhere.snapshots(Map.empty)
      val present = PublishWhere.snapshots(
        Map("SONATYPE_USERNAME" -> "maven", "SONATYPE_PASSWORD" -> "secret")
      )
      val authed = present match
        case Right(where: PublishWhere.Snapshots) =>
          where.user.value == "maven" &&
          where.password == zio.Config.Secret(
            zio.Chunk.fromArray("secret".toCharArray)
          ) &&
          !where.password.toString.contains("secret") &&
          !where.toString.contains("secret")
        case _ => false
      assertTrue(missing == Left(LocalPublishError.Credentials), authed),
  )

  private def repository: ZIO[Any, Throwable, Path] =
    System
      .env("HISTOS_REPO")
      .flatMap: named =>
        val cwd  = Path.of("").toAbsolutePath.normalize
        val root = named.filter(_.nonEmpty).fold(cwd)(Path.of(_))
        val file = root.resolve("histos.scala")
        ZIO
          .attempt(Files.isRegularFile(file))
          .flatMap:
            case true  => ZIO.succeed(root)
            case false => ZIO.fail(new IllegalStateException(s"histos.scala is not at $file"))

  private val opened =
    val api = module("api", Jvm)(ScalaVersion("3.10.0")).artifact(
      ArtifactBase("histos-api")
    )
    val app = module("app", Jvm)(ScalaVersion("3.10.0"))
      .artifact(ArtifactBase("histos"))
      .assembly(MainClass("histos.cli.Main"))
    Projects
      .organization(Organization("rocks.earlyeffect"))(
        histos
          .ships(histos.manual("histos", ManualVersion("0.0.1-SNAPSHOT"))(api, app)),
        api,
        app,
      )

  def spec = suite("launcher")(
    checks @@ TestAspect.withLiveSystem
  )
end LauncherSpec
