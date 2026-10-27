package histos.publish

import histos.resolve.ArtifactName
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

import java.nio.file.Path
import histos.workspace.Workspace

import zio.{System, ZIO}
import zio.test.*

object LauncherSpec extends ZIOSpecDefault:
  private val version = "0.0.1-SNAPSHOT"

  private val checks = suite("launcher publish")(
    test("the committed scripts are the manual ship"):
      for
        value <- System
          .env("HISTOS_REPO")
          .mapError(_ => LauncherFixtureError.Environment)
        root <- ZIO
          .fromOption(value.filter(_.nonEmpty).map(Path.of(_)))
          .orElseFail(LauncherFixtureError.Repository)
        posix   <- Workspace.readString(root.resolve("histos"))
        windows <- Workspace.readString(root.resolve("histos.bat"))
        pin     <- LauncherPin.read(root)
      yield assertTrue(
        posix == LauncherScript.posix(version),
        windows == LauncherScript.windows(version),
        pin.version == version,
      )
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
    test("snapshots need both sonatype variables, and a path stays a directory"):
      val repo    = Path.of("/tmp/histos-publish")
      val home    = Path.of("/fixtures/home")
      val missing = PublishWhere.snapshots(Map.empty)
      val present = PublishWhere.snapshots(
        Map("SONATYPE_USERNAME" -> "maven", "SONATYPE_PASSWORD" -> "secret")
      )
      val directory = PublishWhere.directory(repo, Some("target/repo"), home)
      val absent    = PublishWhere.directory(repo, None, home)
      val authed    = present match
        case Right(where: PublishWhere.Snapshots) =>
          where.user.value == "maven" &&
          where.password == zio.Config.Secret(
            zio.Chunk.fromArray("secret".toCharArray)
          ) &&
          !where.password.toString.contains("secret") &&
          !where.toString.contains("secret")
        case _ => false
      val folder = directory match
        case Right(PublishWhere.Directory(path)) =>
          path.endsWith(Path.of("target", "repo"))
        case _ => false
      val maven = absent match
        case Right(PublishWhere.Directory(path)) =>
          path == home.resolve(".m2").resolve("repository")
        case _ => false
      assertTrue(missing == Left(LocalPublishError.Credentials), authed, folder, maven),
  )

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

  private val homes = suite("maven home")(
    test("user.home names the default directory, and an empty home is refused"):
      for
        _       <- TestSystem.putProperty("user.home", "/fixtures/home")
        found   <- PublishRun.home
        _       <- TestSystem.putProperty("user.home", "")
        missing <- PublishRun.home.either
      yield assertTrue(
        found == Path.of("/fixtures/home"),
        missing == Left(LocalPublishError.NoHome),
      )
  )

  def spec = suite("launcher")(
    checks @@ TestAspect.withLiveSystem,
    homes,
  )
end LauncherSpec

private enum LauncherFixtureError:
  case Environment, Repository
