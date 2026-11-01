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
  private val version = "0.0.2-SNAPSHOT"

  private val checks = suite("launcher publish")(
    test("a release ship boots the snapshot channel of that number"):
      val posix   = LauncherScript.posix("0.0.1")
      val windows = LauncherScript.windows("0.0.1")
      ZIO.succeed(
        assertTrue(
          LauncherScript.boots("0.0.1-SNAPSHOT", "0.0.1-SNAPSHOT"),
          LauncherScript.boots("0.0.1-SNAPSHOT", "0.0.1-20261101.030257-1"),
          LauncherScript.boots("0.0.1", "0.0.1-SNAPSHOT"),
          LauncherScript.boots("0.0.1", "0.0.1-20261101.030257-1"),
          !LauncherScript.boots("0.0.1", "0.0.2-SNAPSHOT"),
          !LauncherScript.boots("0.0.1", "0.0.2"),
          !LauncherScript.boots("0.0.2-SNAPSHOT", "0.0.1-SNAPSHOT"),
          posix.contains("cs fetch"),
          posix.contains("build.properties"),
          posix.contains("COURSIER_TTL=0s"),
          windows.contains("cs fetch"),
          windows.contains("build.properties"),
          windows.contains("COURSIER_TTL"),
        )
      )
    ,
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
        LauncherScript.boots(version, pin.version),
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

private enum LauncherFixtureError:
  case Environment, Repository
