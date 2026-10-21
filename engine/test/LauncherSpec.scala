package histos.publish

import histos.central.Publisher
import histos.resolve.ArtifactName
import histos.version.ManualVersion
import histos.{
  ArtifactBase,
  Jvm,
  Module,
  Organization,
  Platform,
  Projects,
  ScalaVersion,
  module
}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}
import zio.test.*

object LauncherSpec extends ZIOSpecDefault:
  private val version = "0.0.1-SNAPSHOT"
  private val jarBytes =
    Array[Byte](0x50.toByte, 0x4b.toByte, 0x03.toByte, 0x04.toByte)

  private val checks = suite("launcher publish")(
    test("the api name keeps the binary suffix and the assembly name does not"):
      ZIO
        .fromEither(opened)
        .mapError(error => new IllegalStateException(error.message))
        .map: graph =>
          val names = graph.projects.collect:
            case item: Module =>
              val base = item.published.fold(item.name.value)(_.value)
              ArtifactName.published(base, ScalaVersion("3.10.0"), Platform.Jvm)
          assertTrue(
            names == List("histos-api_3"),
            graph.assembly.map(_.value) == Some("histos"),
            graph.organization.map(_.value) == Some("rocks.earlyeffect")
          )
    ,
    test(
      "snapshots need both sonatype variables, and a path stays a directory"
    ):
      val repo = Path.of("/tmp/histos-publish")
      val missing = PublishWhere.resolve(repo, "snapshots", Map.empty)
      val present = PublishWhere.resolve(
        repo,
        "snapshots",
        Map("SONATYPE_USERNAME" -> "maven", "SONATYPE_PASSWORD" -> "secret")
      )
      val directory = PublishWhere.resolve(repo, "target/repo", Map.empty)
      val authed = present match
        case Right(PublishWhere.Snapshots(user, password)) =>
          user == "maven" && password == "secret"
        case _ => false
      val folder = directory match
        case Right(PublishWhere.Directory(path)) =>
          path.endsWith(Path.of("target", "repo"))
        case _ => false
      assertTrue(missing == Left(LocalPublishError.Credentials), authed, folder)
    ,
    test("a missing assembly is refused before a write"):
      ZIO.scoped:
        for
          root <- temp
          graph <- ZIO
            .fromEither(opened)
            .mapError(error => new IllegalStateException(error.message))
          _ <- ZIO.attempt(
            Files.writeString(root.resolve(".histos-version"), s"$version\n")
          )
          ready = LauncherPublish.ready(root, graph)
          missingJar = ready match
            case Left(LocalPublishError.NoAssembly(path)) =>
              path.endsWith(Path.of("dist", "histos.jar"))
            case _ => false
        yield assertTrue(missingJar)
    ,
    test("the version file has to match the manual ship"):
      ZIO.scoped:
        for
          root <- temp
          graph <- ZIO
            .fromEither(opened)
            .mapError(error => new IllegalStateException(error.message))
          _ <- ZIO.attempt {
            Files.writeString(root.resolve(".histos-version"), "9.9.9\n")
            Files.createDirectories(root.resolve("dist"))
            Files.write(root.resolve("dist").resolve("histos.jar"), jarBytes)
          }
          ready = LauncherPublish.ready(root, graph)
        yield assertTrue(
          ready == Left(LocalPublishError.VersionMismatch("9.9.9", version))
        )
    ,
    test("the assembly is a jar with an empty pom"):
      ZIO.scoped:
        for
          root <- temp
          repo <- temp
          graph <- ZIO
            .fromEither(opened)
            .mapError(error => new IllegalStateException(error.message))
          _ <- ZIO.attempt {
            Files.writeString(root.resolve(".histos-version"), s"$version\n")
            Files.createDirectories(root.resolve("dist"))
            Files.write(root.resolve("dist").resolve("histos.jar"), jarBytes)
          }
          group <- ZIO
            .fromEither(Organization.from("rocks.earlyeffect"))
            .mapError(error => new IllegalStateException(error.message))
          written = PublishOutput(
            Nil,
            List(
              PublishedModule(
                group,
                "histos-api_3",
                version,
                root.resolve("api.jar")
              )
            )
          )
          published <- LauncherPublish
            .upload(root, graph, written)
            .provide(Publisher.local(repo))
          pom <- ZIO.attempt(
            Files.readString(
              repo.resolve(
                "rocks/earlyeffect/histos/0.0.1-SNAPSHOT/histos-0.0.1-SNAPSHOT.pom"
              )
            )
          )
        yield assertTrue(
          published.modules.map(line =>
            s"${line.group.value} ${line.artifact} ${line.revision}"
          ) == List(
            "rocks.earlyeffect histos 0.0.1-SNAPSHOT",
            "rocks.earlyeffect histos-api_3 0.0.1-SNAPSHOT"
          ),
          pom.contains("<artifactId>histos</artifactId>"),
          !pom.contains("<dependencies>"),
          !pom.contains("_3")
        )
  )

  private val opened =
    val api = module("api", Jvm)(ScalaVersion("3.10.0")).artifact(
      ArtifactBase("histos-api")
    )
    Projects
      .organization(Organization("rocks.earlyeffect"))
      .assembly(ArtifactBase("histos"))(
        histos
          .ships(histos.manual("histos", ManualVersion("0.0.1-SNAPSHOT"))(api)),
        api
      )

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-launch"))):
      path =>
        ZIO.attempt {
          if Files.exists(path) then
            val walk = Files.walk(path)
            val paths =
              try walk.iterator.asScala.toList
              finally walk.close()
            paths
              .sortBy(_.getNameCount)
              .reverse
              .foreach(item => Files.deleteIfExists(item))
        }.ignore

  def spec = checks
