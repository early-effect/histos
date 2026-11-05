package histos.scaffold

import histos.cache.ContentStore
import histos.load.{ApiJar, Loader}
import histos.resolve.IdleResolve
import histos.workspace.Workspace

import java.nio.file.Files

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue}

object ImportDraftSpec extends ZIOSpecDefault:
  private val build =
    """ThisBuild / organization := "com.example"
      |ThisBuild / scalaVersion := "3.10.0"
      |val zioVersion = "2.1.27"
      |
      |lazy val core = (project in file("core"))
      |  .settings(
      |    libraryDependencies += "dev.zio" %% "zio" % zioVersion,
      |  )
      |
      |lazy val app = (project in file("app"))
      |  .dependsOn(core)
      |  .settings(
      |    libraryDependencies += "dev.zio" %% "zio-test" % "2.1.27" % Test,
      |    Compile / mainClass := Some("demo.Main"),
      |    scalacOptions += "-Wunused:all",
      |  )
      |
      |lazy val probe = (project in file("probe"))
      |  .dependsOn(core % "test->compile")
      |""".stripMargin

  private val plugins =
    """addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
      |""".stripMargin

  private val zipx =
    """val scalaVersion = "3.10.0"
      |val zio = "2.1.27"
      |Lib(s"dev.zio::zio:$zio")
      |Lib("dev.zio::zio-test:2.1.27", "test")
      |Pin("sha256:abc", "pkg:generic/protoc@1")
      |Plugin("pages")
      |""".stripMargin

  def spec: zio.test.Spec[TestEnvironment & Scope, Any] = suite("import draft")(
    test("sbt and zipx drafts keep mapped coordinates and comment the rest"):
      val drafted = ImportDraft.sbt(build, plugins, "demo")
      val fromZip = ImportDraft.zipx(zipx, List("core"), "demo")
      assertTrue(
        drafted.contains("scala\"3.10.0\""),
        drafted.contains("dep\"dev.zio::zio:2.1.27\""),
        drafted.contains(".testDependencies(dep\"dev.zio::zio-test:2.1.27\")"),
        drafted.contains(".dependsOn(core)"),
        drafted.contains("MainClass(\"demo.Main\")"),
        drafted.contains("organization\"com.example\""),
        drafted.contains("// unmapped: scalacOptions += \"-Wunused:all\""),
        drafted.contains("// unmapped: addSbtPlugin"),
        drafted.contains("// unmapped: .dependsOn(core % \"test->compile\")"),
        fromZip.contains("dep\"dev.zio::zio:2.1.27\""),
        fromZip.contains(".testDependencies(dep\"dev.zio::zio-test:2.1.27\")"),
        fromZip.contains("// unmapped: Pin"),
        fromZip.contains("// unmapped: Plugin"),
        fromZip.contains("module(\"core\""),
      )
    ,
    test("an sbt draft loads as a histos graph"):
      ZIO.scoped:
        for
          dir    <- Workspace.work("histos-import").mapError(_.message)
          draft  <- ZIO.succeed(ImportDraft.sbt(build, plugins, "demo"))
          _      <- Workspace.writeString(dir.resolve("build.properties"), "histos.version=0.0.2\n").mapError(_.message)
          _      <- Workspace.writeString(dir.resolve("histos.scala"), draft).mapError(_.message)
          jar    <- ApiJar.locate.mapError(_.message)
          loaded <- ZIO.serviceWithZIO[Loader](_.load(dir, jar)).mapError(_.message)
        yield assertTrue(loaded.graph.projects.map(_.name.value).sorted == List("app", "core", "probe"))
    ,
  ).provide(
    Loader.live,
    IdleResolve.layer,
    ZLayer.scoped:
      ZIO
        .acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-import-cache")).orDie)(path =>
          Workspace.delete(path).ignore
        )
        .map(ContentStore.in),
  ) @@ TestAspect.withLiveSystem
