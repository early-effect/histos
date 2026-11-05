package histos.scaffold

import histos.cache.ContentStore
import histos.jdk.{Machine, Os}
import histos.load.{ApiJar, Loader}
import histos.native.NativeOrigin
import histos.resolve.Resolve
import histos.toolchain.Architecture
import histos.workflow.Workflow
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO, ZLayer}
import zio.durationInt
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue}

object ProvisionSpec extends ZIOSpecDefault:
  def spec: zio.test.Spec[TestEnvironment & Scope, Any] = suite("provision")(
    test("bare init from sbt writes the draft and skips ci and the lock"):
      ZIO.scoped:
        for
          dir <- Workspace.work("histos-bare-init").mapError(_.message)
          _   <- Workspace
            .writeString(
              dir.resolve("build.sbt"),
              """scalaVersion := "3.10.0"
                |libraryDependencies += "dev.zio" %% "zio" % "2.1.27"
                |""".stripMargin,
            )
            .mapError(_.message)
          jar <- ApiJar.locate.mapError(_.message)
          report <- Provision
            .init(
              dir,
              "0.0.2",
              jar,
              machine(dir),
              Provision.Request(ScaffoldKind.Bare, Some(ImportSource.Sbt), None, ZIO.succeed("main"), Map.empty),
            )
            .mapError(_.message)
          text <- ZIO.attempt(Files.readString(dir.resolve("histos.scala"))).mapError(_.getMessage)
        yield assertTrue(
          report.out.endsWith("histos test"),
          text.contains("""dep"dev.zio::zio:2.1.27""""),
          !Files.exists(dir.resolve(".gitignore")),
          !Files.exists(Workflow.file(dir, Workflow.fileName)),
          !Files.exists(dir.resolve("dependencies.lock")),
        )
    ,
    test("full init writes gitignore, the wrapper, ci, and a lock"):
      ZIO.scoped:
        for
          parent <- Workspace.work("histos-full-new").mapError(_.message)
          jar    <- ApiJar.locate.mapError(_.message)
          report <- Provision
            .create(
              parent,
              "my-app",
              "0.0.2",
              jar,
              machine(parent),
              Provision.Request(
                ScaffoldKind.Full,
                None,
                Some("main"),
                ZIO.succeed("main"),
                Map("CI" -> "true"),
              ),
            )
            .mapError(_.message)
          dir = parent.resolve("my-app")
          ignore <- ZIO.attempt(Files.readString(dir.resolve(".gitignore"))).mapError(_.getMessage)
          script <- ZIO.attempt(Files.readString(dir.resolve("histos"))).mapError(_.getMessage)
          ci     <- ZIO.attempt(Files.readString(Workflow.file(dir, Workflow.fileName))).mapError(_.getMessage)
          lock <- ZIO.attempt(Files.readString(dir.resolve("dependencies.lock"))).mapError(_.getMessage)
          executable <- ZIO.attempt(Files.isExecutable(dir.resolve("histos"))).mapError(_.getMessage)
        yield assertTrue(
          report.out.contains("histos test"),
          ignore.contains(".histos/"),
          ignore.contains("out/"),
          script.contains("HISTOS_HOME"),
          executable,
          ci.contains("name:"),
          lock.nonEmpty,
        )
    ,
  ).provide(
    ZLayer.scoped:
      ZIO
        .acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-provision-cache")).orDie)(path =>
          Workspace.delete(path).ignore
        )
        .map(ContentStore.in),
    Resolve.coursier,
    Loader.live,
  ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(5.minutes)

  private def machine(root: Path): Machine =
    Machine(
      Map.empty,
      root.toString,
      Os.Linux,
      Architecture.X86_64,
      root.resolve("cache"),
      root,
      None,
      NativeOrigin.Central,
    )
