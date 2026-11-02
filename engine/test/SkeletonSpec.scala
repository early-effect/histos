package histos.scaffold

import histos.load.{ApiJar, Loader}
import histos.resolve.IdleResolve
import histos.cache.ContentStore
import histos.workspace.Workspace

import java.nio.file.Files

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue}

object SkeletonSpec extends ZIOSpecDefault:
  def spec: zio.test.Spec[TestEnvironment & Scope, Any] = suite("skeleton")(
    test("new writes one module pinned to the toolchain that wrote it"):
      ZIO.scoped:
        for
          parent <- Workspace.work("histos-new").mapError(_.message)
          dir    <- Skeleton.create(parent, "my-app", "0.0.2-20260101.120000-1").mapError(_.message)
          jar    <- ApiJar.locate.mapError(_.message)
          loaded <- ZIO.serviceWithZIO[Loader](_.load(dir, jar)).mapError(_.message)
          pin    <- ZIO.attempt(Files.readString(dir.resolve("build.properties"))).mapError(_.getMessage)
          found  <- ZIO
            .attempt {
              val walk = Files.walk(dir)
              try walk.iterator.asScala.map(dir.relativize(_).toString).toList
              finally walk.close()
            }
            .mapError(_.getMessage)
        yield assertTrue(
          loaded.graph.projects.map(_.name.value) == List("app"),
          loaded.graph.projects.flatMap {
            case module: histos.Module => module.platforms.all
            case _                     => Nil
          } == List(histos.Platform.Jvm),
          pin.trim == "histos.version=0.0.2-20260101.120000-1",
          found.filter(_.endsWith(".scala")).sorted == List(
            "app/src/main/scala/myapp/Main.scala",
            "app/src/test/scala/myapp/MainSpec.scala",
            "histos.scala",
          ),
        )
    ,
    test("new refuses a name that is not a project and a directory that is not empty"):
      ZIO.scoped:
        for
          parent <- Workspace.work("histos-new-refuse").mapError(_.message)
          bad    <- Skeleton.create(parent, "1bad", "0.0.2").either
          _      <- ZIO.attempt(Files.createDirectories(parent.resolve("taken"))).mapError(_.getMessage)
          _      <- ZIO.attempt(Files.writeString(parent.resolve("taken/notes.txt"), "x")).mapError(_.getMessage)
          full   <- Skeleton.create(parent, "taken", "0.0.2").either
        yield assertTrue(
          bad == Left(SkeletonError.Name("1bad")),
          full == Left(SkeletonError.NotEmpty(parent.resolve("taken"))),
        )
    ,
    test("init refuses when the build file is already there"):
      ZIO.scoped:
        for
          dir     <- Workspace.work("histos-init").mapError(_.message)
          _       <- ZIO.attempt(Files.writeString(dir.resolve("histos.scala"), "already\n")).mapError(_.getMessage)
          refused <- Skeleton.init(dir, "0.0.2").either
        yield assertTrue(refused == Left(SkeletonError.Exists(dir.resolve("histos.scala")))),
  ).provide(
    Loader.live,
    IdleResolve.layer,
    ZLayer.scoped:
      ZIO
        .acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-skeleton-cache")).orDie)(path =>
          Workspace.delete(path).ignore
        )
        .map(ContentStore.in),
  ) @@ TestAspect.withLiveSystem
