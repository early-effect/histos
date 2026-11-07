package histos.watch

import histos.workspace.Workspace
import histos.{Module, Platform, Platforms, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import java.nio.file.Path

import scala.jdk.CollectionConverters.*

import zio.{ZIO, duration2DurationOps}
import zio.test.*

object WatchLawSpec extends ZIOSpecDefault:
  def spec = suite("watch laws")(
    test("roots are the build file and source trees, and .histos is not one of them"):
      val repo  = Path.of("/repo")
      val one   = SourceWatch.roots(repo, List(module("core")))
      val two   = SourceWatch.roots(repo, List(module("core"), module("extra")))
      val build = repo.resolve("histos.scala")
      assertTrue(
        one.contains(build),
        one.contains(repo.resolve("core").resolve("src")),
        one.contains(repo.resolve("core").resolve("test")),
        one.contains(repo.resolve("src")),
        one.contains(repo.resolve("test")),
        !two.contains(repo.resolve("src")),
        !two.contains(repo.resolve("test")),
        two.contains(repo.resolve("extra").resolve("src")),
        (one ::: two).forall(path => !path.iterator.asScala.exists(_.toString == ".histos")),
      )
    ,
    test("a missing root is an empty snapshot"):
      ZIO.scoped:
        Workspace
          .work("histos-watch-miss")
          .flatMap: repo =>
            SourceWatch.take(List(repo.resolve("gone"))).map(found => assertTrue(found.entries == Nil))
    ,
    test("an unchanged tree is still waiting after several polls"):
      ZIO.scoped:
        Workspace
          .work("histos-watch-still")
          .flatMap: repo =>
            val roots = SourceWatch.roots(repo, List(module("core")))
            for
              start  <- SourceWatch.take(roots)
              fiber  <- SourceWatch.since(roots, start).fork
              _      <- zio.test.TestClock.adjust(SourceWatch.Poll * 3)
              polled <- fiber.poll
              _      <- fiber.interrupt
            yield assertTrue(polled == None, start.entries == Nil)
    ,
    test("a write is reported once the poll and the settle have passed"):
      ZIO.scoped:
        Workspace
          .work("histos-watch-woke")
          .flatMap: repo =>
            val build = repo.resolve("histos.scala")
            val roots = SourceWatch.roots(repo, List(module("core")))
            for
              start <- SourceWatch.take(roots)
              _     <- Workspace.writeString(build, "module\n")
              fiber <- SourceWatch.since(roots, start).fork
              _     <- zio.test.TestClock.adjust(SourceWatch.Poll + SourceWatch.Settle)
              paths <- fiber.join
            yield assertTrue(paths == List(build.toAbsolutePath.normalize)),
  )

  private def module(name: String): Module =
    Module(
      ProjectName.trusted(name),
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion.trusted("3.3.7"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )
end WatchLawSpec
