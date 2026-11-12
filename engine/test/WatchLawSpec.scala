package histos.watch

import histos.workspace.Workspace
import histos.{Module, Platform, Platforms, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{ZIO, duration2DurationOps, durationInt}
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
            yield assertTrue(paths == List(build.toAbsolutePath.normalize))
    ,
    test("a same-size edit with the modified time put back still wakes the watch"):
      ZIO.scoped:
        Workspace
          .work("histos-watch-same")
          .flatMap: repo =>
            val build = repo.resolve("histos.scala")
            val roots = SourceWatch.roots(repo, List(module("core")))
            for
              _     <- Workspace.writeString(build, "one\n")
              start <- SourceWatch.take(roots)
              stamp <- ZIO.attemptBlocking(Files.getLastModifiedTime(build))
              _     <- Workspace.writeString(build, "two\n")
              _     <- ZIO.attemptBlocking(Files.setLastModifiedTime(build, stamp))
              kept  <- ZIO.attemptBlocking(Files.getLastModifiedTime(build))
              fiber <- SourceWatch.since(roots, start).fork
              _     <- zio.test.TestClock.adjust(SourceWatch.Poll + SourceWatch.Settle)
              paths <- fiber.join
            yield assertTrue(kept == stamp, paths == List(build.toAbsolutePath.normalize))
    // A watch that misses the edit polls forever. The timeout runs on the live clock and ends the law.
    @@ TestAspect.timeout(30.seconds),
    test("a declared output inside a watched directory does not wake, and another file there does"):
      ZIO.scoped:
        Workspace
          .work("histos-watch-output")
          .flatMap: repo =>
            val docs   = repo.resolve("docs")
            val stamp  = docs.resolve("dev-stamp")
            val source = docs.resolve("index.md")
            for
              _     <- Workspace.createDirectories(docs)
              _     <- Workspace.writeString(stamp, "1\n")
              _     <- Workspace.writeString(source, "page\n")
              start <- SourceWatch.take(List(docs), Set(stamp))
              quiet <- SourceWatch.since(List(docs), start, Set(stamp)).fork
              _     <- Workspace.writeString(stamp, "2\n")
              _     <- zio.test.TestClock.adjust(SourceWatch.Poll * 3)
              held  <- quiet.poll
              _     <- quiet.interrupt
              woke  <- SourceWatch.since(List(docs), start, Set(stamp)).fork
              _     <- Workspace.writeString(source, "next\n")
              _     <- zio.test.TestClock.adjust(SourceWatch.Poll + SourceWatch.Settle)
              paths <- woke.join
            yield assertTrue(
              held == None,
              start.entries.map(_.path).contains(source.toAbsolutePath.normalize.toString),
              !start.entries.map(_.path).contains(stamp.toAbsolutePath.normalize.toString),
              paths == List(source.toAbsolutePath.normalize),
            ),
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
