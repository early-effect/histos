package histos.clean

import histos.cache.CacheEntry
import histos.schedule.Digest
import histos.select.SelectError
import histos.workspace.Workspace
import histos.{
  Aggregate,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}

import java.nio.file.{Files, Path}
import java.time.Instant

import zio.{Exit, ZIO}
import zio.test.*

object CleanLawSpec extends ZIOSpecDefault:
  private val core  = module("core")
  private val extra = module("extra")
  private val all   = Aggregate(ProjectName.trusted("all"), List(core.name), SourcePos("histos.scala", 2, 1))
  private val graph = ProjectGraph.trusted(List(core, all, extra))

  /** Where compile, test, pack, link, run, doc, and coverage write one module's products. */
  private val moduleRoots =
    List(
      "classes",
      "analysis",
      "test-classes",
      "test-analysis",
      "test-work",
      "test-quick",
      "tests",
      "jars",
      "link",
      "run",
      "doc",
      "coverage/classes",
      "coverage/analysis",
    )

  /** Products every module shares, including the coverage report. */
  private val sharedRoots = List("provider", "provider-src", "coverage")

  def spec = suite("clean laws")(
    test("an empty selection wipes every module root, every shared root, and the observation, and keeps the rest"):
      ZIO.scoped:
        Workspace
          .work("histos-clean-all")
          .flatMap: repo =>
            for
              _     <- plant(repo)
              wiped <- Products.named(repo, graph, Nil)
              left  <- remaining(planted(repo))
              kept  <- present(repo.resolve(".histos").resolve("keep.txt"))
              tally <- missing(Products.observation(repo))
            yield assertTrue(wiped == Nil, left == Nil, kept, tally, Products.line(Nil) == "cleaned")
    ,
    test("a module name wipes that module under every module root, and its neighbor and the shared roots stay"):
      ZIO.scoped:
        Workspace
          .work("histos-clean-one")
          .flatMap: repo =>
            for
              _     <- plant(repo)
              named <- Products.named(repo, graph, List(all.name))
              neighbor = moduleRoots.map(root => file(repo, root, "extra")) ++ shared(repo)
              gone   <- remaining(moduleRoots.map(root => file(repo, root, "core")))
              stayed <- remaining(neighbor)
              tally  <- missing(Products.observation(repo))
            yield assertTrue(
              named == List(core.name),
              gone == Nil,
              stayed == neighbor,
              tally,
              Products.line(List(core.name)) == "cleaned core",
            )
    ,
    test("an unknown or repeated name deletes nothing"):
      ZIO.scoped:
        Workspace
          .work("histos-clean-keep")
          .flatMap: repo =>
            val missing = ProjectName.trusted("missing")
            for
              _         <- plant(repo)
              unknown   <- Products.named(repo, graph, List(missing)).exit
              duplicate <- Products.named(repo, graph, List(core.name, core.name)).exit
              left      <- remaining(planted(repo))
            yield assertTrue(
              unknown match
                case Exit.Failure(cause) =>
                  cause.failureOption match
                    case Some(SelectError.Unknown(name, _)) => name == missing
                    case _                                  => false
                case Exit.Success(_) => false
              ,
              duplicate match
                case Exit.Failure(cause) =>
                  cause.failureOption.contains(SelectError.Duplicate(core.name))
                case Exit.Success(_) => false
              ,
              left == planted(repo),
              Products.forgotten(Nil) == "cleaned\nremoved nothing",
              Products.forgotten(List(entry)) == s"cleaned\nremoved ${entry.digest.value}",
              Products.observation(repo) == repo.resolve(".histos").resolve("observation.json"),
            ),
  )

  private val entry =
    CacheEntry(Digest.trusted("ab" * 32), Path.of("cas"), 1L, Instant.EPOCH)

  private def module(name: String): Module =
    Module(
      ProjectName.trusted(name),
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion.trusted("3.3.7"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )

  private def file(repo: Path, root: String, module: String): Path =
    repo.resolve(".histos").resolve(root).resolve(module).resolve("3.3.7").resolve("jvm").resolve("A")

  private def shared(repo: Path): List[Path] =
    sharedRoots.map(root => repo.resolve(".histos").resolve(root).resolve("x"))

  private def planted(repo: Path): List[Path] =
    moduleRoots.flatMap(root => List(file(repo, root, "core"), file(repo, root, "extra"))) ++ shared(repo)

  private def plant(repo: Path): zio.IO[histos.workspace.IoFailure, Unit] =
    ZIO.foreachDiscard(planted(repo))(Workspace.writeString(_, "p")) *>
      Workspace.writeString(repo.resolve(".histos").resolve("keep.txt"), "a") *>
      Workspace.writeString(Products.observation(repo), "{}")

  /** The files of `paths` still on disk. */
  private def remaining(paths: List[Path]): zio.UIO[List[Path]] =
    ZIO.filter(paths)(present)

  private def present(path: Path): zio.UIO[Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(path)).orDie

  private def missing(path: Path): zio.UIO[Boolean] =
    ZIO.attemptBlocking(Files.notExists(path)).orDie
end CleanLawSpec
