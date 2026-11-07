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

  def spec = suite("clean laws")(
    test("an empty selection wipes module roots, shared roots, and the observation"):
      ZIO.scoped:
        Workspace
          .work("histos-clean-all")
          .flatMap: repo =>
            for
              _          <- plant(repo)
              wiped      <- Products.named(repo, graph, Nil)
              coreGone   <- missing(classFile(repo, "core"))
              extraGone  <- missing(classFile(repo, "extra"))
              sharedGone <- missing(repo.resolve(".histos").resolve("provider").resolve("x"))
              tallyGone  <- missing(Products.observation(repo))
            yield assertTrue(
              wiped == Nil,
              coreGone,
              extraGone,
              sharedGone,
              tallyGone,
              Products.line(Nil) == "cleaned",
              Products.moduleRoots == List(
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
              ),
            )
    ,
    test("a module name leaves its neighbor and the shared roots, and an aggregate expands"):
      ZIO.scoped:
        Workspace
          .work("histos-clean-one")
          .flatMap: repo =>
            for
              _          <- plant(repo)
              named      <- Products.named(repo, graph, List(all.name))
              coreGone   <- missing(classFile(repo, "core"))
              extraKept  <- present(classFile(repo, "extra"))
              sharedKept <- present(repo.resolve(".histos").resolve("provider").resolve("x"))
              tallyGone  <- missing(Products.observation(repo))
            yield assertTrue(
              named == List(core.name),
              coreGone,
              extraKept,
              sharedKept,
              tallyGone,
              Products.line(List(core.name)) == "cleaned core",
            )
    ,
    test("an unknown or repeated name deletes nothing"):
      ZIO.scoped:
        Workspace
          .work("histos-clean-keep")
          .flatMap: repo =>
            val file    = classFile(repo, "core")
            val missing = ProjectName.trusted("missing")
            for
              _         <- plant(repo)
              unknown   <- Products.named(repo, graph, List(missing)).exit
              still     <- present(file)
              duplicate <- Products.named(repo, graph, List(core.name, core.name)).exit
              again     <- present(file)
            yield assertTrue(
              unknown match
                case Exit.Failure(cause) =>
                  cause.failureOption match
                    case Some(SelectError.Unknown(name, _)) => name == missing
                    case _                                  => false
                case Exit.Success(_) => false
              ,
              still,
              duplicate match
                case Exit.Failure(cause) =>
                  cause.failureOption.contains(SelectError.Duplicate(core.name))
                case Exit.Success(_) => false
              ,
              again,
              Products.forgotten(Nil) == "cleaned\nremoved nothing",
              Products.forgotten(List(entry)) == s"cleaned\nremoved ${entry.digest.value}",
              Products.sharedRoots == List("provider", "provider-src"),
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

  private def classFile(repo: Path, name: String): Path =
    repo.resolve(".histos").resolve("classes").resolve(name).resolve("A.class")

  private def plant(repo: Path): zio.IO[histos.workspace.IoFailure, Unit] =
    Workspace.writeString(classFile(repo, "core"), "c") *>
      Workspace.writeString(classFile(repo, "extra"), "e") *>
      Workspace.writeString(repo.resolve(".histos").resolve("provider").resolve("x"), "p") *>
      Workspace.writeString(Products.observation(repo), "{}")

  private def present(path: Path): zio.UIO[Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(path)).orDie

  private def missing(path: Path): zio.UIO[Boolean] =
    ZIO.attemptBlocking(Files.notExists(path)).orDie
end CleanLawSpec
