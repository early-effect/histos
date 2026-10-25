package histos.clean

import histos.cache.CacheEntry
import histos.select.Selection
import histos.workflow.CiError
import histos.workspace.{IoFailure, Workspace}
import histos.{Module, ProjectGraph, ProjectName}

import java.nio.file.Path

import zio.{IO, ZIO}

/** Build products under `.histos/`. Locks, the assembly, and the server stay.
  */
object Products:
  val moduleRoots: List[String] = List(
    "classes",
    "analysis",
    "test-classes",
    "test-analysis",
    "test-work",
    "test-quick",
    "jars",
    "link",
    "run"
  )

  val sharedRoots: List[String] = List("provider", "provider-src")

  /** The compile tally `observe` reads. A module wipe leaves it. */
  def observation(repo: Path): Path =
    dot(repo).resolve("observation.json")

  /** Every product root, and the stored tally. Does not read the build. */
  def wipe(repo: Path): IO[IoFailure, Unit] =
    ZIO.foreachDiscard(moduleRoots ++ sharedRoots)(name =>
      Workspace.delete(dot(repo).resolve(name))
    ) *> Workspace.delete(observation(repo))

  /** One module's subtree under each per-module root. Shared roots stay. */
  def wipe(repo: Path, modules: List[Module]): IO[IoFailure, Unit] =
    ZIO.foreachDiscard(modules)(module =>
      ZIO.foreachDiscard(moduleRoots)(name =>
        Workspace.delete(dot(repo).resolve(name).resolve(module.name.value))
      )
    )

  /** Empty names are the full wipe, including shared roots. A name selects
    * modules only: an aggregate expands to its modules, and upstream stays. A
    * bad name deletes nothing.
    */
  def named(
      repo: Path,
      graph: ProjectGraph,
      names: List[String]
  ): IO[CiError | IoFailure, List[ProjectName]] =
    if names.isEmpty then wipe(repo).as(Nil)
    else
      ZIO.fromEither(Selection.select(graph, names)).flatMap { selected =>
        wipe(repo, selected.modules).as(selected.modules.map(_.name))
      }

  def line(names: List[ProjectName]): String =
    if names.isEmpty then "cleaned"
    else s"cleaned ${names.map(_.value).mkString(", ")}"

  /** `cleaned`, then one `removed` line per entry the store dropped. */
  def forgotten(entries: List[CacheEntry]): String =
    val dropped =
      if entries.isEmpty then "removed nothing"
      else
        entries
          .map(entry => s"removed ${entry.digest.value}")
          .mkString("\n")
    s"cleaned\n$dropped"

  def fault(error: CiError | IoFailure): String =
    error match
      case found: CiError   => found.message
      case found: IoFailure => found.message

  private def dot(repo: Path): Path = repo.resolve(".histos")
