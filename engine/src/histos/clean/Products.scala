package histos.clean

import histos.cache.CacheEntry
import histos.select.{SelectError, Selection}
import histos.workspace.{IoFailure, Workspace}
import histos.{Cell, CompilationStage, Module, ProjectGraph, ProjectName}
import histos.build.{BuildDag, BuildDagError, BuildNode}
import histos.plugin.{OutputKind, PluginTaskId, SourceSet}

import java.nio.file.Path

import zio.{IO, ZIO}

final case class GeneratedCleaner[R, E](clean: PluginTaskId => ZIO[R, E, Unit])
final case class OwnedProduct(path: Path, extent: OutputKind)
final case class CleanProjection(modules: List[Module], roots: List[OwnedProduct], tasks: List[PluginTaskId])

/** Build products under `.histos/` and declared generated ownership. The server and launcher pins stay. */
object Products:
  def project(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
  ): Either[SelectError | BuildDagError, CleanProjection] =
    val tasks = graph.plugins
      .flatMap(plugin => plugin.contribution.tasks.map(task => PluginTaskId(plugin.id, task.name) -> task))
      .toMap
    for
      selected <- Selection.select(graph, names)
      dag      <- BuildDag.plan(
        graph,
        selected.modules.flatMap(module =>
          for
            version  <- module.versions.all
            platform <- module.platforms.all
            scope    <- List(SourceSet.Main, SourceSet.Test)
          yield BuildNode.Compile(CompilationStage(Cell(module.name, version, platform), scope))
        ),
      )
    yield
      val generators = if names.isEmpty then tasks.keys.toList.sortBy(_.render)
      else dag.nodes.collect { case BuildNode.Generate(id) => id }
      val ordinary = if names.isEmpty then
        (moduleRoots ++ sharedRoots).map(name => OwnedProduct(dot(repo).resolve(name), OutputKind.Directory))
      else
        selected.modules.flatMap(module =>
          moduleRoots.map(name =>
            OwnedProduct(dot(repo).resolve(name).resolve(module.name.value), OutputKind.Directory)
          )
        )
      val generated = generators.flatMap(id =>
        tasks
          .get(id)
          .toList
          .flatMap(_.outputs.map(output => OwnedProduct(repo.resolve(output.path.value), output.kind)))
      )
      CleanProjection(
        selected.modules,
        (ordinary ++ generated :+ OwnedProduct(observation(repo), OutputKind.File)).distinct,
        generators,
      )

  /** The capability must retain the product store's ownership validation and active-reader gate. */
  def namedUsing[R, E](generated: GeneratedCleaner[R, E])(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
  ): ZIO[R, E | SelectError | BuildDagError | IoFailure, List[ProjectName]] =
    for
      projection <- ZIO.fromEither(project(repo, graph, names))
      _          <- ZIO.foreachDiscard(projection.tasks)(generated.clean)
      _          <- if names.isEmpty then wipe(repo) else wipe(repo, projection.modules)
    yield if names.isEmpty then Nil else projection.modules.map(_.name)

  val moduleRoots: List[String] = List(
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

  /** The coverage report spans every module, so only the full wipe removes it. */
  val sharedRoots: List[String] = List("provider", "provider-src", "coverage")

  /** The compile tally `observe` reads. A successful clean deletes it. */
  def observation(repo: Path): Path =
    dot(repo).resolve("observation.json")

  /** Every product root, and the stored tally. Does not read the build. */
  def wipe(repo: Path): IO[IoFailure, Unit] =
    ZIO.foreachDiscard(moduleRoots ++ sharedRoots)(name => Workspace.delete(dot(repo).resolve(name))) *> Workspace
      .delete(observation(repo))

  /** One module's subtree under each per-module root. Shared roots stay. The stored tally goes too: it named products
    * this wipe just removed.
    */
  def wipe(repo: Path, modules: List[Module]): IO[IoFailure, Unit] =
    ZIO.foreachDiscard(modules)(module =>
      ZIO.foreachDiscard(moduleRoots)(name => Workspace.delete(dot(repo).resolve(name).resolve(module.name.value)))
    ) *> Workspace.delete(observation(repo))

  /** Empty names are the full wipe, including shared roots. A name selects modules only: an aggregate expands to its
    * modules, and upstream stays. An unknown or repeated name deletes nothing.
    */
  def named(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
  ): IO[SelectError | IoFailure, List[ProjectName]] =
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

  def fault(error: SelectError | IoFailure): String =
    error match
      case found: SelectError => found.message
      case found: IoFailure   => found.message

  private def dot(repo: Path): Path = repo.resolve(".histos")
end Products
