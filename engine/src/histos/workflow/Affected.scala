package histos.workflow

import histos.{Cell, CompilationStage, Module, ProjectGraph, ProjectName}
import histos.build.{BuildDagError, BuildNode}
import histos.plugin.SourceSet
import histos.watch.SourceWatch
import histos.zinc.StageRootError
import java.nio.file.Path

/** Shared roots and producer inputs select every consuming owner, including dependency consumers. */
object Affected:
  enum Selection:
    case Everything
    case Modules(names: List[ProjectName])

  def project(graph: ProjectGraph, paths: List[String]): Either[BuildDagError | StageRootError, Selection] =
    if paths.exists(buildFile) then Right(Selection.Everything)
    else
      val repo    = Path.of(".").toAbsolutePath.normalize
      val changed = paths.map(repo.resolve(_).normalize)
      val modules = graph.projects.collect { case module: Module => module }
      modules
        .foldLeft[Either[BuildDagError | StageRootError, List[ProjectName]]](Right(Nil)) { (acc, module) =>
          for
            seen       <- acc
            projection <- SourceWatch.project(
              repo,
              graph,
              for
                version  <- module.versions.all
                platform <- module.platforms.all
                scope    <- List(SourceSet.Main, SourceSet.Test)
              yield BuildNode.Compile(CompilationStage(Cell(module.name, version, platform), scope)),
            )
          yield
            val inputs = changed.filterNot(path => projection.excluded.exists(_.contains(path)))
            if inputs.exists(path => projection.inputs.exists(_.contains(path))) then seen :+ module.name else seen
        }
        .map(Selection.Modules(_))

  /** Invalid declarations conservatively select everything; typed callers use project. */
  def decide(graph: ProjectGraph, paths: List[String]): Selection =
    project(graph, paths).getOrElse(Selection.Everything)

  /** `base == "all"` and a failed diff both select the whole suite. An empty diff selects nothing. */
  def report(graph: ProjectGraph, base: String, paths: Option[List[String]]): String =
    if base == "all" || paths.isEmpty then """["all"]"""
    else encode(decide(graph, paths.getOrElse(Nil)))

  private def encode(selection: Selection): String = selection match
    case Selection.Everything     => """["all"]"""
    case Selection.Modules(names) => names.map(name => s"\"${name.value}\"").mkString("[", ",", "]")

  private def buildFile(path: String): Boolean =
    path == "histos.scala" || path.endsWith("/histos.scala") ||
      path == "histos.plugins.scala" || path == "histos.strategies.scala"
end Affected
