package histos.build

import histos.plugin.{DeclaredTask, PluginTaskId, SourceSet, TaskDep}
import histos.zinc.{Emitted, UpstreamCells}
import histos.{
  BuildError,
  Cell,
  CompilationStage,
  ContractFault,
  GraphContracts,
  JsOptimize,
  Module,
  Platform,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
}

enum BuildNode:
  case Compile(stage: CompilationStage)
  case Generate(task: PluginTaskId)
  case Link(cell: Cell, optimize: JsOptimize)

  def logical: histos.BuildNode = this match
    case Compile(stage) => histos.BuildNode.Compile(stage)
    case Generate(task) => histos.BuildNode.Task(task)
    case Link(cell, _)  => histos.BuildNode.Link(cell)
end BuildNode

enum BuildDagError:
  case Declaration(error: BuildError)
  case UnknownNode(node: BuildNode)
  case MissingUpstream(stage: CompilationStage, module: ProjectName)
  case MissingCell(task: PluginTaskId, module: ProjectName, version: Option[ScalaVersion], platform: Option[Platform])

final class ExecutionPlan private[build] (
    val roots: List[BuildNode],
    val nodes: List[BuildNode],
    edges: Map[BuildNode, List[BuildNode]],
):
  def dependencies(node: BuildNode): Either[BuildDagError, List[BuildNode]] =
    edges.get(node).toRight(BuildDagError.UnknownNode(node))

/** Runtime ordering preserves engine cross-version candidates; TASTy acceptance remains at the compile leaf. */
object BuildDag:
  def plan(graph: ProjectGraph, roots: List[BuildNode]): Either[BuildDagError, ExecutionPlan] =
    val modules  = graph.projects.collect { case module: Module => module }
    val declared = modules.flatMap { module =>
      for
        version  <- module.versions.all
        platform <- module.platforms.all
      yield (module.name, version, platform)
    }.toSet
    val tasks = graph.plugins.flatMap { plugin =>
      plugin.contribution.tasks.map(task => PluginTaskId(plugin.id, task.name) -> task)
    }.toMap

    def module(cell: Cell, node: BuildNode): Either[BuildDagError, Module] =
      modules
        .find(_.name == cell.module)
        .filter(item => item.versions.all.contains(cell.version) && item.platforms.all.contains(cell.platform))
        .toRight(BuildDagError.UnknownNode(node))

    def upstream(stage: CompilationStage, item: Module): Either[BuildDagError, List[BuildNode]] =
      val cell = stage.cell
      sequence(item.upstream(cell.platform, stage.scope).map { name =>
        modules.find(_.name == name) match
          case None             => Left(BuildDagError.MissingUpstream(stage, name))
          case Some(dependency) =>
            val selected = UpstreamCells.required(cell.version, dependency, cell.platform, declared)
            if selected.isEmpty then Left(BuildDagError.MissingUpstream(stage, name))
            else
              Right(selected.map { (name, version, platform) =>
                BuildNode.Compile(CompilationStage(Cell(name, version, platform), stage.scope))
              })
      }).map(_.flatten)

    def cells(
        task: PluginTaskId,
        name: ProjectName,
        version: Option[ScalaVersion],
        platform: Option[Platform],
    ): Either[BuildDagError, List[Cell]] =
      val selected = modules.filter(_.name == name).flatMap { item =>
        for
          scala  <- item.versions.all.filter(value => version.forall(_ == value))
          target <- item.platforms.all.filter(value => platform.forall(_ == value))
        yield Cell(name, scala, target)
      }
      if selected.isEmpty then Left(BuildDagError.MissingCell(task, name, version, platform)) else Right(selected)

    def taskDependencies(id: PluginTaskId, task: DeclaredTask): Either[BuildDagError, List[BuildNode]] =
      sequence(task.dependsOn.map {
        case TaskDep.Compile(name, version, platform) =>
          cells(id, name, version, platform).map(
            _.map(cell => BuildNode.Compile(CompilationStage(cell, SourceSet.Test)))
          )
        case TaskDep.CompileScoped(name, version, platform, scope) =>
          cells(id, name, version, platform).map(_.map(cell => BuildNode.Compile(CompilationStage(cell, scope))))
        case TaskDep.Link(name, version, optimize) =>
          cells(id, name, version, Some(Platform.Js)).flatMap { selected =>
            sequence(
              selected.map(cell =>
                module(cell, BuildNode.Compile(CompilationStage(cell, SourceSet.Main))).map { item =>
                  BuildNode.Link(cell, JsOptimize.effective(graph.tools.js.optimize, item.jsOptimize, optimize))
                }
              )
            )
          }
        case TaskDep.Same(name)     => Right(List(BuildNode.Generate(PluginTaskId(id.plugin, name))))
        case TaskDep.Generate(name) => Right(List(BuildNode.Generate(PluginTaskId(id.plugin, name))))
        case TaskDep.Emit(_) | TaskDep.EmitScoped(_, _, _, _, _) => Right(Nil)
      }).map(_.flatten.distinct)

    def dependencies(node: BuildNode): Either[BuildDagError, List[BuildNode]] = node match
      case BuildNode.Compile(stage) =>
        for
          item     <- module(stage.cell, node)
          previous <- upstream(stage, item)
        yield
          val own = stage.scope match
            case SourceSet.Main => Nil
            case SourceSet.Test => List(BuildNode.Compile(stage.copy(scope = SourceSet.Main)))
          (own ++ previous ++ Emitted.producers(graph, stage).map(BuildNode.Generate(_))).distinct
      case BuildNode.Generate(id) =>
        tasks.get(id).toRight(BuildDagError.UnknownNode(node)).flatMap(taskDependencies(id, _))
      case BuildNode.Link(cell, _) =>
        if cell.platform != Platform.Js then Left(BuildDagError.UnknownNode(node))
        else module(cell, node).map(_ => List(BuildNode.Compile(CompilationStage(cell, SourceSet.Main))))

    def walk(
        node: BuildNode,
        path: List[BuildNode],
        seen: Set[BuildNode],
        ordered: List[BuildNode],
        edges: Map[BuildNode, List[BuildNode]],
    ): Either[BuildDagError, (Set[BuildNode], List[BuildNode], Map[BuildNode, List[BuildNode]])] =
      if path.exists(_.logical == node.logical) then
        val cycle = (node :: path).reverse.dropWhile(_.logical != node.logical).map(_.logical)
        Left(BuildDagError.Declaration(BuildError.Contract(ContractFault.Cycle(cycle))))
      else if seen.contains(node) then Right((seen, ordered, edges))
      else
        dependencies(node).flatMap { children =>
          children
            .foldLeft[Either[BuildDagError, (Set[BuildNode], List[BuildNode], Map[BuildNode, List[BuildNode]])]](
              Right((seen, ordered, edges.updated(node, children)))
            ) { (acc, child) =>
              acc.flatMap { (done, built, known) => walk(child, node :: path, done, built, known) }
            }
            .map { (done, built, known) => (done + node, built :+ node, known) }
        }

    for
      _       <- GraphContracts.validate(graph.projects, graph.plugins).left.map(BuildDagError.Declaration(_))
      closure <- roots.distinct
        .foldLeft[Either[BuildDagError, (Set[BuildNode], List[BuildNode], Map[BuildNode, List[BuildNode]])]](
          Right((Set.empty, Nil, Map.empty))
        ) { (acc, root) => acc.flatMap { (done, built, known) => walk(root, Nil, done, built, known) } }
    yield new ExecutionPlan(roots.distinct, closure._2, closure._3)
  end plan

  private def sequence[A](values: List[Either[BuildDagError, A]]): Either[BuildDagError, List[A]] =
    values.foldRight[Either[BuildDagError, List[A]]](Right(Nil))((value, rest) =>
      value.flatMap(item => rest.map(item :: _))
    )
end BuildDag
