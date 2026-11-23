package histos.zinc

import histos.plugin.{DeclaredOutput, EmittedContent, OutputKind, PluginTaskId, SourceSet, TaskDep}
import histos.{BuildError, CompilationStage, ContractFault, Module, ProjectGraph, ProjectName}

/** Emission metadata is attached to exact consuming stages, never to a File's parent. */
object Emitted:
  def roots(graph: ProjectGraph, stage: CompilationStage): Either[BuildError, List[StageRoot]] =
    val declarations = graph.plugins.flatMap: plugin =>
      plugin.contribution.tasks.flatMap: task =>
        val id = PluginTaskId(plugin.id, task.name)
        task.dependsOn.flatMap:
          case TaskDep.Emit(name) if name == stage.cell.module && stage.scope == SourceSet.Main =>
            task.outputs.map: output =>
              output match
                case DeclaredOutput.Directory(name, path) =>
                  Right(StageRoot.Sources(path, StageLayout.Main, StageOrigin.Generated(id, name)))
                case DeclaredOutput.File(name, _) =>
                  Left(
                    BuildError.Contract(
                      ContractFault.EmittedOutputKind(id, name, OutputKind.Directory, OutputKind.File)
                    )
                  )
          case TaskDep.EmitScoped(name, scope, platforms, content, outputName)
              if name == stage.cell.module && scope == stage.scope && platforms.contains(stage.cell.platform) =>
            val selected = task.outputs
              .find(_.name == outputName)
              .toRight(
                BuildError.Contract(ContractFault.EmittedOutput(id, outputName))
              )
              .flatMap: output =>
                if output.kind != content.outputKind then
                  Left(
                    BuildError
                      .Contract(ContractFault.EmittedOutputKind(id, outputName, content.outputKind, output.kind))
                  )
                else
                  val origin = StageOrigin.Generated(id, outputName)
                  Right(
                    content match
                      case EmittedContent.SourceFile(entry) => StageRoot.SourceFile(output.path, entry, origin)
                      case EmittedContent.SourceTree        => StageRoot.Sources(output.path, StageLayout.Exact, origin)
                      case EmittedContent.ResourceFile(name) => StageRoot.ResourceFile(output.path, name, origin)
                      case EmittedContent.ResourceTree => StageRoot.Resources(output.path, StageLayout.Exact, origin)
                  )
            List(selected)
          case TaskDep.Compile(_, _, _) | TaskDep.CompileScoped(_, _, _, _) | TaskDep.Link(_, _, _) | TaskDep.Same(_) |
              TaskDep.Generate(_) | TaskDep.Emit(_) | TaskDep.EmitScoped(_, _, _, _, _) =>
            Nil
    declarations
      .foldRight[Either[BuildError, List[StageRoot]]](Right(Nil)) { (value, rest) =>
        value.flatMap(root => rest.map(root :: _))
      }
      .map(_.distinct)

  def producers(graph: ProjectGraph, stage: CompilationStage): List[PluginTaskId] =
    graph.plugins.flatMap: plugin =>
      plugin.contribution.tasks.collect:
        case task if task.dependsOn.exists {
              case TaskDep.Emit(name) => name == stage.cell.module && stage.scope == SourceSet.Main
              case TaskDep.EmitScoped(name, scope, platforms, _, _) =>
                name == stage.cell.module && scope == stage.scope && platforms.contains(stage.cell.platform)
              case TaskDep.Compile(_, _, _) | TaskDep.CompileScoped(_, _, _, _) | TaskDep.Link(_, _, _) |
                  TaskDep.Same(_) | TaskDep.Generate(_) =>
                false
            } =>
          PluginTaskId(plugin.id, task.name)

  /** Legacy task selection includes the transitive module closure and both emission forms. */
  def tasks(graph: ProjectGraph, projects: List[ProjectName]): List[PluginTaskId] =
    val modules = graph.projects.collect { case module: Module => module }
    val wanted  = if projects.isEmpty then modules else Module.closure(modules, projects.toSet)
    wanted
      .flatMap: module =>
        for
          version  <- module.versions.all
          platform <- module.platforms.all
          scope    <- List(SourceSet.Main, SourceSet.Test)
          id       <- producers(graph, CompilationStage(histos.Cell(module.name, version, platform), scope))
        yield id
      .distinct
end Emitted
