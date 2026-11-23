package histos.zinc

import histos.plugin.{PluginTaskId, TaskDep}
import histos.{ProjectGraph, ProjectName}

/** Output directories a plugin task emits as sources for a module. */
object Emitted:
  def directories(graph: ProjectGraph): Map[ProjectName, List[String]] =
    val pairs = graph.plugins.flatMap: plugin =>
      plugin.contribution.tasks.flatMap: task =>
        val modules = task.dependsOn.collect { case TaskDep.Emit(name) => name }
        val roots   = task.outputs.map(_.path.value)
        modules.map(name => name -> roots)
    pairs.groupBy(_._1).map { (name, rows) =>
      name -> rows.flatMap(_._2).distinct
    }

  /** Tasks to run before compiling `projects`. An empty list is every module. */
  def tasks(graph: ProjectGraph, projects: List[ProjectName]): List[PluginTaskId] =
    val wanted = if projects.isEmpty then None else Some(projects.toSet)
    graph.plugins.flatMap: plugin =>
      plugin.contribution.tasks.flatMap: task =>
        val emits = task.dependsOn.collect { case TaskDep.Emit(name) => name }
        val hit   = wanted match
          case None      => emits.nonEmpty
          case Some(set) => emits.exists(set.contains)
        if hit then List(PluginTaskId(plugin.id, task.name)) else Nil
end Emitted
