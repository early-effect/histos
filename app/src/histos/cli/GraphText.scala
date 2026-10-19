package histos.cli

import histos.load.PluginLink
import histos.plugin.{DeclaredInput, PluginLinked, RegisteredPlugin}
import histos.{Aggregate, Module, Project, ProjectGraph}

/** `histos graph` and the MCP `graph` tool. Env values are not printed. */
object GraphText:
  def render(graph: ProjectGraph, links: List[PluginLink]): String =
    val projects = renderProjects(graph.projects)
    val plugins = renderPlugins(graph.plugins, links)
    (projects, plugins) match
      case ("", "")     => ""
      case (found, "")  => found
      case ("", found)  => found
      case (left, right) => s"$left\n$right"

  private def renderProjects(projects: List[Project]): String =
    projects.map:
      case Module(name, platforms, versions, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        s"${name.value} ${platforms.all.map(_.toString.toLowerCase).mkString(",")} ${versions.all.map(_.value).mkString(",")}"
      case Aggregate(name, children, _) =>
        s"${name.value} aggregate ${children.map(_.value).mkString(",")}"
    .mkString("\n")

  private def renderPlugins(plugins: List[RegisteredPlugin], links: List[PluginLink]): String =
    plugins.flatMap(lines(_, links)).mkString("\n")

  private def lines(plugin: RegisteredPlugin, links: List[PluginLink]): List[String] =
    val kind = links.collectFirst { case link if link.id == plugin.id => link.linked } match
      case Some(PluginLinked.FromSource(published, running)) => s"source ${published.value} ${running.value}"
      case _                                                  => "binary"
    val tasks = plugin.contribution.tasks.map: task =>
      val needs = listed(task.needs.toList.map(_.toString).sorted)
      val paths = listed(task.inputs.collect:
        case DeclaredInput.File(path)      => path.value
        case DeclaredInput.Directory(path) => path.value
      )
      s"task ${plugin.id.value}/${task.name.value} ${task.seal} $needs $paths"
    val commands = plugin.contribution.commands.map(spec => s"command ${spec.name.value}")
    val jobs = plugin.contribution.jobs.map(job => s"job ${job.name.value}")
    s"plugin ${plugin.id.value} $kind" :: tasks ::: commands ::: jobs

  private def listed(values: List[String]): String =
    if values.isEmpty then "-" else values.mkString(",")
