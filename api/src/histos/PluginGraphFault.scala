package histos

import histos.plugin.{CommandName, JobName, NoteKey, PluginId, TaskName}

/** Faults of plugins declared on the project graph. Nested so the build-error schema stays inside the JVM method limit.
  */
enum PluginGraphFault:
  case DuplicatePlugin(id: PluginId)
  case DuplicatePluginTask(plugin: PluginId, name: TaskName)
  case DuplicateCommand(name: CommandName)
  case DuplicateJob(name: JobName)
  case ReservedJob(name: JobName)
  case DuplicateSite(first: JobName, second: JobName)
  case DuplicateNote(module: ProjectName, plugin: PluginId, key: NoteKey)
  case MissingNote(module: ProjectName, plugin: PluginId, key: NoteKey)
  case PluginMissingModule(plugin: PluginId, name: ProjectName)
  case PluginMissingCell(
      plugin: PluginId,
      module: ProjectName,
      version: Option[ScalaVersion],
      platform: Option[Platform],
  )
  case UnknownPluginTask(plugin: PluginId, name: TaskName)
  case CommandShadow(name: CommandName)
  case PluginDefect(plugin: PluginId, binary: String)
  case UnknownPluginModule(raw: String, position: SourcePos)
  case PluginOnAggregate(name: ProjectName, position: SourcePos)
  case DuplicatePluginCoordinate(render: String, position: SourcePos)

  def message: String = this match
    case DuplicatePlugin(id)               => s"plugin ${id.value} is registered twice"
    case DuplicatePluginTask(plugin, name) =>
      s"plugin ${plugin.value} declares task ${name.value} twice"
    case DuplicateCommand(name)       => s"command ${name.value} is declared twice"
    case DuplicateJob(name)           => s"job ${name.value} is declared twice"
    case ReservedJob(name)            => s"job ${name.value} is reserved"
    case DuplicateSite(first, second) =>
      s"job ${first.value} and job ${second.value} both name a site"
    case DuplicateNote(module, plugin, key) =>
      s"module ${module.value} repeats note ${plugin.value}/${key.value}"
    case MissingNote(module, plugin, key) =>
      s"module ${module.value} has no note ${plugin.value}/${key.value}"
    case PluginMissingModule(plugin, name) =>
      s"plugin ${plugin.value} names '${name.value}', which is not a module"
    case PluginMissingCell(plugin, module, version, platform) =>
      val named = (version, platform) match
        case (Some(found), Some(target)) =>
          s"Scala ${found.value} on ${target.toString.toLowerCase}"
        case (Some(found), None)  => s"Scala ${found.value}"
        case (None, Some(target)) => target.toString.toLowerCase
        case (None, None)         => "a cell"
      s"plugin ${plugin.value} names $named of '${module.value}', and '${module.value}' does not build it"
    case UnknownPluginTask(plugin, name) =>
      s"plugin ${plugin.value} does not declare task ${name.value}"
    case CommandShadow(name)          => s"command ${name.value} is a built-in"
    case PluginDefect(plugin, binary) =>
      s"plugin ${plugin.value} failed while contributing ($binary)"
    case UnknownPluginModule(raw, position) =>
      s"'$raw' is not a module in this graph (${position.line}:${position.column})"
    case PluginOnAggregate(name, position) =>
      s"'${name.value}' is an aggregate, and a plugin coordinate names a module (${position.line}:${position.column})"
    case DuplicatePluginCoordinate(render, position) =>
      s"plugin $render is named twice (${position.line}:${position.column})"
end PluginGraphFault
