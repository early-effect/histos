package histos

import histos.plugin.{CommandName, JobName, NoteKey, PluginId, TaskName}
import histos.version.ShipName

enum BuildError:
  case EmptyName(position: SourcePos)
  case BadName(raw: String, position: SourcePos)
  case DuplicateName(name: ProjectName, first: SourcePos, second: SourcePos)
  case DuplicatePlatform(
      name: ProjectName,
      platform: Platform,
      position: SourcePos,
  )
  case DuplicateScalaVersion(
      name: ProjectName,
      version: ScalaVersion,
      position: SourcePos,
  )
  case DuplicateChild(
      name: ProjectName,
      child: ProjectName,
      position: SourcePos,
  )
  case AggregateIncludesItself(name: ProjectName, position: SourcePos)
  case AggregateChild(
      aggregate: ProjectName,
      child: ProjectName,
      position: SourcePos,
  )
  case MissingProject(name: String, referencedFrom: SourcePos)
  case DependsOnAggregate(
      module: ProjectName,
      aggregate: ProjectName,
      position: SourcePos,
  )
  case SelfDependency(name: ProjectName, position: SourcePos)
  case DuplicateDependency(
      module: ProjectName,
      dependency: ProjectName,
      position: SourcePos,
  )
  case EdgeWithoutPlatform(
      module: ProjectName,
      platform: Platform,
      position: SourcePos,
  )
  case UpstreamMissingPlatform(
      module: ProjectName,
      dependency: ProjectName,
      platform: Platform,
      position: SourcePos,
  )
  case Cycle(platform: Platform, path: List[ProjectName])
  case MissingClang(name: ProjectName, position: SourcePos)
  case EmptyShipName(position: SourcePos)
  case BadShipName(raw: String, position: SourcePos)
  case DuplicateShip(name: ShipName, first: SourcePos, second: SourcePos)
  case RepeatedShipMember(
      ship: ShipName,
      member: ProjectName,
      position: SourcePos,
  )
  case AggregateShipMember(
      ship: ShipName,
      aggregate: ProjectName,
      position: SourcePos,
  )
  case MemberInTwoShips(
      member: ProjectName,
      first: ShipName,
      second: ShipName,
      position: SourcePos,
  )
  case MixedScheme(position: SourcePos)
  case DuplicateResolver(token: String)
  case MixedTest(name: ProjectName, position: SourcePos)
  case DuplicateDirectory(
      name: ProjectName,
      other: ProjectName,
      directory: String,
  )
  case DuplicatePlugin(id: PluginId)
  case DuplicatePluginTask(plugin: PluginId, name: TaskName)
  case DuplicateCommand(name: CommandName)
  case DuplicateJob(name: JobName)
  case ReservedJob(name: JobName)
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

  def message: String = this match
    case EmptyName(position) =>
      s"a project name is not empty (${position.line}:${position.column})"
    case BadName(raw, position) =>
      s"'$raw' is not a project name (${position.line}:${position.column})"
    case DuplicateName(name, _, second) =>
      s"project '${name.value}' is declared twice (${second.line}:${second.column})"
    case DuplicatePlatform(name, platform, _) =>
      s"project '${name.value}' lists ${platform.toString.toLowerCase} twice"
    case DuplicateScalaVersion(name, version, _) =>
      s"project '${name.value}' lists Scala ${version.value} twice"
    case DuplicateChild(name, child, _) =>
      s"aggregate '${name.value}' lists '${child.value}' twice"
    case AggregateIncludesItself(name, _) =>
      s"aggregate '${name.value}' includes itself"
    case AggregateChild(aggregate, child, _) =>
      s"aggregate '${aggregate.value}' lists '${child.value}', which is not a module"
    case MissingProject(name, at) =>
      s"'$name' is referenced but not passed to Projects (${at.line}:${at.column})"
    case DependsOnAggregate(module, aggregate, _) =>
      s"'${module.value}' depends on aggregate '${aggregate.value}'"
    case SelfDependency(name, _)                    => s"'${name.value}' depends on itself"
    case DuplicateDependency(module, dependency, _) =>
      s"'${module.value}' depends on '${dependency.value}' twice"
    case EdgeWithoutPlatform(module, platform, _) =>
      s"'${module.value}' depends on ${platform.toString.toLowerCase} and does not build it"
    case UpstreamMissingPlatform(module, dependency, platform, _) =>
      s"'${module.value}' depends on '${dependency.value}' on ${platform.toString.toLowerCase}, and '${dependency.value}' does not build it"
    case Cycle(platform, path) =>
      s"dependency cycle on ${platform.toString.toLowerCase} ${path.map(_.value).mkString(" -> ")}"
    case MissingClang(name, position) =>
      s"project '${name.value}' targets Native and does not name a clang major (${position.line}:${position.column})"
    case EmptyShipName(position) =>
      s"a ship name is not empty (${position.line}:${position.column})"
    case BadShipName(raw, position) =>
      s"'$raw' is not a ship name (${position.line}:${position.column})"
    case DuplicateShip(name, _, second) =>
      s"ship '${name.value}' is declared twice (${second.line}:${second.column})"
    case RepeatedShipMember(ship, member, position) =>
      s"ship '${ship.value}' lists '${member.value}' twice (${position.line}:${position.column})"
    case AggregateShipMember(ship, aggregate, position) =>
      s"ship '${ship.value}' lists aggregate '${aggregate.value}' (${position.line}:${position.column})"
    case MemberInTwoShips(member, first, second, position) =>
      s"'${member.value}' is in ship '${first.value}' and ship '${second.value}' (${position.line}:${position.column})"
    case MixedScheme(position) =>
      s"a catalog is either early-effect ships or manual versions, and this one is both (${position.line}:${position.column})"
    case DuplicateResolver(token)  => s"repository $token is listed twice"
    case MixedTest(name, position) =>
      s"'${name.value}' names a test main and a test provider (${position.line}:${position.column})"
    case DuplicateDirectory(name, other, directory) =>
      s"'${name.value}' and '${other.value}' both use directory $directory"
    case DuplicatePlugin(id)               => s"plugin ${id.value} is registered twice"
    case DuplicatePluginTask(plugin, name) =>
      s"plugin ${plugin.value} declares task ${name.value} twice"
    case DuplicateCommand(name)             => s"command ${name.value} is declared twice"
    case DuplicateJob(name)                 => s"job ${name.value} is declared twice"
    case ReservedJob(name)                  => s"job ${name.value} is reserved"
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
end BuildError
