package histos

import histos.plugin.Need
import histos.version.ShipName

/** Why a toolchain launcher was refused. */
enum ToolchainLauncherFault:
  case Missing
  case NotMember
  case NotAssembly

/** Why a toolchain client was refused. */
enum ToolchainClientFault:
  case Missing
  case NotNative
  case NoMain

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
  case DuplicateResolver(token: String)
  case MixedTest(name: ProjectName, position: SourcePos)
  case DuplicateDirectory(
      name: ProjectName,
      other: ProjectName,
      directory: String,
  )
  case PluginGraph(fault: PluginGraphFault)
  case Ci(fault: CiFault)
  case TwoPublications(name: ProjectName, position: SourcePos)
  case AssemblyShape(name: ProjectName, position: SourcePos)
  case AssetWithoutAssembly(name: ProjectName, position: SourcePos)
  case TwoAssets(name: ProjectName, position: SourcePos)
  case DuplicateResource(name: ProjectName, resource: ResourceName, position: SourcePos)
  case RemoteNeeds(name: ProjectName, needs: Set[Need], position: SourcePos)
  case ToolchainLauncher(
      ship: ShipName,
      module: ProjectName,
      fault: ToolchainLauncherFault,
      position: SourcePos,
  )
  case ToolchainClient(
      ship: ShipName,
      module: ProjectName,
      fault: ToolchainClientFault,
      position: SourcePos,
  )

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
    case DuplicateResolver(token)  => s"repository $token is listed twice"
    case MixedTest(name, position) =>
      s"'${name.value}' names a test main and a test provider (${position.line}:${position.column})"
    case DuplicateDirectory(name, other, directory) =>
      s"'${name.value}' and '${other.value}' both use directory $directory"
    case PluginGraph(fault)              => fault.message
    case Ci(fault)                       => fault.message
    case TwoPublications(name, position) =>
      s"'${name.value}' publishes two artifacts (${position.line}:${position.column})"
    case AssemblyShape(name, position) =>
      s"'${name.value}' publishes an assembly, which is one jvm jar (${position.line}:${position.column})"
    case AssetWithoutAssembly(name, position) =>
      s"'${name.value}' names an asset and does not publish an assembly (${position.line}:${position.column})"
    case TwoAssets(name, position) =>
      s"'${name.value}' names two assets (${position.line}:${position.column})"
    case DuplicateResource(name, resource, position) =>
      s"'${name.value}' names resource '${resource.value}' twice (${position.line}:${position.column})"
    case RemoteNeeds(name, needs, position) =>
      val listed = needs.toList.map(needWord).sorted.mkString(", ")
      s"'${name.value}' stores remote test results and needs $listed (${position.line}:${position.column})"
    case ToolchainLauncher(ship, module, fault, position) =>
      val why = fault match
        case ToolchainLauncherFault.Missing   => "that module is not in the build"
        case ToolchainLauncherFault.NotMember =>
          s"'${module.value}' is not a member of that ship"
        case ToolchainLauncherFault.NotAssembly =>
          s"'${module.value}' does not publish an assembly"
      s"ship '${ship.value}' names launcher '${module.value}', and $why (${position.line}:${position.column})"
    case ToolchainClient(ship, module, fault, position) =>
      val why = fault match
        case ToolchainClientFault.Missing   => "that module is not in the build"
        case ToolchainClientFault.NotNative => s"'${module.value}' does not build Native"
        case ToolchainClientFault.NoMain    => s"'${module.value}' does not name a main"
      s"ship '${ship.value}' names client '${module.value}', and $why (${position.line}:${position.column})"

  private def needWord(need: Need): String = need match
    case Need.Network => "network"
    case Need.Docker  => "docker"
end BuildError
