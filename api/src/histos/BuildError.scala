package histos

import histos.version.ShipName

enum BuildError:
  case EmptyName(position: SourcePos)
  case BadName(raw: String, position: SourcePos)
  case DuplicateName(name: ProjectName, first: SourcePos, second: SourcePos)
  case DuplicatePlatform(name: ProjectName, platform: Platform, position: SourcePos)
  case DuplicateScalaVersion(name: ProjectName, version: ScalaVersion, position: SourcePos)
  case DuplicateChild(name: ProjectName, child: ProjectName, position: SourcePos)
  case AggregateIncludesItself(name: ProjectName, position: SourcePos)
  case MissingProject(name: String, referencedFrom: SourcePos)
  case DependsOnAggregate(module: ProjectName, aggregate: ProjectName, position: SourcePos)
  case SelfDependency(name: ProjectName, position: SourcePos)
  case Cycle(path: List[ProjectName])
  case MissingClang(name: ProjectName, position: SourcePos)
  case ClangWithoutNative(name: ProjectName, position: SourcePos)
  case EmptyShipName(position: SourcePos)
  case BadShipName(raw: String, position: SourcePos)
  case DuplicateShip(name: ShipName, first: SourcePos, second: SourcePos)
  case RepeatedShipMember(ship: ShipName, member: ProjectName, position: SourcePos)
  case AggregateShipMember(ship: ShipName, aggregate: ProjectName, position: SourcePos)
  case MemberInTwoShips(member: ProjectName, first: ShipName, second: ShipName, position: SourcePos)
  case MixedScheme(position: SourcePos)
  case DuplicateResolver(token: String)
  case MixedTest(name: ProjectName, position: SourcePos)

  def message: String = this match
    case EmptyName(position)                 => s"a project name is not empty (${position.line}:${position.column})"
    case BadName(raw, position)              =>
      s"'$raw' is not a project name (${position.line}:${position.column})"
    case DuplicateName(name, _, second)      =>
      s"project '${name.value}' is declared twice (${second.line}:${second.column})"
    case DuplicatePlatform(name, platform, _) =>
      s"project '${name.value}' lists ${platform.toString.toLowerCase} twice"
    case DuplicateScalaVersion(name, version, _) =>
      s"project '${name.value}' lists Scala ${version.value} twice"
    case DuplicateChild(name, child, _)      =>
      s"aggregate '${name.value}' lists '${child.value}' twice"
    case AggregateIncludesItself(name, _)    => s"aggregate '${name.value}' includes itself"
    case MissingProject(name, at)            =>
      s"'$name' is referenced but not passed to Projects (${at.line}:${at.column})"
    case DependsOnAggregate(module, aggregate, _) =>
      s"'${module.value}' depends on aggregate '${aggregate.value}'"
    case SelfDependency(name, _)             => s"'${name.value}' depends on itself"
    case Cycle(path)                         => s"dependency cycle ${path.map(_.value).mkString(" -> ")}"
    case MissingClang(name, position)        =>
      s"project '${name.value}' targets Native and does not name a clang major (${position.line}:${position.column})"
    case ClangWithoutNative(name, position)  =>
      s"project '${name.value}' names a clang major and does not target Native (${position.line}:${position.column})"
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
    case DuplicateResolver(token) => s"repository $token is listed twice"
    case MixedTest(name, position) =>
      s"'${name.value}' names a test main and a test provider (${position.line}:${position.column})"
