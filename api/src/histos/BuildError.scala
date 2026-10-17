package histos

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
