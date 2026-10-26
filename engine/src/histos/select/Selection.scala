package histos.select

import histos.{Aggregate, Module, ProjectGraph, ProjectName}

/** The modules a compile, test, or publish name asked for. `expanded` came from an aggregate. */
final case class Selected(modules: List[Module], expanded: Set[ProjectName])

enum SelectError:
  case BadName(raw: String)
  case Duplicate(name: ProjectName)
  case Unknown(name: ProjectName)

  def message: String = this match
    case BadName(raw)    => s"'$raw' is not a project name"
    case Duplicate(name) => s"'${name.value}' is listed twice"
    case Unknown(name)   => s"'${name.value}' is not a project"
end SelectError

object Selection:
  /** Empty names are every module, in graph order. A module name is that module. An aggregate name is its direct module
    * children. A repeated name is `SelectError.Duplicate`.
    */
  def select(graph: ProjectGraph, names: List[String]): Either[SelectError, Selected] =
    val modules = graph.projects.collect { case module: Module => module }
    if names.isEmpty then Right(Selected(modules, Set.empty))
    else
      names
        .foldLeft[Either[SelectError, List[ProjectName]]](Right(Nil)): (acc, raw) =>
          acc.flatMap: seen =>
            ProjectName.from(raw) match
              case Left(_)                            => Left(SelectError.BadName(raw))
              case Right(name) if seen.contains(name) => Left(SelectError.Duplicate(name))
              case Right(name)                        => Right(seen :+ name)
        .flatMap: parsed =>
          parsed.foldLeft[Either[SelectError, Selected]](Right(Selected(Nil, Set.empty))): (acc, name) =>
            acc.flatMap: selected =>
              expand(graph, name, selected)

  private def expand(graph: ProjectGraph, name: ProjectName, selected: Selected): Either[SelectError, Selected] =
    graph.get(name) match
      case None                 => Left(SelectError.Unknown(name))
      case Some(module: Module) =>
        if selected.modules.exists(_.name == module.name) then Left(SelectError.Duplicate(module.name))
        else Right(selected.copy(modules = selected.modules :+ module))
      case Some(aggregate: Aggregate) =>
        aggregate.children.foldLeft[Either[SelectError, Selected]](Right(selected)): (acc, child) =>
          acc.flatMap: have =>
            graph.get(child) match
              case Some(module: Module) =>
                if have.modules.exists(_.name == module.name) then Left(SelectError.Duplicate(module.name))
                else Right(Selected(have.modules :+ module, have.expanded + module.name))
              case Some(_: Aggregate) => Left(SelectError.Unknown(child))
              case None               => Left(SelectError.Unknown(child))
end Selection
