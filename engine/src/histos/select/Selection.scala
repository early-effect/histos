package histos.select

import histos.workflow.CiError
import histos.{Aggregate, Module, ProjectGraph, ProjectName}

/** The modules a compile, test, or publish name asked for. `expanded` came from an aggregate. */
final case class Selected(modules: List[Module], expanded: Set[ProjectName])

object Selection:
  /** Empty names are every module, in graph order. A module name is that module. An aggregate name is its direct module
    * children. A repeated name is `CiError.Duplicate`.
    */
  def select(graph: ProjectGraph, names: List[String]): Either[CiError, Selected] =
    val modules = graph.projects.collect { case module: Module => module }
    if names.isEmpty then Right(Selected(modules, Set.empty))
    else
      names
        .foldLeft[Either[CiError, List[String]]](Right(Nil)): (acc, raw) =>
          acc.flatMap: seen =>
            ProjectName.from(raw) match
              case Left(_)                                  => Left(CiError.BadName(raw))
              case Right(name) if seen.contains(name.value) => Left(CiError.Duplicate(name))
              case Right(name)                              => Right(seen :+ name.value)
        .flatMap: raw =>
          raw.foldLeft[Either[CiError, Selected]](Right(Selected(Nil, Set.empty))): (acc, item) =>
            acc.flatMap: selected =>
              ProjectName.from(item) match
                case Left(_)     => Left(CiError.BadName(item))
                case Right(name) => expand(graph, name, selected)

  private def expand(graph: ProjectGraph, name: ProjectName, selected: Selected): Either[CiError, Selected] =
    graph.get(name) match
      case None                 => Left(CiError.Unknown(name))
      case Some(module: Module) =>
        if selected.modules.exists(_.name == module.name) then Left(CiError.Duplicate(module.name))
        else Right(selected.copy(modules = selected.modules :+ module))
      case Some(aggregate: Aggregate) =>
        aggregate.children.foldLeft[Either[CiError, Selected]](Right(selected)): (acc, child) =>
          acc.flatMap: have =>
            graph.get(child) match
              case Some(module: Module) =>
                if have.modules.exists(_.name == module.name) then Left(CiError.Duplicate(module.name))
                else Right(Selected(have.modules :+ module, have.expanded + module.name))
              case Some(_: Aggregate) => Left(CiError.Unknown(child))
              case None               => Left(CiError.Unknown(child))
end Selection
