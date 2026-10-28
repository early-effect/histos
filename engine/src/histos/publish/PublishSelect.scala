package histos.publish

import histos.central.Revision
import histos.select.SelectError
import histos.version.ShipName
import histos.{Aggregate, Module, ProjectGraph, ProjectName, Publication}

/** What a publish token named. A ship wins over a module with the same text. */
enum PublishName:
  case Ship(name: ShipName)
  case Aggregate(name: ProjectName)
  case Module(name: ProjectName)

/** One ship this session can write, and whether that coordinate is a snapshot. */
final case class ShipWrite(ship: ShipName, members: List[ProjectName], snapshot: Boolean)

/** The modules a publish name asked for. `roots` were named. `modules` includes a published `dependsOn`. */
final case class Picked(
    roots: Set[ProjectName],
    modules: Set[ProjectName],
    skipped: List[ProjectName],
    refused: List[LocalPublishError.SnapshotDependency],
)

/** The directory and the names in one publish invocation. Empty names are every ship. */
final case class PublishRequest(directory: Option[String], names: List[PublishName])

/** Publish names are not compile names. A ship, an aggregate, then a module. One directory, and only on a local
  * publish. A snapshot dependency is another ship's snapshot coordinate, or a library revision ending in `-SNAPSHOT`. A
  * module in the same ship is that ship's own version. An assembly may depend on a module that no ship owns: that
  * module is compiled and packed, and it is not published. A library, an application, or an sbt plugin may not. An
  * owned module that this run did not select is still unpublished.
  */
object PublishSelect:
  def snapshotCoordinate(revision: String): Boolean =
    revision.endsWith("-SNAPSHOT")

  def resolve(
      graph: ProjectGraph,
      tokens: List[String],
      explicit: Option[String],
      allowDirectory: Boolean,
  ): Either[LocalPublishError, PublishRequest] =
    tokens
      .foldLeft[Either[LocalPublishError, Acc]](Right(Acc(explicit, Nil))): (acc, token) =>
        acc.flatMap: state =>
          classify(graph, token) match
            case Classified.Named(name) =>
              if state.names.contains(name) then Left(duplicate(name))
              else Right(state.copy(names = state.names :+ name))
            case Classified.Directory(path) =>
              if !allowDirectory then Left(LocalPublishError.Directory)
              else
                state.directory match
                  case Some(_) => Left(LocalPublishError.TwoDirectories)
                  case None    => Right(state.copy(directory = Some(path)))
      .map(state => PublishRequest(state.directory, state.names))

  /** Source roots for a watch. A directory token is not a module. An empty list is every module. */
  def watch(graph: ProjectGraph, tokens: List[String]): Option[List[ProjectName]] =
    if tokens.isEmpty then None
    else
      Some(tokens.flatMap: token =>
        classify(graph, token) match
          case Classified.Named(PublishName.Ship(ship)) =>
            graph.catalog.get(ship) match
              case Some(row) => row.members.toList
              case None      => Nil
          case Classified.Named(PublishName.Aggregate(name)) =>
            graph.get(name) match
              case Some(aggregate: Aggregate) =>
                aggregate.children.flatMap: child =>
                  graph.get(child) match
                    case Some(module: Module) => List(module.name)
                    case _                    => Nil
              case _ => Nil
          case Classified.Named(PublishName.Module(name)) => List(name)
          case Classified.Directory(_)                    => Nil)

  def pick(
      graph: ProjectGraph,
      names: List[PublishName],
      writing: List[ShipWrite],
  ): Either[LocalPublishError, Picked] =
    val owned    = ownership(graph)
    val writable = writing.map(row => row.ship -> row).toMap
    val allowed  = writing.flatMap(_.members).toSet
    expanded(graph, names, owned, writable).flatMap: acc =>
      val closed   = closure(graph, acc.roots, allowed)
      val refused  = refusals(graph, closed, writable, owned)
      val blocked  = refused.map(_.ship).toSet
      val liveable = allowed.filter: name =>
        owned.get(name) match
          case Some(ship) => !blocked.contains(ship)
          case None       => false
      val liveRoots = acc.roots.filter(liveable.contains)
      val live      = closure(graph, liveRoots, liveable)
      unpublished(graph, live, owned).flatMap: _ =>
        if live.isEmpty && refused.isEmpty && acc.skipped.isEmpty && names.nonEmpty && !held(graph, names, writable)
        then Left(LocalPublishError.NothingToPublish)
        else Right(Picked(liveRoots, live, acc.skipped, refused))

  private final case class Acc(directory: Option[String], names: List[PublishName])

  private enum Classified:
    case Named(name: PublishName)
    case Directory(path: String)

  private def classify(graph: ProjectGraph, token: String): Classified =
    ShipName.from(token) match
      case Right(ship) if graph.catalog.get(ship).isDefined =>
        Classified.Named(PublishName.Ship(ship))
      case _ =>
        ProjectName.from(token) match
          case Right(project) =>
            graph.get(project) match
              case Some(_: Aggregate) => Classified.Named(PublishName.Aggregate(project))
              case Some(_: Module)    => Classified.Named(PublishName.Module(project))
              case None               => Classified.Directory(token)
          case Left(_) => Classified.Directory(token)

  private def duplicate(name: PublishName): LocalPublishError =
    val raw = name match
      case PublishName.Ship(ship)         => ship.value
      case PublishName.Aggregate(project) => project.value
      case PublishName.Module(project)    => project.value
    ProjectName.from(raw) match
      case Right(project) => LocalPublishError.Target(SelectError.Duplicate(project))
      case Left(_)        => LocalPublishError.Directory

  private final case class Roots(roots: Set[ProjectName], skipped: List[ProjectName]):
    def add(name: ProjectName): Roots =
      copy(roots = roots + name)
    def addAll(members: List[ProjectName]): Roots =
      copy(roots = roots ++ members)
    def skip(name: ProjectName): Roots =
      if skipped.contains(name) then this else copy(skipped = skipped :+ name)

  private def expanded(
      graph: ProjectGraph,
      names: List[PublishName],
      owned: Map[ProjectName, ShipName],
      writable: Map[ShipName, ShipWrite],
  ): Either[LocalPublishError, Roots] =
    if names.isEmpty then Right(Roots(writable.values.flatMap(_.members).toSet, Nil))
    else
      names.foldLeft[Either[LocalPublishError, Roots]](Right(Roots(Set.empty, Nil))): (acc, name) =>
        acc.flatMap: roots =>
          one(graph, name, owned, writable, roots)

  private def one(
      graph: ProjectGraph,
      name: PublishName,
      owned: Map[ProjectName, ShipName],
      writable: Map[ShipName, ShipWrite],
      roots: Roots,
  ): Either[LocalPublishError, Roots] =
    name match
      case PublishName.Ship(ship) =>
        writable.get(ship) match
          case Some(row) => Right(roots.addAll(row.members))
          case None      =>
            graph.catalog.get(ship) match
              case Some(_) => Right(roots)
              case None    => Left(LocalPublishError.UnknownShip(ship))
      case PublishName.Aggregate(project) =>
        graph.get(project) match
          case Some(aggregate: Aggregate) =>
            aggregate.children.foldLeft[Either[LocalPublishError, Roots]](Right(roots)): (acc, child) =>
              acc.flatMap: have =>
                graph.get(child) match
                  case Some(module: Module) =>
                    owned.get(module.name) match
                      case Some(ship) if writable.contains(ship) => Right(have.add(module.name))
                      case Some(_)                               => Right(have)
                      case None                                  => Right(have.skip(module.name))
                  case _ => Left(LocalPublishError.Target(SelectError.Unknown(child)))
          case _ => Left(LocalPublishError.Target(SelectError.Unknown(project)))
      case PublishName.Module(project) =>
        owned.get(project) match
          case Some(ship) if writable.contains(ship) => Right(roots.add(project))
          case Some(_)                               => Right(roots)
          case None                                  =>
            graph.get(project) match
              case Some(_: Module) => Left(LocalPublishError.NotPublished(project))
              case _               => Left(LocalPublishError.Target(SelectError.Unknown(project)))

  private def ownership(graph: ProjectGraph): Map[ProjectName, ShipName] =
    graph.catalog.ships.flatMap(ship => ship.members.toList.map(member => member -> ship.name)).toMap

  private def held(
      graph: ProjectGraph,
      names: List[PublishName],
      writable: Map[ShipName, ShipWrite],
  ): Boolean =
    val owned = ownership(graph)
    names.exists:
      case PublishName.Ship(ship) =>
        graph.catalog.get(ship).isDefined && !writable.contains(ship)
      case PublishName.Module(name) =>
        owned.get(name).exists(ship => !writable.contains(ship))
      case PublishName.Aggregate(_) => false

  private def closure(
      graph: ProjectGraph,
      asked: Set[ProjectName],
      allowed: Set[ProjectName],
  ): Set[ProjectName] =
    def step(seen: Set[ProjectName], more: Set[ProjectName]): Set[ProjectName] =
      if more.isEmpty then seen
      else
        val next = more.flatMap: name =>
          graph.get(name) match
            case Some(module: Module) => upstream(module).filter(allowed.contains).toSet
            case _                    => Set.empty
        step(seen ++ more, next -- seen)
    step(Set.empty, asked)

  private def upstream(module: Module): List[ProjectName] =
    module.platforms.all.flatMap(module.upstream).distinct

  private def unpublished(
      graph: ProjectGraph,
      chosen: Set[ProjectName],
      owned: Map[ProjectName, ShipName],
  ): Either[LocalPublishError, Unit] =
    val modules = graph.projects.collect:
      case module: Module if chosen.contains(module.name) => module
    modules.foldLeft[Either[LocalPublishError, Unit]](Right(())): (acc, module) =>
      acc.flatMap: _ =>
        upstream(module).find(name => !chosen.contains(name) && !assemblyPacks(module, name, owned)) match
          case Some(dependency) =>
            Left(LocalPublishError.UnpublishedDependency(module.name, dependency))
          case None => Right(())

  /** The fat jar packs a module no ship owns. It does not pack a coordinate this run did not select. */
  private def assemblyPacks(
      module: Module,
      dependency: ProjectName,
      owned: Map[ProjectName, ShipName],
  ): Boolean =
    (module.publication match
      case Publication.Assembly(_)                                                  => !owned.contains(dependency)
      case Publication.Library | Publication.SbtPlugin(_) | Publication.Application => false
    )

  private def refusals(
      graph: ProjectGraph,
      modules: Set[ProjectName],
      writable: Map[ShipName, ShipWrite],
      owned: Map[ProjectName, ShipName],
  ): List[LocalPublishError.SnapshotDependency] =
    val ordered = graph.projects.collect:
      case module: Module if modules.contains(module.name) => module
    ordered.foldLeft(List.empty[LocalPublishError.SnapshotDependency]): (seen, module) =>
      owned.get(module.name) match
        case Some(ship) if !seen.exists(_.ship == ship) =>
          dependency(module, ship, writable, owned) match
            case Some(found) => seen :+ LocalPublishError.SnapshotDependency(ship, found)
            case None        => seen
        case _ => seen

  private def dependency(
      module: Module,
      own: ShipName,
      writable: Map[ShipName, ShipWrite],
      owned: Map[ProjectName, ShipName],
  ): Option[String] =
    module.dependencies
      .collectFirst:
        case dep if snapshotCoordinate(dep.revision) =>
          s"${dep.organization}:${dep.name}:${dep.revision}"
      .orElse:
        upstream(module).collectFirst:
          case name if owned.get(name).exists { other =>
                other != own && writable.get(other).exists(_.snapshot)
              } =>
            name.value
end PublishSelect
