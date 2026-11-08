package histos

import _root_.scala.annotation.publicInBinary

import histos.version.{Catalog, CatalogError, Ship as CatalogShip, ShipName, ShipNameError, ShipNumber}

/** A ship as written, before [[Projects]] checks names and membership. The number is the next release. */
final case class ShipDef private (
    name: String,
    number: ShipNumber,
    members: ::[ProjectDef],
    position: SourcePos,
    description: Option[PomText] = None,
    url: Option[HttpUrl] = None,
    clients: ClientBuilds = ClientBuilds.Releases,
):
  /** Replaces the organization's description for every member of this ship. */
  def description(text: PomText): ShipDef = copy(description = Some(text))

  /** Replaces the organization's url for every member of this ship. */
  def url(value: HttpUrl): ShipDef = copy(url = Some(value))

  /** Which publishes link the native client. Unset is `Releases`. */
  def clients(builds: ClientBuilds): ShipDef = copy(clients = builds)
end ShipDef

object ShipDef:
  /** Spliced by `ship` and `Ship(...).next(...)`. Source code cannot call it. */
  @publicInBinary private[histos] def line(
      name: String,
      number: ShipNumber,
      members: ::[ProjectDef],
      position: SourcePos,
  ): ShipDef =
    new ShipDef(name, number, members, position)

/** One or more ships. The empty catalog is [[CatalogSpec.empty]], not a call with no ships. */
final case class CatalogSpec private (ships: List[ShipDef])

object CatalogSpec:
  val empty: CatalogSpec = new CatalogSpec(Nil)

  def apply(first: ShipDef, rest: ShipDef*): CatalogSpec =
    new CatalogSpec(first :: rest.toList)
end CatalogSpec

object ShipLink:
  def catalog(spec: CatalogSpec, projects: List[Project]): Either[BuildError, Catalog] =
    spec.ships
      .foldLeft[Either[BuildError, List[(CatalogShip, SourcePos)]]](Right(Nil)): (acc, defn) =>
        acc.flatMap: seen =>
          one(defn, projects, seen).map(ship => seen :+ (ship, defn.position))
      .flatMap: built =>
        Catalog.from(built.map(_._1)).left.map(error => asBuild(built, error))

  private def one(
      defn: ShipDef,
      projects: List[Project],
      seen: List[(CatalogShip, SourcePos)],
  ): Either[BuildError, CatalogShip] =
    ShipName.from(defn.name) match
      case Left(ShipNameError.Empty)    => Left(BuildError.EmptyShipName(defn.position))
      case Left(ShipNameError.Bad(raw)) => Left(BuildError.BadShipName(raw, defn.position))
      case Right(name)                  =>
        seen.collectFirst { case (ship, pos) if ship.name == name => pos } match
          case Some(first) => Left(BuildError.DuplicateShip(name, first, defn.position))
          case None        =>
            members(defn, name, projects, seen.map(_._1)).map: parsed =>
              CatalogShip.Line(name, defn.number, parsed, defn.description, defn.url, defn.clients)

  private def members(
      defn: ShipDef,
      ship: ShipName,
      projects: List[Project],
      seen: List[CatalogShip],
  ): Either[BuildError, ::[ProjectName]] =
    defn.members match
      case member :: rest =>
        resolve(defn, ship, member, projects, seen, Nil).flatMap: first =>
          rest
            .foldLeft[Either[BuildError, List[ProjectName]]](Right(Nil)): (acc, item) =>
              acc.flatMap: names =>
                resolve(defn, ship, item, projects, seen, first :: names).map(names :+ _)
            .map(tail => ::(first, tail))

  private def resolve(
      defn: ShipDef,
      ship: ShipName,
      member: ProjectDef,
      projects: List[Project],
      seen: List[CatalogShip],
      chosen: List[ProjectName],
  ): Either[BuildError, ProjectName] =
    projects.find(_.name.value == member.name) match
      case None                     => Left(BuildError.MissingProject(member.name, member.position))
      case Some(project: Aggregate) => Left(BuildError.AggregateShipMember(ship, project.name, defn.position))
      case Some(project: Module)    =>
        val name = project.name
        if chosen.contains(name) then Left(BuildError.RepeatedShipMember(ship, name, defn.position))
        else
          seen.collectFirst:
            case other if other.members.contains(name) => other.name
          match
            case Some(first) => Left(BuildError.MemberInTwoShips(name, first, ship, defn.position))
            case None        => Right(name)

  private def asBuild(built: List[(CatalogShip, SourcePos)], error: CatalogError): BuildError =
    error match
      case CatalogError.DuplicateName(name) =>
        built.collect { case (ship, pos) if ship.name == name => pos } match
          case first :: second :: _ => BuildError.DuplicateShip(name, first, second)
          case pos :: _             => BuildError.DuplicateShip(name, pos, pos)
          case Nil                  => BuildError.BadShipName(name.value, at(built))
      case CatalogError.RepeatedMember(ship, member) =>
        BuildError.RepeatedShipMember(ship, member, at(built))
      case CatalogError.InTwoShips(member, first, second) =>
        BuildError.MemberInTwoShips(member, first, second, at(built))

  private def at(built: List[(CatalogShip, SourcePos)]): SourcePos =
    built match
      case (_, pos) :: _ => pos
      case Nil           => SourcePos("<catalog>", 1, 1)
end ShipLink
