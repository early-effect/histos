package histos

import _root_.scala.annotation.publicInBinary

import histos.version.{Catalog, Ship, ShipName, ShipNameError, ShipNumber}

/** A ship as written, before [[Projects]] checks names and membership. */
final case class ShipDef private (
    name: String,
    number: ShipNumber,
    members: ::[ProjectDef],
    position: SourcePos,
)

object ShipDef:
  /** Spliced by `ship`. Source code cannot call it. */
  @publicInBinary private[histos] def make(
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

object ShipLink:
  def catalog(spec: CatalogSpec, projects: List[Project]): Either[BuildError, Catalog] =
    spec.ships.foldLeft[Either[BuildError, List[(Ship, SourcePos)]]](Right(Nil)): (acc, defn) =>
      acc.flatMap: seen =>
        one(defn, projects, seen).map(ship => seen :+ (ship, defn.position))
    .map: built =>
      Catalog.trusted(built.map(_._1))

  private def one(
      defn: ShipDef,
      projects: List[Project],
      seen: List[(Ship, SourcePos)],
  ): Either[BuildError, Ship] =
    ShipName.from(defn.name) match
      case Left(ShipNameError.Empty) => Left(BuildError.EmptyShipName(defn.position))
      case Left(ShipNameError.Bad(raw)) => Left(BuildError.BadShipName(raw, defn.position))
      case Right(name) =>
        seen.collectFirst { case (ship, pos) if ship.name == name => pos } match
          case Some(first) => Left(BuildError.DuplicateShip(name, first, defn.position))
          case None =>
            members(defn, name, projects, seen.map(_._1)).map(parsed => Ship(name, defn.number, parsed))

  private def members(
      defn: ShipDef,
      ship: ShipName,
      projects: List[Project],
      seen: List[Ship],
  ): Either[BuildError, ::[ProjectName]] =
    defn.members match
      case member :: rest =>
        resolve(defn, ship, member, projects, seen, Nil).flatMap: first =>
          rest.foldLeft[Either[BuildError, List[ProjectName]]](Right(Nil)): (acc, item) =>
            acc.flatMap: names =>
              resolve(defn, ship, item, projects, seen, first :: names).map(names :+ _)
          .map(tail => ::(first, tail))

  private def resolve(
      defn: ShipDef,
      ship: ShipName,
      member: ProjectDef,
      projects: List[Project],
      seen: List[Ship],
      chosen: List[ProjectName],
  ): Either[BuildError, ProjectName] =
    projects.find(_.name.value == member.name) match
      case None => Left(BuildError.MissingProject(member.name, member.position))
      case Some(project: Aggregate) => Left(BuildError.AggregateShipMember(ship, project.name, defn.position))
      case Some(project: Module) =>
        val name = project.name
        if chosen.contains(name) then Left(BuildError.RepeatedShipMember(ship, name, defn.position))
        else
          seen.collectFirst:
            case other if other.members.contains(name) => other.name
          match
            case Some(first) => Left(BuildError.MemberInTwoShips(name, first, ship, defn.position))
            case None        => Right(name)
