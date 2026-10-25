package histos

import _root_.scala.annotation.publicInBinary

import histos.version.{Catalog, CatalogError, ManualVersion, Ship, ShipName, ShipNameError, ShipNumber}

/** Whether a written ship derives its coordinates or states them. */
enum ShipSpec:
  case Line(number: ShipNumber)
  case Manual(version: ManualVersion)

/** A ship as written, before [[Projects]] checks names and membership. */
final case class ShipDef private (
    name: String,
    spec: ShipSpec,
    members: ::[ProjectDef],
    position: SourcePos,
)

object ShipDef:
  /** Spliced by `ship`. Source code cannot call it. */
  @publicInBinary private[histos] def line(
      name: String,
      number: ShipNumber,
      members: ::[ProjectDef],
      position: SourcePos,
  ): ShipDef =
    new ShipDef(name, ShipSpec.Line(number), members, position)

  /** Spliced by `manual`. Source code cannot call it. */
  @publicInBinary private[histos] def manual(
      name: String,
      version: ManualVersion,
      members: ::[ProjectDef],
      position: SourcePos,
  ): ShipDef =
    new ShipDef(name, ShipSpec.Manual(version), members, position)
end ShipDef

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
      .foldLeft[Either[BuildError, List[(Ship, SourcePos)]]](Right(Nil)): (acc, defn) =>
        acc.flatMap: seen =>
          one(defn, projects, seen).map(ship => seen :+ (ship, defn.position))
      .flatMap: built =>
        Catalog.from(built.map(_._1)).left.map(error => asBuild(built, error))

  private def one(
      defn: ShipDef,
      projects: List[Project],
      seen: List[(Ship, SourcePos)],
  ): Either[BuildError, Ship] =
    ShipName.from(defn.name) match
      case Left(ShipNameError.Empty)    => Left(BuildError.EmptyShipName(defn.position))
      case Left(ShipNameError.Bad(raw)) => Left(BuildError.BadShipName(raw, defn.position))
      case Right(name)                  =>
        seen.collectFirst { case (ship, pos) if ship.name == name => pos } match
          case Some(first) => Left(BuildError.DuplicateShip(name, first, defn.position))
          case None        =>
            members(defn, name, projects, seen.map(_._1)).map: parsed =>
              defn.spec match
                case ShipSpec.Line(number)    => Ship.Line(name, number, parsed)
                case ShipSpec.Manual(version) => Ship.Manual(name, version, parsed)

  private def members(
      defn: ShipDef,
      ship: ShipName,
      projects: List[Project],
      seen: List[Ship],
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
      seen: List[Ship],
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

  private def asBuild(built: List[(Ship, SourcePos)], error: CatalogError): BuildError =
    error match
      case CatalogError.MixedScheme         => BuildError.MixedScheme(mismatch(built))
      case CatalogError.DuplicateName(name) =>
        built.collect { case (ship, pos) if ship.name == name => pos } match
          case first :: second :: _ => BuildError.DuplicateShip(name, first, second)
          case pos :: _             => BuildError.DuplicateShip(name, pos, pos)
          case Nil                  => BuildError.BadShipName(name.value, at(built))
      case CatalogError.RepeatedMember(ship, member) =>
        BuildError.RepeatedShipMember(ship, member, at(built))
      case CatalogError.InTwoShips(member, first, second) =>
        BuildError.MemberInTwoShips(member, first, second, at(built))

  private def mismatch(built: List[(Ship, SourcePos)]): SourcePos =
    built match
      case (first, _) :: rest =>
        val other = first match
          case _: Ship.Line   => rest.collectFirst { case (_: Ship.Manual, pos) => pos }
          case _: Ship.Manual => rest.collectFirst { case (_: Ship.Line, pos) => pos }
        other match
          case Some(pos) => pos
          case None      => at(built)
      case Nil => at(built)

  private def at(built: List[(Ship, SourcePos)]): SourcePos =
    built match
      case (_, pos) :: _ => pos
      case Nil           => SourcePos("<catalog>", 1, 1)
end ShipLink
