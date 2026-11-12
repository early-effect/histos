package histos

import _root_.scala.annotation.publicInBinary

import histos.version.{Catalog, Ship as CatalogShip, ShipName, ShipNameError, ShipNumber}

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

/** Resolves each ship's name and members against the graph. [[Catalog.from]] checks the rows with the place each was
  * written.
  */
object ShipLink:
  def catalog(spec: CatalogSpec, projects: List[Project]): Either[BuildError, Catalog] =
    spec.ships
      .foldLeft[Either[BuildError, List[(CatalogShip.Line, SourcePos)]]](Right(Nil)): (acc, defn) =>
        acc.flatMap(rows => line(defn, projects).map(ship => rows :+ (ship, defn.position)))
      .flatMap(Catalog.from)

  private def line(defn: ShipDef, projects: List[Project]): Either[BuildError, CatalogShip.Line] =
    ShipName.from(defn.name) match
      case Left(ShipNameError.Empty)    => Left(BuildError.EmptyShipName(defn.position))
      case Left(ShipNameError.Bad(raw)) => Left(BuildError.BadShipName(raw, defn.position))
      case Right(name)                  =>
        members(defn, name, projects).map: parsed =>
          CatalogShip.Line(name, defn.number, parsed, defn.description, defn.url, defn.clients)

  private def members(defn: ShipDef, ship: ShipName, projects: List[Project]): Either[BuildError, ::[ProjectName]] =
    defn.members match
      case member :: rest =>
        resolve(defn, ship, member, projects).flatMap: first =>
          rest
            .foldLeft[Either[BuildError, List[ProjectName]]](Right(Nil)): (acc, item) =>
              acc.flatMap(names => resolve(defn, ship, item, projects).map(names :+ _))
            .map(tail => ::(first, tail))

  private def resolve(
      defn: ShipDef,
      ship: ShipName,
      member: ProjectDef,
      projects: List[Project],
  ): Either[BuildError, ProjectName] =
    projects.find(_.name.value == member.name) match
      case None                     => Left(BuildError.MissingProject(member.name, member.position))
      case Some(project: Aggregate) => Left(BuildError.AggregateShipMember(ship, project.name, defn.position))
      case Some(project: Module)    => Right(project.name)
end ShipLink
