package histos

import _root_.scala.annotation.{implicitNotFound, publicInBinary}

import histos.version.{Catalog, Ship as CatalogShip, ShipName, ShipNameError, ShipNumber, ShipToolchain}

/** The launcher and client a ship named, before [[Projects]] checks them. */
private[histos] final case class ToolchainAsk(launcher: ModuleDef[?], client: ModuleDef[?])

/** A ship as written, before [[Projects]] checks names and membership. The number is the next release. */
final case class ShipDef private (
    name: String,
    number: ShipNumber,
    members: ::[ProjectDef],
    position: SourcePos,
    description: Option[PomText] = None,
    url: Option[HttpUrl] = None,
    private[histos] val toolchainAsk: Option[ToolchainAsk] = None,
):
  /** Replaces the organization's description for every member of this ship. */
  def description(text: PomText): ShipDef = copy(description = Some(text))

  /** Replaces the organization's url for every member of this ship. */
  def url(value: HttpUrl): ShipDef = copy(url = Some(value))

  /** Publish this launcher and link `client` on every dist platform, on every publish. `client` builds Native. */
  def toolchain[C](
      launcher: ModuleDef[?],
      client: ModuleDef[C],
  )(using
      @implicitNotFound(
        "the toolchain client builds Native, and this module does not"
      ) native: Platform.Native.type <:< C
  ): ShipDef =
    copy(toolchainAsk = Some(ToolchainAsk(launcher, client)))

  /** The load law passes a client the public method refuses. The load still checks Native and a main. */
  private[histos] def asking(launcher: ModuleDef[?], client: ModuleDef[?]): ShipDef =
    copy(toolchainAsk = Some(ToolchainAsk(launcher, client)))
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

/** Resolves each ship's name and members against the graph. [[histos.version.Catalog.from]] checks the rows with the
  * place each was written.
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
      case Right(ship)                  =>
        members(defn, ship, projects).flatMap: parsed =>
          toolchain(defn, ship, parsed, projects).map: linked =>
            CatalogShip.Line(ship, defn.number, parsed, defn.description, defn.url, linked)

  private def toolchain(
      defn: ShipDef,
      ship: ShipName,
      members: ::[ProjectName],
      projects: List[Project],
  ): Either[BuildError, Option[ShipToolchain]] =
    defn.toolchainAsk match
      case None      => Right(None)
      case Some(ask) =>
        launcher(defn, ship, ask.launcher, members, projects).flatMap: launch =>
          client(defn, ship, ask.client, projects).map: linked =>
            Some(ShipToolchain(launch, linked))

  private def launcher(
      defn: ShipDef,
      ship: ShipName,
      asked: ModuleDef[?],
      members: ::[ProjectName],
      projects: List[Project],
  ): Either[BuildError, ProjectName] =
    projectName(asked.name, defn.position).flatMap: name =>
      projects.find(_.name == name) match
        case None =>
          Left(BuildError.ToolchainLauncher(ship, name, ToolchainLauncherFault.Missing, defn.position))
        case Some(_) if !members.contains(name) =>
          Left(BuildError.ToolchainLauncher(ship, name, ToolchainLauncherFault.NotMember, defn.position))
        case Some(module: Module) =>
          module.publication match
            case Publication.Assembly(_) => Right(name)
            case _                       =>
              Left(BuildError.ToolchainLauncher(ship, name, ToolchainLauncherFault.NotAssembly, defn.position))
        case Some(_) =>
          Left(BuildError.ToolchainLauncher(ship, name, ToolchainLauncherFault.NotAssembly, defn.position))

  private def client(
      defn: ShipDef,
      ship: ShipName,
      asked: ModuleDef[?],
      projects: List[Project],
  ): Either[BuildError, ProjectName] =
    projectName(asked.name, defn.position).flatMap: name =>
      projects.find(_.name == name) match
        case Some(module: Module) if module.platforms.all.contains(Platform.Native) =>
          module.application match
            case Some(_) => Right(name)
            case None    =>
              Left(BuildError.ToolchainClient(ship, name, ToolchainClientFault.NoMain, defn.position))
        case Some(_) =>
          Left(BuildError.ToolchainClient(ship, name, ToolchainClientFault.NotNative, defn.position))
        case None =>
          Left(BuildError.ToolchainClient(ship, name, ToolchainClientFault.Missing, defn.position))

  private def projectName(raw: String, at: SourcePos): Either[BuildError, ProjectName] =
    ProjectName.from(raw) match
      case Right(found)                     => Right(found)
      case Left(ProjectNameError.Empty)     => Left(BuildError.EmptyName(at))
      case Left(ProjectNameError.Bad(text)) => Left(BuildError.BadName(text, at))

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
