package histos.doctor

import histos.central.{ArtifactId, FeedError, MavenModule, ReleaseFeed}
import histos.publish.{LauncherChoice, LauncherPin}
import histos.resolve.ArtifactName
import histos.version.{
  BumpReason,
  Catalog,
  Registry,
  Session,
  SessionSource,
  Ship,
  ShipAdvice,
  ShipDiagnostic,
  ShipName,
  TagRelation,
  VersionTask,
}
import histos.version.ShipNumber
import histos.{Module, ProjectGraph}

import java.nio.file.{Files, Path}

import zio.{UIO, ZIO}

/** What `releases` looked like. A missing file is not an empty registry. */
enum ReleasesFile:
  case Missing
  case Unreadable
  case Text(body: String)

/** What the registry answered for one artifact. `Unasked` is a build that names no published coordinate. */
enum Listing:
  case Unasked
  case Unreachable(error: FeedError)
  case Newest(release: Option[ShipNumber])

/** Cheap ship checks. They warn when a number or the pin should move. They do not rewrite either. */
object ShipChecks:
  val sessionId  = CheckId.trusted("ship.session")
  val releasesId = CheckId.trusted("ship.releases")
  val bumpId     = CheckId.trusted("ship.should-bump")
  val pinId      = CheckId.trusted("ship.pin")

  def read(path: Path): UIO[ReleasesFile] =
    ZIO
      .attemptBlocking {
        if !Files.exists(path) then ReleasesFile.Missing
        else if !Files.isRegularFile(path) || !Files.isReadable(path) then ReleasesFile.Unreadable
        else ReleasesFile.Text(Files.readString(path))
      }
      .orElseSucceed(ReleasesFile.Unreadable)

  /** `ship.session`, `ship.releases`, `ship.should-bump`, and `ship.pin`. Only the last two read the registry. */
  def checks(repo: Path, graph: ProjectGraph, env: Map[String, String], feed: ReleaseFeed): List[DoctorCheck] =
    val file = repo.resolve(VersionTask.fileName)
    List(
      local(sessionId)(ZIO.succeed(List(session(graph.catalog, env)))),
      local(releasesId)(read(file).map(releases => List(ShipChecks.releases(graph.catalog, releases)))),
      local(bumpId):
        read(file).zipWithPar(listings(graph, feed))((releases, listed) => bumps(graph.catalog, releases, listed))
      ,
      local(pinId):
        LauncherPin
          .read(repo)
          .option
          .flatMap:
            case None      => ZIO.succeed(List(pin(None, Listing.Unasked)))
            case Some(pin) =>
              listing(feed, Some(ReleaseFeed.histos)).map(found => List(ShipChecks.pin(Some(pin.choice), found))),
    )

  def outcomes(
      catalog: Catalog,
      env: Map[String, String],
      file: ReleasesFile,
      listed: Map[ShipName, Listing],
  ): List[Outcome] =
    session(catalog, env) :: releases(catalog, file) :: bumps(catalog, file, listed)

  def session(catalog: Catalog, env: Map[String, String]): Outcome =
    catalog match
      case Catalog.Empty       => Outcome.Ok(sessionId, "no catalog")
      case Catalog.Lines(rows) =>
        val source = SessionSource(
          env.get(Session.Key),
          env.get(Session.ShaKey),
          env.get(Session.BranchKey),
          env.get(Session.PullRequestKey),
        )
        Session.from(source) match
          case Left(error) =>
            Outcome.Found(ShipDiagnostic.Session(ShipAdvice.incomplete(error)))
          case Right(Session.Development) =>
            val shown = rows.map(row => s"${row.name.value} ${row.number.render}-ci").mkString(", ")
            Outcome.Ok(sessionId, s"development compiles $shown")
          case Right(_) =>
            Outcome.Ok(sessionId, "the publish session is complete")

  def releases(catalog: Catalog, file: ReleasesFile): Outcome =
    catalog match
      case Catalog.Empty    => Outcome.Ok(releasesId, "no catalog")
      case Catalog.Lines(_) =>
        file match
          case ReleasesFile.Missing =>
            Outcome.Found(ShipDiagnostic.Releases(ShipAdvice.unchecked))
          case ReleasesFile.Unreadable =>
            Outcome.Found(ShipDiagnostic.Releases(ShipAdvice.unreadable))
          case ReleasesFile.Text(body) =>
            Registry.parse(lines(body)) match
              case Left(error) =>
                Outcome.Found(ShipDiagnostic.Releases(s"${error.message} ${ShipAdvice.releasesFile}"))
              case Right(_) =>
                Outcome.Ok(releasesId, "releases was read")

  /** `releases` speaks first. Otherwise a listed release at or above the catalog number is a bump. */
  def bumps(catalog: Catalog, file: ReleasesFile, listed: Map[ShipName, Listing]): List[Outcome] =
    catalog match
      case Catalog.Empty    => List(Outcome.Ok(bumpId, "no catalog"))
      case Catalog.Lines(_) =>
        val registry = parsed(file)
        val due      = catalog.ships.collect { case ship: Ship.Line =>
          bump(ship, registry.flatMap(_.relation(ship.name)), listed.getOrElse(ship.name, Listing.Unasked))
        }.flatten
        due match
          case Nil =>
            List(
              Outcome.Ok(
                bumpId,
                if registry.isDefined then "catalog numbers are not released" else "releases was not read",
              )
            )
          case found => found

  /** A pin that a published Histos release passed is a finding. */
  def pin(choice: Option[LauncherChoice], listing: Listing): Outcome =
    (choice, listing) match
      case (None, _)                                         => Outcome.Ok(pinId, "histos.version was not read")
      case (Some(_), Listing.Unreachable(error))             => skipped(pinId, error)
      case (Some(_), Listing.Unasked | Listing.Newest(None)) => Outcome.Ok(pinId, "no Histos release is listed")
      case (Some(pinned), Listing.Newest(Some(release)))     =>
        pinned.lag(release) match
          case Some(lag) => Outcome.Found(ShipDiagnostic.PinBehind(lag))
          case None      =>
            Outcome.Ok(pinId, s"histos.version runs ${pinned.line.render}, and ${release.render} does not pass it")

  /** The first member a ship publishes, under the build's organization. */
  def module(graph: ProjectGraph, ship: Ship.Line): Option[MavenModule] =
    for
      group  <- graph.organization
      member <- ship.members.flatMap(name => graph.get(name).collect { case module: Module => module }).headOption
      id     <- ArtifactId.from(ArtifactName.module(member, member.versions.first, member.platforms.first)).toOption
    yield MavenModule(group, id)

  private def bump(ship: Ship.Line, relation: Option[TagRelation], listing: Listing): Option[Outcome] =
    relation match
      case Some(TagRelation.Matches) =>
        Some(Outcome.Found(ShipDiagnostic.ShouldBump(ship.name, ship.number, BumpReason.Tagged)))
      case Some(TagRelation.Moved) =>
        Some(Outcome.Found(ShipDiagnostic.ShouldBump(ship.name, ship.number, BumpReason.Shadowed)))
      case Some(TagRelation.Unknown) | None =>
        listing match
          case Listing.Newest(Some(release)) if Ordering[ShipNumber].gteq(release, ship.number) =>
            Some(Outcome.Found(ShipDiagnostic.ShouldBump(ship.name, ship.number, BumpReason.Published(release))))
          case Listing.Unreachable(error)          => Some(skipped(bumpId, error))
          case Listing.Newest(_) | Listing.Unasked => None

  private def skipped(id: CheckId, error: FeedError): Outcome =
    Outcome.Skipped(
      id,
      Probe.Registry,
      Fix.Steps(::(s"${error.message}. Reach it, then run histos doctor again.", Nil)),
    )

  private def listings(graph: ProjectGraph, feed: ReleaseFeed): UIO[Map[ShipName, Listing]] =
    ZIO
      .foreachPar(graph.catalog.ships.collect { case ship: Ship.Line => ship })(ship =>
        listing(feed, module(graph, ship)).map(ship.name -> _)
      )
      .map(_.toMap)

  private def listing(feed: ReleaseFeed, module: Option[MavenModule]): UIO[Listing] =
    module match
      case None         => ZIO.succeed(Listing.Unasked)
      case Some(listed) => feed.newest(listed).fold(Listing.Unreachable(_), Listing.Newest(_))

  private def local(check: CheckId)(outcomes: UIO[List[Outcome]]): DoctorCheck =
    new DoctorCheck:
      def id                      = check
      def area                    = Area.Release
      def needs                   = Set(Probe.Local)
      def run(ctx: DoctorContext) = outcomes

  private def parsed(file: ReleasesFile): Option[Registry] =
    file match
      case ReleasesFile.Text(body) => Registry.parse(lines(body)).toOption
      case _                       => None

  private def lines(body: String): List[String] =
    body.split("\n", -1).toList
end ShipChecks
