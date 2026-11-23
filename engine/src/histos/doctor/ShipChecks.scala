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

import zio.{Layer, UIO, ZIO, ZLayer}

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

/** Ship diagnostics and JVM compatibility evidence. These checks do not rewrite the catalog or launcher pin. */
object ShipChecks:
  val sessionId  = CheckId.trusted("ship.session")
  val releasesId = CheckId.trusted("ship.releases")
  val bumpId     = CheckId.trusted("ship.should-bump")
  val pinId      = CheckId.trusted("ship.pin")
  val binaryId   = CheckId.trusted("ship.binary")

  def read(path: Path): UIO[ReleasesFile] =
    ZIO
      .attemptBlocking {
        if !Files.exists(path) then ReleasesFile.Missing
        else if !Files.isRegularFile(path) || !Files.isReadable(path) then ReleasesFile.Unreadable
        else ReleasesFile.Text(Files.readString(path))
      }
      .orElseSucceed(ReleasesFile.Unreadable)

  /** Session, releases, bump, pin, and binary checks. Bump, pin, and binary checks read the registry when run. */
  def checks(repo: Path, graph: ProjectGraph, env: Map[String, String], feed: ReleaseFeed): List[DoctorCheck] =
    val verification =
      (ZLayer.succeed(
        feed
      ) ++ CompatibilityBaseline.live ++ CompatibilityAnalyzer.jvm) >>> CompatibilityVerification.layer
    checks(repo, graph, env, feed, verification)

  def checks(
      repo: Path,
      graph: ProjectGraph,
      env: Map[String, String],
      feed: ReleaseFeed,
      verification: Layer[CompatibilityError, CompatibilityVerification],
  ): List[DoctorCheck] =
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
      local(binaryId)(
        ZIO
          .serviceWithZIO[CompatibilityVerification](binaries(repo, graph, _))
          .provideLayer(verification)
          .catchAll(error =>
            ZIO.succeed(
              List(
                Outcome.Found(
                  IncompleteCompatibility("JVM analyzer", CompatibilityIncomplete.AnalysisFailed(error))
                )
              )
            )
          )
      ),
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

  /** Representative coordinate for ship-bump policy. Binary verification independently enumerates every JVM cell. */
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

  /** Every published JVM member/version needs evidence, including when the newest release caught the catalog. */
  private def binaries(repo: Path, graph: ProjectGraph, verification: CompatibilityVerification): UIO[List[Outcome]] =
    graph.catalog.ships match
      case Nil   => ZIO.succeed(List(Outcome.Ok(binaryId, "no catalog")))
      case ships => ZIO.foreach(ships)(binaryOne(repo, graph, verification, _)).map(_.flatten)

  private def binaryOne(
      repo: Path,
      graph: ProjectGraph,
      verification: CompatibilityVerification,
      ship: Ship,
  ): UIO[List[Outcome]] =
    ship match
      case line: Ship.Line =>
        val targets = line.members
          .flatMap(name => graph.get(name).collect { case member: Module => member })
          .filter(_.platforms.all.contains(histos.Platform.Jvm))
          .flatMap: member =>
            member.versions.all.map: version =>
              for
                group <- graph.organization.toRight(CompatibilityIncomplete.MissingOrganization(member.name))
                id    <- ArtifactId
                  .from(ArtifactName.module(member, version, histos.Platform.Jvm))
                  .left
                  .map(CompatibilityIncomplete.InvalidArtifact(member.name, _))
              yield CompatibilityTarget(
                member.name,
                version,
                MavenModule(group, id),
                histos.zinc.GraphCompile.classesDir(repo, member.name, version),
              )
        if targets.isEmpty then ZIO.succeed(List(Outcome.Ok(binaryId, s"${line.name.value} has no JVM member")))
        else
          ZIO.foreach(targets):
            case Left(reason)  => ZIO.succeed(Outcome.Found(IncompleteCompatibility(line.name.value, reason)))
            case Right(target) => verification.verify(target).map(judge(line, target, _))

  private def judge(ship: Ship.Line, target: CompatibilityTarget, evidence: CompatibilityEvidence): Outcome =
    evidence match
      case CompatibilityEvidence.FirstPublication =>
        Outcome.Ok(binaryId, s"${target.label}: first publication, no stable baseline")
      case CompatibilityEvidence.Incomplete(reason) =>
        Outcome.Found(IncompleteCompatibility(target.label, reason))
      case CompatibilityEvidence.Compared(previous, result) =>
        BinaryCheck.difference(ship.number, result) match
          case BinaryCheck.Difference.None =>
            Outcome.Ok(binaryId, s"${target.label} matches ${previous.render}")
          case BinaryCheck.Difference.Break(part, _) if BinaryCheck.covers(previous, ship.number, part) =>
            Outcome.Ok(
              binaryId,
              s"${target.label}: ${ship.number.render} covers a ${part.token} from ${previous.render}",
            )
          case BinaryCheck.Difference.Break(part, _) =>
            Outcome.Found(BinaryFinding(target.label, ShipDiagnostic.BinaryBump(ship.name, ship.number, part.token)))

  private final case class BinaryFinding(label: String, underlying: ShipDiagnostic) extends Diagnostic:
    def check   = underlying.check
    def level   = underlying.level
    def title   = underlying.title
    def summary = s"$label: ${underlying.summary}"
    def explain = underlying.explain
    def fixes   = underlying.fixes

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
