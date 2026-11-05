package histos.doctor

import histos.version.{Catalog, Registry, Session, SessionSource, Ship, ShipAdvice, ShipDiagnostic, TagRelation}

import java.nio.file.{Files, Path}

import zio.{UIO, ZIO}

/** What `releases` looked like. A missing file is not an empty registry. */
enum ReleasesFile:
  case Missing
  case Unreadable
  case Text(body: String)

/** Cheap ship checks. They warn when a number should move. They do not open a pull request. */
object ShipChecks:
  val sessionId  = CheckId.trusted("ship.session")
  val releasesId = CheckId.trusted("ship.releases")
  val bumpId     = CheckId.trusted("ship.should-bump")

  def read(path: Path): UIO[ReleasesFile] =
    ZIO
      .attemptBlocking {
        if !Files.exists(path) then ReleasesFile.Missing
        else if !Files.isRegularFile(path) || !Files.isReadable(path) then ReleasesFile.Unreadable
        else ReleasesFile.Text(Files.readString(path))
      }
      .orElseSucceed(ReleasesFile.Unreadable)

  def outcomes(catalog: Catalog, env: Map[String, String], file: ReleasesFile): List[Outcome] =
    session(catalog, env) :: releases(catalog, file) :: bumps(catalog, file)

  def session(catalog: Catalog, env: Map[String, String]): Outcome =
    catalog match
      case Catalog.Empty => Outcome.Ok(sessionId, "no catalog")
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
      case Catalog.Empty => Outcome.Ok(releasesId, "no catalog")
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

  def bumps(catalog: Catalog, file: ReleasesFile): List[Outcome] =
    catalog match
      case Catalog.Empty => List(Outcome.Ok(bumpId, "no catalog"))
      case Catalog.Lines(_) =>
        parsed(file) match
          case None =>
            List(Outcome.Ok(bumpId, "releases was not read"))
          case Some(registry) =>
            val due = catalog.ships.collect:
              case ship: Ship.Line =>
                registry.relation(ship.name) match
                  case Some(TagRelation.Matches) =>
                    Some(Outcome.Found(ShipDiagnostic.ShouldBump(ship.name, ship.number, false)))
                  case Some(TagRelation.Moved) =>
                    Some(Outcome.Found(ShipDiagnostic.ShouldBump(ship.name, ship.number, true)))
                  case _ => None
            due.flatten match
              case Nil  => List(Outcome.Ok(bumpId, "catalog numbers are not released"))
              case found => found

  private def parsed(file: ReleasesFile): Option[Registry] =
    file match
      case ReleasesFile.Text(body) => Registry.parse(lines(body)).toOption
      case _                       => None

  private def lines(body: String): List[String] =
    body.split("\n", -1).toList
end ShipChecks
