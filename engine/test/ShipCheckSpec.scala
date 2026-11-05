package histos.doctor

import histos.version.{Catalog, Ship, ShipAdvice, ShipDiagnostic, ShipName, ShipNumber}
import histos.ProjectName

import zio.test.*

object ShipCheckSpec extends ZIOSpecDefault:
  private val number = ShipNumber("0.2.0")

  private val opened: Either[String, (Catalog, ShipName)] =
    for
      name    <- ShipName.from("histos").left.map(_.message)
      project <- ProjectName.from("api").left.map(_.message)
      catalog <- Catalog.from(List(Ship.Line(name, number, ::(project, Nil)))).left.map(_.message)
    yield (catalog, name)

  private def use(file: ReleasesFile, env: Map[String, String])(
      f: (List[Outcome], ShipName) => TestResult
  ): TestResult =
    opened match
      case Left(error)           => assertTrue(error == "opened")
      case Right((catalog, name)) =>
        f(ShipChecks.outcomes(catalog, env, file), name)

  def spec = suite("ship checks")(
    test("a missing releases file names the file and does not ask for a bump pull request"):
      use(ReleasesFile.Missing, Map.empty): (outcomes, _) =>
        val found = outcomes.collect { case Outcome.Found(diagnostic) => diagnostic }
        assertTrue(
          found.map(_.check.value) == List("ship.releases"),
          found.head.level == Level.Error,
          found.head.summary.contains("Create releases"),
          found.head.summary.contains("<ship> matches"),
          found.head.fixes.head match
            case Fix.Command(line) => line.value == "touch releases"
            case _                 => false,
          !found.head.summary.toLowerCase.contains("pull request") || found.head.summary.contains("Do not") ,
        )
    ,
    test("an empty releases file is enough, and development is not an error"):
      use(ReleasesFile.Text(""), Map.empty): (outcomes, _) =>
        assertTrue(
          outcomes.collect { case Outcome.Found(_) => true }.isEmpty,
          outcomes.exists {
            case Outcome.Ok(id, note) =>
              id.value == "ship.session" && note.contains("histos 0.2.0-ci")
            case _ => false
          },
        )
    ,
    test("a snapshot session missing HISTOS_SHA names the export and histos run versions"):
      use(ReleasesFile.Text(""), Map("HISTOS_SESSION" -> "snapshot")): (outcomes, _) =>
        val found = outcomes.collect { case Outcome.Found(diagnostic: ShipDiagnostic) => diagnostic }
        assertTrue(
          found.map(_.check.value) == List("ship.session"),
          found.head.summary.contains(ShipAdvice.snapshotExport),
          found.head.summary.contains(ShipAdvice.versionsCommand),
          found.head.summary.contains("HISTOS_SHA"),
        )
    ,
    test("a released number warns to edit next and does not open a bump pull request"):
      val body = "histos matches\n"
      use(ReleasesFile.Text(body), Map("HISTOS_SESSION" -> "release")): (outcomes, name) =>
        val bumps = outcomes.collect { case Outcome.Found(diagnostic: ShipDiagnostic.ShouldBump) => diagnostic }
        assertTrue(
          bumps.map(_.ship) == List(name),
          bumps.head.level == Level.Warning,
          bumps.head.summary.contains("Edit Ship(\"histos\").next(...)"),
          bumps.head.summary.contains("Do not open a bump pull request"),
          !bumps.head.shadowed,
        )
    ,
  )
end ShipCheckSpec
