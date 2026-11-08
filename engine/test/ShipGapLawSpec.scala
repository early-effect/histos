package histos.doctor

import histos.version.{Catalog, Session, Ship, ShipAdvice, ShipDiagnostic, ShipName, ShipNumber}
import histos.workspace.Workspace
import histos.ProjectName

import java.nio.file.Files

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A releases file is missing, unreadable, or text. A development session names the catalog. A release session does not. */
object ShipGapLawSpec extends ZIOSpecDefault:
  private val ship   = ShipName.trusted("core")
  private val number = ShipNumber.trusted(1, 2, 3)
  private val line = Ship.Line(ship, number, ::(ProjectName.trusted("core"), Nil)) match
    case row: Ship.Line => row
  private val catalog = Catalog.Lines(::(line, Nil))

  def spec = suite("ship gap laws")(
    test("development names every catalog row, and a release session is complete"):
      val developing = ShipChecks.session(catalog, Map.empty)
      val named      = ShipChecks.session(catalog, Map(Session.Key -> "development"))
      val released   = ShipChecks.session(catalog, Map(Session.Key -> "release"))
      val unread     = ShipChecks.releases(catalog, ReleasesFile.Unreadable)
      val held       = ShipChecks.bumps(catalog, ReleasesFile.Missing)
      assertTrue(
        developing == Outcome.Ok(ShipChecks.sessionId, "development compiles core 1.2.3-ci"),
        named == developing,
        released == Outcome.Ok(ShipChecks.sessionId, "the publish session is complete"),
        unread == Outcome.Found(ShipDiagnostic.Releases(ShipAdvice.unreadable)),
        held == List(Outcome.Ok(ShipChecks.bumpId, "releases was not read")),
      )
    ,
    test("a missing path, a directory, and a file are three different releases reads"):
      Workspace.work("histos-ship-gap").flatMap: dir =>
        val missing = dir.resolve("absent")
        val folder  = dir.resolve("dir")
        val file    = dir.resolve("releases")
        for
          absent <- ShipChecks.read(missing)
          _      <- ZIO.attempt(Files.createDirectory(folder)).orDie
          held   <- ShipChecks.read(folder)
          _      <- ZIO.attempt(Files.writeString(file, "core matches\n")).orDie
          text   <- ShipChecks.read(file)
        yield assertTrue(
          absent == ReleasesFile.Missing,
          held == ReleasesFile.Unreadable,
          text == ReleasesFile.Text("core matches\n"),
        )
  )
end ShipGapLawSpec
