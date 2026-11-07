package histos.doctor

import histos.version.{Catalog, Ship, ShipName, ShipNumber}
import histos.ProjectName

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Secrets, a key, and Pages are decided from the values the probes already returned. */
object HygieneGapLawSpec extends ZIOSpecDefault:
  private val ship   = ShipName.trusted("core")
  private val number = ShipNumber.trusted(1, 2, 3)
  private val line = Ship.Line(ship, number, ::(ProjectName.trusted("core"), Nil)) match
    case row: Ship.Line => row
  private val catalog = Catalog.Lines(::(line, Nil))

  def spec = suite("hygiene gap laws")(
    test("a secret list is missing until both portal names are present"):
      val userOnly = Set("SONATYPE_USERNAME")
      val both     = Set("SONATYPE_USERNAME", "SONATYPE_PASSWORD")
      val missing  = HygieneChecks.credentials(catalog, Map.empty, Some(userOnly), CentralProbe.Auth.NotRun)
      val present  = HygieneChecks.credentials(catalog, Map.empty, Some(both), CentralProbe.Auth.Unreachable)
      assertTrue(
        missing == Outcome.Found(ReleaseDiagnostic.CredentialsMissing()),
        present == Outcome.Ok(HygieneChecks.credentialsId, "repository secrets are set"),
      )
    ,
    test("a present key passes, a missing key fails, and an unreachable keyserver is skipped"):
      val present = HygieneChecks.key(PgpProbe.Hit.Present("abcd"))
      val missing = HygieneChecks.key(PgpProbe.Hit.Missing("abcd"))
      val down    = HygieneChecks.key(PgpProbe.Hit.Unreachable("abcd"))
      assertTrue(
        present == Outcome.Ok(HygieneChecks.keyId, "keyserver.ubuntu.com has the key"),
        missing == Outcome.Found(ReleaseDiagnostic.KeyMissing("abcd")),
        down == Outcome.Skipped(
          HygieneChecks.keyId,
          Probe.JobEnv,
          Fix.Command(ShellLine.trusted("gpg --keyserver keyserver.ubuntu.com --recv-keys abcd")),
        ),
      )
    ,
    test("pages without a token is skipped, and a blocking branch is a finding"):
      val absent = HygieneChecks.pages(true, PagesProbe.Hit.Absent)
      val blocks = HygieneChecks.pages(
        true,
        PagesProbe.Hit.Ready(PagesProbe.Source.Actions, PagesProbe.Gate.Blocks("main")),
      )
      assertTrue(
        absent == List(
          Outcome.Skipped(HygieneChecks.pagesId, Probe.GitHubRead, Fix.Command(ShellLine.trusted("gh auth login")))
        ),
        blocks == List(Outcome.Found(PagesDiagnostic.Branch("main"))),
      )
  )
end HygieneGapLawSpec
