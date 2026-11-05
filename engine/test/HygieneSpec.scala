package histos.doctor

import histos.supply.{Net, Reply}
import histos.version.Catalog
import histos.ProjectName

import zio.ZIO
import zio.test.*

object HygieneSpec extends ZIOSpecDefault:
  private val user = "SONATYPE_USERNAME"
  private val pass = "SONATYPE_PASSWORD"

  def spec = suite("release and pages checks")(
    test("missing Central credentials are not a rejected token"):
      val missing = HygieneChecks.credentials(catalog, Map.empty, Some(Set.empty), CentralProbe.Auth.NotRun)
      val named = HygieneChecks.credentials(
        catalog,
        Map.empty,
        Some(Set(user, pass)),
        CentralProbe.Auth.NotRun,
      )
      val rejected = HygieneChecks.credentials(
        catalog,
        Map(user -> "maven", pass -> "nope"),
        Some(Set(user, pass)),
        CentralProbe.Auth.Rejected(401),
      )
      assertTrue(
        found(missing).exists(_.summary.contains("SONATYPE_USERNAME")),
        fix(missing).contains("Add repository secrets"),
        named == Outcome.Ok(HygieneChecks.credentialsId, "repository secrets are set"),
        found(rejected).exists(_.summary.contains("401")),
        fix(rejected).contains("Regenerate the portal user token."),
        !fix(rejected).contains("are required"),
      )
    ,
    test("a 401 from the portal is rejected and a missing password does not call it"):
      val net = new Net:
        def send(method: String, url: String, body: String, headers: List[(String, String)]) =
          ZIO.succeed(Reply(401, """{"error":"Invalid token"}"""))
      for
        rejected <- CentralProbe.read(net, Map(user -> "maven", pass -> "nope"))
        absent   <- CentralProbe.read(net, Map(user -> "maven"))
      yield assertTrue(rejected == CentralProbe.Auth.Rejected(401), absent == CentralProbe.Auth.NotRun)
    ,
    test("a missing signing key names the keyserver send command"):
      val fingerprint = "AABBCCDDEEFF00112233445566778899AABBCCDD"
      val missing     = HygieneChecks.key(PgpProbe.Hit.Missing(fingerprint))
      val present     = HygieneChecks.key(PgpProbe.Hit.Present(fingerprint))
      assertTrue(
        fix(missing) == s"gpg --keyserver keyserver.ubuntu.com --send-keys $fingerprint",
        present == Outcome.Ok(HygieneChecks.keyId, "keyserver.ubuntu.com has the key"),
      )
    ,
    test("pages names the source setting and the environment allow-list"):
      val source = HygieneChecks.pages(true, PagesProbe.Hit.Ready(PagesProbe.Source.Other, PagesProbe.Gate.Open))
      val branch = HygieneChecks.pages(
        true,
        PagesProbe.Hit.Ready(PagesProbe.Source.Actions, PagesProbe.Gate.Blocks("main")),
      )
      val quiet = HygieneChecks.pages(false, PagesProbe.Hit.Absent)
      assertTrue(
        source.exists(outcome => fixOf(outcome).contains("Settings > Pages > Source: GitHub Actions")),
        source.exists(outcome => fixOf(outcome).contains("Environments > github-pages allow-list")),
        branch.exists(outcome => fixOf(outcome).contains("Environments > github-pages allow-list")),
        quiet == List(Outcome.Ok(HygieneChecks.pagesId, "no pages site")),
      )
    ,
  )

  private def catalog: Catalog =
    val built =
      for
        name    <- histos.version.ShipName.from("histos").left.map(_.message)
        project <- ProjectName.from("api").left.map(_.message)
        number  <- histos.version.ShipNumber.from("0.2.0").left.map(_.message)
        rows    <- Catalog.from(List(histos.version.Ship.Line(name, number, ::(project, Nil)))).left.map(_.message)
      yield rows
    built.getOrElse(Catalog.empty)

  private def found(outcome: Outcome): Option[Diagnostic] =
    outcome match
      case Outcome.Found(diagnostic) => Some(diagnostic)
      case _                         => None

  private def fix(outcome: Outcome): String =
    found(outcome).fold("")(diagnostic => text(diagnostic.fixes.head))

  private def fixOf(outcome: Outcome): String =
    outcome match
      case Outcome.Found(diagnostic) => text(diagnostic.fixes.head)
      case _                         => ""

  private def text(fix: Fix): String =
    fix match
      case Fix.Command(line)    => line.value
      case Fix.Steps(items)     => items.head
      case Fix.Settings(url, _) => url.value
      case Fix.Config(file, _)  => file.value
end HygieneSpec
