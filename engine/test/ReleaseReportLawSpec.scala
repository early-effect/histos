package histos.publish

import histos.central.{DeploymentId, DeploymentState}
import histos.version.ShipName

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A release report is the lines a caller prints. A state with no token is still a deployment. */
object ReleaseReportLawSpec extends ZIOSpecDefault:
  def spec = suite("release report laws")(
    test("refused, held, failed, and deployed stay in that order, and a pending deploy has no token"):
      val ship = ShipName("demo")
      DeploymentId.from("dep-1") match
        case Left(_)   => assertTrue(false)
        case Right(id) =>
          val validated = Deployed(ship, id, DeploymentState.Validated, "1.2.3")
          val pending   = Deployed(ship, id, DeploymentState.Pending, "1.2.3")
          val failed    = Deployed(ship, id, DeploymentState.Failed, "1.2.3")
          val report    = ReleaseReport(List("refused-one"), List("held-one"), List("failed-one"), List(validated))
          val quiet     = ReleaseReport(Nil, Nil, Nil, List(pending))
          assertTrue(
            validated.line == "demo dep-1 VALIDATED",
            pending.line == "demo dep-1",
            failed.line == "demo dep-1",
            report.lines == List("refused-one", "held-one", "failed-one", "demo dep-1 VALIDATED"),
            report.anyFailed,
            quiet.anyFailed == false,
            quiet.lines == List("demo dep-1"),
            ReleaseError.Record("demo").message == "demo deployment record is not readable",
            ReleaseError.Io("dist/out").message == "could not write dist/out",
            ReleaseError.Credentials.message == "SONATYPE_USERNAME and SONATYPE_PASSWORD are required",
            ReleaseError.Rejected.message == "the portal rejected the token. Regenerate the portal user token.",
          )
  )
