package histos

import _root_.scala.compiletime.testing.typeCheckErrors

import zio.test.*

object SupplyLiteralSpec extends ZIOSpecDefault:
  def spec = suite("SupplyLiteralSpec")(
    test("supply literals reject a bad zone, branch, check, advisory, reason, and date"):
      val zone     = typeCheckErrors("""histos.supply.TimeZone("Not/AZone")""")
      val branch   = typeCheckErrors("""histos.supply.BranchName("has space")""")
      val check    = typeCheckErrors("""histos.doctor.CheckId("Has Space")""")
      val advisory = typeCheckErrors("""import histos.*; ghsa"nope"""")
      val reason   = typeCheckErrors("""import histos.*; reason""""")
      val date     = typeCheckErrors("""import histos.*; date"2026-13-01"""")
      assertTrue(
        zone.nonEmpty,
        branch.nonEmpty,
        check.nonEmpty,
        advisory.nonEmpty,
        reason.nonEmpty,
        date.nonEmpty,
      )
  )
