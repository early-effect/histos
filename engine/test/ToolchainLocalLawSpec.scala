package histos.doctor

import histos.ToolchainIdentity

import zio.test.*

/** The stale-toolchain sentence is the warning, and a warning does not fail doctor. */
object ToolchainLocalLawSpec extends ZIOSpecDefault:
  private val built = "0123456789abcdef0123456789abcdef01234567"
  private val head  = "fedcba9876543210fedcba9876543210fedcba98"
  private val line  = s"local toolchain built from $built (dirty); this checkout is $head"

  def spec = suite("toolchain local laws")(
    test("a stale local toolchain warns with the skew line, and a warning leaves the exit at 0"):
      val identity = ToolchainIdentity("0.3.0-abc-SNAPSHOT", built, true)
      val found    = ToolchainChecks.reported(true, Some(identity), Some(head))
      val fresh    = ToolchainChecks.reported(true, Some(identity.copy(dirty = false, commit = head)), Some(head))
      val pin      = ToolchainChecks.reported(false, Some(identity), Some(head))
      val report   = DoctorReport(List(found), Nil)
      assertTrue(
        found == Outcome.Found(ToolchainDiagnostic.Local(line)),
        ToolchainDiagnostic.Local(line).level == Level.Warning,
        ToolchainDiagnostic.Local(line).check.value == "toolchain.local",
        fresh == Outcome.Ok(
          ToolchainChecks.localId,
          "the local toolchain matches this checkout, or this command runs the pin",
        ),
        pin == fresh,
        report.exit == 0,
        report.warnings == 1,
        report.errors == 0,
      )
  )
end ToolchainLocalLawSpec
