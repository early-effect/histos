package histos.doctor

import histos.version.{BumpPart, ShipNumber}

import zio.test.*

object BinaryCoversSpec extends ZIOSpecDefault:
  private val previous = ShipNumber.trusted(0, 15, 0)

  def spec = suite("binary bump coverage")(
    test("a 0.x minor break is not covered by the next patch") {
      assertTrue(!BinaryCheck.covers(previous, ShipNumber.trusted(0, 15, 1), BumpPart.Minor))
    },
    test("a 0.x minor break is covered by the next minor") {
      assertTrue(BinaryCheck.covers(previous, ShipNumber.trusted(0, 16, 0), BumpPart.Minor))
    },
    test("a patch break is covered by any greater number") {
      assertTrue(BinaryCheck.covers(previous, ShipNumber.trusted(0, 15, 1), BumpPart.Patch))
    },
  )
end BinaryCoversSpec
