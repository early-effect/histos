package histos.suite

import zio.*
import zio.test.*

object SuiteWallSpec extends HistosSpec:
  def spec = suite("suite wall")(
    test("the line is the label and the rendered duration") {
      assertTrue(SuiteWall.line("inner", 1500.millis) == "suite inner 1 s 500 ms")
    }
  )
