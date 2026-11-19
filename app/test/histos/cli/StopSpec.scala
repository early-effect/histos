package histos.cli

import zio.test.*

object StopSpec extends ZIOSpecDefault:
  def spec = suite("histos stop")(
    test("stop is a command") {
      for parsed <- Grammar.parse(List("stop")).either
      yield assertTrue(parsed == Right(Command.Stop))
    }
  )
