package histos.cli

import histos.suite.HistosSpec

import zio.test.*

object WithdrawSpec extends HistosSpec:
  def spec = suite("histos publish withdraw")(
    test("withdraw is a command") {
      for parsed <- Grammar.parse(List("publish", "withdraw")).either
      yield assertTrue(parsed == Right(Command.PublishWithdraw))
    }
  )
