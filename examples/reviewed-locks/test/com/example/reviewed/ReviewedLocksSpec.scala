package com.example.reviewed

import histos.lock.*
import histos.plugin.testkit.LockHarness
import zio.ZIO
import zio.test.*

object ReviewedLocksSpec extends ZIOSpecDefault:
  private val decisions = suite("reviewed lock policy")(
    test("the interface layer keeps CI frozen"):
      val decision = ZIO.serviceWithZIO[LockStrategy]: strategy =>
        strategy.decide(LockHarness.missingLock(ExecutionKind.Ci))
      decision
        .provide(ReviewedLocks.layer(ReviewConfig(local = ReviewMode.Ask)))
        .map(found => assertTrue(found == LockDecision.Frozen))
  )

  def spec = decisions
end ReviewedLocksSpec
