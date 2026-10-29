package histos

import zio.Duration
import zio.test.{TestAspect, TestClock, ZIOSpecDefault}

/** One adjust marks the test-clock warning done for the rest of the test. A later stamp does not arm it again. */
abstract class SuiteClock extends ZIOSpecDefault:
  override def aspects =
    super.aspects :+ TestAspect.before(TestClock.adjust(Duration.Zero))
