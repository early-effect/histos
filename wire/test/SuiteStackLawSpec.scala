package histos.native

import zio.durationInt
import zio.test.*

/** Histos links a Native suite without the optimizer, so a ZIO run loop frame is about 5.5KB, and ZIO nests 300 of them
  * before it trampolines. A suite thread, at `ThreadStack.default` since `wire` names none, has to hold that on every
  * platform. A smaller thread dies with a StackOverflowError, and the timeout turns that into a failure.
  */
object SuiteStackLawSpec extends ZIOSpecDefault:
  def spec = suite("suite stack laws")(
    test("a suite thread holds the ZIO run loop at the depth ZIO trampolines"):
      for sample <- Gen.listOfN(300)(Gen.char).runHead
      yield assertTrue(sample.map(_.length).contains(300))
  ) @@ TestAspect.timeout(1.minute)
