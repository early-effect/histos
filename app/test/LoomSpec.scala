package histos.cli

import zio.ZIO
import zio.test.*

object LoomSpec extends ZIOSpecDefault:
  override val bootstrap =
    zio.test.testEnvironment ++ (zio.ZIOAppArgs.empty >>> histos.cli.Main.bootstrap)

  def spec = suite("loom runtime")(
    test("the cli bootstrap runs an effect and a blocking effect on virtual threads"):
      for
        _       <- ZIO.yieldNow
        main    <- ZIO.succeed(Thread.currentThread.isVirtual)
        blocked <- ZIO.attemptBlocking(Thread.currentThread.isVirtual)
      yield assertTrue(main, blocked)
  )
end LoomSpec
