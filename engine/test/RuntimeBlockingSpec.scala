package histos.jdk

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicBoolean

import zio.*
import zio.test.*

object RuntimeBlockingSpec extends histos.SuiteClock:
  def spec = suite("runtime blocking scope miniature")(
    test("ordinary blocking work finishes before its scope finalizer"):
      ZIO.scoped:
        for
          started <- ZIO.succeed(new CountDownLatch(1))
          finish  <- ZIO.succeed(new CountDownLatch(1))
          wrote   <- ZIO.succeed(new AtomicBoolean(false))
          closed  <- Ref.make(false)
          fiber   <- ZIO
            .scoped(
              ZIO.acquireRelease(ZIO.unit)(_ => closed.set(true)) *>
                ZIO.attemptBlocking {
                  started.countDown()
                  finish.await()
                  wrote.set(true)
                }
            )
            .forkScoped
          _           <- ZIO.attemptBlocking(started.await())
          interrupted <- fiber.interrupt.timeout(100.millis)
          early       <- closed.get
          _           <- ZIO.succeed(finish.countDown())
          exit        <- fiber.await
          released    <- closed.get
        yield assertTrue(
          interrupted.isEmpty,
          !early,
          exit.isInterrupted,
          wrote.get(),
          released,
        )
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds)
