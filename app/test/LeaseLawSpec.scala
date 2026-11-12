package histos.server

import zio.*
import zio.test.*

/** A program run holds the class-directory lock until its compile is done, so a restart's compile takes the lock while
  * the program runs. Every other writer holds the lock for its whole call. The lock is free when the call ends.
  */
object LeaseLawSpec extends ZIOSpecDefault:
  private val program = Call.Run("app", None)
  private val compile = Call.Compile(Nil)

  def spec = suite("lease laws")(
    test("a run holds the lock until it compiled, and a compile takes it while the program still runs"):
      for
        permit   <- Semaphore.make(1)
        signal   <- Promise.make[Nothing, UIO[Unit]]
        exited   <- Promise.make[Nothing, Unit]
        running  <- Daemon.lease(permit, program)(compiled => signal.succeed(compiled) *> exited.await).fork
        compiled <- signal.await
        before   <- permit.available
        _        <- compiled
        _        <- Daemon.occupy(permit, compile)(ZIO.unit)
        alive    <- running.poll
        _        <- exited.succeed(())
        _        <- running.join
        after    <- permit.available
      yield assertTrue(before == 0L, alive.isEmpty, after == 1L)
    ,
    test("a run that fails before it compiled frees the lock"):
      for
        permit <- Semaphore.make(1)
        failed <- Daemon.lease(permit, program)(_ => ZIO.fail("no compile")).flip
        after  <- permit.available
      yield assertTrue(failed == "no compile", after == 1L)
    ,
    test("any other writer holds the lock for its whole call, and the signal it is given does nothing"):
      for
        permit <- Semaphore.make(1)
        seen   <- Ref.make(-1L)
        _      <- Daemon.lease(permit, compile)(compiled => compiled *> permit.available.flatMap(seen.set))
        inside <- seen.get
        after  <- permit.available
      yield assertTrue(inside == 0L, after == 1L),
  )
end LeaseLawSpec
