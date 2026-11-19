package histos.server

import java.nio.file.Path
import java.util.concurrent.TimeUnit

import zio.{Clock, Ref, ZIO, durationInt}
import zio.test.*

object ServerWaitSpec extends ZIOSpecDefault:
  private val socket = Path.of("sock")

  def spec = suite("a spawned server that exits")(
    test("a dead child fails with its detail and does not sleep out the poll") {
      for
        start <- Clock.currentTime(TimeUnit.MILLISECONDS)
        exit  <- ServerWait
          .awaitListening(
            ZIO.fail(ServerError.Unreachable(socket)),
            ZIO.succeed(false),
            ZIO.succeed("Guard.class disagreed"),
            socket,
            1200,
          )
          .either
        end <- Clock.currentTime(TimeUnit.MILLISECONDS)
      yield assertTrue(
        exit == Left(ServerError.Exited("Guard.class disagreed")),
        end == start,
      )
    },
    test("a live child whose connect fails sleeps and retries") {
      for
        tries <- Ref.make(0)
        connect = tries.updateAndGet(_ + 1).flatMap { n =>
          if n == 1 then ZIO.fail(ServerError.Unreachable(socket)) else ZIO.unit
        }
        start <- Clock.currentTime(TimeUnit.MILLISECONDS)
        fiber <- ServerWait
          .awaitListening(connect, ZIO.succeed(true), ZIO.dieMessage("detail"), socket, 1)
          .either
          .fork
        _     <- TestClock.adjust(50.millis)
        exit  <- fiber.join
        end   <- Clock.currentTime(TimeUnit.MILLISECONDS)
        count <- tries.get
      yield assertTrue(exit == Right(()), count == 2, end - start >= 50L)
    },
    test("a successful connect returns") {
      for
        start <- Clock.currentTime(TimeUnit.MILLISECONDS)
        read  <- Ref.make(false)
        exit  <- ServerWait
          .awaitListening(ZIO.unit, ZIO.succeed(true), read.set(true).as(""), socket, 5)
          .either
        end   <- Clock.currentTime(TimeUnit.MILLISECONDS)
        asked <- read.get
      yield assertTrue(exit == Right(()), !asked, end == start)
    },
  )
end ServerWaitSpec
