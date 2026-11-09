package histos.server

import zio.Promise
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** One consumer writes every offered line, and close waits until that write has finished. */
object DaemonOutboxLawSpec extends ZIOSpecDefault:
  def spec = suite("daemon outbox laws")(
    test("an offered line is sent before close returns"):
      for
        sent <- Promise.make[Nothing, String]
        box  <- Daemon.outbox(line => sent.succeed(line).unit)
        _    <- box.offer("hello")
        got  <- sent.await
        _    <- box.close
      yield assertTrue(got == "hello")
  )
