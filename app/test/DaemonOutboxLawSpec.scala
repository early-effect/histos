package histos.server

import zio.{Chunk, Promise, Ref, ZIO, durationInt}
import zio.test.{TestClock, ZIOSpecDefault, assertTrue, suite, test}

/** One consumer writes every offered line, and close waits until that write has finished. */
object DaemonOutboxLawSpec extends ZIOSpecDefault:
  def spec = suite("daemon outbox laws")(
    test("every line offered before close is sent, in order, before close returns"):
      for
        sent    <- Ref.make(Chunk.empty[String])
        started <- Promise.make[Nothing, Unit]
        box     <- Daemon.outbox(line =>
          val held = if line == "one" then started.succeed(()) *> ZIO.sleep(1.second) else ZIO.unit
          held *> sent.update(_ :+ line)
        )
        _      <- box.offer("one")
        _      <- started.await
        _      <- box.offer("two") *> box.offer("marker")
        closer <- box.close.fork
        _      <- TestClock.adjust(1.second)
        _      <- closer.join
        got    <- sent.get
      yield assertTrue(got == Chunk("one", "two", "marker"))
  )
