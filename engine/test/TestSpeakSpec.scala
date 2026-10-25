package histos.testrun

import zio.{durationInt, Chunk, Promise, Queue, ZIO}
import zio.stream.Take
import zio.test.*

object TestSpeakSpec extends ZIOSpecDefault:
  def spec = suite("test console")(
    test("the speaking cell is emitted before it ends"):
      ZIO.scoped:
        for
          first  <- Queue.unbounded[Take[ModuleTestError, TestFeed]]
          second <- Queue.unbounded[Take[ModuleTestError, TestFeed]]
          seen   <- Promise.make[Nothing, TestFeed]
          _      <- second.offer(Take.single(TestFeed.Out("later")))
          _      <- second.offer(Take.end)
          _      <- first.offer(Take.single(TestFeed.Out("now")))
          fiber  <- ModuleTest
            .inOrder(List(first, second))
            .tap:
              case line @ TestFeed.Out("now") => seen.succeed(line).unit
              case _                          => ZIO.unit
            .runCollect
            .forkScoped
          got <- seen.await.timeout(1.second)
          _   <- first.offer(Take.end)
          all <- fiber.join
        yield assertTrue(
          got.contains(TestFeed.Out("now")),
          all == Chunk(TestFeed.Out("now"), TestFeed.Out("later")),
        )
  ) @@ TestAspect.withLiveClock
