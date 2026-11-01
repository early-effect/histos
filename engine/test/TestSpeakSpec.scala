package histos.testrun

import histos.{Jvm, ProjectName, ScalaVersion}

import zio.{Promise, ZIO}
import zio.stream.ZStream
import zio.test.*

object TestSpeakSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")

  def spec = suite("test console")(
    test("a finished cell is emitted when it finishes, not after the cell still running"):
      ZIO.scoped:
        for
          release <- Promise.make[Nothing, Unit]
          seen    <- Promise.make[Nothing, TestedCell]
          slow = ZStream
            .fromZIO(release.await)
            .as(
              TestFeed.Done(TestedCell(ProjectName("slow"), scala3, Jvm, TestStatus.Passed))
            )
          fast = ZStream.succeed(
            TestFeed.Done(TestedCell(ProjectName("fast"), scala3, Jvm, TestStatus.Passed))
          )
          fiber <- fast
            .merge(slow)
            .tap:
              case TestFeed.Done(cell) if cell.module.value == "fast" => seen.succeed(cell).unit
              case _                                                  => ZIO.unit
            .runDrain
            .forkScoped
          got <- seen.await
          _   <- release.succeed(())
          _   <- fiber.join
        yield assertTrue(got.module.value == "fast", got.status == TestStatus.Passed)
  )
end TestSpeakSpec
