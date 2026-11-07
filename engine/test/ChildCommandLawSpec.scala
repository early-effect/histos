package histos.process

import histos.workspace.Workspace

import java.nio.file.Path

import zio.Chunk
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** A linked program runs through the group launcher. The status is the program's status. */
object ChildCommandLawSpec extends ZIOSpecDefault:
  private val path = Map("PATH" -> "/bin:/usr/bin")

  def spec = suite("child command laws")(
    test("an empty argv is no program, echo's line is stdout, and a missing program exits 127"):
      Workspace.work("histos-child-command").flatMap: dir =>
        for
          empty   <- ChildProcess.command(Nil, dir, path).runCollect.either
          echoed  <- ChildProcess.command(List("/bin/echo", "histos-child"), dir, path).runCollect.either
          stopped <- ChildProcess.command(List("/usr/bin/false"), dir, path).runCollect.either
          missing <- ChildProcess.command(List("/bin/no-such-histos-bin"), dir, path).runCollect.either
        yield assertTrue(
          empty == Left(ChildError.NoProgram),
          shown(echoed) == "0|histos-child",
          shown(stopped) == "1|",
          shown(missing) == "127|",
        )
    @@ TestAspect.withLiveSystem
  )

  private def shown(events: Either[ChildError, Chunk[ChildEvent]]): String =
    events match
      case Left(error) => error.toString
      case Right(chunk) =>
        val code = chunk.collect { case ChildEvent.Exited(exit) => exit.value.toString }.mkString(",")
        val out  = chunk.collect { case ChildEvent.Out(text) => text }.mkString(",")
        s"$code|$out"
end ChildCommandLawSpec
