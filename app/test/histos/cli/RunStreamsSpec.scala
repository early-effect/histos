package histos.cli

import histos.build.{Beat, BuildEvent}
import histos.process.ChildEvent
import histos.server.{Attendance, Call, RunLaunch, WatchPolicy}

import java.nio.file.Path

import zio.{Chunk, Console, Ref, ZIO, ZLayer}
import zio.test.*

object RunStreamsSpec extends ZIOSpecDefault:
  private given Appearance = Appearance.plain
  private val repo         = Path.of("/repo")
  private val call         = Call.Run("fixture", None, RunLaunch.Tool(Map.empty, None), Nil)

  private val tests = suite("run streams")(
    test("program stdout preserves malformed UTF-8, CRLF and a final fragment without text events") {
      val bytes = Chunk[Byte](0, 0xff.toByte, 0x80.toByte, '\r'.toByte, '\n'.toByte, 'x'.toByte)
      ZIO.withSystem(zio.System.SystemLive):
        ZIO.scoped:
          RunStreamsFixture
            .child(bytes)
            .map: output =>
              assertTrue(
                output.collect { case ChildEvent.Bytes(data) => data }.flatten == bytes,
                output.collect { case ChildEvent.Out(text) => text }.isEmpty,
                output.collect { case ChildEvent.Exited(code) => code.value } == Chunk(0),
              )
    },
    test("local CLI build, compile, cached and status events leave stdout to the program") {
      for
        shown <- Ref.make(Show.start)
        _     <- ZIO
          .foreachDiscard(RunStreamsFixture.events(repo))(event =>
            Cli.heard(repo, shown, Beat.Heard(event), call.attendance)
          )
          .provide(ZLayer.succeed(Stdio.redirected))
        out <- TestConsole.output
        err <- TestConsole.outputErr
      yield assertTrue(
        out.isEmpty,
        err.exists(_.contains("run 12345678")),
        err.exists(_.contains("compiling")),
        err.exists(_.contains("skipped unchanged")),
        err.exists(_.contains("fixture warning")),
        err.exists(_.contains("fixture progress")),
        err.exists(_.contains("fixture stderr")),
      )
    },
    test("the Native frontend renderer keeps every event's text but puts run diagnostics on stderr") {
      val events = RunStreamsFixture.events(repo)
      val plain  = events.flatMap(event => Show(repo, event))
      val run    = events.flatMap(event => Show(repo, event, attendance = call.attendance))
      val watch  =
        events.flatMap(event => Show(repo, event, attendance = Call.Watch(call, WatchPolicy.After).attendance))
      assertTrue(
        run.nonEmpty,
        run.map(_.text) == plain.map(_.text),
        run.forall {
          case CompileConsole.Line.Err(_) => true
          case CompileConsole.Line.Out(_) => false
        },
        run == watch,
      )
    },
    test("non-program commands retain their normal stdout") {
      for
        shown <- Ref.make(Show.start)
        _     <- Show.follow(
          repo,
          shown,
          BuildEvent.Out("graph output"),
          Appearance.plain,
          text => Console.printLine(text),
          text => Console.printLineError(text),
          Attendance.Quiet,
        )
        out <- TestConsole.output
        err <- TestConsole.outputErr
      yield assertTrue(out == Vector("graph output\n"), err.isEmpty)
    },
  )

  private val verified = tests @@ TestAspect.withLiveClock @@ TestAspect.silent

  def spec = verified
end RunStreamsSpec
