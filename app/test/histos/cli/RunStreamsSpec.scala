package histos.cli

import histos.build.{Beat, BuildEvent}
import histos.process.ChildEvent
import histos.server.{Attendance, Call, RunLaunch, WatchPolicy}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import zio.{Chunk, Console, Ref, ZIO, ZLayer, durationInt}
import zio.test.*

object RunStreamsSpec extends ZIOSpecDefault:
  private given Appearance = Appearance.plain
  private val repo         = Path.of("/repo")
  private val call         = Call.Run("fixture", None, RunLaunch.Tool(Map.empty, None), Nil)

  private enum ProbeError:
    case Java, IO, Deadline

  private enum InputEnd:
    case Open, Eof

  private val json      = Chunk.fromArray(" {\"text\":\"π\"} \r\n".getBytes(UTF_8))
  private val largeJson = Chunk.fromArray(("{\"text\":\"" + "π".repeat(300000) + "\"}\n").getBytes(UTF_8))

  private val tests = suite("run streams")(
    test("the owned remote endpoint forwards bytes and EOF and removes its socket") {
      remote("remote", json, InputEnd.Eof, json)
    },
    test("the owned remote endpoint closes while caller stdin remains open") {
      remote("remote-close", json, InputEnd.Open, json)
    },
    test("input queued while the peer is paused remains byte exact") {
      remote("remote-pause", largeJson, InputEnd.Eof, largeJson)
    },
    test("peer completion releases a backpressured stdin writer and the owned endpoint") {
      remote("remote-done", largeJson, InputEnd.Open, Chunk.fromArray("{\"closed\":true}\n".getBytes(UTF_8)))
    },
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

  private def remote(mode: String, bytes: Chunk[Byte], inputEnd: InputEnd, expected: Chunk[Byte]) = ZIO.scoped:
    for
      java <- ZIO
        .systemWith(_.property("java.home"))
        .mapError(_ => ProbeError.Java)
        .flatMap(home => ZIO.fromOption(home.filter(_.nonEmpty)).orElseFail(ProbeError.Java))
        .map(home => Path.of(home, "bin", "java").toString)
      root <- ZIO.acquireRelease(
        ZIO.attemptBlocking(Files.createTempDirectory(Path.of("/tmp"), "hi-")).mapError(_ => ProbeError.IO)
      )(root => ZIO.attemptBlocking(Files.deleteIfExists(root)).ignore)
      process <- ZIO.acquireRelease(
        ZIO
          .attemptBlocking(
            ProcessBuilder(
              java,
              "--enable-native-access=ALL-UNNAMED",
              "-Djava.io.tmpdir=" + root,
              "-cp",
              _root_.java.lang.System.getProperty("java.class.path"),
              "histos.cli.RunStreamsFixture",
              mode,
            ).start()
          )
          .mapError(_ => ProbeError.IO)
      )(process =>
        ZIO.attemptBlocking {
          process.destroyForcibly()
          process.waitFor()
          process.getOutputStream.close()
          process.getInputStream.close()
          process.getErrorStream.close()
        }.ignore
      )
      output <- ZIO
        .attemptBlockingCancelable(process.getInputStream.readAllBytes())(
          ZIO.succeed(process.destroyForcibly())
        )
        .mapError(_ => ProbeError.IO)
        .forkScoped
      errors <- ZIO
        .attemptBlockingCancelable(process.getErrorStream.readAllBytes())(
          ZIO.succeed(process.destroyForcibly())
        )
        .mapError(_ => ProbeError.IO)
        .forkScoped
      _ <- ZIO
        .attemptBlockingCancelable {
          process.getOutputStream.write(bytes.toArray)
          process.getOutputStream.flush()
          inputEnd match
            case InputEnd.Eof  => process.getOutputStream.close()
            case InputEnd.Open => ()
        }(ZIO.succeed(process.destroyForcibly()))
        .mapError(_ => ProbeError.IO)
        .timeoutFail(ProbeError.Deadline)(10.seconds)
      done   <- ZIO.attemptBlocking(process.waitFor(10, TimeUnit.SECONDS)).mapError(_ => ProbeError.IO)
      result <-
        if done then
          for
            out   <- output.join
            err   <- errors.join
            clean <- ZIO
              .attemptBlocking {
                val entries = Files.list(root)
                try entries.count() == 0
                finally entries.close()
              }
              .mapError(_ => ProbeError.IO)
            text = new String(err, UTF_8)
          yield assertTrue(
            process.exitValue() == 0,
            !process.isAlive,
            Chunk.fromArray(out) == expected,
            text.contains("fixture progress"),
            text.contains("run 12345678"),
            text.contains("stream fixture closed"),
            clean,
          )
        else ZIO.succeed(assertTrue(done))
    yield result

  private val verified = tests @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.silent

  def spec = verified
end RunStreamsSpec
