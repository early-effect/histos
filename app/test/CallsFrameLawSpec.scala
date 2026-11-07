package histos.server

import histos.bsp.Frame
import histos.build.BuildEvent
import histos.report.{BuildFault, CallResult, Diagnostic, Observation, Report, Severity}
import histos.task.RunId

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.net.{InetSocketAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.*
import zio.json.*
import zio.test.*

object CallsFrameLawSpec extends ZIOSpecDefault:
  private val socket = Path.of("frames.sock")
  private val noted  = Diagnostic(Some("A.scala"), Some(3), Some(1), None, None, Severity.Error, "nope")
  private val kept   = Diagnostic(None, None, None, None, None, Severity.Warn, "kept")
  private val tally  = Observation(Chunk.empty, Some(1), None, Some(4), 0)

  def spec = suite("calls frame laws")(
    test("a reported frame carries the minted run, the kept tally, and the noted diagnostics"):
      for
        (result, bytes) <- framed { say =>
          say.mint(RunId.trusted("run-9")) *> say.keep(tally) *> say.note(Chunk(noted)) *>
            say(Message.Done(CallResult.reported())) *>
            say(Message.Done(CallResult.Reported(Report.empty.copy(diagnostics = Chunk(kept))))) *>
            say(Message.Done(CallResult.Fault(BuildFault.Interrupted)))
        }
        decoded <- messages(bytes)
      yield
        val header  = new String(bytes, StandardCharsets.US_ASCII).takeWhile(_ != '{')
        val carried = Report.empty.copy(run = Some("run-9"), observation = Some(tally), diagnostics = Chunk(noted))
        val own     = Report.empty.copy(run = Some("run-9"), observation = Some(tally), diagnostics = Chunk(kept))
        assertTrue(
          result == Right(()),
          lengthOf(header) == Some(carriedJson(carried)),
          decoded == Right(
            Chunk(
              Message.Done(CallResult.Reported(carried)),
              Message.Done(CallResult.Reported(own)),
              Message.Done(CallResult.Fault(BuildFault.Interrupted)),
            )
          ),
        )
    ,
    test("dropExit turns a fault into an err frame and swallows a report"):
      for
        (result, bytes) <- framed { say =>
          val dropped = say.dropExit
          dropped(Message.Done(CallResult.Fault(BuildFault.Interrupted))) *>
            dropped(Message.Done(CallResult.Fault(BuildFault.Release("")))) *>
            dropped(Message.Done(CallResult.reported(summary = "hidden"))) *>
            dropped(Message.Event(BuildEvent.Out("kept")))
        }
        decoded <- messages(bytes)
      yield assertTrue(
        result == Right(()),
        decoded == Right(
          Chunk(
            Message.Event(BuildEvent.Err("interrupted")),
            Message.Event(BuildEvent.Out("kept")),
          )
        ),
      )
    ,
    test("a closed socket is a write fault"):
      ZIO.scoped {
        for
          server <- ZIO.acquireRelease(
            ZIO.attemptBlocking {
              val socket = new ServerSocket()
              socket.bind(new InetSocketAddress("127.0.0.1", 0))
              socket
            }.orDie
          )(socket => ZIO.attempt(socket.close()).ignore)
          client <- ZIO.acquireRelease(
            ZIO.attemptBlocking {
              val socket = new Socket()
              socket.connect(server.getLocalSocketAddress)
              socket
            }.orDie
          )(socket => ZIO.attempt(socket.close()).ignore)
          accepted <- ZIO.acquireRelease(ZIO.attemptBlocking(server.accept()).orDie)(socket =>
            ZIO.attempt(socket.close()).ignore
          )
          out = client.getOutputStream
          _      <- ZIO.attemptBlocking(accepted.close()).orDie
          _      <- ZIO.attemptBlocking(client.close()).orDie
          failed <- Say.frames(out, socket)(Message.Event(BuildEvent.Out("x"))).either
        yield assertTrue(failed == Left(ServerError.Io(ServerIo.Write(socket))))
      },
  )

  private def carriedJson(report: Report): Int =
    (Message.Done(CallResult.Reported(report)): Message).toJson.getBytes(StandardCharsets.UTF_8).length

  private def lengthOf(header: String): Option[Int] =
    val prefix = "Content-Length: "
    if header.startsWith(prefix) && header.endsWith("\r\n\r\n") then
      val raw = header.drop(prefix.length).dropRight(4)
      if raw.nonEmpty && raw.forall(_.isDigit) then raw.toIntOption else None
    else None

  private def framed(use: Say => IO[ServerError, Unit]): UIO[(Either[ServerError, Unit], Array[Byte])] =
    val out = new ByteArrayOutputStream()
    use(Say.frames(out, socket)).either.map(result => (result, out.toByteArray))

  private def messages(bytes: Array[Byte]): UIO[Either[String, Chunk[Message]]] =
    val input                                                          = new ByteArrayInputStream(bytes)
    def loop(acc: Chunk[Message]): UIO[Either[String, Chunk[Message]]] =
      Frame.read(input).either.flatMap {
        case Left(error)          => ZIO.succeed(Left(error.message))
        case Right(None)          => ZIO.succeed(Right(acc))
        case Right(Some(payload)) =>
          payload.fromJson[Message] match
            case Left(error)    => ZIO.succeed(Left(error))
            case Right(message) => loop(acc :+ message)
      }
    loop(Chunk.empty)
end CallsFrameLawSpec
