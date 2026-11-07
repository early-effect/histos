package histos.server

import histos.bsp.{Bsp, Frame, Rpc}
import histos.report.CallResult
import histos.workspace.Workspace

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, ServerSocketChannel}
import java.nio.file.{Files, Path}

import zio.*
import zio.json.*
import zio.test.*

object DaemonAttachLawSpec extends ZIOSpecDefault:
  private val idle: ZIO[Any, ServerError, Unit] = ZIO.unit
  def spec                                      = suite("daemon attach laws")(
    test("the first frame has to be ready, and a missing socket is unreachable"):
      val result        = CallResult.reported(summary = "ada")
      val done: Message = Message.Done(result)
      for
        refused <- talk(Chunk(done.toJson))
        heard   <- talk(Chunk((Message.Ready: Message).toJson, done.toJson))
        missing <- Client.talk(Path.of("missing.sock"), Call.Version).runCollect.either
      yield assertTrue(
        refused == Left(ServerError.Protocol),
        heard == Right(Chunk(Feed.Done(result))),
        missing == Left(ServerError.Unreachable(Path.of("missing.sock"))),
      )
    ,
    test("a duplicate id is rejected, cancel names that id, and exit stops the loop"):
      val targets   = """{"id":1,"method":"workspace/buildTargets"}"""
      val exitFrame = """{"method":"build/exit"}"""
      val duplicate = Rpc.fail(Rpc.RpcId.Num(1), -32600, "duplicate request")
      val missing   = Rpc.fail(Rpc.RpcId.Num(3), -32602, "bsp params are required")
      val unknown   = Rpc.fail(Rpc.RpcId.Num(9), -32602, "unknown request")
      val cancelled = Rpc.fail(Rpc.RpcId.Num(1), -32800, "cancelled")
      val frames    = List(
        "not-json",
        targets,
        targets,
        """{"id":3,"method":"$/cancelRequest"}""",
        """{"id":8,"method":"$/cancelRequest","params":{"id":9}}""",
        """{"id":2,"method":"$/cancelRequest","params":{"id":1}}""",
        exitFrame,
      )
      ZIO.scoped {
        for
          gate    <- Promise.make[Nothing, Unit]
          entered <- Promise.make[Nothing, Unit]
          seen    <- Ref.make(Chunk.empty[String])
          lines   <- Ref.make(Chunk.empty[String])
          calls   <- Ref.make(0)
          // Frame 2 is the duplicate. Wait until the live answer has recorded frame 1.
          next   = scripted(calls, frames, n => if n == 2 then entered.await else ZIO.unit, "read past exit")
          answer = (payload: String) =>
            seen.update(_ :+ payload) *>
              ZIO.when(payload == targets)(entered.succeed(())) *>
              (payload.fromJson[Rpc.Incoming] match
                case Right(incoming) if incoming.method.contains("build/exit") =>
                  ZIO.succeed(Bsp.Answer.Stop(0))
                case Left(_)  => ZIO.succeed(Bsp.Answer.Reply("parse error"))
                case Right(_) => gate.await.as(Bsp.Answer.Reply("late")))
          also  <- idle.fork
          code  <- Daemon.requests(next, line => lines.update(_ :+ line).unit, answer, also)
          sent  <- seen.get
          heard <- lines.get
        yield assertTrue(
          code == 0,
          sent == Chunk("not-json", targets, exitFrame),
          heard == Chunk("parse error", duplicate, missing, unknown, cancelled),
        )
      }
    ,
    test("shutdown offers the reply and does not read another frame"):
      val shutdown = """{"id":4,"method":"build/shutdown"}"""
      ZIO.scoped {
        for
          calls <- Ref.make(0)
          lines <- Ref.make(Chunk.empty[String])
          also  <- idle.fork
          code  <- Daemon.requests(
            scripted(calls, List(shutdown), _ => ZIO.unit, "read past shutdown"),
            line => lines.update(_ :+ line).unit,
            _ => ZIO.succeed(Bsp.Answer.Reply("shutdown-ack")),
            also,
          )
          heard <- lines.get
        yield assertTrue(code == 0, heard == Chunk("shutdown-ack"))
      }
    ,
    test("exit that is not a stop is status 1 and the reply is not forwarded"):
      val exitFrame = """{"method":"build/exit"}"""
      ZIO.scoped {
        for
          calls <- Ref.make(0)
          lines <- Ref.make(Chunk.empty[String])
          also  <- idle.fork
          code  <- Daemon.requests(
            scripted(calls, List(exitFrame), _ => ZIO.unit, "read past exit"),
            line => lines.update(_ :+ line).unit,
            _ => ZIO.succeed(Bsp.Answer.Reply("not-a-stop")),
            also,
          )
          heard <- lines.get
        yield assertTrue(code == 1, heard.isEmpty)
      },
  )

  /** Each index is read once. The index after the script is a defect, not another frame. */
  private def scripted(
      calls: Ref[Int],
      frames: List[String],
      before: Int => UIO[Unit],
      past: String,
  ): IO[ServerError, Option[String]] =
    calls.modify(n => (n, n + 1)).flatMap { n =>
      before(n) *> (frames.drop(n) match
        case frame :: _ => ZIO.succeed(Some(frame))
        case Nil        => ZIO.die(new IllegalStateException(past)))
    }

  /** Unix domain socket names are short, so the peer lives under `/tmp`, not the law directory. */
  private def talk(lines: Chunk[String]): UIO[Either[ServerError, Chunk[Feed]]] =
    ZIO.scoped {
      for
        dir <- ZIO.acquireRelease(
          ZIO.attemptBlocking(Files.createTempDirectory(Path.of("/tmp"), "hs")).orDie
        )(path =>
          Workspace.delete(path).foldZIO(error => ZIO.die(new IllegalStateException(error.message)), _ => ZIO.unit)
        )
        socket = dir.resolve("s")
        server <- ZIO.acquireRelease(
          ZIO.attemptBlocking {
            val channel = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
            channel.bind(UnixDomainSocketAddress.of(socket))
            channel
          }.orDie
        )(channel => ZIO.attempt(channel.close()).ignore)
        _     <- serve(server, lines).forkScoped
        heard <- Client.talk(socket, Call.Version).runCollect.either
      yield heard
    }

  private def serve(server: ServerSocketChannel, lines: Chunk[String]): UIO[Unit] =
    ZIO.scoped {
      ZIO
        .acquireRelease(ZIO.attemptBlockingInterrupt(server.accept()).orDie)(channel =>
          ZIO.attempt(channel.close()).ignore
        )
        .flatMap { channel =>
          val input  = Channels.newInputStream(channel)
          val output = Channels.newOutputStream(channel)
          Frame
            .read(input)
            .foldZIO(
              lost,
              {
                case Some(_) =>
                  ZIO.foreachDiscard(lines)(line => Frame.write(output, line).foldZIO(lost, _ => ZIO.unit))
                case None => ZIO.unit
              },
            )
        }
    }

  private def lost(error: Frame.FrameError): UIO[Nothing] =
    ZIO.die(new IllegalStateException(error.message))
end DaemonAttachLawSpec
