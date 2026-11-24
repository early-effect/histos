package histos.cli

import histos.bsp.Frame
import histos.build.{Beat, BuildEvent}
import histos.process.{ChildError, ChildEvent, ChildProcess, RelayCommand, RunRelay}
import histos.report.CallResult
import histos.server.{Attendance, Call, Message, RunBytes, RunInput, RunLaunch}
import histos.theme.Role
import histos.zinc.{CompileFeed, CompileProblem, CompileSeverity, CompiledCell, PositionBase}
import histos.{Platform, ProjectName, ScalaVersion}

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, ServerSocketChannel}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, Console, IO, Promise, Queue, Ref, Scope, ZIO, ZIOAppDefault, ZLayer, durationInt}

/** An executable stdin/stdout fixture for the local and socket CLI boundaries. */
object RunStreamsFixture extends ZIOAppDefault:
  private given Appearance = Appearance.plain
  private given LockChoice = LockChoice(None)

  private[cli] enum FixtureError:
    case Input, JsonInput, Mode, Filesystem, Socket, TextOutput, ChildExit
    case Frame(error: histos.bsp.Frame.FrameError)
    case Remote(error: histos.server.ServerError)
    case Child(error: ChildError)

  private enum Peer:
    case Eof, First, Paused, Done

  def run = ZIO.scoped:
    for
      args <- getArgs
      _    <- args.toList match
        case "local" :: Nil =>
          for
            line <- Console.readLine.mapError(_ => FixtureError.Input)
            _    <- ZIO.fromEither(line.fromJson[Json]).mapError(_ => FixtureError.JsonInput)
            _    <- local(Chunk.fromArray((line + "\n").getBytes(UTF_8)))
          yield ()
        case "remote" :: Nil       => remote(Peer.Eof)
        case "remote-close" :: Nil => remote(Peer.First)
        case "remote-pause" :: Nil => remote(Peer.Paused)
        case "remote-done" :: Nil  => remote(Peer.Done)
        case _                     => ZIO.fail(FixtureError.Mode)
      _ <- Console.printLineError("stream fixture closed").mapError(_ => FixtureError.Input)
    yield ()

  private[cli] def events(repo: Path): Chunk[BuildEvent] =
    val module  = ProjectName("fixture")
    val version = ScalaVersion("3.10.1")
    val problem =
      CompileProblem("fixture warning", None, CompileSeverity.Warn, None, None, None, None, PositionBase.Dotty)
    Chunk(
      BuildEvent.Layout(8),
      BuildEvent.Theme(histos.theme.Theme.standard),
      BuildEvent.Out("run 12345678-1234-1234-1234-123456789abc"),
      BuildEvent.Out("fixture 3.10.1 jvm"),
      BuildEvent.Compile(CompileFeed.Began(module, version, Platform.Jvm)),
      BuildEvent.Compile(CompileFeed.Diagnostic(module, version, Platform.Jvm, problem)),
      BuildEvent.Compile(
        CompileFeed.Done(CompiledCell(module, version, Platform.Jvm, repo, 1, None, true, None, Some(1)))
      ),
      BuildEvent.Compile(
        CompileFeed.Done(CompiledCell(module, version, Platform.Jvm, repo, 1, None, false, None, None))
      ),
      BuildEvent.Status("fixture progress", Role.Pulse),
      BuildEvent.Err("fixture stderr"),
    )

  private def local(bytes: Chunk[Byte]): ZIO[Scope, FixtureError, Unit] =
    val repo = Path.of(".")
    for
      shown <- Ref.make(Show.start)
      _     <- ZIO.foreachDiscard(events(repo))(event =>
        Cli.heard(repo, shown, Beat.Heard(event), Attendance.Program).provide(ZLayer.succeed(Stdio.redirected))
      )
      output <- child(bytes)
      _      <- ZIO.foreachDiscard(output):
        case ChildEvent.Bytes(data) =>
          Cli.heard(repo, shown, Beat.Bytes(data), Attendance.Program).provide(ZLayer.succeed(Stdio.redirected))
        case ChildEvent.Err(text) =>
          Cli
            .heard(repo, shown, Beat.Heard(BuildEvent.Err(text)), Attendance.Program)
            .provide(ZLayer.succeed(Stdio.redirected))
        case ChildEvent.Out(_)       => ZIO.fail(FixtureError.TextOutput)
        case ChildEvent.Exited(code) => ZIO.fail(FixtureError.ChildExit).unless(code.value == 0).unit
    yield ()

  private[cli] def child(bytes: Chunk[Byte]): ZIO[Scope, FixtureError, Chunk[ChildEvent]] =
    for
      root <- ZIO.acquireRelease(
        ZIO
          .attemptBlocking(Files.createTempDirectory(Path.of("/tmp"), "histos-bytes-"))
          .mapError(_ => FixtureError.Filesystem)
      )(root => ZIO.attemptBlocking(Files.deleteIfExists(root)).ignore)
      queue  <- ZIO.acquireRelease(Queue.unbounded[RelayCommand])(_.shutdown)
      _      <- queue.offer(RelayCommand.Stdin(bytes))
      _      <- queue.offer(RelayCommand.End)
      output <- ChildProcess
        .command(
          List("/bin/cat"),
          root,
          Map.empty,
          RunRelay.Open(queue.take.map(Some(_))),
          ChildProcess.Stdout.Bytes,
        )
        .runCollect
        .mapError(FixtureError.Child(_))
    yield output

  private def remote(peer: Peer): ZIO[Scope, FixtureError, Unit] =
    for
      root <- ZIO.acquireRelease(
        ZIO
          .attemptBlocking(Files.createTempDirectory("histos-streams-"))
          .mapError(_ => FixtureError.Filesystem)
      )(root => ZIO.attemptBlocking(Files.deleteIfExists(root)).ignore)
      socket = root.resolve("socket")
      server <- ZIO.acquireRelease(
        ZIO.attemptBlocking(ServerSocketChannel.open(StandardProtocolFamily.UNIX)).mapError(_ => FixtureError.Socket)
      )(server =>
        ZIO.attemptBlocking(server.close()).ignore *> ZIO.attemptBlocking(Files.deleteIfExists(socket)).ignore
      )
      _       <- ZIO.attemptBlocking(server.bind(UnixDomainSocketAddress.of(socket))).mapError(_ => FixtureError.Socket)
      closed  <- Promise.make[Nothing, Unit]
      serving <- serve(server, root, peer, closed).forkScoped
      call = Call.Run("fixture", None, RunLaunch.Tool(Map.empty, None), Nil)
      done <- Cli
        .remote(root, socket, call)
        .mapError(FixtureError.Remote(_))
        .provide(ZLayer.succeed(Stdio.redirected))
      _ <- closed.succeed(())
      _ <- serving.join
      _ <- ZIO.fail(FixtureError.Remote(histos.server.ServerError.Protocol)).unless(done.exit == CliExit.Success)
    yield ()

  private def serve(
      server: ServerSocketChannel,
      root: Path,
      peer: Peer,
      closed: Promise[Nothing, Unit],
  ): IO[FixtureError, Unit] =
    ZIO.scoped:
      for
        channel <- ZIO.acquireRelease(
          ZIO.attemptBlockingInterrupt(server.accept()).mapError(_ => FixtureError.Socket)
        )(channel => ZIO.attemptBlocking(channel.close()).ignore)
        input  = Channels.newInputStream(channel)
        output = Channels.newOutputStream(channel)
        request <- Frame.read(input).mapError(FixtureError.Frame(_))
        _       <- ZIO.fail(FixtureError.Input).when(request.isEmpty)
        messages = Chunk[Message](Message.Ready) ++ events(root).map(Message.Event(_))
        _ <- ZIO.foreachDiscard(messages)(message => post(output, message))
        _ <- peer match
          case Peer.Paused | Peer.Done => ZIO.sleep(300.millis)
          case Peer.Eof | Peer.First   => ZIO.unit
        bytes <- peer match
          case Peer.Done => ZIO.succeed(Chunk.fromArray("{\"closed\":true}\n".getBytes(UTF_8)))
          case Peer.Eof | Peer.First | Peer.Paused => collect(input, peer, Chunk.empty)
        _ <- post(output, Message.Output(RunBytes.encode(bytes)))
        _ <- post(output, Message.Done(CallResult.reported(program = Some(0))))
        // Do not relieve stdin backpressure until the client has closed its scope.
        _ <- closed.await
        _ <- ZIO
          .attemptBlockingInterrupt {
            val buffer = new Array[Byte](4096)
            while input.read(buffer) >= 0 do ()
          }
          .mapError(_ => FixtureError.Socket)
      yield ()

  private def post(output: java.io.OutputStream, message: Message): IO[FixtureError, Unit] =
    Frame.write(output, message.toJson).mapError(FixtureError.Frame(_))

  private def collect(
      input: java.io.InputStream,
      peer: Peer,
      seen: Chunk[Byte],
  ): IO[FixtureError, Chunk[Byte]] =
    Frame
      .read(input)
      .mapError(FixtureError.Frame(_))
      .flatMap:
        case None      => ZIO.fail(FixtureError.Input)
        case Some(raw) =>
          ZIO
            .fromEither(raw.fromJson[RunInput])
            .mapError(_ => FixtureError.Input)
            .flatMap:
              case RunInput.Stdin(encoded) =>
                ZIO
                  .fromOption(RunBytes.decode(encoded))
                  .orElseFail(FixtureError.Input)
                  .flatMap: bytes =>
                    val all = seen ++ bytes
                    peer match
                      case Peer.First if all.contains('\n'.toByte)         => ZIO.succeed(all)
                      case Peer.Eof | Peer.First | Peer.Paused | Peer.Done => collect(input, peer, all)
              case RunInput.StdinEnd  => ZIO.succeed(seen)
              case RunInput.Interrupt => collect(input, peer, seen)
end RunStreamsFixture
