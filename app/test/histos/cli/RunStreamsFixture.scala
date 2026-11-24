package histos.cli

import histos.bsp.Frame
import histos.build.{Beat, BuildEvent}
import histos.process.{ChildError, ChildEvent, ChildProcess, RelayCommand, RunRelay}
import histos.report.CallResult
import histos.server.{Attendance, Call, Message, RunBytes, RunLaunch}
import histos.theme.Role
import histos.zinc.{CompileFeed, CompileProblem, CompileSeverity, CompiledCell, PositionBase}
import histos.{Platform, ProjectName, ScalaVersion}

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, ServerSocketChannel}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, Console, IO, Queue, Ref, Scope, ZIO, ZIOAppDefault, ZLayer}

/** An executable stdin/stdout fixture for the local and socket CLI boundaries. */
object RunStreamsFixture extends ZIOAppDefault:
  private given Appearance = Appearance.plain
  private given LockChoice = LockChoice(None)

  private[cli] enum FixtureError:
    case Input, JsonInput, Mode, Filesystem, Socket, TextOutput, ChildExit
    case Frame(error: histos.bsp.Frame.FrameError)
    case Remote(error: histos.server.ServerError)
    case Child(error: ChildError)

  def run = ZIO.scoped:
    for
      args <- getArgs
      line <- Console.readLine.mapError(_ => FixtureError.Input)
      _    <- ZIO.fromEither(line.fromJson[Json]).mapError(_ => FixtureError.JsonInput)
      bytes = Chunk.fromArray((line + "\n").getBytes(UTF_8))
      _ <- args.toList match
        case "local" :: Nil  => local(bytes)
        case "remote" :: Nil => remote(bytes)
        case _               => ZIO.fail(FixtureError.Mode)
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

  private def remote(bytes: Chunk[Byte]): ZIO[Scope, FixtureError, Unit] =
    for
      root <- ZIO.acquireRelease(
        ZIO
          .attemptBlocking(Files.createTempDirectory(Path.of("/tmp"), "histos-streams-"))
          .mapError(_ => FixtureError.Filesystem)
      )(root => ZIO.attemptBlocking(Files.deleteIfExists(root)).ignore)
      socket = root.resolve("socket")
      server <- ZIO.acquireRelease(
        ZIO.attemptBlocking(ServerSocketChannel.open(StandardProtocolFamily.UNIX)).mapError(_ => FixtureError.Socket)
      )(server =>
        ZIO.attemptBlocking(server.close()).ignore *> ZIO.attemptBlocking(Files.deleteIfExists(socket)).ignore
      )
      _       <- ZIO.attemptBlocking(server.bind(UnixDomainSocketAddress.of(socket))).mapError(_ => FixtureError.Socket)
      serving <- serve(server, root, bytes).forkScoped
      call = Call.Run("fixture", None, RunLaunch.Tool(Map.empty, None), Nil)
      done <- Cli
        .remote(root, socket, call, histos.process.Terminal.Detached)
        .mapError(FixtureError.Remote(_))
        .provide(ZLayer.succeed(Stdio.redirected))
      _ <- serving.join
      _ <- ZIO.fail(FixtureError.Remote(histos.server.ServerError.Protocol)).unless(done.exit == CliExit.Success)
    yield ()

  private def serve(server: ServerSocketChannel, root: Path, bytes: Chunk[Byte]): IO[FixtureError, Unit] =
    ZIO.scoped:
      for
        channel <- ZIO.acquireRelease(
          ZIO.attemptBlockingInterrupt(server.accept()).mapError(_ => FixtureError.Socket)
        )(channel => ZIO.attemptBlocking(channel.close()).ignore)
        input  = Channels.newInputStream(channel)
        output = Channels.newOutputStream(channel)
        request <- Frame.read(input).mapError(FixtureError.Frame(_))
        _       <- ZIO.fail(FixtureError.Input).when(request.isEmpty)
        messages = Chunk[Message](Message.Ready) ++ events(root).map(Message.Event(_)) ++
          Chunk[Message](Message.Output(RunBytes.encode(bytes)), Message.Done(CallResult.reported(program = Some(0))))
        _ <- ZIO.foreachDiscard(messages)(message =>
          Frame.write(output, message.toJson).mapError(FixtureError.Frame(_))
        )
      yield ()
end RunStreamsFixture
