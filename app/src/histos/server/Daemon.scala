package histos.server

import histos.bsp.{Bsp, Frame}
import histos.cli.{Cli, Lines}
import histos.jdk.Sha256
import histos.load.HeaderScan
import histos.testrun.{ModuleTest, ModuleTestError, TestFeed, TestStatus}
import histos.zinc.{CompileFeed, GraphCompile, GraphCompileError, ResolvedJdk}
import histos.ProjectGraph

import java.io.{InputStream, OutputStream}
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.json.*
import zio.stream.ZStream
import zio.{Chunk, IO, Promise, Scope, UIO, ZIO}

/** What a batch command found in `.histos/server.json`. A stale record does not attach. */
enum Decision:
  case Absent
  case Stale(drift: Drift)
  case Open(socket: Path)

enum Drift:
  case Version
  case Fingerprint

enum ServerError:
  case Unreachable(path: Path)
  case Stale
  case Protocol
  case Address(path: Path)
  case Io(op: ServerIo)

  def message: String = this match
    case Unreachable(path) => s"histos server at $path is not listening"
    case Stale             => "histos server does not match this build"
    case Protocol          => "histos server sent a frame this client does not understand"
    case Address(path)     => s"histos server socket path is too long: $path"
    case Io(op)            => s"histos server failed while ${op.label}"

enum ServerIo:
  case Bind, Accept, Read, Write, Save

  def label: String = this match
    case Bind   => "binding"
    case Accept => "accepting"
    case Read   => "reading"
    case Write  => "writing"
    case Save => "writing the server record"

/** One event from a command running in the daemon. */
enum Feed:
  case Out(text: String)
  case Err(text: String)
  case Exit(code: Int, err: String)

enum Call derives JsonCodec:
  case Compile(projects: List[String])
  case Test(projects: List[String])
  case Bsp

object Call:
  /** The codec is on [[Call]]. A case value does not carry it. */
  def encode(call: Call): String =
    call.toJson

enum Message derives JsonCodec:
  case Ready
  case Stale
  case Out(text: String)
  case Err(text: String)
  case Exit(code: Int, err: String)
  case Marker(histosExit: Int)

opaque type Fingerprint = String

object Fingerprint:
  given CanEqual[Fingerprint, Fingerprint] = CanEqual.derived

  def read(repo: Path): IO[ServerError, Fingerprint] =
    val build = repo.resolve("histos.scala")
    for
      built <- tagged(build)
      lock  <- tagged(repo.resolve(".histos").resolve("dependencies.lock"))
      extra <- pathBytes(repo, build)
    yield Sha256.of(built ++ Chunk.single(0.toByte) ++ lock ++ extra).value

  private[server] def trusted(raw: String): Fingerprint = raw

  extension (fingerprint: Fingerprint) def value: String = fingerprint

  private def tagged(path: Path): IO[ServerError, Chunk[Byte]] =
    ZIO.attemptBlocking {
      if Files.isRegularFile(path) then Chunk.single(1.toByte) ++ Chunk.fromArray(Files.readAllBytes(path))
      else Chunk.single(0.toByte)
    }.mapError(_ => ServerError.Io(ServerIo.Read))

  /** Regular files under each header path, after the lock. No path plugin leaves the old hash. */
  private def pathBytes(repo: Path, build: Path): IO[ServerError, Chunk[Byte]] =
    ZIO.attemptBlocking {
      if !Files.isRegularFile(build) then Chunk.empty
      else
        val paths = HeaderScan.paths(Files.readString(build))
        if paths.isEmpty then Chunk.empty
        else
          val files = paths.flatMap(tree(repo, _)).sortBy(relative(repo, _))
          files.foldLeft(Chunk.single(0.toByte)): (acc, path) =>
            val name = relative(repo, path).getBytes(StandardCharsets.UTF_8)
            acc ++ Chunk.fromArray(name) ++ Chunk.single(0.toByte) ++
              Chunk.fromArray(Files.readAllBytes(path)) ++ Chunk.single(0.toByte)
    }.mapError(_ => ServerError.Io(ServerIo.Read))

  private def tree(repo: Path, project: histos.ProjectPath): List[Path] =
    val root = repo.resolve(project.value)
    if !Files.isDirectory(root) then Nil
    else
      val walk = Files.walk(root)
      try walk.iterator.asScala.filter(path => Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)).toList
      finally walk.close()

  private def relative(repo: Path, path: Path): String =
    repo.relativize(path).toString.replace('\\', '/')

object Record:
  final case class Written(version: String, fingerprint: Fingerprint, socket: Path)
  private final case class Stored(version: String, fingerprint: String, socket: String) derives JsonCodec

  def file(repo: Path): Path =
    repo.resolve(".histos").resolve("server.json")

  def socket(repo: Path): Path =
    repo.resolve(".histos").resolve("server.sock")

  def write(repo: Path, version: String, fingerprint: Fingerprint, socket: Path): IO[ServerError, Unit] =
    val dest = file(repo)
    val tmp = dest.resolveSibling("server.json.tmp")
    val json = Stored(version, fingerprint.value, socket.toAbsolutePath.normalize.toString).toJson
    ZIO.attemptBlocking {
      Files.createDirectories(dest.getParent)
      Files.writeString(tmp, json)
      Files.move(tmp, dest, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      ()
    }.mapError(_ => ServerError.Io(ServerIo.Save))

  def read(repo: Path): UIO[Option[Written]] =
    ZIO.attemptBlocking {
      val path = file(repo)
      if Files.isRegularFile(path) then Some(Files.readString(path)) else None
    }.orElseSucceed(None).map(_.flatMap(parse))

  private def parse(text: String): Option[Written] =
    text.fromJson[Stored].toOption.map(stored => Written(stored.version, Fingerprint.trusted(stored.fingerprint), Path.of(stored.socket)))

object Attach:
  /** Missing, corrupt, or unreadable is [[Decision.Absent]]. The command then runs in-process. */
  def decide(repo: Path, version: String): UIO[Decision] =
    Record.read(repo).flatMap:
      case None => ZIO.succeed(Decision.Absent)
      case Some(record) if record.version != version => ZIO.succeed(Decision.Stale(Drift.Version))
      case Some(record) =>
        Fingerprint.read(repo).fold(
          _ => Decision.Absent,
          current =>
            if current == record.fingerprint then Decision.Open(record.socket)
            else Decision.Stale(Drift.Fingerprint),
        )

object Client:
  def talk(socket: Path, call: Call): ZStream[Any, ServerError, Feed] =
    ZStream.unwrapScoped {
      for
        channel <- ZIO.acquireRelease(connect(socket))(channel => ZIO.attempt(channel.close()).ignore)
        input = Channels.newInputStream(channel)
        output = Channels.newOutputStream(channel)
        _ <- Frame.write(output, call.toJson).mapError(written)
        first <- Frame.read(input).mapError(read)
        _ <- ZIO.fromEither(ready(first))
      yield frames(input)
    }

  /** Copy BSP frames to a daemon session. The daemon's exit marker is this process's status. */
  def splice(socket: Path): ZIO[Any, ServerError, Int] =
    ZIO.scoped {
      for
        channel <- ZIO.acquireRelease(connect(socket))(channel => ZIO.attempt(channel.close()).ignore)
        input = Channels.newInputStream(channel)
        output = Channels.newOutputStream(channel)
        _ <- Frame.write(output, Call.encode(Call.Bsp)).mapError(written)
        first <- Frame.read(input).mapError(read)
        _ <- ZIO.fromEither(ready(first))
        _ <- copyStdin(output, channel).forkScoped
        code <- copyToStdout(input)
      yield code
    }

  private def connect(socket: Path): IO[ServerError, SocketChannel] =
    ZIO.attemptBlocking(SocketChannel.open(UnixDomainSocketAddress.of(socket))).mapError(_ => ServerError.Unreachable(socket))

  private def ready(frame: Option[String]): Either[ServerError, Unit] =
    frame.flatMap(_.fromJson[Message].toOption) match
      case Some(Message.Ready) => Right(())
      case Some(Message.Stale) => Left(ServerError.Stale)
      case _                   => Left(ServerError.Protocol)

  private def frames(input: InputStream): ZStream[Any, ServerError, Feed] =
    ZStream.fromZIO(Frame.read(input).mapError(read)).flatMap:
      case None => ZStream.fail(ServerError.Protocol)
      case Some(payload) =>
        payload.fromJson[Message] match
          case Right(Message.Out(text))        => ZStream.succeed(Feed.Out(text)) ++ frames(input)
          case Right(Message.Err(text))        => ZStream.succeed(Feed.Err(text)) ++ frames(input)
          case Right(Message.Exit(code, err))  => ZStream.succeed(Feed.Exit(code, err))
          case _                               => ZStream.fail(ServerError.Protocol)

  private def copyStdin(output: OutputStream, channel: SocketChannel): IO[ServerError, Unit] =
    def loop: IO[ServerError, Unit] =
      Frame.read(java.lang.System.in).mapError(read).flatMap:
        case None => ZIO.attemptBlocking(channel.shutdownOutput()).mapError(_ => ServerError.Io(ServerIo.Write)).unit
        case Some(payload) => Frame.write(output, payload).mapError(written) *> loop
    loop

  private def copyToStdout(input: InputStream): IO[ServerError, Int] =
    def loop: IO[ServerError, Int] =
      Frame.read(input).mapError(read).flatMap:
        case None => ZIO.succeed(1)
        case Some(payload) =>
          payload.fromJson[Message] match
            case Right(Message.Marker(code)) => ZIO.succeed(code)
            case _ => Frame.write(java.lang.System.out, payload).mapError(written) *> loop
    loop

  private def written(error: Frame.FrameError): ServerError = ServerError.Io(ServerIo.Write)
  private def read(error: Frame.FrameError): ServerError = ServerError.Io(ServerIo.Read)

object Daemon:
  def open(
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      version: String,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
  ): ZIO[Scope & histos.resolve.Resolve, ServerError, Record.Written] =
    for
      listening <- Promise.make[ServerError, Record.Written]
      _ <- run(repo, graph, jdk, version, buildClasspath, build, listening).forkScoped
      record <- listening.await
    yield record

  private def run(
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      version: String,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
      listening: Promise[ServerError, Record.Written],
  ): ZIO[histos.resolve.Resolve, ServerError, Unit] =
    val socket = Record.socket(repo)
    bind(socket).foldZIO(
      error => listening.fail(error).unit,
      channel =>
        ZIO.scoped {
          for
            _ <- ZIO.addFinalizer(ZIO.attempt(channel.close()).ignore)
            fingerprint <- Fingerprint.read(repo)
            _ <- Record.write(repo, version, fingerprint, socket)
            _ <- ZIO.addFinalizer(remove(repo, socket))
            _ <- listening.succeed(Record.Written(version, fingerprint, socket.toAbsolutePath.normalize)).unit
            _ <- accept(channel, repo, graph, jdk, fingerprint, buildClasspath, build)
          yield ()
        },
    )

  private def bind(socket: Path): IO[ServerError, ServerSocketChannel] =
    for
      _ <- ZIO.attemptBlocking(Files.createDirectories(socket.getParent)).mapError(_ => ServerError.Io(ServerIo.Bind))
      _ <- ZIO.attemptBlocking(Files.deleteIfExists(socket)).mapError(_ => ServerError.Io(ServerIo.Bind))
      where <- ZIO.attempt(UnixDomainSocketAddress.of(socket.toAbsolutePath.normalize)).mapError(_ => ServerError.Address(socket))
      channel <- ZIO.attemptBlocking {
                   val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
                   server.bind(where)
                   server
                 }.mapError(_ => ServerError.Io(ServerIo.Bind))
    yield channel

  private def accept(
      server: ServerSocketChannel,
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      fingerprint: Fingerprint,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
  ): ZIO[histos.resolve.Resolve, ServerError, Unit] =
    def loop: ZIO[histos.resolve.Resolve, ServerError, Unit] =
      ZIO.attemptBlockingInterrupt(server.accept()).mapError(_ => ServerError.Io(ServerIo.Accept)).flatMap { socket =>
        handle(socket, repo, graph, jdk, fingerprint, buildClasspath, build).ensuring(ZIO.attempt(socket.close()).ignore).ignore *> loop
      }
    loop

  private def handle(
      socket: SocketChannel,
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      fingerprint: Fingerprint,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
  ): ZIO[histos.resolve.Resolve, ServerError, Unit] =
    val input = Channels.newInputStream(socket)
    val output = Channels.newOutputStream(socket)
    Frame.read(input).mapError(_ => ServerError.Io(ServerIo.Read)).flatMap:
      case None => ZIO.unit
      case Some(payload) =>
        payload.fromJson[Call].toOption match
          case None => ZIO.unit
          case Some(asked) =>
            Fingerprint.read(repo).flatMap { current =>
              if current != fingerprint then write(output, wire(Message.Stale))
              else
                write(output, wire(Message.Ready)) *> (asked match
                  case Call.Compile(projects) => compile(repo, graph, projects, jdk, output)
                  case Call.Test(projects)    => test(repo, graph, projects, jdk, output)
                  case Call.Bsp               => bsp(repo, graph, jdk, buildClasspath, build, input, output)
                )
            }

  private def compile(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      jdk: ResolvedJdk,
      output: OutputStream,
  ): ZIO[histos.resolve.Resolve, ServerError, Unit] =
    GraphCompile.console(repo, graph, projects, jdk).mapZIO {
      case CompileFeed.Diagnostic(_, _, _, problem) => write(output, wire(Message.Err(problem.text)))
      case CompileFeed.Done(cell)       => write(output, wire(Message.Out(Lines.compiled(repo, cell))))
    }.runDrain.foldZIO(
      {
        case error: GraphCompileError =>
          write(output, wire(Message.Exit(histos.cli.CliExit.CompileFailed.code, Cli.compileFailure(error))))
        case error: ServerError => ZIO.fail(error)
      },
      _ => write(output, wire(Message.Exit(histos.cli.CliExit.Success.code, ""))),
    )

  private def test(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      jdk: ResolvedJdk,
      output: OutputStream,
  ): ZIO[histos.resolve.Resolve, ServerError, Unit] =
    zio.Ref.make(false).flatMap { failed =>
      val ran =
        for
          cells <- GraphCompile.console(repo, graph, projects, jdk).mapError(ModuleTestError.Compile(_)).mapZIO { feed =>
                     val sent = feed match
                       case CompileFeed.Diagnostic(_, _, _, problem) => write(output, wire(Message.Err(problem.text)))
                       case CompileFeed.Done(cell)       => write(output, wire(Message.Out(Lines.compiled(repo, cell))))
                     sent.as(compiledCell(feed))
                   }.collectSome.runCollect
          _ <- ModuleTest.suites(repo, graph, projects, jdk, cells.toList).foreach {
                 case TestFeed.Begin(module, version, platform) =>
                   write(output, wire(Message.Out(Lines.begin(module, version, platform))))
                 case TestFeed.Out(text)              => write(output, wire(Message.Out(text)))
                 case TestFeed.Err(text)              => write(output, wire(Message.Err(text)))
                 case TestFeed.Cached(module, version, platform, suite) =>
                   write(output, wire(Message.Out(histos.cli.Lines.cached(module, version, platform, suite))))
                 case TestFeed.Done(cell) =>
                   ZIO.when(failedTest(cell.status))(failed.set(true)) *>
                     write(output, wire(Message.Out(Lines.tested(cell)))) *>
                     write(output, wire(Message.Out("")))
               }
          bad <- failed.get
        yield if bad then histos.cli.CliExit.TestFailed.code else histos.cli.CliExit.Success.code
      ran.foldZIO(
        {
          case error: ServerError => ZIO.fail(error)
          case error: ModuleTestError =>
            val (code, message) = Cli.testFailure(error)
            write(output, wire(Message.Exit(code, message)))
        },
        code => write(output, wire(Message.Exit(code, ""))),
      )
    }

  private def bsp(
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
      input: InputStream,
      output: OutputStream,
  ): ZIO[histos.resolve.Resolve, ServerError, Unit] =
    val notify = (line: String) => Frame.write(output, line).mapError(_ => ServerError.Io(ServerIo.Write)).ignore
    Bsp.open(repo, graph, jdk, buildClasspath, notify, build).flatMap { session =>
      def loop: ZIO[histos.resolve.Resolve, ServerError, Int] =
        Frame.read(input).mapError(_ => ServerError.Io(ServerIo.Read)).flatMap:
          case None => ZIO.succeed(1)
          case Some(payload) =>
            session.answer(payload).flatMap:
              case Bsp.Answer.Reply(text) => write(output, text) *> loop
              case Bsp.Answer.Quiet       => loop
              case Bsp.Answer.Stop(code)  => ZIO.succeed(code)
      loop.flatMap(code => write(output, wire(Message.Marker(code))))
    }

  private def compiledCell(feed: CompileFeed): Option[histos.zinc.CompiledCell] =
    feed match
      case CompileFeed.Done(cell)    => Some(cell)
      case CompileFeed.Diagnostic(_, _, _, _) => None

  private def failedTest(status: TestStatus): Boolean =
    status match
      case TestStatus.Failed(_, _, _)                 => true
      case TestStatus.NoTests | TestStatus.Unmatched | TestStatus.Passed(_, _) => false

  private def wire(message: Message): String =
    message.toJson

  private def write(output: OutputStream, payload: String): IO[ServerError, Unit] =
    Frame.write(output, payload).mapError(_ => ServerError.Io(ServerIo.Write))

  private def remove(repo: Path, socket: Path): UIO[Unit] =
    ZIO.attemptBlocking {
      Files.deleteIfExists(Record.file(repo))
      Files.deleteIfExists(socket)
    }.ignore
