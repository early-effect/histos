package histos.server

import histos.bsp.{Bsp, Frame}
import histos.jdk.Sha256
import histos.load.HeaderScan
import histos.zinc.ResolvedJdk
import histos.ProjectGraph

import java.io.{InputStream, OutputStream}
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.json.*
import zio.stream.ZStream
import zio.{
  Chunk,
  Exit,
  Fiber,
  IO,
  Promise,
  Ref,
  Scope,
  Semaphore,
  UIO,
  ZIO,
  ZLayer
}

/** What a batch command found in `.histos/server.json`. */
enum Decision:
  case Absent
  case DifferentVersion
  case Open(socket: Path)

enum ServerError:
  case Unreachable(path: Path)
  case DifferentVersion
  case NoServer
  case Protocol
  case Address(path: Path)
  case Io(op: ServerIo)

  def message: String = this match
    case Unreachable(path) => s"histos server at $path is not listening"
    case DifferentVersion  =>
      "histos server is a different version. Run histos reboot"
    case NoServer => "no histos server is running"
    case Protocol =>
      "histos server sent a frame this client does not understand"
    case Address(path) => s"histos server socket path is too long: $path"
    case Io(op)        => s"histos server failed while ${op.label}"

enum ServerIo:
  case Bind, Accept, Read, Write, Save

  def label: String = this match
    case Bind   => "binding"
    case Accept => "accepting"
    case Read   => "reading"
    case Write  => "writing"
    case Save   => "writing the server record"

/** One event from a command running in the daemon. */
enum Feed:
  case Out(text: String)
  case Err(text: String)
  case Exit(code: Int, err: String)

enum Call derives JsonCodec:
  case Compile(projects: List[String])
  case Test(projects: List[String], patterns: List[String], quick: Boolean)
  case Publish(repository: String, projects: List[String])
  case Graph
  case Run(id: String, platform: Option[String])
  case Task(id: String)
  case Cancel(id: String)
  case Toolchain
  case Resolve(action: String)
  case Workflow(action: String)
  case Assembly(destination: Option[String])
  case Reload
  case Reboot
  case Bsp

object Call:
  /** The codec is on [[Call]]. A case value does not carry it. */
  def encode(call: Call): String =
    call.toJson

enum Message derives JsonCodec:
  case Ready
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
      lock <- tagged(repo.resolve(".histos").resolve("dependencies.lock"))
      extra <- pathBytes(repo, build)
    yield Sha256.of(built ++ Chunk.single(0.toByte) ++ lock ++ extra).value

  private[server] def trusted(raw: String): Fingerprint = raw

  extension (fingerprint: Fingerprint) def value: String = fingerprint

  private def tagged(path: Path): IO[ServerError, Chunk[Byte]] =
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(path) then
          Chunk.single(1.toByte) ++ Chunk.fromArray(Files.readAllBytes(path))
        else Chunk.single(0.toByte)
      }
      .mapError(_ => ServerError.Io(ServerIo.Read))

  /** Regular files under each header path, after the lock. No path plugin
    * leaves the old hash.
    */
  private def pathBytes(repo: Path, build: Path): IO[ServerError, Chunk[Byte]] =
    ZIO
      .attemptBlocking {
        if !Files.isRegularFile(build) then Chunk.empty
        else
          val paths = HeaderScan.paths(Files.readString(build))
          if paths.isEmpty then Chunk.empty
          else
            val files = paths.flatMap(tree(repo, _)).sortBy(relative(repo, _))
            files.foldLeft(Chunk.single(0.toByte)): (acc, path) =>
              val name = relative(repo, path).getBytes(StandardCharsets.UTF_8)
              acc ++ Chunk.fromArray(name) ++ Chunk.single(0.toByte) ++
                Chunk.fromArray(Files.readAllBytes(path)) ++ Chunk.single(
                  0.toByte
                )
      }
      .mapError(_ => ServerError.Io(ServerIo.Read))

  private def tree(repo: Path, project: histos.ProjectPath): List[Path] =
    val root = repo.resolve(project.value)
    if !Files.isDirectory(root) then Nil
    else
      val walk = Files.walk(root)
      try
        walk.iterator.asScala
          .filter(path => Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS))
          .toList
      finally walk.close()

  private def relative(repo: Path, path: Path): String =
    repo.relativize(path).toString.replace('\\', '/')

object Record:
  final case class Written(
      version: String,
      fingerprint: Fingerprint,
      socket: Path
  )
  private final case class Stored(
      version: String,
      fingerprint: String,
      socket: String
  ) derives JsonCodec

  def file(repo: Path): Path =
    repo.resolve(".histos").resolve("server.json")

  def socket(repo: Path): Path =
    repo.resolve(".histos").resolve("server.sock")

  def write(
      repo: Path,
      version: String,
      fingerprint: Fingerprint,
      socket: Path
  ): IO[ServerError, Unit] =
    val dest = file(repo)
    val tmp = dest.resolveSibling("server.json.tmp")
    val json = Stored(
      version,
      fingerprint.value,
      socket.toAbsolutePath.normalize.toString
    ).toJson
    ZIO
      .attemptBlocking {
        Files.createDirectories(dest.getParent)
        Files.writeString(tmp, json)
        Files.move(
          tmp,
          dest,
          StandardCopyOption.REPLACE_EXISTING,
          StandardCopyOption.ATOMIC_MOVE
        )
        ()
      }
      .mapError(_ => ServerError.Io(ServerIo.Save))

  def read(repo: Path): UIO[Option[Written]] =
    ZIO
      .attemptBlocking {
        val path = file(repo)
        if Files.isRegularFile(path) then Some(Files.readString(path)) else None
      }
      .orElseSucceed(None)
      .map(_.flatMap(parse))

  private def parse(text: String): Option[Written] =
    text
      .fromJson[Stored]
      .toOption
      .map(stored =>
        Written(
          stored.version,
          Fingerprint.trusted(stored.fingerprint),
          Path.of(stored.socket)
        )
      )

object Attach:
  /** Missing, corrupt, or unreadable is [[Decision.Absent]]. The command then
    * runs in-process.
    */
  def decide(repo: Path, version: String): UIO[Decision] =
    Record
      .read(repo)
      .flatMap:
        case None => ZIO.succeed(Decision.Absent)
        case Some(record) if record.version != version =>
          ZIO.succeed(Decision.DifferentVersion)
        case Some(record) =>
          ZIO.succeed(Decision.Open(record.socket))

object Client:
  def talk(socket: Path, call: Call): ZStream[Any, ServerError, Feed] =
    ZStream.unwrapScoped {
      for
        channel <- ZIO.acquireRelease(connect(socket))(channel =>
          ZIO.attempt(channel.close()).ignore
        )
        input = Channels.newInputStream(channel)
        output = Channels.newOutputStream(channel)
        _ <- Frame.write(output, call.toJson).mapError(written)
        first <- Frame.read(input).mapError(read)
        _ <- ZIO.fromEither(ready(first))
      yield frames(input)
    }

  /** Copy BSP frames to a daemon session. The daemon's exit marker is this
    * process's status.
    */
  def splice(socket: Path): ZIO[Any, ServerError, Int] =
    ZIO.scoped {
      for
        channel <- ZIO.acquireRelease(connect(socket))(channel =>
          ZIO.attempt(channel.close()).ignore
        )
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
    ZIO
      .attemptBlocking(SocketChannel.open(UnixDomainSocketAddress.of(socket)))
      .mapError(_ => ServerError.Unreachable(socket))

  private def ready(frame: Option[String]): Either[ServerError, Unit] =
    frame.flatMap(_.fromJson[Message].toOption) match
      case Some(Message.Ready) => Right(())
      case _                   => Left(ServerError.Protocol)

  private def frames(input: InputStream): ZStream[Any, ServerError, Feed] =
    ZStream
      .fromZIO(Frame.read(input).mapError(read))
      .flatMap:
        case None          => ZStream.fail(ServerError.Protocol)
        case Some(payload) =>
          payload.fromJson[Message] match
            case Right(Message.Out(text)) =>
              ZStream.succeed(Feed.Out(text)) ++ frames(input)
            case Right(Message.Err(text)) =>
              ZStream.succeed(Feed.Err(text)) ++ frames(input)
            case Right(Message.Exit(code, err)) =>
              ZStream.succeed(Feed.Exit(code, err))
            case _ => ZStream.fail(ServerError.Protocol)

  private def copyStdin(
      output: OutputStream,
      channel: SocketChannel
  ): IO[ServerError, Unit] =
    def loop: IO[ServerError, Unit] =
      Frame
        .read(java.lang.System.in)
        .mapError(read)
        .flatMap:
          case None =>
            ZIO
              .attemptBlocking(channel.shutdownOutput())
              .mapError(_ => ServerError.Io(ServerIo.Write))
              .unit
          case Some(payload) =>
            Frame.write(output, payload).mapError(written) *> loop
    loop

  private def copyToStdout(input: InputStream): IO[ServerError, Int] =
    def loop: IO[ServerError, Int] =
      Frame
        .read(input)
        .mapError(read)
        .flatMap:
          case None          => ZIO.succeed(1)
          case Some(payload) =>
            payload.fromJson[Message] match
              case Right(Message.Marker(code)) => ZIO.succeed(code)
              case _                           =>
                Frame
                  .write(java.lang.System.out, payload)
                  .mapError(written) *> loop
    loop

  private def written(error: Frame.FrameError): ServerError =
    ServerError.Io(ServerIo.Write)
  private def read(error: Frame.FrameError): ServerError =
    ServerError.Io(ServerIo.Read)

object Daemon:
  def open(
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      version: String,
      apiJar: Path,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
      plugins: histos.load.PluginRuntime = Calls.quiet,
      machine: histos.jdk.Machine = Calls.idleMachine,
      relaunch: Path => IO[ServerError, Unit] = successor
  ): ZIO[
    Scope & histos.resolve.Resolve & histos.load.Loader &
      histos.cache.ContentStore,
    ServerError,
    Running
  ] =
    for
      listening <- Promise.make[ServerError, Record.Written]
      serving <- run(
        repo,
        graph,
        jdk,
        version,
        apiJar,
        buildClasspath,
        build,
        plugins,
        machine,
        relaunch(repo),
        listening
      ).tapErrorCause(listening.failCause).forkScoped
      record <- listening.await
    yield Running(record, serving.join)

  /** A listening server. `done` completes when it stops. */
  final case class Running(record: Record.Written, done: IO[ServerError, Unit])

  /** `stopping` lets the accept loop end cleanly when a reboot closes the channel. */
  private final case class Lifecycle(
      armed: Ref[Boolean],
      stopping: Promise[Nothing, Unit],
      relaunch: IO[ServerError, Unit]
  )

  private def successor(repo: Path): IO[ServerError, Unit] =
    ZIO
      .attemptBlocking {
        new ProcessBuilder("./histos", "server")
          .directory(repo.toFile)
          .redirectOutput(ProcessBuilder.Redirect.DISCARD)
          .redirectError(ProcessBuilder.Redirect.DISCARD)
          .start()
        ()
      }
      .mapError(_ => ServerError.Io(ServerIo.Bind))

  private def run(
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      version: String,
      apiJar: Path,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
      plugins: histos.load.PluginRuntime,
      machine: histos.jdk.Machine,
      relaunch: IO[ServerError, Unit],
      listening: Promise[ServerError, Record.Written]
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Unit
  ] =
    val socket = Record.socket(repo)
    bind(socket).flatMap: channel =>
      ZIO.scoped {
          for
            _ <- ZIO.addFinalizer(ZIO.attempt(channel.close()).ignore)
            armed <- Ref.make(true)
            _ <- ZIO.addFinalizer(
              ZIO.whenZIO(armed.get)(remove(repo, socket)).unit
            )
            stopping <- Promise.make[Nothing, Unit]
            life = Lifecycle(armed, stopping, relaunch)
            fingerprint <- Fingerprint.read(repo)
            _ <- Record.write(repo, version, fingerprint, socket)
            held <- Scope.make
            uses <- Ref.make[Uses](Uses.Live(0))
            graphs <- Ref.make(graph)
            live <- Ref.make(
              Generation(graph, plugins, fingerprint, held, uses)
            )
            reloading <- Semaphore.make(1)
            spliced <- Ref.make(Set.empty[Bsp.Session])
            warm = Warm(live, graphs, reloading, spliced)
            permit <- Semaphore.make(1)
            runs <- Ref.make(Map.empty[String, Fiber[ServerError, Unit]])
            _ <- listening
              .succeed(
                Record.Written(
                  version,
                  fingerprint,
                  socket.toAbsolutePath.normalize
                )
              )
              .unit
            _ <- accept(
              channel,
              repo,
              jdk,
              version,
              apiJar,
              socket,
              life,
              warm,
              permit,
              runs,
              machine,
              buildClasspath,
              build
            )
          yield ()
        }

  private def bind(socket: Path): IO[ServerError, ServerSocketChannel] =
    for
      _ <- ZIO
        .attemptBlocking(Files.createDirectories(socket.getParent))
        .mapError(_ => ServerError.Io(ServerIo.Bind))
      _ <- ZIO
        .attemptBlocking(Files.deleteIfExists(socket))
        .mapError(_ => ServerError.Io(ServerIo.Bind))
      where <- ZIO
        .attempt(UnixDomainSocketAddress.of(socket.toAbsolutePath.normalize))
        .mapError(_ => ServerError.Address(socket))
      channel <- ZIO
        .attemptBlocking {
          val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
          server.bind(where)
          server
        }
        .mapError(_ => ServerError.Io(ServerIo.Bind))
    yield channel

  private enum Uses:
    case Live(n: Int)
    case Retired(n: Int)

  private final case class Generation(
      graph: ProjectGraph,
      plugins: histos.load.PluginRuntime,
      fingerprint: Fingerprint,
      scope: Scope.Closeable,
      uses: Ref[Uses]
  )

  /** The graph ref is what a BSP splice reads. The lock makes one reload at a
    * time.
    */
  private final case class Warm(
      live: Ref[Generation],
      graphs: Ref[ProjectGraph],
      reloading: Semaphore,
      spliced: Ref[Set[Bsp.Session]]
  )

  private def accept(
      server: ServerSocketChannel,
      repo: Path,
      jdk: ResolvedJdk,
      version: String,
      apiJar: Path,
      bound: Path,
      life: Lifecycle,
      warm: Warm,
      permit: Semaphore,
      runs: Ref[Map[String, Fiber[ServerError, Unit]]],
      machine: histos.jdk.Machine,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes]
  ): ZIO[
    Scope & histos.resolve.Resolve & histos.load.Loader &
      histos.cache.ContentStore,
    ServerError,
    Unit
  ] =
    def loop: ZIO[
      Scope & histos.resolve.Resolve & histos.load.Loader &
        histos.cache.ContentStore,
      ServerError,
      Unit
    ] =
      ZIO
        .attemptBlockingInterrupt(server.accept())
        .foldZIO(
          _ =>
            ZIO.ifZIO(life.stopping.isDone)(
              ZIO.unit,
              ZIO.fail(ServerError.Io(ServerIo.Accept))
            ),
          socket =>
            handle(
              socket,
              repo,
              jdk,
              version,
              apiJar,
              bound,
              server,
              life,
              warm,
              permit,
              runs,
              machine,
              buildClasspath,
              build
            ).ignore
              .ensuring(ZIO.attempt(socket.close()).ignore)
              .forkScoped *> loop
        )
    loop

  private def handle(
      socket: SocketChannel,
      repo: Path,
      jdk: ResolvedJdk,
      version: String,
      apiJar: Path,
      bound: Path,
      server: ServerSocketChannel,
      life: Lifecycle,
      warm: Warm,
      permit: Semaphore,
      runs: Ref[Map[String, Fiber[ServerError, Unit]]],
      machine: histos.jdk.Machine,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes]
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Unit
  ] =
    val input = Channels.newInputStream(socket)
    val output = Channels.newOutputStream(socket)
    Frame
      .read(input)
      .mapError(_ => ServerError.Io(ServerIo.Read))
      .flatMap:
        case None          => ZIO.unit
        case Some(payload) =>
          payload.fromJson[Call].toOption match
            case None        => ZIO.unit
            case Some(asked) =>
              val loaded =
                asked match
                  case Call.Reload => force(repo, apiJar, version, bound, warm)
                  case _           => adopt(repo, apiJar, version, bound, warm)
              loaded.flatMap:
                case Left(message) =>
                  write(output, wire(Message.Ready)) *> write(
                    output,
                    wire(
                      Message.Exit(histos.cli.CliExit.LoadFailed.code, message)
                    )
                  )
                case Right(held) =>
                  val say = Say.frames(output)
                  val ran =
                    asked match
                      case Call.Compile(projects) =>
                        Calls.compile(repo, held.graph, projects, jdk, say)
                      case Call.Test(projects, patterns, quick) =>
                        Calls.test(
                          repo,
                          held.graph,
                          projects,
                          patterns,
                          quick,
                          jdk,
                          say
                        )
                      case Call.Publish(repository, projects) =>
                        Calls.publish(
                          repo,
                          held.graph,
                          machine,
                          jdk,
                          repository,
                          projects,
                          say
                        )
                      case Call.Graph =>
                        Calls.graph(held.graph, held.plugins, say)
                      case Call.Run(id, platform) =>
                        Calls.tracked(runs, id, say)(
                          Calls.run(
                            repo,
                            held.graph,
                            jdk,
                            machine,
                            id,
                            platform,
                            say
                          )
                        )
                      case Call.Task(id) =>
                        Calls.tracked(runs, id, say)(
                          Calls.task(
                            repo,
                            held.graph,
                            held.plugins,
                            machine,
                            id,
                            say
                          )
                        )
                      case Call.Cancel(id) =>
                        Calls.cancel(runs, id, say)
                      case Call.Toolchain =>
                        Calls.toolchain(machine, held.graph, say)
                      case Call.Resolve(action) =>
                        Calls.resolve(repo, held.graph, action, say)
                      case Call.Workflow(action) =>
                        Calls.workflow(repo, held.graph, action, say)
                      case Call.Assembly(destination) =>
                        Calls.assembly(
                          repo,
                          held.graph,
                          jdk,
                          destination,
                          say
                        )
                      case Call.Reload =>
                        write(
                          output,
                          wire(
                            Message.Exit(histos.cli.CliExit.Success.code, "")
                          )
                        )
                      case Call.Reboot =>
                        reboot(repo, bound, server, life, output)
                      case Call.Bsp =>
                        bsp(
                          repo,
                          apiJar,
                          version,
                          bound,
                          jdk,
                          warm,
                          permit,
                          buildClasspath,
                          build,
                          input,
                          output
                        )
                  val leased =
                    asked match
                      case Call.Bsp => release(held) *> ran
                      case _        => ran.ensuring(release(held))
                  write(output, wire(Message.Ready)) *> occupy(permit, asked)(
                    leased
                  )

  /** A fingerprint change loads the build. Failure keeps the previous graph and
    * is the call's error.
    */
  private def adopt(
      repo: Path,
      apiJar: Path,
      version: String,
      bound: Path,
      warm: Warm
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Either[String, Generation]
  ] =
    warm.reloading.withPermit {
      for
        now <- Fingerprint.read(repo)
        generation <- warm.live.get
        held <-
          if now == generation.fingerprint then
            acquire(generation).as(Right(generation))
          else loadInto(repo, apiJar, version, bound, warm, now)
      yield held
    }

  private def force(
      repo: Path,
      apiJar: Path,
      version: String,
      bound: Path,
      warm: Warm
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Either[String, Generation]
  ] =
    warm.reloading.withPermit {
      Fingerprint
        .read(repo)
        .flatMap(now => loadInto(repo, apiJar, version, bound, warm, now))
    }

  private def loadInto(
      repo: Path,
      apiJar: Path,
      version: String,
      bound: Path,
      warm: Warm,
      now: Fingerprint
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Either[String, Generation]
  ] =
    for
      scope <- Scope.make
      loaded <- ZIO
        .serviceWithZIO[histos.load.Loader](_.load(repo, apiJar))
        .provideSomeLayer[
          histos.load.Loader & histos.cache.ContentStore &
            histos.resolve.Resolve
        ](ZLayer.succeed(scope))
        .either
      result <- loaded match
        case Left(error)  => scope.close(Exit.unit).as(Left(error.message))
        case Right(found) =>
          for
            _ <- Record.write(repo, version, now, bound)
            uses <- Ref.make[Uses](Uses.Live(1))
            next = Generation(found.graph, found.plugins, now, scope, uses)
            previous <- warm.live.getAndSet(next)
            _ <- warm.graphs.set(found.graph)
            _ <- retire(previous)
            _ <- warm.spliced.get.flatMap(
              ZIO.foreachDiscard(_)(_.changed(previous.graph, found.graph))
            )
          yield Right(next)
    yield result

  /** The caller holds one use. A retired generation closes when the last use
    * ends.
    */
  private def acquire(generation: Generation): UIO[Unit] =
    generation.uses.update {
      case Uses.Live(n)    => Uses.Live(n + 1)
      case Uses.Retired(n) => Uses.Retired(n + 1)
    }

  private def release(generation: Generation): UIO[Unit] =
    generation.uses
      .modify {
        case Uses.Live(n) if n > 0    => (false, Uses.Live(n - 1))
        case Uses.Live(n)             => (false, Uses.Live(n))
        case Uses.Retired(n) if n > 1 => (false, Uses.Retired(n - 1))
        case Uses.Retired(_)          => (true, Uses.Retired(0))
      }
      .flatMap { close =>
        ZIO.when(close)(generation.scope.close(Exit.unit)).unit
      }

  private def retire(generation: Generation): UIO[Unit] =
    generation.uses
      .modify {
        case Uses.Live(n)          => (n == 0, Uses.Retired(n))
        case retired: Uses.Retired => (false, retired)
      }
      .flatMap { close =>
        ZIO.when(close)(generation.scope.close(Exit.unit)).unit
      }

  private def occupy[R, E](permit: Semaphore, call: Call)(
      body: ZIO[R, E, Unit]
  ): ZIO[R, E, Unit] =
    call match
      case Call.Compile(_) | Call.Test(_, _, _) | Call.Publish(_, _) |
          Call.Assembly(_) | Call.Run(_, _) =>
        permit.withPermit(body)
      case _ => body

  private def reboot(
      repo: Path,
      bound: Path,
      server: ServerSocketChannel,
      life: Lifecycle,
      output: OutputStream
  ): ZIO[Any, ServerError, Unit] =
    for
      _ <- life.armed.set(false)
      _ <- remove(repo, bound)
      _ <- life.relaunch
      _ <- write(
        output,
        wire(Message.Exit(histos.cli.CliExit.Success.code, ""))
      )
      _ <- life.stopping.succeed(())
      _ <- ZIO.attemptBlocking(server.close()).ignore
    yield ()

  private def bsp(
      repo: Path,
      apiJar: Path,
      version: String,
      bound: Path,
      jdk: ResolvedJdk,
      warm: Warm,
      permit: Semaphore,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
      input: InputStream,
      output: OutputStream
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Unit
  ] =
    Semaphore.make(1).flatMap { writer =>
      def send(payload: String): IO[ServerError, Unit] =
        writer.withPermit(
          Frame
            .write(output, payload)
            .mapError(_ => ServerError.Io(ServerIo.Write))
        )
      val notify = (line: String) => send(line).ignore
      warm.live.get.flatMap { generation =>
        Bsp
          .open(
            repo,
            generation.graph,
            jdk,
            buildClasspath,
            notify,
            build,
            effect => permit.withPermit(effect),
            Some(warm.graphs)
          )
          .flatMap { session =>
            def loop: ZIO[
              histos.resolve.Resolve & histos.load.Loader &
                histos.cache.ContentStore,
              ServerError,
              Int
            ] =
              Frame
                .read(input)
                .mapError(_ => ServerError.Io(ServerIo.Read))
                .flatMap:
                  case None          => ZIO.succeed(1)
                  case Some(payload) =>
                    adopt(repo, apiJar, version, bound, warm)
                      .flatMap {
                        case Right(gen) => ZIO.succeed(gen)
                        case Left(_)    =>
                          warm.reloading.withPermit(
                            warm.live.get.flatMap(gen => acquire(gen).as(gen))
                          )
                      }
                      .flatMap { gen =>
                        session
                          .answer(gen.graph, payload)
                          .ensuring(release(gen))
                      }
                      .flatMap:
                        case Bsp.Answer.Reply(text) => send(text) *> loop
                        case Bsp.Answer.Quiet       => loop
                        case Bsp.Answer.Stop(code)  => ZIO.succeed(code)
            warm.spliced.update(_ + session) *>
              loop
                .ensuring(warm.spliced.update(_ - session))
                .flatMap(code => send(wire(Message.Marker(code))))
          }
      }
    }

  private def wire(message: Message): String =
    message.toJson

  private def write(
      output: OutputStream,
      payload: String
  ): IO[ServerError, Unit] =
    Frame.write(output, payload).mapError(_ => ServerError.Io(ServerIo.Write))

  private def remove(repo: Path, socket: Path): UIO[Unit] =
    ZIO.attemptBlocking(Files.deleteIfExists(Record.file(repo))).ignore *>
      ZIO.attemptBlocking(Files.deleteIfExists(socket)).ignore
