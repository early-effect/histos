package histos.server

import histos.jdk.JdkRuntime
import histos.bsp.{Bsp, Frame, Rpc}
import histos.cli.Operation
import histos.jdk.Sha256
import histos.load.{HeaderScan, LoadError, LoadedBuild}
import histos.plugin.{FlagName, NamedText, ParamName, PluginArgs}
import histos.report.{BuildFault, CallResult, Rejection, Report}
import histos.task.RunId
import histos.toolchain.Runtimes
import histos.workflow.WorkflowAction
import histos.{Platform, ProjectGraph}

import java.io.{InputStream, OutputStream}
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import histos.build.BuildEvent
import zio.json.*
import histos.report.WireJson.given JsonCodec[CallResult]
import histos.report.WireJson.given JsonCodec[BuildEvent]
import histos.report.WireJson.given JsonCodec[Platform]
import zio.stream.ZStream
import zio.{Chunk, Exit, Fiber, IO, Promise, Queue, Ref, Scope, Semaphore, UIO, ZIO, ZLayer}

/** What a batch command found in `.histos/server.json`. */
enum Decision:
  case Absent
  case DifferentVersion
  case Open(socket: Path)

enum ServerError:
  case Toolchain(error: histos.jdk.ToolchainError)
  case Compiled(error: histos.zinc.GraphCompileError)
  case Resolve(error: histos.resolve.ResolveError)
  case Unreachable(path: Path)
  case DifferentVersion
  case NoServer
  case Protocol
  case Address(path: Path)
  case Io(op: ServerIo)

  def message: String = this match
    case Toolchain(error)  => error.message
    case Compiled(error)   => error.message
    case Resolve(error)    => error.message
    case Unreachable(path) => s"histos server at $path is not listening"
    case DifferentVersion  =>
      "histos server is a different version. Run histos reboot"
    case NoServer => "no histos server is running"
    case Protocol =>
      "histos server sent a frame this client does not understand"
    case Address(path) => s"histos server socket path is too long: $path"
    case Io(op)        => s"histos server failed while ${op.label}"
end ServerError

enum ServerIo:
  case Bind(path: Path)
  case Accept(path: Path)
  case Read(path: Path)
  case Write(path: Path)
  case Save(path: Path)

  def label: String = this match
    case Bind(_)   => "binding"
    case Accept(_) => "accepting"
    case Read(_)   => "reading"
    case Write(_)  => "writing"
    case Save(_)   => "writing the server record"
end ServerIo

/** One event from a command running in the daemon. */
enum Feed:
  case Event(event: BuildEvent)
  case Done(result: CallResult)

private object CallJson:
  given JsonCodec[ParamName] = JsonCodec.string.transformOrFail(
    raw => ParamName.from(raw).left.map(_.message),
    _.value,
  )
  given JsonCodec[FlagName] = JsonCodec.string.transformOrFail(
    raw => FlagName.from(raw).left.map(_.message),
    _.value,
  )
  given JsonCodec[NamedText]     = JsonCodec.derived
  given JsonCodec[PluginArgs]    = JsonCodec.derived
  given JsonCodec[ResolveAction] =
    JsonCodec.string.transformOrFail(
      raw => ResolveAction.from(raw).left.map(_.message),
      _.token,
    )
  given JsonCodec[WorkflowAction] =
    JsonCodec.string.transformOrFail(
      raw => WorkflowAction.from(raw).left.map(_.message),
      _.token,
    )

import CallJson.given

enum Call derives JsonCodec:
  case Compile(projects: List[String])
  case Test(projects: List[String], patterns: List[String], quick: Boolean)
  case Publish(repository: String, projects: List[String])
  case Graph
  case Run(id: String, platform: Option[Platform])
  case Task(id: String, args: PluginArgs)
  case Cancel(id: String)
  case Toolchain
  case Resolve(action: ResolveAction)
  case Workflow(action: WorkflowAction)
  case Assembly(destination: Option[String])
  case Clean(projects: List[String])
  case CleanFull
  case Reload
  case Reboot
  case Version
  case CachePath
  case CacheList
  case CacheClean(dryRun: Boolean)
  case LauncherUpdate
  case Watch(inner: Call)
  case Bsp
  case PreviewCompile(projects: List[String])
  case PreviewTest(projects: List[String])
  case PreviewPublish(projects: List[String])
  case PreviewClean(projects: List[String])
  case PreviewCleanFull
  case PreviewRun(id: String, platform: Option[Platform])

object Call:
  /** The codec is on [[Call]]. A case value does not carry it. */
  def encode(call: Call): String =
    call.toJson

enum Message derives JsonCodec:
  case Ready
  case Event(event: BuildEvent)
  case Done(result: CallResult)
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
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(path) then Chunk.single(1.toByte) ++ Chunk.fromArray(Files.readAllBytes(path))
        else Chunk.single(0.toByte)
      }
      .mapError(_ => ServerError.Io(ServerIo.Read(path)))

  /** Regular files under each header path, after the lock. No path plugin leaves the old hash.
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
      .mapError(_ => ServerError.Io(ServerIo.Read(build)))

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
end Fingerprint

object Record:
  final case class Written(
      version: String,
      fingerprint: Fingerprint,
      socket: Path,
  )
  private final case class Stored(
      version: String,
      fingerprint: String,
      socket: String,
  ) derives JsonCodec

  def file(repo: Path): Path =
    repo.resolve(".histos").resolve("server.json")

  def socket(repo: Path): Path =
    repo.resolve(".histos").resolve("server.sock")

  def write(
      repo: Path,
      version: String,
      fingerprint: Fingerprint,
      socket: Path,
  ): IO[ServerError, Unit] =
    val dest = file(repo)
    val tmp  = dest.resolveSibling("server.json.tmp")
    val json = Stored(
      version,
      fingerprint.value,
      socket.toAbsolutePath.normalize.toString,
    ).toJson
    ZIO
      .attemptBlocking {
        Files.createDirectories(dest.getParent)
        Files.writeString(tmp, json)
        Files.move(
          tmp,
          dest,
          StandardCopyOption.REPLACE_EXISTING,
          StandardCopyOption.ATOMIC_MOVE,
        )
        ()
      }
      .mapError(_ => ServerError.Io(ServerIo.Save(dest)))

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
          Path.of(stored.socket),
        )
      )
end Record

object Attach:
  /** Missing, corrupt, or unreadable is [[Decision.Absent]]. The command then runs in-process.
    */
  def decide(repo: Path, version: String): UIO[Decision] =
    Record
      .read(repo)
      .flatMap:
        case None                                      => ZIO.succeed(Decision.Absent)
        case Some(record) if record.version != version =>
          ZIO.succeed(Decision.DifferentVersion)
        case Some(record) =>
          ZIO.succeed(Decision.Open(record.socket))

object Client:
  def talk(socket: Path, call: Call): ZStream[Any, ServerError, Feed] =
    ZStream.unwrapScoped {
      for
        channel <- ZIO.acquireRelease(connect(socket))(channel => ZIO.attempt(channel.close()).ignore)
        input  = Channels.newInputStream(channel)
        output = Channels.newOutputStream(channel)
        _     <- Frame.write(output, call.toJson).mapError(written(socket))
        first <- Frame.read(input).mapError(read(socket))
        _     <- ZIO.fromEither(ready(first))
      yield frames(input, socket)
    }

  /** Copy BSP frames to a daemon session. The daemon's exit marker is this process's status.
    */
  def splice(socket: Path): ZIO[Any, ServerError, Int] =
    ZIO.scoped {
      for
        channel <- ZIO.acquireRelease(connect(socket))(channel => ZIO.attempt(channel.close()).ignore)
        input  = Channels.newInputStream(channel)
        output = Channels.newOutputStream(channel)
        _ <- Frame
          .write(output, Call.encode(Call.Bsp))
          .mapError(written(socket))
        first <- Frame.read(input).mapError(read(socket))
        _     <- ZIO.fromEither(ready(first))
        _     <- copyStdin(output, channel, socket).forkScoped
        code  <- copyToStdout(input, socket)
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

  private def frames(
      input: InputStream,
      socket: Path,
  ): ZStream[Any, ServerError, Feed] =
    ZStream
      .fromZIO(Frame.read(input).mapError(read(socket)))
      .flatMap:
        case None          => ZStream.fail(ServerError.Protocol)
        case Some(payload) =>
          payload.fromJson[Message] match
            case Right(Message.Event(event)) =>
              ZStream.succeed(Feed.Event(event)) ++ frames(input, socket)
            case Right(Message.Done(result)) =>
              ZStream.succeed(Feed.Done(result))
            case _ => ZStream.fail(ServerError.Protocol)

  private def copyStdin(
      output: OutputStream,
      channel: SocketChannel,
      socket: Path,
  ): IO[ServerError, Unit] =
    def loop: IO[ServerError, Unit] =
      Frame
        .read(java.lang.System.in)
        .mapError(read(socket))
        .flatMap:
          case None =>
            ZIO
              .attemptBlocking(channel.shutdownOutput())
              .mapError(_ => ServerError.Io(ServerIo.Write(socket)))
              .unit
          case Some(payload) =>
            Frame.write(output, payload).mapError(written(socket)) *> loop
    loop

  private def copyToStdout(
      input: InputStream,
      socket: Path,
  ): IO[ServerError, Int] =
    def loop: IO[ServerError, Int] =
      Frame
        .read(input)
        .mapError(read(socket))
        .flatMap:
          case None          => ZIO.succeed(1)
          case Some(payload) =>
            payload.fromJson[Message] match
              case Right(Message.Marker(code)) => ZIO.succeed(code)
              case _                           =>
                Frame
                  .write(java.lang.System.out, payload)
                  .mapError(written(socket)) *> loop
    loop

  private def written(socket: Path)(error: Frame.FrameError): ServerError =
    ServerError.Io(ServerIo.Write(socket))
  private def read(socket: Path)(error: Frame.FrameError): ServerError =
    ServerError.Io(ServerIo.Read(socket))
end Client

object Daemon:
  def open(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      version: String,
      apiJar: Path,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
      plugins: histos.load.PluginRuntime = Calls.quiet,
      machine: histos.jdk.Machine = Calls.idleMachine,
      relaunch: Path => IO[ServerError, Unit] = successor,
  ): ZIO[
    Scope & histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Running,
  ] =
    for
      listening <- Promise.make[ServerError, Record.Written]
      serving   <- run(
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
        listening,
      ).tapErrorCause(listening.failCause).forkScoped
      record <- listening.await
    yield Running(record, serving.join)

  /** A listening server. `done` completes when it stops. */
  final case class Running(record: Record.Written, done: IO[ServerError, Unit])

  /** `stopping` lets the accept loop end cleanly when a reboot closes the channel.
    */
  private final case class Lifecycle(
      armed: Ref[Boolean],
      stopping: Promise[Nothing, Unit],
      relaunch: IO[ServerError, Unit],
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
      .mapError(_ => ServerError.Io(ServerIo.Bind(repo)))

  private def run(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      version: String,
      apiJar: Path,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
      plugins: histos.load.PluginRuntime,
      machine: histos.jdk.Machine,
      relaunch: IO[ServerError, Unit],
      listening: Promise[ServerError, Record.Written],
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Unit,
  ] =
    val socket = Record.socket(repo)
    bind(socket).flatMap: channel =>
      ZIO.scoped {
        for
          _     <- ZIO.addFinalizer(ZIO.attempt(channel.close()).ignore)
          armed <- Ref.make(true)
          _     <- ZIO.addFinalizer(
            ZIO.whenZIO(armed.get)(remove(repo, socket)).unit
          )
          stopping <- Promise.make[Nothing, Unit]
          life = Lifecycle(armed, stopping, relaunch)
          fingerprint <- Fingerprint.read(repo)
          _           <- Record.write(repo, version, fingerprint, socket)
          held        <- Scope.make
          uses        <- Ref.make[Uses](Uses.Live(0))
          graphs      <- Ref.make(graph)
          live        <- Ref.make(
            Generation(graph, plugins, fingerprint, held, uses)
          )
          reloading <- Semaphore.make(1)
          spliced   <- Ref.make(Set.empty[Bsp.Session])
          warm = Warm(live, graphs, reloading, spliced)
          permit <- Semaphore.make(1)
          runs   <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
          _      <- listening
            .succeed(
              Record.Written(
                version,
                fingerprint,
                socket.toAbsolutePath.normalize,
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
            build,
          )
        yield ()
      }

  private def bind(socket: Path): IO[ServerError, ServerSocketChannel] =
    for
      _ <- ZIO
        .attemptBlocking(Files.createDirectories(socket.getParent))
        .mapError(_ => ServerError.Io(ServerIo.Bind(socket)))
      _ <- ZIO
        .attemptBlocking(Files.deleteIfExists(socket))
        .mapError(_ => ServerError.Io(ServerIo.Bind(socket)))
      where <- ZIO
        .attempt(UnixDomainSocketAddress.of(socket.toAbsolutePath.normalize))
        .mapError(_ => ServerError.Address(socket))
      channel <- ZIO
        .attemptBlocking {
          val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
          server.bind(where)
          server
        }
        .mapError(_ => ServerError.Io(ServerIo.Bind(socket)))
    yield channel

  private enum Uses:
    case Live(n: Int)
    case Retired(n: Int)

  private final case class Generation(
      graph: ProjectGraph,
      plugins: histos.load.PluginRuntime,
      fingerprint: Fingerprint,
      scope: Scope.Closeable,
      uses: Ref[Uses],
  )

  /** The graph ref is what a BSP splice reads. The lock makes one reload at a time.
    */
  private final case class Warm(
      live: Ref[Generation],
      graphs: Ref[ProjectGraph],
      reloading: Semaphore,
      spliced: Ref[Set[Bsp.Session]],
  )

  private def accept(
      server: ServerSocketChannel,
      repo: Path,
      jdk: JdkRuntime,
      version: String,
      apiJar: Path,
      bound: Path,
      life: Lifecycle,
      warm: Warm,
      permit: Semaphore,
      runs: Ref[Map[RunId, Fiber[ServerError, Unit]]],
      machine: histos.jdk.Machine,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
  ): ZIO[
    Scope & histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Unit,
  ] =
    def loop: ZIO[
      Scope & histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
      ServerError,
      Unit,
    ] =
      ZIO
        .attemptBlockingInterrupt(server.accept())
        .foldZIO(
          _ =>
            ZIO.ifZIO(life.stopping.isDone)(
              ZIO.unit,
              ZIO.fail(ServerError.Io(ServerIo.Accept(bound))),
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
              build,
            ).ignore
              .ensuring(ZIO.attempt(socket.close()).ignore)
              .forkScoped *> loop,
        )
    loop

  private def handle(
      socket: SocketChannel,
      repo: Path,
      jdk: JdkRuntime,
      version: String,
      apiJar: Path,
      bound: Path,
      server: ServerSocketChannel,
      life: Lifecycle,
      warm: Warm,
      permit: Semaphore,
      runs: Ref[Map[RunId, Fiber[ServerError, Unit]]],
      machine: histos.jdk.Machine,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Unit,
  ] =
    val input  = Channels.newInputStream(socket)
    val output = Channels.newOutputStream(socket)
    Frame
      .read(input)
      .mapError(_ => ServerError.Io(ServerIo.Read(bound)))
      .flatMap:
        case None          => ZIO.unit
        case Some(payload) =>
          payload.fromJson[Call].toOption match
            case None        => ZIO.unit
            case Some(asked) =>
              val say = Say.frames(output, bound)

              /** Reload, reboot, and bsp own the process. Everything else that reads the graph adopts one. */
              def adopts(target: Call): Boolean =
                Perform.needsGraph(target) || (target match
                  case Call.Reload | Call.Reboot | Call.Bsp => true
                  case _                                    => false)

              def edges(
                  out: Say
              ): Perform.Edges[
                histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore
              ] =
                Perform.Edges(
                  watch = (inner, edge) => watched(inner, edge),
                  reload = Calls.finish(out, CallResult.Reported(Report.empty)),
                  reboot = reboot(repo, bound, server, life, output),
                  bsp = bsp(
                    repo,
                    apiJar,
                    version,
                    bound,
                    jdk,
                    machine,
                    warm,
                    permit,
                    buildClasspath,
                    build,
                    input,
                    output,
                  ),
                )

              def execute(
                  target: Call,
                  out: Say,
                  loaded: Either[BuildFault, Option[LoadedBuild]],
                  chosen: histos.jdk.Machine,
              ): ZIO[
                histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
                ServerError,
                Unit,
              ] =
                Perform.run(
                  repo,
                  apiJar,
                  loaded,
                  chosen,
                  runs,
                  track = true,
                  version,
                  target,
                  out,
                  edges(out),
                )

              def run(
                  target: Call,
                  out: Say,
              ): ZIO[
                histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
                ServerError,
                Unit,
              ] =
                if adopts(target) then withGraph(target, out)
                else
                  val chosen =
                    target match
                      case Call.Resolve(_) =>
                        histos.jdk.Machine.load(repo).mapError(ServerError.Toolchain(_))
                      case _ =>
                        ZIO.succeed(machine)
                  chosen.flatMap { fresh =>
                    occupy(permit, target)(execute(target, out, Right(None), fresh))
                  }

              def watched(
                  inner: Call,
                  out: Say,
              ): ZIO[
                histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
                ServerError,
                Unit,
              ] =
                Operation.underWatch(inner) match
                  case Left(error) =>
                    Calls.finish(out, CallResult.Fault(BuildFault.Rejected(Rejection.Watch(error.message))))
                  case Right(call) =>
                    Watching.session(
                      repo,
                      out,
                      run(call, out.dropExit),
                      watchRoots(call),
                    )

              def watchRoots(call: Call): ZIO[
                histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
                ServerError,
                List[Path],
              ] =
                adopt(repo, apiJar, version, bound, warm).flatMap:
                  case Left(_) =>
                    ZIO.succeed(List(repo.resolve("histos.scala")))
                  case Right(held) =>
                    ZIO
                      .succeed(Watching.roots(repo, call, held.graph))
                      .ensuring(release(held))

              /** A graph command adopts a generation. Reload forces one. Bsp drops that lease before the session, which
                * adopts its own. A missing machine finishes as a fault and still releases the lease.
                */
              def withGraph(
                  target: Call,
                  out: Say,
              ): ZIO[
                histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
                ServerError,
                Unit,
              ] =
                val loaded =
                  target match
                    case Call.Reload =>
                      force(repo, apiJar, version, bound, warm)
                    case _ => adopt(repo, apiJar, version, bound, warm)
                loaded.flatMap:
                  case Left(error) =>
                    Calls.finish(out, CallResult.Fault(BuildFault.Load(error)))
                  case Right(held) =>
                    val have = Right(Some(LoadedBuild(held.graph, held.plugins)))
                    val ran  =
                      target match
                        case Call.Bsp | Call.Reboot | Call.Reload =>
                          execute(target, out, have, machine)
                        case _ =>
                          histos.jdk.Machine
                            .load(repo)
                            .foldZIO(
                              error => Calls.finish(out, CallResult.Fault(BuildFault.Toolchain(error))),
                              fresh => execute(target, out, have, fresh),
                            )
                    val leased =
                      target match
                        case Call.Bsp => release(held) *> ran
                        case _        => ran.ensuring(release(held))
                    occupy(permit, target)(leased)

              write(output, wire(Message.Ready), bound) *> run(asked, say)
  end handle

  /** A fingerprint change loads the build. Failure keeps the previous graph and is the call's error.
    */
  private def adopt(
      repo: Path,
      apiJar: Path,
      version: String,
      bound: Path,
      warm: Warm,
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Either[LoadError, Generation],
  ] =
    warm.reloading.withPermit {
      for
        now        <- Fingerprint.read(repo)
        generation <- warm.live.get
        held       <-
          if now == generation.fingerprint then acquire(generation).as(Right(generation))
          else loadInto(repo, apiJar, version, bound, warm, now)
      yield held
    }

  private def force(
      repo: Path,
      apiJar: Path,
      version: String,
      bound: Path,
      warm: Warm,
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Either[LoadError, Generation],
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
      now: Fingerprint,
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Either[LoadError, Generation],
  ] =
    for
      scope  <- Scope.make
      loaded <- scope
        .extend(ZIO.serviceWithZIO[histos.load.Loader](_.load(repo, apiJar)))
        .either
      result <- loaded match
        case Left(error)  => scope.close(Exit.unit).as(Left(error))
        case Right(found) =>
          for
            _    <- Record.write(repo, version, now, bound)
            uses <- Ref.make[Uses](Uses.Live(1))
            next = Generation(found.graph, found.plugins, now, scope, uses)
            previous <- warm.live.getAndSet(next)
            _        <- warm.graphs.set(found.graph)
            _        <- retire(previous)
            _        <- warm.spliced.get.flatMap(
              ZIO.foreachDiscard(_)(_.changed(previous.graph, found.graph))
            )
          yield Right(next)
    yield result

  /** The caller holds one use. A retired generation closes when the last use ends.
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

  /** Class-directory writers. Queries, preview, cache, and `Call.Task` stay out. */
  private[server] def writesOutputs(call: Call): Boolean =
    call match
      case Call.Compile(_) | Call.Test(_, _, _) | Call.Publish(_, _) | Call.Run(_, _) | Call.Assembly(_) |
          Call.Clean(_) | Call.CleanFull =>
        true
      case Call.Graph | Call.Task(_, _) | Call.Cancel(_) | Call.Toolchain | Call.Resolve(_) | Call.Workflow(_) |
          Call.Reload | Call.Reboot | Call.Version | Call.CachePath | Call.CacheList | Call.CacheClean(_) |
          Call.LauncherUpdate | Call.Watch(_) | Call.Bsp | Call.PreviewCompile(_) | Call.PreviewTest(_) |
          Call.PreviewPublish(_) | Call.PreviewClean(_) | Call.PreviewCleanFull | Call.PreviewRun(_, _) =>
        false

  private[server] def occupy[R, E](permit: Semaphore, call: Call)(
      body: ZIO[R, E, Unit]
  ): ZIO[R, E, Unit] =
    if writesOutputs(call) then permit.withPermit(body) else body

  private def reboot(
      repo: Path,
      bound: Path,
      server: ServerSocketChannel,
      life: Lifecycle,
      output: OutputStream,
  ): ZIO[Any, ServerError, Unit] =
    for
      _ <- life.armed.set(false)
      _ <- remove(repo, bound)
      _ <- life.relaunch
      _ <- write(
        output,
        wire(Message.Done(CallResult.Reported(Report.empty))),
        bound,
      )
      _ <- life.stopping.succeed(())
      _ <- ZIO.attemptBlocking(server.close()).ignore
    yield ()

  private def bsp(
      repo: Path,
      apiJar: Path,
      version: String,
      bound: Path,
      jdk: JdkRuntime,
      machine: histos.jdk.Machine,
      warm: Warm,
      permit: Semaphore,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
      input: InputStream,
      output: OutputStream,
  ): ZIO[
    histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore,
    ServerError,
    Unit,
  ] =
    val send = (line: String) => Frame.write(output, line).mapError(_ => ServerError.Io(ServerIo.Write(bound)))
    val next = Frame.read(input).mapError(_ => ServerError.Io(ServerIo.Read(bound)))
    ZIO.scoped {
      outbox(send).flatMap { box =>
        warm.live.get.zip(Runtimes.read(machine)).flatMap { (generation, runtimes) =>
          Bsp
            .open(
              repo,
              generation.graph,
              jdk,
              runtimes,
              buildClasspath,
              box.offer,
              build,
              effect => permit.withPermit(effect),
              Some(warm.graphs),
            )
            .mapError(ServerError.Resolve(_))
            .flatMap { session =>
              def adopted(payload: String) =
                adopt(repo, apiJar, version, bound, warm)
                  .flatMap {
                    case Right(gen) => ZIO.succeed(gen)
                    case Left(_)    =>
                      warm.reloading.withPermit(
                        warm.live.get.flatMap(gen => acquire(gen).as(gen))
                      )
                  }
                  .flatMap { gen =>
                    session.answer(gen.graph, payload).ensuring(release(gen))
                  }
              warm.spliced.update(_ + session) *>
                requests(next, box.offer, adopted, box.done)
                  .flatMap(code => box.offer(wire(Message.Marker(code))) *> box.close)
                  .ensuring(warm.spliced.update(_ - session))
            }
        }
      }
    }

  /** One consumer writes the socket. Producers only `offer`. */
  private[histos] final case class Outbox(
      offer: String => UIO[Unit],
      close: IO[ServerError, Unit],
      done: Fiber[ServerError, Unit],
  )

  private[histos] def outbox(send: String => IO[ServerError, Unit]): ZIO[Scope, Nothing, Outbox] =
    for
      outbound <- Queue.unbounded[String]
      fiber    <- ZStream.fromQueueWithShutdown(outbound).foreach(send).forkScoped
    yield Outbox(line => outbound.offer(line).unit, outbound.shutdown *> fiber.join, fiber)

  private final case class BspFlight(
      id: Rpc.RpcId,
      fiber: Fiber[ServerError, Unit],
      settled: Promise[Nothing, Unit],
  )

  /** Read the next frame while a request runs. Cancel interrupts that fiber. */
  private[histos] def requests[R](
      next: IO[ServerError, Option[String]],
      offer: String => UIO[Unit],
      answer: String => ZIO[R, ServerError, Bsp.Answer],
      also: Fiber[ServerError, Unit],
  ): ZIO[R & Scope, ServerError, Int] =
    val InvalidRequest   = -32600
    val InvalidParams    = -32602
    val RequestCancelled = -32800

    def read(
        flights: Ref[Map[Rpc.RpcId, BspFlight]],
        crashed: Promise[ServerError, Nothing],
    ): ZIO[R & Scope, ServerError, Int] =
      next.flatMap:
        case None          => silence(flights).as(1)
        case Some(payload) => route(flights, crashed, payload)

    def route(
        flights: Ref[Map[Rpc.RpcId, BspFlight]],
        crashed: Promise[ServerError, Nothing],
        payload: String,
    ): ZIO[R & Scope, ServerError, Int] =
      payload.fromJson[Rpc.Incoming] match
        case Left(_)         => answerNow(flights, crashed, payload)
        case Right(incoming) =>
          incoming.method match
            case Some("$/cancelRequest") =>
              cancelIncoming(flights, incoming) *> read(flights, crashed)
            case Some("build/exit") =>
              finishExit(flights, payload)
            case Some("build/shutdown") if incoming.id.nonEmpty =>
              finishShutdown(flights, payload)
            case Some("build/initialize") =>
              answerNow(flights, crashed, payload)
            case Some(_) =>
              incoming.id match
                case Some(id) => launch(flights, crashed, id, payload) *> read(flights, crashed)
                case None     => answerNow(flights, crashed, payload)
            case None =>
              answerNow(flights, crashed, payload)

    def answerNow(
        flights: Ref[Map[Rpc.RpcId, BspFlight]],
        crashed: Promise[ServerError, Nothing],
        payload: String,
    ): ZIO[R & Scope, ServerError, Int] =
      answer(payload).flatMap:
        case Bsp.Answer.Reply(text) => offer(text) *> read(flights, crashed)
        case Bsp.Answer.Quiet       => read(flights, crashed)
        case Bsp.Answer.Stop(code)  => silence(flights).as(code)

    def finishExit(
        flights: Ref[Map[Rpc.RpcId, BspFlight]],
        payload: String,
    ): ZIO[R, ServerError, Int] =
      silence(flights) *>
        answer(payload).flatMap:
          case Bsp.Answer.Stop(code) => ZIO.succeed(code)
          case _                     => ZIO.succeed(1)

    def finishShutdown(
        flights: Ref[Map[Rpc.RpcId, BspFlight]],
        payload: String,
    ): ZIO[R, ServerError, Int] =
      answer(payload).flatMap: result =>
        silence(flights) *>
          (result match
            case Bsp.Answer.Reply(text) => offer(text).as(0)
            case Bsp.Answer.Stop(code)  => ZIO.succeed(code)
            case Bsp.Answer.Quiet       => ZIO.succeed(0))

    def launch(
        flights: Ref[Map[Rpc.RpcId, BspFlight]],
        crashed: Promise[ServerError, Nothing],
        id: Rpc.RpcId,
        payload: String,
    ): ZIO[R & Scope, ServerError, Unit] =
      for
        settled <- Promise.make[Nothing, Unit]
        started <- Promise.make[Nothing, Unit]
        fiber   <- (started.await *> answer(payload).flatMap:
          case Bsp.Answer.Reply(text) => settle(settled, Some(text))
          case Bsp.Answer.Quiet       => settle(settled, None)
          case Bsp.Answer.Stop(_)     => settle(settled, None)
        ).forkScoped
        flight = BspFlight(id, fiber, settled)
        duplicate <- flights.modify { map =>
          if map.contains(id) then (true, map) else (false, map.updated(id, flight))
        }
        _ <-
          if duplicate then fiber.interrupt *> offer(Rpc.fail(id, InvalidRequest, "duplicate request"))
          else watch(flights, flight, crashed) *> started.succeed(()).unit
      yield ()

    def settle(settled: Promise[Nothing, Unit], reply: Option[String]): UIO[Unit] =
      ZIO.uninterruptible {
        settled.succeed(()).flatMap { won =>
          reply match
            case Some(text) if won => offer(text)
            case _                 => ZIO.unit
        }
      }

    def cancelIncoming(
        flights: Ref[Map[Rpc.RpcId, BspFlight]],
        incoming: Rpc.Incoming,
    ): UIO[Unit] =
      Rpc.cancelId(incoming.params) match
        case Left(fault) => offer(Rpc.fail(Rpc.idOf(incoming), InvalidParams, fault.message))
        case Right(id)   => cancel(flights, id)

    def cancel(flights: Ref[Map[Rpc.RpcId, BspFlight]], id: Rpc.RpcId): UIO[Unit] =
      flights
        .modify { map =>
          map.get(id) match
            case Some(flight) => (Some(flight), map - id)
            case None         => (None, map)
        }
        .flatMap:
          case Some(flight) =>
            ZIO.uninterruptible {
              flight.settled.succeed(()).flatMap { won =>
                ZIO.when(won)(offer(Rpc.fail(id, RequestCancelled, "cancelled"))).unit
              }
            } *> fiberInterrupt(flight)
          case None =>
            offer(Rpc.fail(id, InvalidParams, "unknown request"))

    def watch(
        flights: Ref[Map[Rpc.RpcId, BspFlight]],
        flight: BspFlight,
        crashed: Promise[ServerError, Nothing],
    ): ZIO[Scope, Nothing, Unit] =
      flight.fiber.await
        .flatMap:
          case Exit.Success(_)                                => drop(flights, flight)
          case Exit.Failure(cause) if cause.isInterruptedOnly => drop(flights, flight)
          case Exit.Failure(cause)                            => crashed.failCause(cause).unit
        .forkScoped
        .unit

    def drop(flights: Ref[Map[Rpc.RpcId, BspFlight]], flight: BspFlight): UIO[Unit] =
      flights.update { map =>
        map.get(flight.id) match
          case Some(found) if found.settled == flight.settled => map - flight.id
          case _                                              => map
      }

    def silence(flights: Ref[Map[Rpc.RpcId, BspFlight]]): UIO[Unit] =
      flights.modify(map => (map.values.toList, Map.empty)).flatMap { live =>
        ZIO.foreachDiscard(live) { flight =>
          ZIO.uninterruptible(flight.settled.succeed(()).unit) *> fiberInterrupt(flight)
        }
      }

    def fiberInterrupt(flight: BspFlight): UIO[Unit] =
      flight.fiber.interrupt.unit

    def watchFiber(
        fiber: Fiber[ServerError, Unit],
        crashed: Promise[ServerError, Nothing],
    ): ZIO[Scope, Nothing, Unit] =
      fiber.await
        .flatMap:
          case Exit.Failure(cause) if !cause.isInterruptedOnly => crashed.failCause(cause).unit
          case _                                               => ZIO.unit
        .forkScoped
        .unit

    for
      crashed <- Promise.make[ServerError, Nothing]
      flights <- Ref.make(Map.empty[Rpc.RpcId, BspFlight])
      _       <- watchFiber(also, crashed)
      code    <- read(flights, crashed).race(crashed.await).ensuring(silence(flights))
    yield code
  end requests

  private def wire(message: Message): String =
    message.toJson

  private def write(
      output: OutputStream,
      payload: String,
      socket: Path,
  ): IO[ServerError, Unit] =
    Frame
      .write(output, payload)
      .mapError(_ => ServerError.Io(ServerIo.Write(socket)))

  private def remove(repo: Path, socket: Path): UIO[Unit] =
    ZIO.attemptBlocking(Files.deleteIfExists(Record.file(repo))).ignore *>
      ZIO.attemptBlocking(Files.deleteIfExists(socket)).ignore
end Daemon
