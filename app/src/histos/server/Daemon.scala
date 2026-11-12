package histos.server

import histos.ApiClock
import histos.jdk.JdkRuntime
import histos.bsp.{Bsp, Frame, Rpc}
import histos.cli.Operation
import histos.jdk.Sha256Digest
import histos.load.{LoadError, LoadedBuild}
import histos.plugin.{FlagName, NamedText, ParamName, PluginArgs}
import histos.publish.IvyRoot
import histos.report.{BuildFault, CallResult, Rejection, Report}
import histos.cache.CacheCleanMode
import histos.schedule.CacheNamespace
import histos.task.RunId
import histos.testrun.{SuitePattern, TestMode}
import histos.toolchain.Runtimes
import histos.workflow.WorkflowAction
import histos.{Platform, ProjectGraph, ProjectName, Resolution}

import java.io.{InputStream, OutputStream}
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, ServerSocketChannel, SocketChannel}
import java.nio.file.{Files, Path, StandardCopyOption}

import histos.build.BuildEvent
import zio.json.*
import histos.report.WireJson.given JsonCodec[CallResult]
import histos.report.WireJson.given JsonCodec[BuildEvent]
import histos.report.WireJson.given JsonCodec[Platform]
import histos.report.WireJson.given JsonCodec[ProjectName]
import zio.stream.ZStream
import zio.{Chunk, Exit, Fiber, IO, Promise, Queue, Ref, Scope, Semaphore, UIO, ZIO, ZLayer, durationInt}

/** What a batch command found in `.histos/server.json`. */
enum Decision:
  case Absent

  /** The process at `socket` was started with another driver. The command reboots it. */
  case DifferentVersion(socket: Path)
  case Open(socket: Path)
end Decision

opaque type Fingerprint = String

object Fingerprint:
  given CanEqual[Fingerprint, Fingerprint] = CanEqual.derived

  /** Lock hashes the pin. Solve does not, so a resolution report write does not reload the server. */
  def read(repo: Path, resolution: Resolution): IO[ServerError, Fingerprint] =
    val build = repo.resolve("histos.scala")
    val pin   = resolution match
      case Resolution.Lock  => tagged(repo.resolve("dependencies.lock"))
      case Resolution.Solve => ZIO.succeed(Chunk.empty[Byte])
    val wiring = repo.resolve("histos.wiring.scala")
    for
      built <- tagged(build)
      wired <- tagged(wiring)
      lock  <- pin
    yield Sha256Digest.of(built ++ Chunk.single(0.toByte) ++ wired ++ lock).value

  private[server] def trusted(raw: String): Fingerprint = raw

  /** The files [[read]] hashes. An unnamed directory is not a root. */
  def roots(repo: Path, resolution: Resolution): UIO[List[Path]] =
    val build  = repo.resolve("histos.scala")
    val wiring = repo.resolve("histos.wiring.scala")
    val lock   = repo.resolve("dependencies.lock")
    ZIO
      .attemptBlocking {
        val wired  = if Files.isRegularFile(wiring) then List(wiring) else Nil
        val pinned = resolution match
          case Resolution.Lock  => List(lock)
          case Resolution.Solve => Nil
        (build :: wired ++ pinned).distinct
      }
      .orElseSucceed:
        resolution match
          case Resolution.Lock  => List(build, lock)
          case Resolution.Solve => List(build)

  extension (fingerprint: Fingerprint) def value: String = fingerprint

  private def tagged(path: Path): IO[ServerError, Chunk[Byte]] =
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(path) then Chunk.single(1.toByte) ++ Chunk.fromArray(Files.readAllBytes(path))
        else Chunk.single(0.toByte)
      }
      .mapError(_ => ServerError.Io(ServerIo.Read(path)))
end Fingerprint

object Record:
  final case class Written(
      version: String,
      fingerprint: Fingerprint,
      socket: Path,
      assembly: Option[String],
  )

  def file(repo: Path): Path =
    repo.resolve(".histos").resolve("server.json")

  def write(
      repo: Path,
      version: String,
      fingerprint: Fingerprint,
      socket: Path,
      assembly: Option[String],
  ): IO[ServerError, Unit] =
    val dest = file(repo)
    val tmp  = dest.resolveSibling("server.json.tmp")
    val json = ServerFile.render(
      ServerFile(
        version,
        fingerprint.value,
        socket.toAbsolutePath.normalize.toString,
        assembly,
      )
    )
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
    ServerFile.parse(text).map { stored =>
      Written(
        stored.version,
        Fingerprint.trusted(stored.fingerprint),
        Path.of(stored.socket),
        stored.assembly,
      )
    }
end Record

object Attach:
  /** Missing, corrupt, or unreadable is [[Decision.Absent]]. The command then runs in-process. A checksum the server
    * did not record is [[Decision.DifferentVersion]], including a record that predates the field.
    */
  def decide(repo: Path, version: String): UIO[Decision] =
    AssemblyMark.read(repo).flatMap { mark =>
      Record.read(repo).flatMap {
        case None                                                                 => ZIO.succeed(Decision.Absent)
        case Some(record) if record.version != version || record.assembly != mark =>
          ZIO.succeed(Decision.DifferentVersion(record.socket))
        case Some(record) =>
          ZIO.succeed(Decision.Open(record.socket))
      }
    }

  /** Poll until the recorded driver matches. The wait is the same as a cold start. */
  def await(repo: Path, version: String): IO[ServerError, Path] =
    def poll(left: Int): IO[ServerError, Path] =
      decide(repo, version).flatMap {
        case Decision.Open(socket) => ZIO.succeed(socket)
        case _ if left <= 0        => SocketPath.at(repo).flatMap(socket => ZIO.fail(ServerError.Unreachable(socket)))
        case _                     => ZIO.sleep(50.millis) *> poll(left - 1)
      }
    poll(1200)

  /** Ask `socket` to reboot. The replacement's socket when that process records this driver. */
  def replace(repo: Path, version: String, socket: Path): UIO[Option[Path]] =
    Client
      .talk(socket, Call.Reboot)
      .runCollect
      .foldZIO(
        _ => ZIO.succeed(None),
        feeds =>
          val rebooted = feeds.exists {
            case Feed.Done(CallResult.Reported(_)) => true
            case _                                 => false
          }
          if rebooted then await(repo, version).map(Some(_)).orElseSucceed(None)
          else ZIO.succeed(None),
      )
end Attach

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
  /** What a server needs from its caller. */
  type Needs = histos.resolve.Resolve & histos.load.Loader & histos.cache.ContentStore & IvyRoot

  /** What a connection runs with: the caller's services and the compilers the server opened. */
  private type Served = Needs & histos.zinc.Compilers & histos.zinc.CompileMemo & histos.resolve.ResolutionService

  /** `relaunch` starts the successor a reboot hands over to, and `assembly` is the checksum the record carries. */
  def open(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      jdk: JdkRuntime,
      version: String,
      apiJar: Path,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
      machine: histos.jdk.Machine,
      relaunch: Path => IO[ServerError, Unit],
      assembly: Option[String],
  ): ZIO[Scope & Needs, ServerError, Running] =
    for
      listening <- Promise.make[ServerError, Record.Written]
      serving   <- run(
        repo,
        loaded.graph,
        loaded.build,
        jdk,
        version,
        apiJar,
        buildClasspath,
        build,
        loaded.plugins,
        machine,
        relaunch(repo),
        listening,
        assembly,
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

  /** Cold start and a reboot successor. The child must not inherit the caller's stdin. */
  private[histos] def launch(repo: Path): ProcessBuilder =
    val log = repo.resolve(".histos").resolve("server.log")
    new ProcessBuilder("./histos", "server")
      .directory(repo.toFile)
      .redirectInput(ProcessBuilder.Redirect.from(Path.of("/dev/null").toFile))
      .redirectErrorStream(true)
      .redirectOutput(ProcessBuilder.Redirect.appendTo(log.toFile))

  private[histos] def spawn(repo: Path): IO[ServerError, Unit] =
    val log = repo.resolve(".histos").resolve("server.log")
    ZIO
      .attemptBlocking {
        Option(log.getParent).foreach(dir => Files.createDirectories(dir))
        if !Files.exists(log) then Files.createFile(log)
        launch(repo).start()
        ()
      }
      .mapError(_ => ServerError.Io(ServerIo.Bind(log)))

  private def run(
      repo: Path,
      graph: ProjectGraph,
      histosBuild: histos.HistosBuild,
      jdk: JdkRuntime,
      version: String,
      apiJar: Path,
      buildClasspath: List[Path],
      build: IO[histos.load.LoadError, histos.load.BuildNotes],
      plugins: histos.load.PluginRuntime,
      machine: histos.jdk.Machine,
      relaunch: IO[ServerError, Unit],
      listening: Promise[ServerError, Record.Written],
      assembly: Option[String],
  ): ZIO[Needs, ServerError, Unit] =
    bindAt(repo).flatMap: (socket, channel) =>
      ZIO.scoped {
        for
          _     <- ZIO.addFinalizer(ZIO.attempt(channel.close()).ignore)
          armed <- Ref.make(true)
          _     <- ZIO.addFinalizer(
            ZIO.whenZIO(armed.get)(remove(repo, socket)).unit
          )
          stopping <- Promise.make[Nothing, Unit]
          life = Lifecycle(armed, stopping, relaunch)
          fingerprint <- Fingerprint.read(repo, graph.resolution)
          _           <- Record.write(repo, version, fingerprint, socket, assembly)
          held        <- Scope.make
          uses        <- Ref.make[Uses](Uses.Live(0))
          graphs      <- Ref.make(graph)
          live        <- Ref.make(
            Generation(graph, plugins, histosBuild, fingerprint, held, uses)
          )
          reloading <- Semaphore.make(1)
          spliced   <- Ref.make(Set.empty[Bsp.Session])
          report    <- histos.resolve.ResolutionService
            .open(repo, graph)
            .mapError(ServerError.Resolve(_))
          formatters <- histos.format.Formatters.make
          warm = Warm(live, graphs, reloading, spliced, report, assembly, formatters)
          permit <- Semaphore.make(1)
          runs   <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
          _      <- listening
            .succeed(
              Record.Written(
                version,
                fingerprint,
                socket.toAbsolutePath.normalize,
                assembly,
              )
            )
            .unit
          _ <- histos.zinc.CompileMemo.provide(
            histos.zinc.Compilers.provide(histos.schedule.Parallelism.default.cpu)(
              accept(
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
              ).provideSomeLayer(ZLayer.succeed(report))
            )
          )
        yield ()
      }

  /** This checkout's socket path, bound. */
  private def bindAt(repo: Path): IO[ServerError, (Path, ServerSocketChannel)] =
    SocketPath.at(repo).flatMap(socket => bind(socket).map(channel => (socket, channel)))

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
      build: histos.HistosBuild,
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
      report: histos.resolve.ResolutionService,
      assembly: Option[String],
      formatters: histos.format.Formatters,
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
  ): ZIO[Scope & Served, ServerError, Unit] =
    def loop: ZIO[Scope & Served, ServerError, Unit] =
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
    Served,
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

              /** Reload and bsp own the process. Reboot replaces it and does not read the graph. */
              def adopts(target: Call): Boolean =
                Perform.needsGraph(target) || (target match
                  case Call.Reload | Call.Bsp => true
                  case _                      => false)

              def edges(
                  out: Say,
                  compiled: UIO[Unit],
              ): Perform.Edges[
                Served
              ] =
                Perform.Edges(
                  watch = (inner, policy, edge) => watched(inner, policy, edge),
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
                  compiled = compiled,
                )

              def execute(
                  target: Call,
                  out: Say,
                  loaded: Either[BuildFault, Option[LoadedBuild]],
                  chosen: histos.jdk.Machine,
                  compiled: UIO[Unit],
              ): ZIO[
                Served,
                ServerError,
                Unit,
              ] =
                Perform
                  .run(
                    repo,
                    apiJar,
                    loaded,
                    chosen,
                    runs,
                    track = true,
                    version,
                    target,
                    out,
                    edges(out, compiled),
                  )
                  .provideSomeEnvironment[Served](_.add(warm.formatters))

              def run(
                  target: Call,
                  out: Say,
              ): ZIO[
                Served,
                ServerError,
                Unit,
              ] =
                ApiClock.admit(repo, target).flatMap {
                  case ApiClock.Admit.Stop(fault) =>
                    Calls.finish(out, CallResult.Fault(fault))
                  case _ =>
                    if adopts(target) then withGraph(target, out)
                    else
                      val chosen =
                        target match
                          case Call.Resolve(_) =>
                            histos.jdk.Machine.load(repo).mapError(ServerError.Toolchain(_))
                          case _ =>
                            ZIO.succeed(machine)
                      chosen.flatMap { fresh =>
                        occupy(permit, target)(execute(target, out, Right(None), fresh, ZIO.unit))
                      }
                }

              def watched(
                  inner: Call,
                  policy: WatchPolicy,
                  out: Say,
              ): ZIO[
                Served,
                ServerError,
                Unit,
              ] =
                Operation.underWatch(inner, policy) match
                  case Left(error) =>
                    Calls.finish(out, CallResult.Fault(BuildFault.Rejected(Rejection.Watch(error.message))))
                  case Right(call) =>
                    policy match
                      case WatchPolicy.Restart =>
                        Watching.beside(
                          out,
                          run(call, out),
                          watchRoots(call),
                          changed => replace(call, changed, out),
                        )
                      case WatchPolicy.After =>
                        Watching.session(
                          repo,
                          out,
                          run(call, out.dropExit),
                          watchRoots(call),
                        )

              /** Compile before the fork is touched. A failed compile keeps it. A successful compile replaces it only
                * when the change is one of the program's roots.
                */
              def replace(
                  call: Call,
                  changed: List[Path],
                  out: Say,
              ): ZIO[
                Served,
                ServerError,
                Boolean,
              ] =
                val compile = Watching.compileFor(call)
                Ref
                  .make(Option.empty[CallResult])
                  .flatMap: finished =>
                    run(compile, out.dropExitInto(finished)) *>
                      finished.get
                        .zip(watchRoots(call))
                        .map((result, watched) => Watching.restarts(result, changed, watched))

              def watchRoots(call: Call): ZIO[
                Served,
                ServerError,
                Watching.Watched,
              ] =
                adopt(repo, apiJar, version, bound, warm).flatMap:
                  case Left(_) =>
                    ZIO.succeed(Watching.Watched(List(repo.resolve("histos.scala")), Set.empty))
                  case Right(held) =>
                    ZIO
                      .succeed(Watching.watched(repo, call, held.graph))
                      .ensuring(release(held))

              /** A graph command adopts a generation. Reload forces one. Bsp drops that lease before the session, which
                * adopts its own. A missing machine finishes as a fault and still releases the lease.
                */
              def withGraph(
                  target: Call,
                  out: Say,
              ): ZIO[
                Served,
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
                    val have                     = Right(Some(LoadedBuild(held.graph, held.plugins, held.build)))
                    def ran(compiled: UIO[Unit]) =
                      target match
                        case Call.Bsp | Call.Reload =>
                          execute(target, out, have, machine, compiled)
                        case _ =>
                          histos.jdk.Machine
                            .load(repo)
                            .foldZIO(
                              error => Calls.finish(out, CallResult.Fault(BuildFault.Toolchain(error))),
                              fresh => execute(target, out, have, fresh, compiled),
                            )
                    def leased(compiled: UIO[Unit]) =
                      target match
                        case Call.Bsp => release(held) *> ran(compiled)
                        case _        => ran(compiled).ensuring(release(held))
                    lease(permit, target)(leased)

              write(output, wire(Message.Ready), bound) *> attended(asked, socket, input)(run(asked, say))
  end handle

  /** A client that closes its socket ends its call, so a run it left does not hold the permit or the reload lock for
    * the next one. A call never reads its socket after the call frame, so a read that ends is the client leaving. Bsp
    * reads the socket itself, and reboot starts its successor after the client heard the reply.
    */
  private def attended[R](call: Call, socket: SocketChannel, input: InputStream)(
      body: ZIO[R, ServerError, Unit]
  ): ZIO[R, ServerError, Unit] =
    call match
      case Call.Bsp | Call.Reboot => body
      case _                      => body.raceFirst(gone(socket, input))

  /** Completes when the peer closes. A stray byte is not a close, so the read goes on. */
  private def gone(socket: SocketChannel, input: InputStream): UIO[Unit] =
    ZIO
      .attemptBlockingCancelable(input.read())(ZIO.attempt(socket.shutdownInput()).ignore)
      .foldZIO(
        _ => ZIO.unit,
        read => if read < 0 then ZIO.unit else gone(socket, input),
      )

  /** A fingerprint change loads the build. Failure keeps the previous graph and is the call's error.
    */
  private def adopt(
      repo: Path,
      apiJar: Path,
      version: String,
      bound: Path,
      warm: Warm,
  ): ZIO[
    Served,
    ServerError,
    Either[LoadError, Generation],
  ] =
    warm.reloading.withPermit {
      for
        generation <- warm.live.get
        now        <- Fingerprint.read(repo, generation.graph.resolution)
        held       <-
          if now == generation.fingerprint then acquire(generation).as(Right(generation))
          else loadInto(repo, apiJar, version, bound, warm)
      yield held
    }

  private def force(
      repo: Path,
      apiJar: Path,
      version: String,
      bound: Path,
      warm: Warm,
  ): ZIO[
    Served,
    ServerError,
    Either[LoadError, Generation],
  ] =
    warm.reloading.withPermit(loadInto(repo, apiJar, version, bound, warm))

  /** The new generation's fingerprint is read after the load, under the graph's own resolution. */
  private def loadInto(
      repo: Path,
      apiJar: Path,
      version: String,
      bound: Path,
      warm: Warm,
  ): ZIO[
    Served,
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
            _     <- warm.report.adopt(found.graph).mapError(ServerError.Resolve(_))
            fresh <- Fingerprint.read(repo, found.graph.resolution)
            _     <- Record.write(repo, version, fresh, bound, warm.assembly)
            uses  <- Ref.make[Uses](Uses.Live(1))
            next = Generation(found.graph, found.plugins, found.build, fresh, scope, uses)
            previous <- warm.live.getAndSet(next)
            _        <- warm.graphs.set(found.graph)
            _        <- Calls.release
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

  /** Class-directory writers, and `fmt`, which rewrites the sources a compile reads. Queries, preview, cache, and
    * `Call.Task` stay out.
    */
  private[server] def writesOutputs(call: Call): Boolean =
    call match
      case Call.Compile(_) | Call.Test(_, _, _) | Call.PublishLocal(_) | Call.PublishSnapshots(_) |
          Call.PublishRelease(_, _) | Call.Run(_, _) | Call.Client | Call.Assembly(_) | Call.Clean(_) | Call.CleanFull |
          Call.Format(_, _) =>
        true
      case Call.PublishPromote(_) | Call.PublishDrop(_) | Call.PublishAssets(_) | Call.Graph | Call.Task(_, _) |
          Call.Cancel(_) | Call.Toolchain | Call.Resolve(_) | Call.Workflow(_) | Call.Reload | Call.Reboot |
          Call.Version | Call.CachePath | Call.CacheList | Call.CacheClean(_) | Call.CachePromote(_, _) |
          Call.Launcher(_) | Call.Watch(_, _) | Call.Bsp | Call.PreviewCompile(_) | Call.PreviewTest(_) |
          Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) | Call.PreviewClean(_) | Call.PreviewCleanFull |
          Call.PreviewRun(_, _) =>
        false

  private[server] def occupy[R, E](permit: Semaphore, call: Call)(
      body: ZIO[R, E, Unit]
  ): ZIO[R, E, Unit] =
    if writesOutputs(call) then permit.withPermit(body) else body

  /** A program run holds the class-directory lock only until `body` signals its compile is done, so a restart can
    * compile while the program runs. A JavaScript or Native link that starts after the signal reads class directories a
    * restart may be rewriting, and only the program about to be replaced sees that. Every other call is [[occupy]]. The
    * lock is free once `body` ends, however it ends.
    */
  private[server] def lease[R, E](permit: Semaphore, call: Call)(
      body: UIO[Unit] => ZIO[R, E, Unit]
  ): ZIO[R, E, Unit] =
    call match
      case Call.Run(_, _) =>
        for
          held     <- Promise.make[Nothing, Unit]
          compiled <- Promise.make[Nothing, Unit]
          holder   <- permit.withPermit(held.succeed(()) *> compiled.await).fork
          _        <- held.await
          _        <- body(compiled.succeed(()).unit).ensuring(compiled.succeed(()) *> holder.join)
        yield ()
      case _ => occupy(permit, call)(body(ZIO.unit))

  private def reboot(
      repo: Path,
      bound: Path,
      server: ServerSocketChannel,
      life: Lifecycle,
      output: OutputStream,
  ): ZIO[Any, ServerError, Unit] =
    for
      _ <- write(
        output,
        wire(Message.Done(CallResult.Reported(Report.empty))),
        bound,
      )
      _ <- life.armed.set(false)
      _ <- remove(repo, bound)
      _ <- life.relaunch
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
    Served,
    ServerError,
    Unit,
  ] =
    val send = (line: String) => Frame.write(output, line).mapError(_ => ServerError.Io(ServerIo.Write(bound)))
    val next = Frame.read(input).mapError(_ => ServerError.Io(ServerIo.Read(bound)))
    ZIO.scoped {
      outbox(send).flatMap { box =>
        warm.live.get
          .flatMap(generation =>
            Runtimes.read(machine, generation.graph.runnerTools.nodePath).map(runtimes => (generation, runtimes))
          )
          .flatMap { (generation, runtimes) =>
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
                  session.follow *>
                  requests(next, box.offer, adopted, box.done)
                    .flatMap(code => box.offer(wire(Message.Marker(code))) *> box.close)
                    .ensuring(warm.spliced.update(_ - session))
              }
          }
      }
    }

  /** One consumer writes the socket. Producers only `offer`. `close` sends every line offered before it, then returns.
    */
  private[histos] final case class Outbox(
      offer: String => UIO[Unit],
      close: IO[ServerError, Unit],
      done: Fiber[ServerError, Unit],
  )

  /** `None` ends the stream after the lines queued ahead of it. A shut-down queue would drop them. */
  private[histos] def outbox(send: String => IO[ServerError, Unit]): ZIO[Scope, Nothing, Outbox] =
    for
      outbound <- ZIO.acquireRelease(Queue.unbounded[Option[String]])(_.shutdown)
      fiber    <- ZStream.fromQueue(outbound).collectWhileSome.foreach(send).forkScoped
    yield Outbox(line => outbound.offer(Some(line)).unit, outbound.offer(None) *> fiber.join, fiber)

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
