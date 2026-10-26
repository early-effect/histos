package histos.client

import histos.cli.{Cli, Operation, Show}
import histos.report.{BuildFault, CallResult, Rejection, Report}
import histos.jdk.Machine
import histos.load.{LoadError, LoadedBuild, Loader}
import histos.plugin.PluginArgs
import histos.server.{Attach, Call, Calls, Decision, Feed, Fingerprint, Say, ServerError, Watching}
import histos.task.{RunId, TaskId}
import histos.testrun.ModuleTestError
import histos.zinc.{GraphCompile, GraphCompileError}
import histos.Platform

import java.nio.file.Path

import zio.{Chunk, Exit, Fiber, Ref, Scope, UIO, ZIO, ZLayer}

/** One MCP process. It sends [[Call]] while the server matches, and keeps a single graph otherwise.
  */
object Session:
  enum SessionError:
    case DifferentVersion
    case Remote(error: ServerError)
    case DidNotFinish

    def message: String = this match
      case DifferentVersion => ServerError.DifferentVersion.message
      case Remote(error)    => error.message
      case DidNotFinish     => "the call did not finish"
  end SessionError

  /** A tool failed. A fault is a request that never became a report. Transport never started the call. */
  enum CallProblem:
    case Fault(fault: BuildFault)
    case Transport(error: SessionError)

  final case class Held(
      fingerprint: Fingerprint,
      loaded: LoadedBuild,
      scope: Scope.Closeable,
  )

  final case class Desk(
      held: Ref[Option[Held]],
      runs: Ref[Map[RunId, Fiber[ServerError, Unit]]],
  )

  def desk: UIO[Desk] =
    for
      held <- Ref.make(Option.empty[Held])
      runs <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
    yield Desk(held, runs)

  def perform(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call,
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope,
    SessionError,
    CallResult,
  ] =
    Attach.decide(repo, Cli.VersionText).flatMap {
      case Decision.DifferentVersion =>
        drop(desk) *> ZIO.fail(SessionError.DifferentVersion)
      case Decision.Open(socket) =>
        drop(desk) *> Remote
          .hear(repo, socket, call)
          .foldZIO(
            {
              case ServerError.Unreachable(_) =>
                locally(repo, apiJar, machine, desk, call)
              case other => ZIO.fail(SessionError.Remote(other))
            },
            heard => ZIO.fromEither(spoken(repo, heard.feeds.toList)),
          )
      case Decision.Absent =>
        locally(repo, apiJar, machine, desk, call)
    }

  /** A report is the tool value, including a compile or test that ran and failed. A fault fails the tool. */
  def deliver[R](call: ZIO[R, SessionError, CallResult]): ZIO[R, CallProblem, Report] =
    call
      .mapError(CallProblem.Transport(_))
      .flatMap:
        case CallResult.Reported(report) => ZIO.succeed(report)
        case CallResult.Fault(fault)     => ZIO.fail(CallProblem.Fault(fault))

  /** `inspect`, `versions`, and a plugin task reject a platform. A main may name one.
    */
  def runCall(
      id: String,
      platform: Option[String],
      args: PluginArgs,
  ): Either[BuildFault, Call] =
    val task =
      id.contains("/") || TaskId
        .from(id)
        .toOption
        .exists(found => found == TaskId.Inspect || found == TaskId.Versions)
    if !task && args != PluginArgs.empty then Left(BuildFault.Rejected(Rejection.TaskArguments(id)))
    else
      platform match
        case Some(_) if task =>
          Left(BuildFault.Rejected(Rejection.TaskPlatform(id)))
        case Some(raw) =>
          Platform.from(raw) match
            case Left(error) => Left(BuildFault.Platform(error))
            case Right(_)    => Right(Call.Run(id, platform))
        case None if task => Right(Call.Task(id, args))
        case None         => Right(Call.Run(id, None))

  /** The terminal frame is the result. Live lines already on that report are not copied again. */
  def spoken(repo: Path, feeds: List[Feed]): Either[SessionError, CallResult] =
    val events          = Chunk.fromIterable(feeds.collect { case Feed.Event(event) => event })
    val (out, errLines) = Show.transcript(repo, events)
    feeds.reverse.collectFirst { case Feed.Done(result) => result } match
      case None                              => Left(SessionError.DidNotFinish)
      case Some(fault @ CallResult.Fault(_)) => Right(fault)
      case Some(CallResult.Reported(report)) =>
        Right(
          CallResult.Reported(
            report.copy(
              out = mergeLines(report.out, out),
              err = report.err ++ errLines.filterNot(report.err.contains),
            )
          )
        )

  private def locally(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call,
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope,
    SessionError,
    CallResult,
  ] =
    call match
      case Call.Reboot =>
        Cli
          .bootCall(repo, apiJar)
          .provideSomeLayer(ZLayer.succeed(machine))
          .mapError(SessionError.Remote(_))
      case other =>
        for
          feeds <- Ref.make(Chunk.empty[Feed])
          say = Say.record(repo, feeds)
          _      <- answer(repo, apiJar, machine, desk, other, say).mapError(SessionError.Remote(_))
          chunk  <- feeds.get
          result <- ZIO.fromEither(spoken(repo, chunk.toList))
        yield result

  private def mergeLines(have: Chunk[String], more: Chunk[String]): Chunk[String] =
    if have.isEmpty then more else have ++ more.filterNot(have.contains)

  /** Commands that do not use the adopted graph. */
  private def answer(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call,
      say: Say,
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope,
    ServerError,
    Unit,
  ] =
    call match
      case Call.Version =>
        Calls.version(Cli.VersionText, say)
      case Call.CachePath =>
        Calls.cachePath(say)
      case Call.CacheList =>
        Calls.cacheList(say)
      case Call.CacheClean(dryRun) =>
        Calls.cacheClean(dryRun, say)
      case Call.LauncherUpdate =>
        Calls.launcherUpdate(repo, say)
      case Call.Clean(Nil) =>
        Calls.wipe(repo, say)
      case Call.CleanFull =>
        Calls.cleanFull(repo, say)
      case Call.PreviewClean(Nil) =>
        Calls.previewCleanAll(repo, say)
      case Call.PreviewCleanFull =>
        Calls.previewCleanFull(repo, say)
      case Call.Resolve(action) =>
        Calls.resolve(repo, apiJar, action, say, machine.native)
      case Call.Reload =>
        ZIO.fail(ServerError.NoServer)
      case Call.Reboot =>
        Cli.bootCall(repo, apiJar).provideSomeLayer(ZLayer.succeed(machine)).flatMap(Calls.finish(say, _))
      case Call.Bsp =>
        Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.NotATool)))
      case Call.Watch(inner) =>
        watch(repo, apiJar, machine, desk, inner, say)
      case other =>
        fresh(repo, apiJar, desk).foldZIO(
          {
            case error: LoadError   => Calls.finish(say, CallResult.Fault(BuildFault.Load(error)))
            case error: ServerError => ZIO.fail(error)
          },
          loaded => dispatch(repo, apiJar, loaded, machine, desk, other, say),
        )

  private def watch(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      inner: Call,
      say: Say,
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope,
    ServerError,
    Unit,
  ] =
    Operation.underWatch(inner) match
      case Left(error) =>
        Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.Watch(error.message))))
      case Right(call) =>
        Watching.session(
          repo,
          say,
          answer(repo, apiJar, machine, desk, call, say.dropExit),
          fresh(repo, apiJar, desk).foldZIO(
            {
              case _: LoadError       => ZIO.succeed(List(repo.resolve("histos.scala")))
              case error: ServerError => ZIO.fail(error)
            },
            loaded => ZIO.succeed(Watching.roots(repo, call, loaded.graph)),
          ),
        )

  private def dispatch(
      repo: Path,
      apiJar: Path,
      loaded: LoadedBuild,
      machine: Machine,
      desk: Desk,
      call: Call,
      say: Say,
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope,
    ServerError,
    Unit,
  ] =
    val graph = loaded.graph
    call match
      case Call.Graph =>
        histos.server.Calls.graph(graph, loaded.plugins, say)
      case Call.Toolchain =>
        histos.server.Calls.toolchain(machine, graph, say)
      case Call.Cancel(id) =>
        histos.server.Calls.cancel(desk.runs, id, say)
      case Call.Compile(projects) =>
        Calls.tracked(desk.runs, say)(
          withJdk(machine, say, call)(jdk => histos.server.Calls.compile(repo, graph, projects, jdk, machine, say))
        )
      case Call.Test(projects, patterns, quick) =>
        Calls.tracked(desk.runs, say)(
          withJdk(machine, say, call)(jdk =>
            histos.server.Calls.test(repo, graph, projects, patterns, quick, jdk, machine, say)
          )
        )
      case Call.Publish(repository, projects) =>
        Calls.tracked(desk.runs, say)(
          withJdk(machine, say, call)(jdk =>
            histos.server.Calls.publish(repo, graph, machine, jdk, repository, projects, say)
          )
        )
      case Call.Assembly(destination) =>
        Calls.tracked(desk.runs, say)(
          withJdk(machine, say, call)(jdk => histos.server.Calls.assembly(repo, graph, jdk, machine, destination, say))
        )
      case Call.Run(id, platform) =>
        Calls.tracked(desk.runs, say)(
          withJdk(machine, say, call)(jdk => histos.server.Calls.run(repo, graph, jdk, machine, id, platform, say))
        )
      case Call.Task(id, args) =>
        Calls.tracked(desk.runs, say)(
          Calls.task(repo, graph, loaded.plugins, machine, id, args, say)
        )
      case Call.Resolve(action) =>
        Calls.resolve(repo, apiJar, action, say, machine.native)
      case Call.Workflow(action) =>
        Calls.workflow(repo, graph, action, say)
      case Call.Clean(projects) =>
        Calls.clean(repo, graph, projects, say)
      case Call.CleanFull =>
        Calls.cleanFull(repo, say)
      case Call.Version =>
        Calls.version(Cli.VersionText, say)
      case Call.CachePath =>
        Calls.cachePath(say)
      case Call.CacheList =>
        Calls.cacheList(say)
      case Call.CacheClean(dryRun) =>
        Calls.cacheClean(dryRun, say)
      case Call.LauncherUpdate =>
        Calls.launcherUpdate(repo, say)
      case Call.Reload =>
        ZIO.fail(ServerError.NoServer)
      case Call.Reboot =>
        Cli.bootCall(repo, apiJar).provideSomeLayer(ZLayer.succeed(machine)).flatMap(Calls.finish(say, _))
      case Call.Bsp =>
        Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.NotATool)))
      case Call.Watch(inner) =>
        watch(repo, apiJar, machine, desk, inner, say)
      case Call.PreviewCompile(projects) =>
        histos.server.Calls.previewCompile(repo, graph, projects, say)
      case Call.PreviewTest(projects) =>
        histos.server.Calls.previewTest(repo, graph, projects, say)
      case Call.PreviewPublish(projects) =>
        histos.server.Calls
          .previewPublish(repo, graph, projects, machine.env, say)
      case Call.PreviewClean(projects) =>
        histos.server.Calls.previewClean(repo, graph, projects, say)
      case Call.PreviewCleanFull =>
        histos.server.Calls.previewCleanFull(repo, say)
      case Call.PreviewRun(id, platform) =>
        histos.server.Calls.previewRun(graph, id, platform, say)

  private def withJdk(machine: Machine, say: Say, call: Call)(
      use: histos.jdk.JdkRuntime => ZIO[histos.resolve.Resolve, ServerError, Unit]
  ): ZIO[histos.resolve.Resolve, ServerError, Unit] =
    GraphCompile
      .jdk(machine)
      .foldZIO(
        error => Calls.finish(say, jdkMissed(call, error)),
        use,
      )

  /** Publish keeps the compile fault so the exit stays 9. A test lock is a resolve fault. */
  private def jdkMissed(call: Call, error: GraphCompileError): CallResult =
    call match
      case Call.Publish(_, _) | Call.PreviewPublish(_) =>
        CallResult.Fault(BuildFault.Compile(error))
      case Call.Test(_, _, _) | Call.PreviewTest(_) =>
        BuildFault.testStopped(ModuleTestError.Compile(error)) match
          case Some(why) => CallResult.Fault(why)
          case None      => CallResult.reported(error.message)
      case _ =>
        BuildFault.compileStopped(error) match
          case Some(why) => CallResult.Fault(why)
          case None      => CallResult.reported(error.message)

  private def fresh(
      repo: Path,
      apiJar: Path,
      desk: Desk,
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore,
    LoadError | ServerError,
    LoadedBuild,
  ] =
    for
      now     <- Fingerprint.read(repo)
      current <- desk.held.get
      loaded  <- current match
        case Some(held) if held.fingerprint == now => ZIO.succeed(held.loaded)
        case other                                 =>
          ZIO.foreachDiscard(other)(_.scope.close(Exit.unit)) *>
            open(repo, apiJar, now, desk)
    yield loaded

  private def open(
      repo: Path,
      apiJar: Path,
      now: Fingerprint,
      desk: Desk,
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore,
    LoadError,
    LoadedBuild,
  ] =
    for
      scope  <- Scope.make
      loaded <- scope
        .extend(ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)))
        .tapError(_ => scope.close(Exit.unit))
      _ <- desk.held.set(Some(Held(now, loaded, scope)))
    yield loaded

  private def drop(desk: Desk): UIO[Unit] =
    desk.held.get.flatMap {
      case None       => ZIO.unit
      case Some(held) =>
        held.scope.close(Exit.unit) *> desk.held.set(None)
    }
end Session
