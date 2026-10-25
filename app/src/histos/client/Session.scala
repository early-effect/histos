package histos.client

import histos.jdk.JdkRuntime
import histos.cli.{Cli, CliExit, Operation}
import histos.report.{Observation, Report}
import histos.jdk.Machine
import histos.load.{LoadedBuild, Loader}
import histos.plugin.PluginArgs
import histos.server.{
  Attach,
  Call,
  Calls,
  Decision,
  Feed,
  Fingerprint,
  Message,
  Say,
  ServerError,
  Watching
}
import histos.task.{RunId, TaskId}
import histos.testrun.ModuleTestError
import histos.zinc.{GraphCompile, GraphCompileError}
import histos.Platform

import java.nio.file.Path

import zio.json.*
import zio.{Chunk, Exit, Fiber, Ref, Scope, UIO, ZIO, ZLayer}

/** One MCP process. It sends [[Call]] while the server matches, and keeps a
  * single graph otherwise.
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

  final case class Held(
      fingerprint: Fingerprint,
      loaded: LoadedBuild,
      scope: Scope.Closeable
  )

  final case class Desk(
      held: Ref[Option[Held]],
      runs: Ref[Map[RunId, Fiber[ServerError, Unit]]]
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
      call: Call
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope,
    SessionError,
    Report
  ] =
    Attach.decide(repo, Cli.VersionText).flatMap {
      case Decision.DifferentVersion =>
        drop(desk) *> ZIO.fail(SessionError.DifferentVersion)
      case Decision.Open(socket) =>
        drop(desk) *> Remote
          .hear(socket, call)
          .foldZIO(
            {
              case ServerError.Unreachable(_) =>
                locally(repo, apiJar, machine, desk, call)
              case other => ZIO.fail(SessionError.Remote(other))
            },
            heard =>
              ZIO.fromEither(
                spoken(heard.feeds.toList, heard.run, heard.observation)
              )
          )
      case Decision.Absent =>
        locally(repo, apiJar, machine, desk, call)
    }

  /** Exit 0 is the tool value. A non-zero exit is that report's JSON text. */
  def deliver[R](call: ZIO[R, SessionError, Report]): ZIO[R, String, Report] =
    call.mapError(_.message).flatMap { report =>
      if report.exit == 0 then ZIO.succeed(report)
      else ZIO.fail(report.toJson)
    }

  /** `inspect`, `versions`, and a plugin task reject a platform. A main may
    * name one.
    */
  def runCall(
      id: String,
      platform: Option[String],
      args: PluginArgs
  ): Either[String, Call] =
    val task =
      id.contains("/") || TaskId
        .from(id)
        .toOption
        .exists(found => found == TaskId.Inspect || found == TaskId.Versions)
    if !task && args != PluginArgs.empty then
      Left(s"run $id does not take arguments")
    else
      platform match
        case Some(_) if task =>
          Left(histos.cli.CliParseError.TaskPlatform(id).message)
        case Some(raw) =>
          Platform.from(raw) match
            case Left(error) => Left(error.message)
            case Right(_)    => Right(Call.Run(id, platform))
        case None if task => Right(Call.Task(id, args))
        case None         => Right(Call.Run(id, None))

  def spoken(
      feeds: List[Feed],
      run: Option[String],
      observation: Option[Observation]
  ): Either[SessionError, Report] =
    val out = Chunk.fromIterable(feeds.collect { case Feed.Out(text) => text })
    val errLines = feeds.collect { case Feed.Err(text) => text }
    feeds.reverse.collectFirst { case Feed.Exit(code, message) =>
      (code, message)
    } match
      case None                  => Left(SessionError.DidNotFinish)
      case Some((code, message)) =>
        val err =
          if message.isEmpty || errLines.contains(message) then
            Chunk.fromIterable(errLines)
          else Chunk.fromIterable(errLines) :+ message
        Right(Report(code, out, err, run, observation))

  private def locally(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope,
    SessionError,
    Report
  ] =
    call match
      case Call.Reboot =>
        Cli
          .reboot(repo, apiJar)
          .provideSomeLayer(ZLayer.succeed(machine))
          .map: done =>
            Report(done.exit.code, lines(done.out), lines(done.err), None, None)
      case other =>
        for
          feeds <- Ref.make(Chunk.empty[Feed])
          say = Say.record(feeds)
          _ <- answer(repo, apiJar, machine, desk, other, say).mapError(
            SessionError.Remote(_)
          )
          spokenFeeds <- feeds.get
          run <- say.run
          observation <- say.observation
          report <- ZIO.fromEither(
            spoken(spokenFeeds.toList, run, observation)
          )
        yield report

  private def lines(text: String): Chunk[String] =
    if text.isEmpty then Chunk.empty
    else Chunk.fromArray(text.split("\n", -1))

  /** Commands that do not use the adopted graph. */
  private def answer(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call,
      say: Say
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope,
    ServerError,
    Unit
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
        say(Message.Exit(CliExit.LoadFailed.code, ServerError.NoServer.message))
      case Call.Reboot =>
        Cli
          .reboot(repo, apiJar)
          .provideSomeLayer(ZLayer.succeed(machine))
          .flatMap(done => say(Message.Exit(done.exit.code, done.err)))
      case Call.Bsp =>
        say(Message.Exit(CliExit.Usage.code, "not a tool"))
      case Call.Watch(inner) =>
        watch(repo, apiJar, machine, desk, inner, say)
      case other =>
        fresh(repo, apiJar, desk).either.flatMap:
          case Left(message) =>
            say(Message.Exit(CliExit.LoadFailed.code, message))
          case Right(loaded) =>
            dispatch(repo, apiJar, loaded, machine, desk, other, say)

  private def watch(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      inner: Call,
      say: Say
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope,
    ServerError,
    Unit
  ] =
    Operation.underWatch(inner) match
      case Left(error) =>
        Calls.finish(say, CliExit.Usage.code, error.message)
      case Right(call) =>
        Watching.session(
          repo,
          say,
          answer(repo, apiJar, machine, desk, call, say.dropExit),
          fresh(repo, apiJar, desk).either.map:
            case Left(_)       => List(repo.resolve("histos.scala"))
            case Right(loaded) => Watching.roots(repo, call, loaded.graph)
        )

  private def dispatch(
      repo: Path,
      apiJar: Path,
      loaded: LoadedBuild,
      machine: Machine,
      desk: Desk,
      call: Call,
      say: Say
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope,
    ServerError,
    Unit
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
          withJdk(machine, say, CliExit.CompileFailed)(jdk =>
            histos.server.Calls
              .compile(repo, graph, projects, jdk, machine, say)
          )
        )
      case Call.Test(projects, patterns, quick) =>
        Calls.tracked(desk.runs, say)(
          GraphCompile
            .jdk(machine)
            .foldZIO(
              error =>
                val (code, message) =
                  Cli.testFailure(ModuleTestError.Compile(error))
                say(histos.server.Message.Exit(code, message))
              ,
              jdk =>
                histos.server.Calls.test(
                  repo,
                  graph,
                  projects,
                  patterns,
                  quick,
                  jdk,
                  machine,
                  say
                )
            )
        )
      case Call.Publish(repository, projects) =>
        Calls.tracked(desk.runs, say)(
          withJdk(machine, say, CliExit.PublishFailed)(jdk =>
            histos.server.Calls
              .publish(repo, graph, machine, jdk, repository, projects, say)
          )
        )
      case Call.Assembly(destination) =>
        Calls.tracked(desk.runs, say)(
          withJdk(machine, say, CliExit.CompileFailed)(jdk =>
            histos.server.Calls
              .assembly(repo, graph, jdk, machine, destination, say)
          )
        )
      case Call.Run(id, platform) =>
        Calls.tracked(desk.runs, say)(
          GraphCompile
            .jdk(machine)
            .foldZIO(
              error =>
                say(histos.server.Message.Exit(jdkCode(error), error.message)),
              jdk =>
                histos.server.Calls
                  .run(repo, graph, jdk, machine, id, platform, say)
            )
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
        say(Message.Exit(CliExit.LoadFailed.code, ServerError.NoServer.message))
      case Call.Reboot =>
        Cli
          .reboot(repo, apiJar)
          .provideSomeLayer(ZLayer.succeed(machine))
          .flatMap(done => say(Message.Exit(done.exit.code, done.err)))
      case Call.Bsp =>
        say(Message.Exit(CliExit.Usage.code, "not a tool"))
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

  private def withJdk(machine: Machine, say: Say, exit: CliExit)(
      use: JdkRuntime => ZIO[histos.resolve.Resolve, ServerError, Unit]
  ): ZIO[histos.resolve.Resolve, ServerError, Unit] =
    GraphCompile
      .jdk(machine)
      .foldZIO(
        error => say(histos.server.Message.Exit(exit.code, error.message)),
        use
      )

  private def jdkCode(error: GraphCompileError): Int =
    error match
      case GraphCompileError.Lock(_) | GraphCompileError.LockRead(_) =>
        CliExit.ResolveFailed.code
      case _ => CliExit.CompileFailed.code

  private def fresh(
      repo: Path,
      apiJar: Path,
      desk: Desk
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore,
    String,
    LoadedBuild
  ] =
    for
      now <- Fingerprint.read(repo).mapError(_.message)
      current <- desk.held.get
      loaded <- current match
        case Some(held) if held.fingerprint == now => ZIO.succeed(held.loaded)
        case other                                 =>
          ZIO.foreachDiscard(other)(_.scope.close(Exit.unit)) *>
            open(repo, apiJar, now, desk)
    yield loaded

  private def open(
      repo: Path,
      apiJar: Path,
      now: Fingerprint,
      desk: Desk
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore,
    String,
    LoadedBuild
  ] =
    for
      scope <- Scope.make
      loaded <- scope
        .extend(ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)))
        .mapError(_.message)
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
