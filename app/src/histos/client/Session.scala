package histos.client

import histos.jdk.JdkRuntime
import histos.cli.{Cli, CliExit}
import histos.jdk.Machine
import histos.load.{LoadedBuild, Loader}
import histos.server.{
  Attach,
  Call,
  Client,
  Decision,
  Feed,
  Fingerprint,
  Say,
  ServerError
}
import histos.task.TaskId
import histos.testrun.ModuleTestError
import histos.zinc.{GraphCompile, GraphCompileError}
import histos.Platform

import java.nio.file.Path

import zio.{Chunk, Exit, Fiber, Ref, Scope, UIO, ZIO, ZLayer}

/** One MCP process. It sends [[Call]] while the server matches, and keeps a
  * single graph otherwise.
  */
object Session:
  final case class Held(
      fingerprint: Fingerprint,
      loaded: LoadedBuild,
      scope: Scope.Closeable
  )

  final case class Desk(
      held: Ref[Option[Held]],
      runs: Ref[Map[String, Fiber[ServerError, Unit]]]
  )

  def desk: UIO[Desk] =
    for
      held <- Ref.make(Option.empty[Held])
      runs <- Ref.make(Map.empty[String, Fiber[ServerError, Unit]])
    yield Desk(held, runs)

  def perform(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore,
    String,
    String
  ] =
    Attach.decide(repo, Cli.VersionText).flatMap {
      case Decision.DifferentVersion =>
        drop(desk) *> ZIO.fail(ServerError.DifferentVersion.message)
      case Decision.Open(socket) =>
        drop(desk) *> Client
          .talk(socket, call)
          .runCollect
          .foldZIO(
            {
              case ServerError.Unreachable(_) =>
                locally(repo, apiJar, machine, desk, call)
              case other => ZIO.fail(other.message)
            },
            feeds => ZIO.fromEither(spoken(feeds.toList))
          )
      case Decision.Absent =>
        locally(repo, apiJar, machine, desk, call)
    }

  /** `inspect`, `versions`, and a plugin task reject a platform. A main may
    * name one.
    */
  def runCall(id: String, platform: Option[String]): Either[String, Call] =
    val task =
      id.contains("/") || TaskId
        .from(id)
        .toOption
        .exists(found => found == TaskId.Inspect || found == TaskId.Versions)
    platform match
      case Some(_) if task =>
        Left(histos.cli.CliParseError.TaskPlatform(id).message)
      case Some(raw) =>
        Platform.from(raw) match
          case Left(error) => Left(error.message)
          case Right(_)    => Right(Call.Run(id, platform))
      case None if task => Right(Call.Task(id))
      case None         => Right(Call.Run(id, None))

  def spoken(feeds: List[Feed]): Either[String, String] =
    val out = feeds.collect { case Feed.Out(text) => text }
    val err = feeds.collect { case Feed.Err(text) => text }
    feeds.reverse.collectFirst { case Feed.Exit(code, message) =>
      (code, message)
    } match
      case None               => Left("the call did not finish")
      case Some((0, _))       => Right(out.mkString("\n"))
      case Some((_, message)) =>
        val shown = (out ++ err).mkString("\n")
        Left(
          if shown.isEmpty then message
          else if message.isEmpty || shown == message then shown
          else s"$shown\n$message"
        )

  private def locally(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore,
    String,
    String
  ] =
    call match
      case Call.Clean(Nil) =>
        finishLocal(say => histos.server.Calls.wipe(repo, say))
      case Call.CleanFull =>
        finishLocal(say => histos.server.Calls.cleanFull(repo, say))
      case _ =>
        for
          loaded <- fresh(repo, apiJar, desk)
          feeds <- Ref.make(Chunk.empty[Feed])
          _ <- dispatch(repo, loaded, machine, desk, call, Say.record(feeds))
            .mapError(_.message)
          spokenFeeds <- feeds.get
          text <- ZIO.fromEither(spoken(spokenFeeds.toList))
        yield text

  /** No graph load. A broken `histos.scala` cannot block the wipe. */
  private def finishLocal(
      run: Say => ZIO[histos.cache.ContentStore, ServerError, Unit]
  ): ZIO[histos.cache.ContentStore, String, String] =
    for
      feeds <- Ref.make(Chunk.empty[Feed])
      _ <- run(Say.record(feeds)).mapError(_.message)
      spokenFeeds <- feeds.get
      text <- ZIO.fromEither(spoken(spokenFeeds.toList))
    yield text

  private def dispatch(
      repo: Path,
      loaded: LoadedBuild,
      machine: Machine,
      desk: Desk,
      call: Call,
      say: Say
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore,
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
        withJdk(machine, say, CliExit.CompileFailed)(jdk =>
          histos.server.Calls.compile(repo, graph, projects, jdk, machine, say)
        )
      case Call.Test(projects, patterns, quick) =>
        GraphCompile
          .jdk(machine)
          .foldZIO(
            error =>
              val (code, message) =
                Cli.testFailure(ModuleTestError.Compile(error))
              say(histos.server.Message.Exit(code, message))
            ,
            jdk =>
              histos.server.Calls
                .test(repo, graph, projects, patterns, quick, jdk, machine, say)
          )
      case Call.Publish(repository, projects) =>
        withJdk(machine, say, CliExit.PublishFailed)(jdk =>
          histos.server.Calls
            .publish(repo, graph, machine, jdk, repository, projects, say)
        )
      case Call.Assembly(destination) =>
        withJdk(machine, say, CliExit.CompileFailed)(jdk =>
          histos.server.Calls
            .assembly(repo, graph, jdk, machine, destination, say)
        )
      case Call.Run(id, platform) =>
        GraphCompile
          .jdk(machine)
          .foldZIO(
            error =>
              say(histos.server.Message.Exit(jdkCode(error), error.message)),
            jdk =>
              histos.server.Calls.tracked(desk.runs, id, say)(
                histos.server.Calls
                  .run(repo, graph, jdk, machine, id, platform, say)
              )
          )
      case Call.Task(id) =>
        histos.server.Calls.tracked(desk.runs, id, say)(
          histos.server.Calls
            .task(repo, graph, loaded.plugins, machine, id, say)
        )
      case Call.Resolve(action) =>
        histos.server.Calls.resolve(repo, graph, action, say, machine.native)
      case Call.Workflow(action) =>
        histos.server.Calls.workflow(repo, graph, action, say)
      case Call.Clean(projects) =>
        histos.server.Calls.clean(repo, graph, projects, say)
      case Call.CleanFull =>
        histos.server.Calls.cleanFull(repo, say)
      case Call.Reload | Call.Reboot | Call.Bsp =>
        say(histos.server.Message.Exit(CliExit.Usage.code, "not a tool"))

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
