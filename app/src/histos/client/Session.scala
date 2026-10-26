package histos.client

import histos.build.{Beat, Build}
import histos.cli.{Cli, Operation, Show}
import histos.jdk.Machine
import histos.load.{LoadError, LoadedBuild, Loader}
import histos.mcp.Notice
import histos.plugin.PluginArgs
import histos.report.{BuildFault, CallResult, Rejection, Report}
import histos.server.{Attach, Call, Calls, Decision, Feed, Fingerprint, Perform, Say, ServerError, Watching}
import histos.task.{RunId, TaskId}
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

  private type Env = Loader & histos.resolve.Resolve & histos.cache.ContentStore & Scope

  def perform(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call,
  ): ZIO[Env, SessionError, CallResult] =
    Attach.decide(repo, Cli.VersionText).flatMap {
      case Decision.DifferentVersion =>
        drop(desk) *> ZIO.fail(SessionError.DifferentVersion)
      case Decision.Open(socket) =>
        drop(desk) *>
          Build
            .attached(socket)
            .perform(call)
            .tap {
              case Beat.Heard(event) => Notice.show(repo, event)
              case Beat.Finished(_)  => ZIO.unit
            }
            .runCollect
            .foldZIO(
              {
                case ServerError.Unreachable(_) =>
                  locally(repo, apiJar, machine, desk, call)
                case other => ZIO.fail(SessionError.Remote(other))
              },
              beats => ZIO.fromEither(spoken(repo, beats.map(Build.feed).toList)),
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
            case Left(error)  => Left(BuildFault.Platform(error))
            case Right(found) => Right(Call.Run(id, Some(found)))
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
  ): ZIO[Env, SessionError, CallResult] =
    val build =
      Build.here[Env](
        repo,
        apiJar,
        machine,
        desk.runs,
        track = true,
        Cli.VersionText,
        prepared(repo, apiJar, desk, _),
        sessionEdges(repo, apiJar, machine, desk),
        {
          case Beat.Heard(event) => Notice.show(repo, event)
          case Beat.Finished(_)  => ZIO.unit
        },
      )
    build.perform(call).runCollect.mapError(SessionError.Remote(_)).flatMap { beats =>
      ZIO.fromEither(spoken(repo, beats.map(Build.feed).toList))
    }

  private def mergeLines(have: Chunk[String], more: Chunk[String]): Chunk[String] =
    if have.isEmpty then more else have ++ more.filterNot(have.contains)

  /** One call on the say the caller already owns. Watch iterations pass [[Say.dropExit]]. */
  private def answer(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call,
      say: Say,
  ): ZIO[Env, ServerError, Unit] =
    prepared(repo, apiJar, desk, call).flatMap { loaded =>
      Perform.run(
        repo,
        apiJar,
        loaded,
        machine,
        desk.runs,
        track = true,
        Cli.VersionText,
        call,
        say,
        sessionEdges(repo, apiJar, machine, desk)(say),
      )
    }

  private def prepared(
      repo: Path,
      apiJar: Path,
      desk: Desk,
      call: Call,
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore,
    ServerError,
    Either[BuildFault, Option[LoadedBuild]],
  ] =
    if !Perform.needsGraph(call) then ZIO.succeed(Right(None))
    else
      fresh(repo, apiJar, desk).foldZIO(
        {
          case error: LoadError   => ZIO.succeed(Left(BuildFault.Load(error)))
          case error: ServerError => ZIO.fail(error)
        },
        loaded => ZIO.succeed(Right(Some(loaded))),
      )

  private def sessionEdges(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
  ): Say => Perform.Edges[Env] =
    say =>
      Perform.Edges(
        watch = (inner, edge) => watch(repo, apiJar, machine, desk, inner, edge),
        reload = ZIO.fail(ServerError.NoServer),
        reboot = Cli.bootCall(repo, apiJar).provideSomeLayer(ZLayer.succeed(machine)).flatMap(Calls.finish(say, _)),
        bsp = Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.NotATool))),
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
