package histos.client

import histos.ApiClock
import histos.build.{Beat, Build, BuildEvent}
import histos.cli.{Cli, Operation, Show}
import histos.jdk.Machine
import histos.load.{LoadError, LoadedBuild, Loader}
import histos.mcp.Notice
import histos.plugin.PluginArgs
import histos.report.{BuildFault, CallResult, Rejection, Report}
import histos.publish.LauncherError
import histos.server.{Attach, Call, Calls, Decision, Feed, Fingerprint, Message, Perform, Say, ServerError, Watching}
import histos.task.{RunId, TaskId}
import histos.Platform

import java.nio.file.Path

import zio.{Chunk, Exit, Fiber, Ref, Scope, UIO, ZIO}

/** One MCP process. It sends [[histos.server.Call]] while the server matches, and keeps a single graph otherwise.
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

  private type Env = Loader & histos.resolve.Resolve & histos.cache.ContentStore & histos.publish.IvyRoot & Scope

  def perform(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call,
  ): ZIO[Env, SessionError, CallResult] =
    Cli
      .versionText(repo)
      .foldZIO(
        {
          case absent: LauncherError.Absent =>
            call match
              case Call.Version => ZIO.succeed(CallResult.Fault(BuildFault.Launcher(absent)))
              case _            => locally(repo, apiJar, machine, desk, call, "")
          case error =>
            ZIO.succeed(CallResult.Fault(BuildFault.Launcher(error)))
        },
        text =>
          ApiClock.admit(repo, call).flatMap {
            case ApiClock.Admit.Stop(fault) => ZIO.succeed(CallResult.Fault(fault))
            case admitted                   =>
              val warned = admitted match
                case ApiClock.Admit.Warn(line) => ZIO.succeed(java.lang.System.err.println(line))
                case _                         => ZIO.unit
              warned *> ApiClock.drifted(repo).flatMap { stale =>
                val hold = stale && ApiClock.skipsReboot(call)
                Attach.decide(repo, text).flatMap {
                  case Decision.DifferentVersion(socket) if hold =>
                    drop(desk) *>
                      ask(repo, apiJar, machine, desk, call, text, socket).flatMap(withProcess(repo, call, _))
                  case Decision.DifferentVersion(socket) =>
                    drop(desk) *>
                      ApiClock.rebootLine(repo).flatMap { line =>
                        ZIO.succeed(java.lang.System.err.println(line)) *>
                          Attach.replace(repo, text, socket).flatMap {
                            case Some(fresh) =>
                              ask(repo, apiJar, machine, desk, call, text, fresh).flatMap(withProcess(repo, call, _))
                            case None =>
                              ApiClock.restart(repo, socket).flatMap(error => ZIO.fail(SessionError.Remote(error)))
                          }
                      }
                  case Decision.Open(socket) =>
                    drop(desk) *> ask(repo, apiJar, machine, desk, call, text, socket)
                      .flatMap(withProcess(repo, call, _))
                  case Decision.Absent =>
                    locally(repo, apiJar, machine, desk, call, text).flatMap(withProcess(repo, call, _))
                }
              }
          },
      )

  /** Version names this MCP process. Reboot returns the line and the socket instead of an empty tool result. */
  private def withProcess(repo: Path, call: Call, result: CallResult): UIO[CallResult] =
    call match
      case Call.Version =>
        ApiClock.mcpLine(repo).map(line => note(result, line, front = false))
      case Call.Reboot =>
        ApiClock.rebootLine(repo).map(line => note(result, line, front = true))
      case _ => ZIO.succeed(result)

  private def note(result: CallResult, line: String, front: Boolean): CallResult =
    result match
      case CallResult.Reported(report) =>
        val out = if front then line +: report.out else report.out :+ line
        CallResult.Reported(report.copy(out = out))
      case other => other

  private def ask(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call,
      shown: String,
      socket: Path,
  ): ZIO[Env, SessionError, CallResult] =
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
            locally(repo, apiJar, machine, desk, call, shown)
          case other => ZIO.fail(SessionError.Remote(other))
        },
        beats => ZIO.fromEither(spoken(repo, beats.map(Build.feed).toList)),
      )

  /** A report is the tool value, including a compile or test that ran and failed. A fault fails the tool. */
  def deliver[R](call: ZIO[R, SessionError, CallResult]): ZIO[R, CallProblem, Report] =
    call
      .mapError(CallProblem.Transport(_))
      .flatMap:
        case CallResult.Reported(report) => ZIO.succeed(report)
        case CallResult.Fault(fault)     => ZIO.fail(CallProblem.Fault(fault))

  /** `inspect`, `versions`, `doc`, and a plugin task reject a platform. A main may name one. An empty id names nothing,
    * as `run ""` on the command line.
    */
  def runCall(
      id: String,
      platform: Option[String],
      args: PluginArgs,
  ): Either[BuildFault, Call] =
    TaskId
      .from(id)
      .left
      .map(_ => BuildFault.Rejected(Rejection.MissingRunName))
      .flatMap: named =>
        val task = id.contains("/") || named.builtIn
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
      shown: String,
  ): ZIO[Env, SessionError, CallResult] =
    val build =
      Build.here[Env](
        repo,
        apiJar,
        machine,
        desk.runs,
        track = true,
        shown,
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
    Cli
      .versionText(repo)
      .foldZIO(
        {
          case absent: LauncherError.Absent if call == Call.Version =>
            say(Message.Done(CallResult.Fault(BuildFault.Launcher(absent))))
          case _: LauncherError.Absent =>
            answered(repo, apiJar, machine, desk, call, say, "")
          case error =>
            say(Message.Done(CallResult.Fault(BuildFault.Launcher(error))))
        },
        text => answered(repo, apiJar, machine, desk, call, say, text),
      )

  private def answered(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      call: Call,
      say: Say,
      shown: String,
  ): ZIO[Env, ServerError, Unit] =
    prepared(repo, apiJar, desk, call).flatMap { loaded =>
      val mode = loaded match
        case Right(Some(have)) => have.graph.resolution
        case _                 => histos.Resolution.Solve
      histos.resolve.ResolutionService
        .open(repo, mode)
        .mapError(ServerError.Resolve(_))
        .flatMap { report =>
          histos.zinc.CompileMemo.provide(
            histos.zinc.Compilers.provide(histos.schedule.Parallelism.default.cpu)(
              Perform
                .run(
                  repo,
                  apiJar,
                  loaded,
                  machine,
                  desk.runs,
                  track = true,
                  shown,
                  call,
                  say,
                  sessionEdges(repo, apiJar, machine, desk)(say),
                )
                .provideSomeLayer(zio.ZLayer.succeed(report) ++ histos.format.Formatters.live)
            )
          )
        }
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
        watch = (inner, policy, edge) => watch(repo, apiJar, machine, desk, inner, policy, edge),
        reload = ZIO.fail(ServerError.NoServer),
        reboot = Cli.bootCall(repo, histos.server.Daemon.spawn).flatMap { socket =>
          say(Message.Event(BuildEvent.Out(socket.toString))) *> Calls.finish(say, CallResult.reported())
        },
        bsp = Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.NotATool))),
        compiled = ZIO.unit,
      )

  private def watch(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Desk,
      inner: Call,
      policy: histos.server.WatchPolicy,
      say: Say,
  ): ZIO[Env, ServerError, Unit] =
    Operation.underWatch(inner, policy) match
      case Left(error) =>
        Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.Watch(error.message))))
      case Right(call) =>
        val watched = fresh(repo, apiJar, desk).foldZIO(
          {
            case _: LoadError       => ZIO.succeed(Watching.Watched(List(repo.resolve("histos.scala")), Set.empty))
            case error: ServerError => ZIO.fail(error)
          },
          loaded => ZIO.succeed(Watching.watched(repo, call, loaded.graph)),
        )
        policy match
          case histos.server.WatchPolicy.Restart =>
            Watching.beside(
              say,
              answer(repo, apiJar, machine, desk, call, say),
              watched,
              changed =>
                val compile = Watching.compileFor(call)
                Ref
                  .make(Option.empty[CallResult])
                  .flatMap: finished =>
                    answer(repo, apiJar, machine, desk, compile, say.dropExitInto(finished)) *>
                      finished.get.zip(watched).map((found, roots) => Watching.restarts(found, changed, roots)),
            )
          case histos.server.WatchPolicy.After =>
            Watching.session(
              repo,
              say,
              answer(repo, apiJar, machine, desk, call, say.dropExit),
              watched,
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
    desk.held.get.flatMap:
      case Some(held) =>
        Fingerprint
          .read(repo, held.loaded.graph.resolution)
          .flatMap: now =>
            if held.fingerprint == now then ZIO.succeed(held.loaded)
            else held.scope.close(Exit.unit) *> open(repo, apiJar, desk)
      case None =>
        open(repo, apiJar, desk)

  private def open(
      repo: Path,
      apiJar: Path,
      desk: Desk,
  ): ZIO[
    Loader & histos.resolve.Resolve & histos.cache.ContentStore,
    LoadError | ServerError,
    LoadedBuild,
  ] =
    for
      scope  <- Scope.make
      loaded <- scope
        .extend(ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)))
        .tapError(_ => scope.close(Exit.unit))
      now <- Fingerprint.read(repo, loaded.graph.resolution)
      _   <- desk.held.set(Some(Held(now, loaded, scope)))
    yield loaded

  private def drop(desk: Desk): UIO[Unit] =
    desk.held.get.flatMap {
      case None       => ZIO.unit
      case Some(held) =>
        held.scope.close(Exit.unit) *> desk.held.set(None)
    }
end Session
