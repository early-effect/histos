package histos.cli

import heddle.LinePipe
import histos.bsp.{Bsp, Connection, Frame}
import histos.central.PortalServer
import histos.build.{Beat, Build}
import histos.load.LoadedBuild
import histos.server.{
  Attach,
  Call,
  Calls,
  Client,
  Daemon,
  Decision,
  Feed,
  Message,
  Perform,
  ResolveAction,
  Say,
  ServerError,
  ServerIo,
  Watching,
}
import histos.load.BuildClasspath
import histos.cache.{CacheCleanMode, ContentStore}
import histos.schedule.{CacheNamespace, NamespaceError, RemoteError}
import histos.jdk.{Machine, Toolchain}
import histos.load.{LoadError, Loader}
import histos.plugin.{FlagNameError, PluginArgs, PluginTaskError, PluginTaskId}
import histos.preview.{Plan, PreviewError}
import histos.program.ProgramError
import histos.report.{BuildFault, CallResult, Rejection, Report, Severity}
import histos.select.SelectError
import histos.toolchain.{Runtimes, Survey}
import histos.mcp.HistosMcp
import histos.resolve.{Resolve, ResolveError}
import histos.task.{RunId, TaskError, TaskId}
import histos.workflow.WorkflowAction
import histos.testrun.{ModuleTestError, SuitePattern, SuitePatternError, TestMode, TestStatus}
import histos.workspace.IoFailure
import histos.zinc.{GraphCompile, GraphCompileError}
import histos.{Platform, PlatformError, ProjectName, ProjectNameError}

import java.nio.file.{Files, Path}

import zio.*
import zio.json.*

enum Command:
  case Version
  case Graph
  case Run(id: TaskId, platform: Option[Platform])
  case Plugin(id: PluginTaskId, args: PluginArgs)
  case Test(projects: List[ProjectName])
  case TestOnly(modules: List[ProjectName], patterns: List[SuitePattern])
  case TestQuick(modules: List[ProjectName], patterns: List[SuitePattern])
  case Watch(command: Command, args: List[String])
  case Compile(projects: List[ProjectName])
  case Clean(projects: List[ProjectName])
  case CleanFull
  case Assembly(destination: Option[String])
  case PublishLocal(tokens: List[String])
  case PublishSnapshots(tokens: List[String])
  case PublishRelease(automatic: Boolean, tokens: List[String])
  case PublishPromote(id: String)
  case PublishDrop(id: String)
  case PublishAssets(tokens: List[String])
  case Mcp
  case Bsp
  case Server
  case Portal
  case Toolchain
  case CachePath
  case CacheList
  case CacheClean(mode: CacheCleanMode)
  case CachePromote(from: CacheNamespace, to: CacheNamespace)
  case Resolve(action: ResolveAction)
  case Workflow(action: WorkflowAction)
  case LauncherUpdate
  case Reload
  case Reboot
  case Cancel(id: RunId)
  case PreviewCompile(projects: List[ProjectName])
  case PreviewTest(projects: List[ProjectName])
  case PreviewPublishLocal(tokens: List[String])
  case PreviewPublishSnapshots(tokens: List[String])
  case PreviewClean(projects: List[ProjectName])
  case PreviewCleanFull
  case PreviewRun(id: TaskId, platform: Option[Platform])
  case PreviewPlugin(id: PluginTaskId)
  case Observe

enum CliParseError:
  case Help
  case UnknownCommand
  case UnknownRoot
  case MissingArgument
  case UnexpectedArguments
  case BadTaskId
  case BadPluginTask(error: PluginTaskError)
  case BadPlatform(error: PlatformError)
  case TaskPlatform(name: String)
  case MissingPattern
  case Pattern(detail: String)
  case ProjectName(error: ProjectNameError)
  case SuitePattern(error: SuitePatternError)
  case RunId(error: TaskError)
  case Flag(error: FlagNameError)
  case Namespace(error: NamespaceError)
  case WatchCommand(detail: String)
  case PublishForm
  case Rejected

  def message: String = this match
    case Help | UnknownCommand | UnknownRoot | Rejected => Cli.usageText
    case MissingArgument                                => "run needs one name"
    case UnexpectedArguments                            => Cli.usageText
    case BadTaskId                                      => "a task id is not empty"
    case BadPluginTask(error)                           => error.message
    case BadPlatform(error)                             => error.message
    case TaskPlatform(name)                             => s"run $name does not take a platform"
    case MissingPattern                                 => "testOnly needs a pattern"
    case Pattern(detail)                                => detail
    case ProjectName(error)                             => error.message
    case SuitePattern(error)                            => error.message
    case RunId(error)                                   => error.message
    case Flag(error)                                    => error.message
    case Namespace(error)                               => error.message
    case WatchCommand(detail)                           => detail
    case PublishForm                                    =>
      "publish is snapshots, release, assets, promote, or drop"
end CliParseError

enum CliExit:
  case Success, TaskFailed, Usage, LoadFailed, ToolchainFailed, ResolveFailed,
    WorkflowFailed, TestFailed, CompileFailed, PublishFailed
  case Program(status: Int)
  case Interrupted

  def code: Int = this match
    case Success         => 0
    case TaskFailed      => 1
    case Program(status) => status
    case Interrupted     => 130
    case Usage           => 2
    case LoadFailed      => 3
    case ToolchainFailed => 4
    case ResolveFailed   => 5
    case WorkflowFailed  => 6
    case TestFailed      => 7
    case CompileFailed   => 8
    case PublishFailed   => 9
end CliExit

object CliExit:
  def fromCode(code: Int): CliExit =
    code match
      case 0     => Success
      case 1     => TaskFailed
      case 2     => Usage
      case 3     => LoadFailed
      case 4     => ToolchainFailed
      case 5     => ResolveFailed
      case 6     => WorkflowFailed
      case 7     => TestFailed
      case 8     => CompileFailed
      case 9     => PublishFailed
      case 130   => Interrupted
      case other => Program(other)

  /** Histos's own process status. The report does not store it. */
  def from(call: Call, result: CallResult): CliExit =
    result match
      case CallResult.Reported(report) => fromReport(call, report)
      case CallResult.Fault(fault)     => fromFault(call, fault)

  private def fromReport(call: Call, report: Report): CliExit =
    report.program match
      case Some(code) if code != 0 => fromCode(code)
      case _                       =>
        if report.tests.exists(failedTest) then TestFailed
        else if failedFacts(report) then failedCall(call)
        else Success

  private def failedTest(cell: histos.testrun.TestedCell): Boolean =
    cell.status match
      case TestStatus.Failed(_)                                          => true
      case TestStatus.NoTests | TestStatus.Unmatched | TestStatus.Passed => false

  private def failedFacts(report: Report): Boolean =
    report.err.nonEmpty || report.diagnostics.exists(_.severity == Severity.Error)

  /** A report that already failed. Publish is 9, test is 7, and every other call is 8. */
  private def failedCall(call: Call): CliExit =
    call match
      case Call.Test(_, _, _) | Call.PreviewTest(_) => TestFailed
      case Call.PublishLocal(_) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PublishPromote(_) |
          Call.PublishDrop(_) | Call.PublishAssets(_) | Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) =>
        PublishFailed
      case _ => CompileFailed

  private def fromFault(call: Call, fault: BuildFault): CliExit =
    fault match
      case BuildFault.Load(_)                            => LoadFailed
      case BuildFault.Resolve(_)                         => resolveExit(call)
      case BuildFault.Toolchain(_)                       => ToolchainFailed
      case BuildFault.Survey(_)                          => ToolchainFailed
      case BuildFault.Select(_)                          => selectExit(call)
      case BuildFault.Pattern(_)                         => Usage
      case BuildFault.Compile(error)                     => compileExit(call, error)
      case BuildFault.Test(error)                        => testFaultExit(error)
      case BuildFault.Publish(_) | BuildFault.Release(_) => PublishFailed
      case BuildFault.Workflow(_)                        => WorkflowFailed
      case BuildFault.Program(error)                     => programFaultExit(error)
      case BuildFault.Platform(_)                        => TaskFailed
      case BuildFault.Task(_)                            => TaskFailed
      case BuildFault.Plugin(_)                          => TaskFailed
      case BuildFault.PluginTask(_)                      => TaskFailed
      case BuildFault.Preview(_)                         => previewExit(call)
      case BuildFault.Assemble(_)                        => CompileFailed
      case BuildFault.Launcher(_)                        => ResolveFailed
      case BuildFault.Cache(_) | BuildFault.Remote(_)    => LoadFailed
      case BuildFault.Store(_)                           => LoadFailed
      case BuildFault.Io(_)                              => ioExit(call)
      case BuildFault.Version(_)                         => TaskFailed
      case BuildFault.Watch(_)                           => TaskFailed
      case BuildFault.Interrupted                        => Interrupted
      case BuildFault.Rejected(rejection)                => rejectedExit(rejection)

  /** No lock is 8 for compile and assembly, 9 for publish, and 5 for test, run, and resolve. */
  private def resolveExit(call: Call): CliExit =
    call match
      case Call.Compile(_) | Call.PreviewCompile(_) | Call.Assembly(_) => CompileFailed
      case Call.PublishLocal(_) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PublishPromote(_) |
          Call.PublishDrop(_) | Call.PublishAssets(_) | Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) =>
        PublishFailed
      case _ => ResolveFailed

  private def selectExit(call: Call): CliExit =
    call match
      case Call.Compile(_) | Call.Assembly(_) | Call.PreviewCompile(_) => CompileFailed
      case Call.Test(_, _, _) | Call.PreviewTest(_)                    => TestFailed
      case Call.PublishLocal(_) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PublishPromote(_) |
          Call.PublishDrop(_) | Call.PublishAssets(_) | Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) =>
        PublishFailed
      case _ => TaskFailed

  private def compileExit(call: Call, error: GraphCompileError): CliExit =
    error match
      case GraphCompileError.Lock(_) | GraphCompileError.LockRead(_) => resolveExit(call)
      case _                                                         =>
        call match
          case Call.Test(_, _, _) | Call.PreviewTest(_) => TestFailed
          case Call.PublishLocal(_) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PublishPromote(_) |
              Call.PublishDrop(_) | Call.PublishAssets(_) | Call.PreviewPublishLocal(_) |
              Call.PreviewPublishSnapshots(_) =>
            PublishFailed
          case _ => CompileFailed

  private def testFaultExit(error: ModuleTestError): CliExit =
    error match
      case ModuleTestError.Pattern(_)                                                         => Usage
      case ModuleTestError.Lock(_) | ModuleTestError.LockRead(_)                              => ResolveFailed
      case ModuleTestError.Compile(GraphCompileError.Lock(_) | GraphCompileError.LockRead(_)) =>
        ResolveFailed
      case _ => TestFailed

  private def programFaultExit(error: ProgramError): CliExit =
    error match
      case ProgramError.Compile(GraphCompileError.Lock(_) | GraphCompileError.LockRead(_)) => ResolveFailed
      case ProgramError.Lock(_) | ProgramError.LockRead(_)                                 => ResolveFailed
      case ProgramError.Compile(_)                                                         => CompileFailed
      case _                                                                               => TaskFailed

  private def previewExit(call: Call): CliExit =
    call match
      case Call.PreviewCompile(_) | Call.Compile(_) => CompileFailed
      case Call.PreviewTest(_) | Call.Test(_, _, _) => TestFailed
      case Call.PublishLocal(_) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PublishPromote(_) |
          Call.PublishDrop(_) | Call.PublishAssets(_) | Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) =>
        PublishFailed
      case _ => TaskFailed

  /** A missing lock file read on test or run is 5. Cache is 3. Clean is 1. */
  private def ioExit(call: Call): CliExit =
    call match
      case Call.Test(_, _, _) | Call.Run(_, _)                         => ResolveFailed
      case Call.Compile(_) | Call.Assembly(_) | Call.PreviewCompile(_) => CompileFailed
      case Call.PublishLocal(_) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PublishPromote(_) |
          Call.PublishDrop(_) | Call.PublishAssets(_) | Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) =>
        PublishFailed
      case Call.CachePath | Call.CacheList | Call.CacheClean(_) | Call.CachePromote(_, _) => LoadFailed
      case _                                                                              => TaskFailed

  private def rejectedExit(rejection: Rejection): CliExit =
    rejection match
      case Rejection.AssemblyDestination(_)                                          => CompileFailed
      case Rejection.TaskArguments(_) | Rejection.RunFailed | Rejection.WatchStopped => TaskFailed
      case _                                                                         => Usage
end CliExit

/** Heard text is already on the console. Held text is printed by finish. */
enum Playback:
  case Heard
  case Held

final case class Completed(
    exit: CliExit,
    out: String,
    err: String,
    playback: Playback,
)

object Cli:
  val VersionText: String = "histos 0.0.2"

  val usageText: String =
    "usage: histos (version | graph | run <name> [jvm|js|native] | test [project ...] | testOnly [--module project ...] <pattern> ... | testQuick [--module project ...] [pattern ...] | ~ <command> | compile [project ...] | clean [project ...] | cleanFull | assembly [path] | publishLocal [token ...] | publish snapshots [token ...] | publish release [token ...] [--automatic] | publish assets [token ...] | publish promote <deployment> | publish drop <deployment> | observe | mcp | bsp | server | portal | reload | reboot | cancel <id> | toolchain | cache path | cache list | cache clean [--dry-run] | cache promote --from <namespace> --to <namespace> | resolve [--update | --check] | workflow [--check | --update] | launcher --update | preview compile [project ...] | preview test [project ...] | preview publishLocal [token ...] | preview publish snapshots [token ...] | preview clean [project ...] | preview cleanFull | preview run <name> [jvm|js|native])"

  def parse(args: List[String]): IO[CliParseError, Command] =
    Grammar.parse(args)

  def execute(
      args: List[String],
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    Connection.ensure(repo).ignore *>
      parse(args).foldZIO(
        {
          case CliParseError.UnknownRoot => contributed(args, repo, apiJar)
          case detail                    =>
            ZIO.succeed(
              Completed(CliExit.Usage, "", detail.message, Playback.Held)
            )
        },
        command => perform(repo, apiJar, command),
      )

  private def perform(
      repo: Path,
      apiJar: Path,
      command: Command,
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    command match
      case Command.Version =>
        attach(
          repo,
          Call.Version,
          ZIO.succeed(held(Call.Version, VersionText, CallResult.reported())),
        )
      case Command.Graph                  => graph(repo, apiJar)
      case Command.Run(id, platform)      => run(repo, apiJar, id, platform)
      case Command.Plugin(id, pluginArgs) =>
        plugin(repo, apiJar, id, pluginArgs)
      case Command.Test(projects) =>
        test(repo, apiJar, projects, Nil, TestMode.Run)
      case Command.TestOnly(modules, patterns) =>
        test(repo, apiJar, modules, patterns, TestMode.Run)
      case Command.TestQuick(modules, patterns) =>
        test(repo, apiJar, modules, patterns, TestMode.Quick)
      case Command.Watch(inner, _)       => watch(repo, apiJar, inner)
      case Command.Compile(projects)     => compile(repo, apiJar, projects)
      case Command.Clean(projects)       => clean(repo, apiJar, projects)
      case Command.CleanFull             => cleanFull(repo)
      case Command.Assembly(destination) => assembly(repo, apiJar, destination)
      case Command.PublishLocal(tokens)  =>
        publishLocal(repo, apiJar, tokens)
      case Command.PublishSnapshots(tokens) =>
        publishSnapshots(repo, apiJar, tokens)
      case Command.PublishRelease(automatic, tokens) =>
        publishRelease(repo, apiJar, automatic, tokens)
      case Command.PublishPromote(id) =>
        publishPromote(repo, apiJar, id)
      case Command.PublishDrop(id) =>
        publishDrop(repo, apiJar, id)
      case Command.PublishAssets(tokens) =>
        publishAssets(repo, apiJar, tokens)
      case Command.Mcp              => mcp(repo, apiJar)
      case Command.Bsp              => bsp(repo, apiJar)
      case Command.Server           => serve(repo, apiJar)
      case Command.Portal           => portal(repo)
      case Command.Toolchain        => toolchain(repo, apiJar)
      case Command.CachePath        => attach(repo, Call.CachePath, cachePath)
      case Command.CacheList        => attach(repo, Call.CacheList, cacheList)
      case Command.CacheClean(mode) =>
        attach(repo, Call.CacheClean(mode), cacheClean(mode))
      case Command.CachePromote(from, to) =>
        attach(repo, Call.CachePromote(from, to), cachePromote(from, to))
      case Command.Resolve(action)  => resolve(repo, apiJar, action)
      case Command.Workflow(action) => workflow(repo, apiJar, action)
      case Command.LauncherUpdate   =>
        attach(repo, Call.LauncherUpdate, launcherLocal(repo))
      case Command.Reload                   => reload(repo)
      case Command.Reboot                   => reboot(repo, apiJar)
      case Command.Cancel(id)               => cancel(repo, id)
      case Command.PreviewCompile(projects) =>
        preview(repo, apiJar, projects)
      case Command.PreviewTest(projects) =>
        previewTest(repo, apiJar, projects)
      case Command.PreviewPublishLocal(tokens) =>
        previewPublishLocal(repo, apiJar, tokens)
      case Command.PreviewPublishSnapshots(tokens) =>
        previewPublishSnapshots(repo, apiJar, tokens)
      case Command.PreviewClean(projects) =>
        previewClean(repo, apiJar, projects)
      case Command.PreviewCleanFull =>
        previewCleanFull(repo)
      case Command.PreviewRun(id, platform) =>
        previewRun(repo, apiJar, id.value, platform)
      case Command.PreviewPlugin(id) =>
        previewRun(repo, apiJar, id.render, None)
      case Command.Observe => observe(repo)

  /** Loopback rehearsal of the Central portal. Does not load the graph. */
  private def portal(repo: Path): URIO[Machine & Scope, Completed] =
    ZIO.serviceWithZIO[Machine] { machine =>
      PortalServer.credentials(machine.env) match
        case Left(error) =>
          ZIO.succeed(Completed(CliExit.PublishFailed, "", error.message, Playback.Held))
        case Right(secret) =>
          PortalServer
            .serve(repo.resolve(".histos").resolve("portal"), secret)
            .foldZIO(
              error => ZIO.succeed(Completed(CliExit.PublishFailed, "", error.message, Playback.Held)),
              bound => say(bound.url) *> ZIO.never.as(Completed(CliExit.Success, bound.url, "", Playback.Heard)),
            )
    }

  /** Read the stored tally. Does not load the graph. */
  private def observe(repo: Path): UIO[Completed] =
    histos.report.Stored
      .read(repo)
      .foldZIO(
        error =>
          Console
            .printLineError(error.message)
            .ignore
            .as(
              Completed(CliExit.TaskFailed, "", error.message, Playback.Heard)
            ),
        found =>
          Console
            .printLine(found.toJson)
            .ignore
            .as(
              Completed(CliExit.Success, found.toJson, "", Playback.Heard)
            ),
      )

  /** One `Call.Watch`. The server, or this process when no server is up, runs that call again after a source save.
    * `mcp`, `bsp`, `server`, `portal`, `observe`, and a nested watch are usage and do not start a transport.
    */
  private def watch(
      repo: Path,
      apiJar: Path,
      inner: Command,
  ): URIO[Local, Completed] =
    Operation.operation(inner) match
      case Left(error) =>
        ZIO.succeed(Completed(CliExit.Usage, "", error.message, Playback.Held))
      case Right(call) =>
        attach(repo, Call.Watch(call), watchLocal(repo, apiJar, call))

  private def watchLocal(
      repo: Path,
      apiJar: Path,
      inner: Call,
  ): URIO[Local, Completed] =
    Ref.make(Completed(CliExit.Success, "", "", Playback.Held)).flatMap { result =>
      val printing = new Say({
        case Message.Event(event) =>
          CompileConsole.show(Show(repo, event), say, sayError)
        case Message.Done(done) =>
          val finished = done match
            case CallResult.Reported(_) =>
              Completed(CliExit.from(Call.Watch(inner), done), "", "", Playback.Heard)
            case CallResult.Fault(fault) =>
              Completed(CliExit.from(Call.Watch(inner), done), "", fault.message, Playback.Held)
          result.set(finished)
        case _ => ZIO.unit
      })
      Watching
        .session(
          repo,
          printing,
          operate(repo, apiJar, inner, printing.dropExit),
          ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.map {
            case Left(_)       => List(repo.resolve("histos.scala"))
            case Right(loaded) => Watching.roots(repo, inner, loaded.graph)
          },
        )
        .foldZIO(
          error =>
            ZIO.succeed(
              Completed(CliExit.LoadFailed, "", error.message, Playback.Held)
            ),
          _ => result.get,
        )
    }

  /** The no-server path for one call. Watch uses [[operate]] and does not call `perform`. */
  private def locally(
      repo: Path,
      apiJar: Path,
      call: Call,
  ): URIO[Local, Completed] =
    call match
      case Call.Toolchain =>
        recorded(repo, call, toolchainSay(repo, apiJar, _))
      case other =>
        ZIO.serviceWithZIO[Machine] { machine =>
          Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]]).flatMap { runs =>
            val build =
              Build.here[Local](
                repo,
                apiJar,
                machine,
                runs,
                track = false,
                Cli.VersionText,
                adopted(repo, apiJar, _),
                cliEdges(repo, apiJar, _),
                heard(repo, _),
              )
            build
              .perform(other)
              .runCollect
              .foldCauseZIO(
                cause =>
                  if cause.isInterruptedOnly then ZIO.succeed(Completed(CliExit.Interrupted, "", "", Playback.Heard))
                  else
                    cause.failureOrCause match
                      case Left(error) =>
                        ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message, Playback.Held))
                      case Right(rest) => ZIO.failCause(rest),
                beats =>
                  val feeds = beats.map(Build.feed)
                  val done  = completed(repo, other, feeds)
                  if finished(feeds) then ZIO.succeed(done)
                  else sayError("the call did not finish").as(done),
              )
          }
        }

  /** A watch iteration. The say is the session, so this does not open a second stream. */
  private def operate(
      repo: Path,
      apiJar: Path,
      call: Call,
      say: Say,
  ): ZIO[Local, ServerError, Unit] =
    call match
      case Call.Toolchain =>
        toolchainSay(repo, apiJar, say)
      case other =>
        ZIO.serviceWithZIO[Machine] { machine =>
          Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]]).flatMap { runs =>
            adopted(repo, apiJar, other).flatMap { loaded =>
              Perform.run(
                repo,
                apiJar,
                loaded,
                machine,
                runs,
                track = false,
                Cli.VersionText,
                other,
                say,
                cliEdges(repo, apiJar, say),
              )
            }
          }
        }

  private def adopted(
      repo: Path,
      apiJar: Path,
      call: Call,
  ): ZIO[Local, ServerError, Either[BuildFault, Option[LoadedBuild]]] =
    if !Perform.needsGraph(call) then ZIO.succeed(Right(None))
    else
      ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.map {
        case Left(error)  => Left(BuildFault.Load(error))
        case Right(build) => Right(Some(build))
      }

  private def cliEdges(repo: Path, apiJar: Path, say: Say): Perform.Edges[Local] =
    Perform.Edges(
      watch = (_, edge) =>
        Calls.finish(
          edge,
          CallResult.Fault(
            BuildFault.Rejected(Rejection.Watch(CliParseError.WatchCommand("watch does not nest").message))
          ),
        ),
      reload = ZIO.fail(ServerError.NoServer),
      reboot = bootCall(repo, apiJar).flatMap(result => Calls.finish(say, result)),
      bsp = Calls.finish(
        say,
        CallResult.Fault(BuildFault.Rejected(Rejection.Watch(CliParseError.Rejected.message))),
      ),
    )

  private def heard(repo: Path, beat: Beat): UIO[Unit] =
    beat match
      case Beat.Heard(event) =>
        CompileConsole.show(Show(repo, event), say, sayError)
      case Beat.Finished(CallResult.Fault(fault)) =>
        val text = fault.message
        if text.isEmpty then ZIO.unit else sayError(text)
      case Beat.Finished(_) => ZIO.unit

  /** No server, so nothing was printed yet. Finish prints this once. */
  private def held(call: Call, out: String, result: CallResult): Completed =
    val err = result match
      case CallResult.Fault(fault)     => fault.message
      case CallResult.Reported(report) => report.err.mkString("\n")
    Completed(CliExit.from(call, result), out, err, Playback.Held)

  /** The adopted graph is missing. Explain the machine. A present graph is a survey. */
  private def toolchainSay(
      repo: Path,
      apiJar: Path,
      say: Say,
  ): ZIO[Local, ServerError, Unit] =
    ZIO.serviceWithZIO[Machine] { machine =>
      ZIO
        .serviceWithZIO[Loader](_.load(repo, apiJar))
        .foldZIO(
          {
            case LoadError.MissingFile(_) =>
              Toolchain
                .explain(machine)
                .foldZIO(
                  error => Calls.finish(say, CallResult.Fault(BuildFault.Toolchain(error))),
                  found => Calls.tell(say, Toolchain.text(found)) *> Calls.finish(say, CallResult.reported()),
                )
            case error =>
              Calls.finish(say, CallResult.Fault(BuildFault.Load(error)))
          },
          loaded => Calls.toolchain(machine, loaded.graph, say),
        )
    }

  private type Local = Loader & Scope & ContentStore & Machine & Resolve

  private def graph(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    attach(repo, Call.Graph, graphLocal(repo, apiJar))

  private def graphLocal(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .either
      .map:
        case Left(err) =>
          held(Call.Graph, "", CallResult.Fault(BuildFault.Load(err)))
        case Right(loaded) =>
          held(
            Call.Graph,
            GraphText.render(loaded.graph, loaded.plugins.links),
            CallResult.reported(),
          )

  private def run(
      repo: Path,
      apiJar: Path,
      id: TaskId,
      platform: Option[Platform],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    val call =
      if id == TaskId.Inspect || id == TaskId.Versions then Call.Task(id.value, PluginArgs.empty)
      else Call.Run(id.value, platform)
    attach(repo, call, locally(repo, apiJar, call))

  private def compile(
      repo: Path,
      apiJar: Path,
      projects: List[ProjectName],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    val call = Call.Compile(projects)
    attach(repo, call, locally(repo, apiJar, call))

  /** Only the local path loads. A matching server already holds the graph. */
  private def loaded[R](repo: Path, apiJar: Path)(
      use: histos.load.LoadedBuild => URIO[R, Completed]
  ): URIO[R & Loader & Scope & ContentStore & Resolve, Completed] =
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .foldZIO(
        error => ZIO.succeed(held(Call.Graph, "", CallResult.Fault(BuildFault.Load(error)))),
        use,
      )

  private def preview(
      repo: Path,
      apiJar: Path,
      projects: List[ProjectName],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(
      repo,
      Call.PreviewCompile(projects),
      loaded(repo, apiJar)(build => previewLocal(repo, build, projects)),
    )

  private def previewLocal(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      projects: List[ProjectName],
  ): UIO[Completed] =
    Plan
      .show(repo, loaded.graph, projects)
      .fold(
        error => held(Call.PreviewCompile(projects), "", CallResult.Fault(BuildFault.Preview(error))),
        text => held(Call.PreviewCompile(projects), text, CallResult.reported()),
      )

  private def previewTest(
      repo: Path,
      apiJar: Path,
      projects: List[ProjectName],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(
      repo,
      Call.PreviewTest(projects),
      loaded(repo, apiJar)(build => previewTestLocal(repo, build, projects)),
    )

  private def previewTestLocal(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      projects: List[ProjectName],
  ): UIO[Completed] =
    Plan
      .test(repo, loaded.graph, projects)
      .fold(
        error => held(Call.PreviewTest(projects), "", CallResult.Fault(BuildFault.Preview(error))),
        plan => held(Call.PreviewTest(projects), Plan.renderTest(repo, plan), CallResult.reported()),
      )

  private def previewPublishLocal(
      repo: Path,
      apiJar: Path,
      tokens: List[String],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    val call = Call.PreviewPublishLocal(tokens)
    ZIO.serviceWithZIO[Machine]: machine =>
      attach(
        repo,
        call,
        loaded(repo, apiJar)(build => previewLocalPlan(repo, build, tokens, machine.env, call)),
      )

  private def previewPublishSnapshots(
      repo: Path,
      apiJar: Path,
      tokens: List[String],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    val call = Call.PreviewPublishSnapshots(tokens)
    ZIO.serviceWithZIO[Machine]: machine =>
      attach(
        repo,
        call,
        loaded(repo, apiJar)(build => previewSnapshotPlan(repo, build, tokens, machine.env, call)),
      )

  private def previewLocalPlan(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      tokens: List[String],
      env: Map[String, String],
      call: Call,
  ): UIO[Completed] =
    Plan
      .publishLocal(repo, loaded.graph, tokens, env)
      .fold(
        error => held(call, "", CallResult.Fault(BuildFault.Preview(error))),
        plan => held(call, Plan.renderPublish(plan), CallResult.reported()),
      )

  private def previewSnapshotPlan(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      tokens: List[String],
      env: Map[String, String],
      call: Call,
  ): UIO[Completed] =
    Plan
      .publishSnapshots(repo, loaded.graph, tokens, env)
      .fold(
        error => held(call, "", CallResult.Fault(BuildFault.Preview(error))),
        plan => held(call, Plan.renderPublish(plan), CallResult.reported()),
      )

  /** Empty names skip the graph. Nothing is deleted. */
  private def previewClean(
      repo: Path,
      apiJar: Path,
      projects: List[ProjectName],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(
      repo,
      Call.PreviewClean(projects),
      if projects.isEmpty then previewCleanAll(repo)
      else loaded(repo, apiJar)(previewCleanLocal(repo, _, projects)),
    )

  private def previewCleanAll(repo: Path): UIO[Completed] =
    val plan = Plan.cleanAll(repo)
    ZIO.succeed(held(Call.PreviewClean(Nil), Plan.renderClean(repo, plan), CallResult.reported()))

  private def previewCleanLocal(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      projects: List[ProjectName],
  ): UIO[Completed] =
    Plan
      .clean(repo, loaded.graph, projects)
      .fold(
        error => held(Call.PreviewClean(projects), "", CallResult.Fault(BuildFault.Preview(error))),
        plan => held(Call.PreviewClean(projects), Plan.renderClean(repo, plan), CallResult.reported()),
      )

  private def previewCleanFull(
      repo: Path
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(repo, Call.PreviewCleanFull, previewCleanFullLocal(repo))

  private def previewCleanFullLocal(repo: Path): UIO[Completed] =
    val plan = Plan.cleanFull(repo)
    ZIO.succeed(held(Call.PreviewCleanFull, Plan.renderClean(repo, plan), CallResult.reported()))

  private def previewRun(
      repo: Path,
      apiJar: Path,
      id: String,
      platform: Option[Platform],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(
      repo,
      Call.PreviewRun(id, platform),
      loaded(repo, apiJar)(previewRunLocal(_, id, platform)),
    )

  private def previewRunLocal(
      loaded: histos.load.LoadedBuild,
      id: String,
      platform: Option[Platform],
  ): UIO[Completed] =
    val call = Call.PreviewRun(id, platform)
    ZIO.succeed:
      Plan.run(loaded.graph, id, platform) match
        case Left(error) =>
          held(call, "", CallResult.Fault(BuildFault.Preview(error)))
        case Right(plan) =>
          held(call, Plan.renderRun(plan), CallResult.reported())

  private def assembly(
      repo: Path,
      apiJar: Path,
      destination: Option[String],
  ): URIO[Local, Completed] =
    val call = Call.Assembly(destination)
    attach(repo, call, locally(repo, apiJar, call))

  /** Diagnostics are already on the console. A compile failure keeps the one-line summary here.
    */
  private[histos] def compileFailure(error: GraphCompileError): String =
    compileReport(error)

  private def compileReport(error: GraphCompileError): String =
    error match
      case GraphCompileError.Failed(module, version, _, _) =>
        s"'${module.value}' ${version.value} failed to compile"
      case GraphCompileError.Cells(errors) =>
        val count = errors.size
        if count == 1 then "1 cell failed to compile"
        else s"$count cells failed to compile"
      case other => other.message

  private def publishLocal(
      repo: Path,
      apiJar: Path,
      tokens: List[String],
  ): URIO[Local, Completed] =
    val call = Call.PublishLocal(tokens)
    attach(repo, call, locally(repo, apiJar, call))

  private def publishSnapshots(
      repo: Path,
      apiJar: Path,
      tokens: List[String],
  ): URIO[Local, Completed] =
    val call = Call.PublishSnapshots(tokens)
    attach(repo, call, locally(repo, apiJar, call))

  private def publishRelease(
      repo: Path,
      apiJar: Path,
      automatic: Boolean,
      tokens: List[String],
  ): URIO[Local, Completed] =
    val call = Call.PublishRelease(tokens, automatic)
    attach(repo, call, locally(repo, apiJar, call))

  private def publishPromote(repo: Path, apiJar: Path, id: String): URIO[Local, Completed] =
    val call = Call.PublishPromote(id)
    attach(repo, call, locally(repo, apiJar, call))

  private def publishDrop(repo: Path, apiJar: Path, id: String): URIO[Local, Completed] =
    val call = Call.PublishDrop(id)
    attach(repo, call, locally(repo, apiJar, call))

  private def publishAssets(repo: Path, apiJar: Path, tokens: List[String]): URIO[Local, Completed] =
    val call = Call.PublishAssets(tokens)
    attach(repo, call, locally(repo, apiJar, call))

  /** Compile, then one subprocess per Scala version. `no tests` is success and is not a passing suite.
    */
  private def test(
      repo: Path,
      apiJar: Path,
      projects: List[ProjectName],
      patterns: List[SuitePattern],
      mode: TestMode,
  ): URIO[Local, Completed] =
    val call = Call.Test(projects, patterns, mode)
    attach(repo, call, locally(repo, apiJar, call))

  private def say(text: String): UIO[Unit] =
    Console.printLine(text).orElseSucceed(())

  private def sayError(text: String): UIO[Unit] =
    Console.printLineError(text).orElseSucceed(())

  private def cachePath: URIO[ContentStore, Completed] =
    ZIO.serviceWith[ContentStore](store => held(Call.CachePath, store.root.toString, CallResult.reported()))

  private def cacheList: URIO[ContentStore, Completed] =
    ZIO
      .serviceWithZIO[ContentStore](_.list)
      .mapBoth(
        err => held(Call.CacheList, "", CallResult.Fault(BuildFault.Cache(err))),
        entries =>
          val text =
            if entries.isEmpty then ""
            else
              entries
                .map(entry => s"${entry.digest.value} ${entry.bytes}")
                .mkString("\n")
          held(Call.CacheList, text, CallResult.reported()),
      )
      .merge

  private def cacheClean(mode: CacheCleanMode): URIO[ContentStore, Completed] =
    ZIO
      .serviceWithZIO[ContentStore]: store =>
        Clock.instant.flatMap(now => store.clean(now, ContentStore.Retention, mode))
      .mapBoth(
        err => held(Call.CacheClean(mode), "", CallResult.Fault(BuildFault.Cache(err))),
        entries =>
          val verb = mode match
            case CacheCleanMode.DryRun => "would remove"
            case CacheCleanMode.Remove => "removed"
          val text =
            if entries.isEmpty then s"$verb nothing"
            else
              entries
                .map(entry => s"$verb ${entry.digest.value}")
                .mkString("\n")
          held(Call.CacheClean(mode), text, CallResult.reported()),
      )
      .merge

  /** The remote URI is configuration. This command names the namespaces and does not open a connection. */
  private def cachePromote(from: CacheNamespace, to: CacheNamespace): UIO[Completed] =
    val call = Call.CachePromote(from, to)
    ZIO.succeed(held(call, "", CallResult.Fault(BuildFault.Remote(RemoteError.Off))))

  /** No names skip the graph. A live server still holds the compile permit. */
  private def clean(
      repo: Path,
      apiJar: Path,
      projects: List[ProjectName],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(
      repo,
      Call.Clean(projects),
      if projects.isEmpty then cleanLocal(repo)
      else loaded(repo, apiJar)(cleanNamed(repo, _, projects)),
    )

  private def cleanLocal(repo: Path): UIO[Completed] =
    histos.clean.Products
      .wipe(repo)
      .fold(
        error => held(Call.Clean(Nil), "", CallResult.Fault(BuildFault.Io(error))),
        _ => held(Call.Clean(Nil), histos.clean.Products.line(Nil), CallResult.reported()),
      )

  private def cleanNamed(
      repo: Path,
      build: histos.load.LoadedBuild,
      projects: List[ProjectName],
  ): UIO[Completed] =
    val call = Call.Clean(projects)
    histos.clean.Products
      .named(repo, build.graph, projects)
      .fold(
        error => held(call, "", CallResult.Fault(cleanFault(error))),
        names => held(call, histos.clean.Products.line(names), CallResult.reported()),
      )

  private def cleanFault(error: SelectError | IoFailure): BuildFault =
    error match
      case found: SelectError => BuildFault.Select(found)
      case found: IoFailure   => BuildFault.Io(found)

  private def cleanFull(repo: Path): URIO[ContentStore, Completed] =
    attach(repo, Call.CleanFull, cleanFullLocal(repo))

  /** Products go first. A store failure is exit 3 and does not claim success.
    */
  private def cleanFullLocal(repo: Path): URIO[ContentStore, Completed] =
    histos.clean.Products
      .wipe(repo)
      .foldZIO(
        error => ZIO.succeed(held(Call.CleanFull, "", CallResult.Fault(BuildFault.Io(error)))),
        _ =>
          ZIO
            .serviceWithZIO[ContentStore]: store =>
              Clock.instant.flatMap(now => store.forget(histos.cache.CacheProject.of(repo), now))
            .fold(
              error => held(Call.CleanFull, "", CallResult.Fault(BuildFault.Cache(error))),
              entries => held(Call.CleanFull, histos.clean.Products.forgotten(entries), CallResult.reported()),
            ),
      )

  private def resolve(
      repo: Path,
      apiJar: Path,
      action: ResolveAction,
  ): URIO[Loader & Scope & ContentStore & Resolve & Machine, Completed] =
    attach(
      repo,
      Call.Resolve(action),
      resolveLocal(repo, apiJar, action),
    )

  private def resolveLocal(
      repo: Path,
      apiJar: Path,
      action: ResolveAction,
  ): URIO[Loader & Scope & ContentStore & Resolve & Machine, Completed] =
    ZIO.serviceWithZIO[Machine]: machine =>
      Calls
        .resolveWork(repo, apiJar, action, machine.native)
        .fold(
          {
            case error: LoadError =>
              held(Call.Resolve(action), "", CallResult.Fault(BuildFault.Load(error)))
            case error: ResolveError =>
              held(Call.Resolve(action), "", CallResult.Fault(BuildFault.Resolve(error)))
          },
          text => held(Call.Resolve(action), text, CallResult.reported()),
        )

  private def workflow(
      repo: Path,
      apiJar: Path,
      action: WorkflowAction,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    attach(
      repo,
      Call.Workflow(action),
      workflowLocal(repo, apiJar, action),
    )

  private def workflowLocal(
      repo: Path,
      apiJar: Path,
      action: WorkflowAction,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .either
      .flatMap:
        case Left(error) =>
          ZIO.succeed(
            held(Call.Workflow(action), "", CallResult.Fault(BuildFault.Load(error)))
          )
        case Right(loaded) =>
          val call = Call.Workflow(action)
          Calls
            .sonaOf(loaded)
            .flatMap(sona => Calls.workflowWork(repo, loaded.graph, sona, action))
            .fold(
              fault => held(call, "", CallResult.Fault(BuildFault.Workflow(fault))),
              file => held(call, file.toString, CallResult.reported()),
            )

  private def toolchain(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(repo, Call.Toolchain, toolchainLocal(repo, apiJar))

  private def toolchainLocal(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    ZIO.serviceWithZIO[Machine] { machine =>
      ZIO
        .serviceWithZIO[Loader](_.load(repo, apiJar))
        .either
        .flatMap:
          case Left(LoadError.MissingFile(_)) =>
            Toolchain
              .explain(machine)
              .mapBoth(
                err => held(Call.Toolchain, "", CallResult.Fault(BuildFault.Toolchain(err))),
                found => held(Call.Toolchain, Toolchain.text(found), CallResult.reported()),
              )
              .merge
          case Left(error) =>
            ZIO.succeed(held(Call.Toolchain, "", CallResult.Fault(BuildFault.Load(error))))
          case Right(loaded) =>
            Survey
              .read(machine, loaded.graph)
              .mapBoth(
                err => held(Call.Toolchain, Survey.shown(err), CallResult.Fault(BuildFault.Survey(err))),
                found => held(Call.Toolchain, Survey.text(found), CallResult.reported()),
              )
              .merge
    }

  private def mcp(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & Machine & Resolve & ContentStore, Completed] =
    ZIO.serviceWithZIO[Machine]: machine =>
      HistosMcp
        .serve(repo, apiJar, LinePipe.standard, machine)
        .fold(
          err => Completed(CliExit.LoadFailed, "", err.message, Playback.Held),
          _ => Completed(CliExit.Success, "", "", Playback.Held),
        )

  private def bsp(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    Attach
      .decide(repo, VersionText)
      .flatMap:
        case Decision.Open(socket) =>
          Client
            .splice(socket)
            .foldZIO(
              {
                case ServerError.Unreachable(_) =>
                  bspLocal(repo, apiJar)
                case error =>
                  ZIO.succeed(
                    Completed(
                      CliExit.LoadFailed,
                      "",
                      error.message,
                      Playback.Held,
                    )
                  )
              },
              code =>
                ZIO.succeed(
                  Completed(
                    if code == 0 then CliExit.Success else CliExit.TaskFailed,
                    "",
                    "",
                    Playback.Held,
                  )
                ),
            )
        case Decision.DifferentVersion =>
          ZIO.succeed(
            Completed(
              CliExit.LoadFailed,
              "",
              ServerError.DifferentVersion.message,
              Playback.Held,
            )
          )
        case Decision.Absent => bspLocal(repo, apiJar)

  private def reload(repo: Path): UIO[Completed] =
    Attach
      .decide(repo, VersionText)
      .flatMap:
        case Decision.DifferentVersion =>
          ZIO.succeed(
            Completed(
              CliExit.LoadFailed,
              "",
              ServerError.DifferentVersion.message,
              Playback.Held,
            )
          )
        case Decision.Absent =>
          ZIO.succeed(
            Completed(
              CliExit.LoadFailed,
              "",
              ServerError.NoServer.message,
              Playback.Held,
            )
          )
        case Decision.Open(socket) =>
          remote(repo, socket, Call.Reload).foldZIO(
            {
              case ServerError.Unreachable(_) =>
                ZIO.succeed(
                  Completed(
                    CliExit.LoadFailed,
                    "",
                    ServerError.NoServer.message,
                    Playback.Held,
                  )
                )
              case error =>
                ZIO.succeed(
                  Completed(
                    CliExit.LoadFailed,
                    "",
                    error.message,
                    Playback.Held,
                  )
                )
            },
            done => ZIO.succeed(done),
          )

  private def launcherLocal(repo: Path): UIO[Completed] =
    histos.publish.LauncherPin
      .update(repo)
      .fold(
        error => held(Call.LauncherUpdate, "", CallResult.Fault(BuildFault.Launcher(error))),
        pin => held(Call.LauncherUpdate, s"${pin.revision} ${pin.sha256.value}", CallResult.reported()),
      )

  /** The id is the run id the server minted. No server has no runs. */
  private def cancel(repo: Path, id: RunId): UIO[Completed] =
    attach(
      repo,
      Call.Cancel(id),
      Ref
        .make(Map.empty[RunId, Fiber[ServerError, Unit]])
        .flatMap(runs => recorded(repo, Call.Cancel(id), Calls.cancel(runs, id, _))),
    )

  private[cli] def recorded[R](
      repo: Path,
      call: Call,
      run: Say => ZIO[R, ServerError, Unit],
  ): URIO[R, Completed] =
    Ref.make(Chunk.empty[Feed]).flatMap { feeds =>
      run(speaking(repo, feeds)).foldCauseZIO(
        cause =>
          if cause.isInterruptedOnly then ZIO.succeed(Completed(CliExit.Interrupted, "", "", Playback.Heard))
          else
            cause.failureOrCause match
              case Left(error) =>
                ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message, Playback.Held))
              case Right(rest) => ZIO.failCause(rest),
        _ =>
          feeds.get.flatMap: chunk =>
            val done = completed(repo, call, chunk)
            if finished(chunk) then ZIO.succeed(done)
            else sayError("the call did not finish").as(done),
      )
    }

  private def finished(feeds: Chunk[Feed]): Boolean =
    feeds.exists:
      case Feed.Done(_) => true
      case _            => false

  /** Print, then append, so the line is on the console before the next message. A fault is printed here and marked
    * heard, so finish does not print it again.
    */
  private def speaking(repo: Path, feeds: Ref[Chunk[Feed]]): Say =
    new Say({
      case Message.Event(event) =>
        CompileConsole.show(Show(repo, event), say, sayError) *>
          feeds.update(_ :+ Feed.Event(event))
      case Message.Done(CallResult.Fault(fault)) =>
        val text  = fault.message
        val shown = if text.isEmpty then ZIO.unit else sayError(text)
        shown *> feeds.update(_ :+ Feed.Done(CallResult.Fault(fault)))
      case Message.Done(result) =>
        feeds.update(_ :+ Feed.Done(result))
      case _ => ZIO.unit
    })

  private def completed(repo: Path, call: Call, feeds: Chunk[Feed]): Completed =
    val events               = feeds.collect { case Feed.Event(event) => event }
    val (outLines, errLines) = Show.transcript(repo, events)
    val faults               = feeds.collect {
      case Feed.Done(CallResult.Fault(fault)) if fault.message.nonEmpty => fault.message
    }
    val out = outLines.mkString("\n")
    val err = (errLines ++ faults).mkString("\n")
    feeds.reverse.collectFirst { case Feed.Done(result) => result } match
      case Some(result) =>
        Completed(CliExit.from(call, result), out, err, Playback.Heard)
      case None =>
        Completed(CliExit.TaskFailed, out, "the call did not finish", Playback.Heard)

  /** A running server replaces itself. No server starts one. */
  private[histos] def reboot(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Resolve & Machine, Completed] =
    Attach
      .decide(repo, VersionText)
      .flatMap:
        case Decision.Open(socket) =>
          remote(repo, socket, Call.Reboot).foldZIO(
            {
              case ServerError.Unreachable(_) | ServerError.DifferentVersion =>
                serve(repo, apiJar)
              case error =>
                ZIO.succeed(
                  Completed(
                    CliExit.LoadFailed,
                    "",
                    error.message,
                    Playback.Held,
                  )
                )
            },
            done => ZIO.succeed(done),
          )
        case Decision.Absent | Decision.DifferentVersion =>
          serve(repo, apiJar)

  private enum Boot:
    case Up
    case Load(error: LoadError)
    case Jdk(error: GraphCompileError)
    case Transport(error: ServerError)

  /** Start a server in this process. The socket line is printed here. The effect waits until the server stops. */
  private def boot(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Resolve & Machine, Boot] =
    ZIO.environmentWithZIO[Machine & Resolve & ContentStore] { env =>
      val machine = env.get[Machine]
      ZIO.serviceWithZIO[Loader] { loader =>
        loader
          .load(repo, apiJar)
          .foldZIO(
            error => ZIO.succeed(Boot.Load(error)),
            loaded =>
              ZIO
                .serviceWithZIO[Machine](GraphCompile.jdk)
                .foldZIO(
                  error => ZIO.succeed(Boot.Jdk(error)),
                  jdk =>
                    loader
                      .buildClasspath(apiJar)
                      .foldZIO(
                        error => ZIO.succeed(Boot.Load(error)),
                        classpath =>
                          Daemon
                            .open(
                              repo,
                              loaded.graph,
                              loaded.build,
                              jdk,
                              VersionText,
                              apiJar,
                              classpath,
                              loader.reportBuild(repo, apiJar).provideEnvironment(env),
                              loaded.plugins,
                              machine,
                            )
                            .flatMap { running =>
                              say(running.record.socket.toString) *> running.done
                            }
                            .foldCauseZIO(
                              cause =>
                                cause.failureOrCause match
                                  case Left(error) => ZIO.succeed(Boot.Transport(error))
                                  case Right(rest) => ZIO.failCause(rest),
                              _ => ZIO.succeed(Boot.Up),
                            ),
                      ),
                ),
          )
      }
    }

  private def serve(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Resolve & Machine, Completed] =
    boot(repo, apiJar).map:
      case Boot.Up               => Completed(CliExit.Success, "", "", Playback.Held)
      case Boot.Load(error)      => held(Call.Reboot, "", CallResult.Fault(BuildFault.Load(error)))
      case Boot.Jdk(error)       => held(Call.Reboot, "", Perform.jdkMissed(Call.Reboot, error))
      case Boot.Transport(error) =>
        Completed(CliExit.LoadFailed, "", error.message, Playback.Held)

  /** A reboot that is not a transport failure. Transport fails the effect. */
  private[histos] def bootCall(
      repo: Path,
      apiJar: Path,
  ): ZIO[Loader & Scope & ContentStore & Resolve & Machine, ServerError, CallResult] =
    boot(repo, apiJar).flatMap:
      case Boot.Up               => ZIO.succeed(CallResult.reported())
      case Boot.Load(error)      => ZIO.succeed(CallResult.Fault(BuildFault.Load(error)))
      case Boot.Jdk(error)       => ZIO.succeed(Perform.jdkMissed(Call.Reboot, error))
      case Boot.Transport(error) => ZIO.fail(error)

  /** A matching server runs the command. Anything else, including a dead socket, runs here.
    */
  private def attach[R](
      repo: Path,
      call: Call,
      local: URIO[R, Completed],
  ): URIO[R, Completed] =
    Attach
      .decide(repo, VersionText)
      .flatMap:
        case Decision.Open(socket) =>
          remote(repo, socket, call).foldZIO(
            {
              case ServerError.Unreachable(_) => local
              case error                      =>
                ZIO.succeed(
                  Completed(
                    CliExit.LoadFailed,
                    "",
                    error.message,
                    Playback.Held,
                  )
                )
            },
            done => ZIO.succeed(done),
          )
        case Decision.DifferentVersion =>
          ZIO.succeed(
            Completed(
              CliExit.LoadFailed,
              "",
              ServerError.DifferentVersion.message,
              Playback.Held,
            )
          )
        case Decision.Absent => local

  private def remote(
      repo: Path,
      socket: java.nio.file.Path,
      call: Call,
  ): ZIO[Any, ServerError, Completed] =
    zio.Ref.make(Completed(CliExit.Success, "", "", Playback.Held)).flatMap { result =>
      Build.attached(socket).perform(call).foreach {
        case Beat.Heard(event) =>
          CompileConsole.show(Show(repo, event), say, sayError)
        case Beat.Finished(done) =>
          val finished = done match
            case CallResult.Reported(_) =>
              Completed(CliExit.from(call, done), "", "", Playback.Heard)
            case CallResult.Fault(fault) =>
              Completed(CliExit.from(call, done), "", fault.message, Playback.Held)
          result.set(finished)
      } *> result.get
    }

  private def bspLocal(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    ZIO.environmentWithZIO[Loader & Resolve & ContentStore & Machine]: env =>
      val loader = env.get[Loader]
      loader
        .load(repo, apiJar)
        .foldZIO(
          error =>
            ZIO.succeed(
              Completed(CliExit.LoadFailed, "", error.message, Playback.Held)
            ),
          loaded =>
            ZIO
              .serviceWithZIO[Machine](GraphCompile.jdk)
              .foldZIO(
                error =>
                  ZIO
                    .succeed(
                      Completed(
                        CliExit.CompileFailed,
                        "",
                        error.message,
                        Playback.Held,
                      )
                    ),
                jdk =>
                  loader
                    .buildClasspath(apiJar)
                    .foldZIO(
                      error =>
                        ZIO.succeed(
                          Completed(
                            CliExit.LoadFailed,
                            "",
                            error.message,
                            Playback.Held,
                          )
                        ),
                      classpath =>
                        val output = java.lang.System.out
                        val notify = (line: String) => histos.bsp.Frame.write(output, line).ignore
                        Runtimes
                          .read(env.get[Machine])
                          .flatMap: runtimes =>
                            Bsp
                              .open(
                                repo,
                                loaded.graph,
                                jdk,
                                runtimes,
                                classpath,
                                notify,
                                loader
                                  .reportBuild(repo, apiJar)
                                  .provideEnvironment(env),
                              )
                              .either
                              .flatMap:
                                case Left(error) =>
                                  ZIO.succeed(
                                    Completed(
                                      CliExit.ResolveFailed,
                                      "",
                                      error.message,
                                      Playback.Held,
                                    )
                                  )
                                case Right(session) =>
                                  speak(session).foldZIO(
                                    error =>
                                      ZIO.succeed(
                                        Completed(
                                          CliExit.LoadFailed,
                                          "",
                                          error.message,
                                          Playback.Held,
                                        )
                                      ),
                                    code =>
                                      ZIO.succeed(
                                        Completed(
                                          if code == 0 then CliExit.Success
                                          else CliExit.TaskFailed,
                                          "",
                                          "",
                                          Playback.Held,
                                        )
                                      ),
                                  ),
                    ),
              ),
        )

  /** Same reader and writer as a server splice. Stdio has no histos exit marker. */
  private def speak(session: Bsp.Session): ZIO[Resolve & ContentStore & Scope, ServerError, Int] =
    val stdin  = Path.of("stdin")
    val stdout = Path.of("stdout")
    val send   =
      (line: String) => Frame.write(java.lang.System.out, line).mapError(_ => ServerError.Io(ServerIo.Write(stdout)))
    val next = Frame.read(java.lang.System.in).mapError(_ => ServerError.Io(ServerIo.Read(stdin)))
    Daemon.outbox(send).flatMap { box =>
      Daemon
        .requests(next, box.offer, payload => session.answer(payload), box.done)
        .flatMap(code => box.close.as(code))
    }

  private def contributed(
      args: List[String],
      repo: Path,
      apiJar: Path,
  ): URIO[Local, Completed] =
    val (watched, tokens) = args match
      case "~" :: rest => (true, rest)
      case other       => (false, other)
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .either
      .flatMap:
        case Left(err) =>
          ZIO.succeed(
            Completed(CliExit.LoadFailed, "", err.message, Playback.Held)
          )
        case Right(loaded) =>
          taskFrom(loaded, tokens) match
            case TaskParsed.Usage(message) =>
              ZIO.succeed(Completed(CliExit.Usage, "", message, Playback.Held))
            case TaskParsed.Ready(task) =>
              if watched then attach(repo, Call.Watch(task), watchLocal(repo, apiJar, task))
              else attach(repo, task, locally(repo, apiJar, task))

  /** MCP watch. Tokens are the inner command, with no leading tilde. The grammar and, for an unknown root,
    * [[PluginCommands]] run once.
    */
  private[histos] def watchCall(
      tokens: List[String],
      repo: Path,
      apiJar: Path,
  ): ZIO[Loader & Scope, BuildFault, Call] =
    tokens match
      case Nil =>
        ZIO.fail(
          BuildFault.Rejected(Rejection.Watch(CliParseError.WatchCommand("watch needs a command").message))
        )
      case _ =>
        Grammar
          .parse(tokens)
          .either
          .flatMap:
            case Left(CliParseError.UnknownRoot) =>
              ZIO
                .serviceWithZIO[Loader](_.load(repo, apiJar))
                .mapError(error => BuildFault.Load(error))
                .flatMap: loaded =>
                  taskFrom(loaded, tokens) match
                    case TaskParsed.Usage(message) =>
                      ZIO.fail(BuildFault.Rejected(Rejection.Watch(message)))
                    case TaskParsed.Ready(task) => ZIO.succeed(Call.Watch(task))
            case Left(error) =>
              ZIO.fail(BuildFault.Rejected(Rejection.Watch(error.message)))
            case Right(command) =>
              ZIO.fromEither(
                Operation
                  .operation(command)
                  .map(Call.Watch(_))
                  .left
                  .map(error => BuildFault.Rejected(Rejection.Watch(error.message)))
              )

  private def taskFrom(
      loaded: histos.load.LoadedBuild,
      tokens: List[String],
  ): TaskParsed =
    val commands = loaded.graph.plugins.flatMap: plugin =>
      plugin.contribution.commands.map(BoundCommand(plugin.id, _))
    PluginCommands.parse(commands, tokens) match
      case Left(error) =>
        val text = error match
          case CliParseError.Flag(_) => error.message
          case _                     => PluginCommands.usage(commands)
        TaskParsed.Usage(text)
      case Right((id, pluginArgs)) =>
        TaskParsed.Ready(Call.Task(id.render, pluginArgs))

  private enum TaskParsed:
    case Ready(call: Call)
    case Usage(text: String)

  private def plugin(
      repo: Path,
      apiJar: Path,
      id: PluginTaskId,
      args: PluginArgs,
  ): URIO[Local, Completed] =
    val call = Call.Task(id.render, args)
    attach(repo, call, locally(repo, apiJar, call))
end Cli

object Main extends ZIOAppDefault:
  def run =
    jars.foldZIO(
      error =>
        Console.printLineError(error.message).ignore *> stop(
          CliExit.LoadFailed.code
        ),
      found =>
        for
          args <- getArgs.map(_.toList)
          _    <- dispatch(args, Path.of("").toAbsolutePath, found)
        yield (),
    )

  private def dispatch(
      args: List[String],
      repo: Path,
      jars: Jars,
  ): URIO[Scope, Unit] =
    Machine
      .load(repo)
      .foldZIO(
        err =>
          finish(
            Completed(CliExit.ToolchainFailed, "", err.message, Playback.Held)
          ),
        machine =>
          Cli
            .parse(args)
            .either
            .flatMap:
              case Left(_) => runCommand(args, repo, jars.api, machine)
              case Right(
                    Command.Toolchain | Command.Clean(_) | Command.CleanFull
                  ) =>
                runCommand(args, repo, jars.api, machine)
              case Right(_) =>
                Toolchain
                  .prepare(machine)
                  .foldZIO(
                    err =>
                      finish(
                        Completed(
                          CliExit.ToolchainFailed,
                          "",
                          err.message,
                          Playback.Held,
                        )
                      ),
                    _ => runCommand(args, repo, jars.api, machine),
                  ),
      )

  private final case class Jars(program: Path, api: Path)

  private def runCommand(
      args: List[String],
      repo: Path,
      jar: Path,
      machine: Machine,
  ): URIO[Scope, Unit] =
    ZIO
      .scoped:
        Cli
          .execute(args, repo, jar)
          .provideSome[Scope](
            Loader.live,
            ZLayer.succeed(ContentStore.in(machine.cache)),
            ZLayer.succeed(machine),
            Resolve.coursier,
          )
      .flatMap(finish)

  /** A successful `run` is process status 0. `exit` is the status the operating system sees.
    */
  private def stop(code: Int): UIO[Unit] =
    exit(zio.ExitCode(code))

  private def finish(done: Completed): UIO[Unit] =
    val pending = done.playback match
      case Playback.Heard => ZIO.unit
      case Playback.Held  =>
        Console.printLine(done.out).when(done.out.nonEmpty).ignore *>
          Console.printLineError(done.err).when(done.err.nonEmpty).ignore
    pending *> stop(done.exit.code)

  private def jars: IO[LoadError, Jars] =
    located.flatMap: program =>
      cacheHome.flatMap: cache =>
        BuildClasspath(program, cache).map(api => Jars(program, api))

  private def located: IO[LoadError, Path] =
    val location = Option(classOf[histos.HistosBuild].getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
    location match
      case None      => ZIO.fail(LoadError.MissingFile(Path.of("histos-api.jar")))
      case Some(url) =>
        ZIO
          .attempt(Path.of(url.toURI))
          .orElseFail(
            LoadError.Io(
              histos.workspace
                .IoFailure(Path.of(url.toString), histos.workspace.IoFault.Read)
            )
          )

  private def cacheHome: IO[LoadError, Path] =
    for
      xdg <- System
        .env("XDG_CACHE_HOME")
        .orElseFail(
          LoadError.Io(
            histos.workspace.IoFailure(
              Path.of("XDG_CACHE_HOME"),
              histos.workspace.IoFault.Read,
            )
          )
        )
      home <- System
        .property("user.home")
        .orElseFail(
          LoadError.Io(
            histos.workspace
              .IoFailure(Path.of("user.home"), histos.workspace.IoFault.Read)
          )
        )
      root <- (xdg.filter(_.nonEmpty), home.filter(_.nonEmpty)) match
        case (Some(dir), _) => ZIO.succeed(Path.of(dir, "histos"))
        case (_, Some(dir)) => ZIO.succeed(Path.of(dir, ".cache", "histos"))
        case _              => ZIO.fail(LoadError.NoHome)
    yield root
end Main
