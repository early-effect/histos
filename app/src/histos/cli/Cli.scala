package histos.cli

import heddle.LinePipe
import histos.bsp.{Bsp, Connection, Frame}
import histos.server.{Attach, Call, Calls, Client, Daemon, Decision, Feed, Message, Say, ServerError, Watching}
import histos.load.BuildClasspath
import histos.cache.ContentStore
import histos.jdk.{Machine, Toolchain}
import histos.load.{LoadError, Loader}
import histos.plugin.{PluginArgs, PluginTaskId}
import histos.toolchain.{Runtimes, Survey}
import histos.mcp.HistosMcp
import histos.resolve.{Resolve, ResolveError}
import histos.task.{RunId, TaskId}
import histos.workflow.WorkflowAction
import histos.testrun.{ModuleTestError, SuitePattern}
import histos.preview.Plan
import histos.zinc.{GraphCompile, GraphCompileError}
import histos.Platform

import java.nio.file.{Files, Path}

import zio.*
import zio.json.*

enum Command:
  case Version
  case Graph
  case Run(id: TaskId, platform: Option[Platform])
  case Plugin(id: PluginTaskId, args: PluginArgs)
  case Test(projects: List[String])
  case TestOnly(modules: List[String], patterns: List[SuitePattern])
  case TestQuick(modules: List[String], patterns: List[SuitePattern])
  case Watch(command: Command, args: List[String])
  case Compile(projects: List[String])
  case Clean(projects: List[String])
  case CleanFull
  case Assembly(destination: Option[String])
  case Publish(repository: String, projects: List[String])
  case Mcp
  case Bsp
  case Server
  case Toolchain
  case CachePath
  case CacheList
  case CacheClean(dryRun: Boolean)
  case Resolve(action: ResolveAction)
  case Workflow(action: WorkflowAction)
  case LauncherUpdate
  case Reload
  case Reboot
  case Cancel(id: String)
  case PreviewCompile(projects: List[String])
  case PreviewTest(projects: List[String])
  case PreviewPublish(projects: List[String])
  case PreviewClean(projects: List[String])
  case PreviewCleanFull
  case PreviewRun(id: TaskId, platform: Option[Platform])
  case PreviewPlugin(id: PluginTaskId)
  case Observe

enum ResolveAction:
  case Verify, Update, Check

enum CliParseError:
  case Help
  case UnknownCommand
  case UnknownRoot
  case MissingArgument
  case UnexpectedArguments
  case BadTaskId
  case BadPluginTask(detail: String)
  case BadPlatform(detail: String)
  case TaskPlatform(name: String)
  case MissingPattern
  case Pattern(detail: String)
  case WatchCommand(detail: String)
  case Rejected

  def message: String = this match
    case Help | UnknownCommand | UnknownRoot | Rejected => Cli.usageText
    case MissingArgument                                => "run needs one name"
    case UnexpectedArguments                            => Cli.usageText
    case BadTaskId                                      => "a task id is not empty"
    case BadPluginTask(detail)                          => detail
    case BadPlatform(detail)                            => detail
    case TaskPlatform(name)                             => s"run $name does not take a platform"
    case MissingPattern                                 => "testOnly needs a pattern"
    case Pattern(detail)                                => detail
    case WatchCommand(detail)                           => detail
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
  val VersionText: String = "histos 0.0.1-SNAPSHOT"

  val usageText: String =
    "usage: histos (version | graph | run <name> [jvm|js|native] | test [project ...] | testOnly [--module project ...] <pattern> ... | testQuick [--module project ...] [pattern ...] | ~ <command> | compile [project ...] | clean [project ...] | cleanFull | assembly [path] | publish <repository> [project ...] | observe | mcp | bsp | server | reload | reboot | cancel <id> | toolchain | cache path | cache list | cache clean [--dry-run] | resolve [--update | --check] | workflow [--check | --update] | launcher --update | preview compile [project ...] | preview test [project ...] | preview publish [project ...] | preview clean [project ...] | preview cleanFull | preview run <name> [jvm|js|native])"

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
          ZIO.succeed(
            Completed(CliExit.Success, VersionText, "", Playback.Held)
          ),
        )
      case Command.Graph                  => graph(repo, apiJar)
      case Command.Run(id, platform)      => run(repo, apiJar, id, platform)
      case Command.Plugin(id, pluginArgs) =>
        plugin(repo, apiJar, id, pluginArgs)
      case Command.Test(projects) =>
        test(repo, apiJar, projects, Nil, quick = false)
      case Command.TestOnly(modules, patterns) =>
        test(repo, apiJar, modules, patterns, quick = false)
      case Command.TestQuick(modules, patterns) =>
        test(repo, apiJar, modules, patterns, quick = true)
      case Command.Watch(inner, _)               => watch(repo, apiJar, inner)
      case Command.Compile(projects)             => compile(repo, apiJar, projects)
      case Command.Clean(projects)               => clean(repo, apiJar, projects)
      case Command.CleanFull                     => cleanFull(repo)
      case Command.Assembly(destination)         => assembly(repo, apiJar, destination)
      case Command.Publish(repository, projects) =>
        publish(repo, apiJar, repository, projects)
      case Command.Mcp             => mcp(repo, apiJar)
      case Command.Bsp             => bsp(repo, apiJar)
      case Command.Server          => serve(repo, apiJar)
      case Command.Toolchain       => toolchain(repo, apiJar)
      case Command.CachePath       => attach(repo, Call.CachePath, cachePath)
      case Command.CacheList       => attach(repo, Call.CacheList, cacheList)
      case Command.CacheClean(dry) =>
        attach(repo, Call.CacheClean(dry), cacheClean(dry))
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
      case Command.PreviewPublish(projects) =>
        previewPublish(repo, apiJar, projects)
      case Command.PreviewClean(projects) =>
        previewClean(repo, apiJar, projects)
      case Command.PreviewCleanFull =>
        previewCleanFull(repo)
      case Command.PreviewRun(id, platform) =>
        previewRun(repo, apiJar, id.value, platform)
      case Command.PreviewPlugin(id) =>
        previewRun(repo, apiJar, id.render, None)
      case Command.Observe => observe(repo)

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
    * `mcp`, `bsp`, `server`, `observe`, and a nested watch are usage and do not start a transport.
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
        case Message.Out(text)       => say(text)
        case Message.Err(text)       => sayError(text)
        case Message.Exit(code, err) =>
          result
            .set(Completed(CliExit.fromCode(code), "", err, Playback.Held))
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
              Completed(CliExit.TaskFailed, "", error.message, Playback.Held)
            ),
          _ => result.get,
        )
    }

  /** The no-server path for one call. Compile, test, run, publish, and assembly are [[Calls]]. Watch uses this and does
    * not call `perform`.
    */
  private def locally(
      repo: Path,
      apiJar: Path,
      call: Call,
  ): URIO[Local, Completed] =
    recorded(operate(repo, apiJar, call, _))

  private def operate(
      repo: Path,
      apiJar: Path,
      call: Call,
      say: Say,
  ): ZIO[Local, ServerError, Unit] =
    call match
      case Call.Compile(projects) =>
        withBuild(repo, apiJar, say)(build => compileSay(repo, build, projects, say))
      case Call.Test(projects, patterns, quick) =>
        withBuild(repo, apiJar, say)(build => testSay(repo, build, projects, patterns, quick, say))
      case Call.Run(id, platform) =>
        withBuild(repo, apiJar, say)(build => runSay(repo, build, id, platform, say))
      case Call.Publish(repository, projects) =>
        withBuild(repo, apiJar, say)(build => publishSay(repo, build, repository, projects, say))
      case Call.Assembly(destination) =>
        withBuild(repo, apiJar, say)(build => assemblySay(repo, build, destination, say))
      case Call.Task(id, args) =>
        withBuild(repo, apiJar, say)(build => taskSay(repo, build, id, args, say))
      case Call.Graph =>
        graphLocal(repo, apiJar).flatMap(report(_, say))
      case Call.Clean(Nil) =>
        cleanLocal(repo).flatMap(report(_, say))
      case Call.Clean(projects) =>
        loaded(repo, apiJar)(cleanNamed(repo, _, projects))
          .flatMap(report(_, say))
      case Call.CleanFull =>
        cleanFullLocal(repo).flatMap(report(_, say))
      case Call.PreviewCompile(projects) =>
        loaded(repo, apiJar)(previewLocal(repo, _, projects)).flatMap(
          report(_, say)
        )
      case Call.PreviewTest(projects) =>
        loaded(repo, apiJar)(previewTestLocal(repo, _, projects)).flatMap(
          report(_, say)
        )
      case Call.PreviewPublish(projects) =>
        loaded(repo, apiJar)(build =>
          ZIO.serviceWithZIO[Machine](machine => previewPublishLocal(repo, build, projects, machine.env))
        ).flatMap(report(_, say))
      case Call.PreviewClean(Nil) =>
        previewCleanAll(repo).flatMap(report(_, say))
      case Call.PreviewClean(projects) =>
        loaded(repo, apiJar)(previewCleanLocal(repo, _, projects)).flatMap(
          report(_, say)
        )
      case Call.PreviewCleanFull =>
        previewCleanFullLocal(repo).flatMap(report(_, say))
      case Call.PreviewRun(id, platform) =>
        loaded(repo, apiJar)(previewRunLocal(_, id, platform)).flatMap(
          report(_, say)
        )
      case Call.Resolve(action) =>
        ZIO.serviceWithZIO[Machine](machine => Calls.resolve(repo, apiJar, action, say, machine.native))
      case Call.Workflow(action) =>
        withBuild(repo, apiJar, say)(build => Calls.workflow(repo, build.graph, action, say))
      case Call.Toolchain =>
        toolchainLocal(repo, apiJar).flatMap(report(_, say))
      case Call.Version =>
        Calls.version(VersionText, say)
      case Call.CachePath =>
        Calls.cachePath(say)
      case Call.CacheList =>
        Calls.cacheList(say)
      case Call.CacheClean(dryRun) =>
        Calls.cacheClean(dryRun, say)
      case Call.LauncherUpdate =>
        Calls.launcherUpdate(repo, say)
      case Call.Cancel(id) =>
        Ref
          .make(Map.empty[RunId, Fiber[ServerError, Unit]])
          .flatMap(runs => Calls.cancel(runs, id, say))
      case Call.Reload =>
        Calls.finish(say, CliExit.LoadFailed.code, ServerError.NoServer.message)
      case Call.Reboot =>
        reboot(repo, apiJar).flatMap(report(_, say))
      case Call.Watch(_) =>
        Calls.finish(
          say,
          CliExit.Usage.code,
          CliParseError.WatchCommand("watch does not nest").message,
        )
      case Call.Bsp =>
        Calls.finish(say, CliExit.Usage.code, CliParseError.Rejected.message)

  private def withBuild(
      repo: Path,
      apiJar: Path,
      say: Say,
  )(
      use: histos.load.LoadedBuild => ZIO[Local, ServerError, Unit]
  ): ZIO[Local, ServerError, Unit] =
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .foldZIO(
        error => say(Message.Exit(CliExit.LoadFailed.code, error.message)),
        use,
      )

  private def compileSay(
      repo: Path,
      build: histos.load.LoadedBuild,
      projects: List[String],
      say: Say,
  ): ZIO[Machine & Resolve, ServerError, Unit] =
    ZIO.serviceWithZIO[Machine]: machine =>
      GraphCompile
        .jdk(machine)
        .foldZIO(
          error => say(Message.Exit(CliExit.CompileFailed.code, error.message)),
          jdk => Calls.compile(repo, build.graph, projects, jdk, machine, say),
        )

  private def testSay(
      repo: Path,
      build: histos.load.LoadedBuild,
      projects: List[String],
      patterns: List[String],
      quick: Boolean,
      say: Say,
  ): ZIO[Machine & Resolve, ServerError, Unit] =
    ZIO.serviceWithZIO[Machine]: machine =>
      GraphCompile
        .jdk(machine)
        .foldZIO(
          error =>
            val (code, message) =
              testFailure(ModuleTestError.Compile(error))
            say(Message.Exit(code, message))
          ,
          jdk =>
            Calls.test(
              repo,
              build.graph,
              projects,
              patterns,
              quick,
              jdk,
              machine,
              say,
            ),
        )

  private def runSay(
      repo: Path,
      build: histos.load.LoadedBuild,
      id: String,
      platform: Option[String],
      say: Say,
  ): ZIO[Machine & Resolve, ServerError, Unit] =
    ZIO.serviceWithZIO[Machine]: machine =>
      GraphCompile
        .jdk(machine)
        .foldZIO(
          error => say(Message.Exit(jdkExit(error), error.message)),
          jdk => Calls.run(repo, build.graph, jdk, machine, id, platform, say),
        )

  private def publishSay(
      repo: Path,
      build: histos.load.LoadedBuild,
      repository: String,
      projects: List[String],
      say: Say,
  ): ZIO[Machine & Resolve, ServerError, Unit] =
    ZIO.serviceWithZIO[Machine]: machine =>
      GraphCompile
        .jdk(machine)
        .foldZIO(
          error => say(Message.Exit(CliExit.PublishFailed.code, error.message)),
          jdk =>
            Calls.publish(
              repo,
              build.graph,
              machine,
              jdk,
              repository,
              projects,
              say,
            ),
        )

  private def assemblySay(
      repo: Path,
      build: histos.load.LoadedBuild,
      destination: Option[String],
      say: Say,
  ): ZIO[Machine & Resolve, ServerError, Unit] =
    ZIO.serviceWithZIO[Machine]: machine =>
      GraphCompile
        .jdk(machine)
        .foldZIO(
          error => say(Message.Exit(CliExit.CompileFailed.code, error.message)),
          jdk => Calls.assembly(repo, build.graph, jdk, machine, destination, say),
        )

  private def taskSay(
      repo: Path,
      build: histos.load.LoadedBuild,
      id: String,
      args: PluginArgs,
      say: Say,
  ): ZIO[Machine & Resolve & Loader & ContentStore, ServerError, Unit] =
    ZIO.serviceWithZIO[Machine]: machine =>
      Calls.task(repo, build.graph, build.plugins, machine, id, args, say)

  private def jdkExit(error: GraphCompileError): Int =
    error match
      case GraphCompileError.Lock(_) | GraphCompileError.LockRead(_) =>
        CliExit.ResolveFailed.code
      case _ => CliExit.CompileFailed.code

  private def report(done: Completed, say: Say): IO[ServerError, Unit] =
    val written =
      if done.out.isEmpty then ZIO.unit else say(Message.Out(done.out))
    written *> say(Message.Exit(done.exit.code, done.err))

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
          Completed(CliExit.LoadFailed, "", err.message, Playback.Held)
        case Right(loaded) =>
          Completed(
            CliExit.Success,
            GraphText.render(loaded.graph, loaded.plugins.links),
            "",
            Playback.Held,
          )

  private def run(
      repo: Path,
      apiJar: Path,
      id: TaskId,
      platform: Option[Platform],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    val call =
      if id == TaskId.Inspect || id == TaskId.Versions then Call.Task(id.value, PluginArgs.empty)
      else Call.Run(id.value, platform.map(_.token))
    attach(repo, call, locally(repo, apiJar, call))

  private def compile(
      repo: Path,
      apiJar: Path,
      projects: List[String],
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
        error =>
          ZIO.succeed(
            Completed(CliExit.LoadFailed, "", error.message, Playback.Held)
          ),
        use,
      )

  private def preview(
      repo: Path,
      apiJar: Path,
      projects: List[String],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(
      repo,
      Call.PreviewCompile(projects),
      loaded(repo, apiJar)(build => previewLocal(repo, build, projects)),
    )

  private def previewLocal(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      projects: List[String],
  ): UIO[Completed] =
    Plan
      .show(repo, loaded.graph, projects)
      .fold(
        error => Completed(CliExit.CompileFailed, "", error.message, Playback.Held),
        text => Completed(CliExit.Success, text, "", Playback.Held),
      )

  private def previewTest(
      repo: Path,
      apiJar: Path,
      projects: List[String],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(
      repo,
      Call.PreviewTest(projects),
      loaded(repo, apiJar)(build => previewTestLocal(repo, build, projects)),
    )

  private def previewTestLocal(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      projects: List[String],
  ): UIO[Completed] =
    Plan
      .test(repo, loaded.graph, projects)
      .fold(
        error => Completed(CliExit.TestFailed, "", error.message, Playback.Held),
        plan =>
          Completed(
            CliExit.Success,
            Plan.renderTest(repo, plan),
            "",
            Playback.Held,
          ),
      )

  private def previewPublish(
      repo: Path,
      apiJar: Path,
      projects: List[String],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    ZIO.serviceWithZIO[Machine]: machine =>
      attach(
        repo,
        Call.PreviewPublish(projects),
        loaded(repo, apiJar)(build => previewPublishLocal(repo, build, projects, machine.env)),
      )

  private def previewPublishLocal(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      projects: List[String],
      env: Map[String, String],
  ): UIO[Completed] =
    Plan
      .publish(repo, loaded.graph, projects, env)
      .fold(
        error => Completed(CliExit.PublishFailed, "", error.message, Playback.Held),
        plan =>
          Completed(
            CliExit.Success,
            Plan.renderPublish(plan),
            "",
            Playback.Held,
          ),
      )

  /** Empty names skip the graph. Nothing is deleted. */
  private def previewClean(
      repo: Path,
      apiJar: Path,
      projects: List[String],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(
      repo,
      Call.PreviewClean(projects),
      if projects.isEmpty then previewCleanAll(repo)
      else loaded(repo, apiJar)(previewCleanLocal(repo, _, projects)),
    )

  private def previewCleanAll(repo: Path): UIO[Completed] =
    val plan = Plan.cleanAll(repo)
    ZIO.succeed(
      Completed(
        CliExit.Success,
        Plan.renderClean(repo, plan),
        "",
        Playback.Held,
      )
    )

  private def previewCleanLocal(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      projects: List[String],
  ): UIO[Completed] =
    Plan
      .clean(repo, loaded.graph, projects)
      .fold(
        error => Completed(CliExit.TaskFailed, "", error.message, Playback.Held),
        plan =>
          Completed(
            CliExit.Success,
            Plan.renderClean(repo, plan),
            "",
            Playback.Held,
          ),
      )

  private def previewCleanFull(
      repo: Path
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(repo, Call.PreviewCleanFull, previewCleanFullLocal(repo))

  private def previewCleanFullLocal(repo: Path): UIO[Completed] =
    val plan = Plan.cleanFull(repo)
    ZIO.succeed(
      Completed(
        CliExit.Success,
        Plan.renderClean(repo, plan),
        "",
        Playback.Held,
      )
    )

  private def previewRun(
      repo: Path,
      apiJar: Path,
      id: String,
      platform: Option[Platform],
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(
      repo,
      Call.PreviewRun(id, platform.map(_.token)),
      loaded(repo, apiJar)(previewRunLocal(_, id, platform.map(_.token))),
    )

  private def previewRunLocal(
      loaded: histos.load.LoadedBuild,
      id: String,
      platform: Option[String],
  ): UIO[Completed] =
    val parsed = platform match
      case None      => Right(None)
      case Some(raw) =>
        Platform.from(raw) match
          case Left(error)  => Left(error.message)
          case Right(found) => Right(Some(found))
    ZIO.succeed:
      parsed match
        case Left(message) =>
          Completed(CliExit.TaskFailed, "", message, Playback.Held)
        case Right(choice) =>
          Plan.run(loaded.graph, id, choice) match
            case Left(error) =>
              Completed(CliExit.TaskFailed, "", error.message, Playback.Held)
            case Right(plan) =>
              Completed(
                CliExit.Success,
                Plan.renderRun(plan),
                "",
                Playback.Held,
              )

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

  private[histos] def testFailure(error: ModuleTestError): (Int, String) =
    (testExit(error).code, error.message)

  private def compileReport(error: GraphCompileError): String =
    error match
      case GraphCompileError.Failed(module, version, _, _) =>
        s"'${module.value}' ${version.value} failed to compile"
      case GraphCompileError.Cells(errors) =>
        val count = errors.size
        if count == 1 then "1 cell failed to compile"
        else s"$count cells failed to compile"
      case other => other.message

  private def publish(
      repo: Path,
      apiJar: Path,
      directory: String,
      projects: List[String],
  ): URIO[Local, Completed] =
    val call = Call.Publish(directory, projects)
    attach(repo, call, locally(repo, apiJar, call))

  /** Compile, then one subprocess per Scala version. `no tests` is success and is not a passing suite.
    */
  private def test(
      repo: Path,
      apiJar: Path,
      projects: List[String],
      patterns: List[SuitePattern],
      quick: Boolean,
  ): URIO[Local, Completed] =
    val call = Call.Test(projects, patterns.map(_.value), quick)
    attach(repo, call, locally(repo, apiJar, call))

  /** A missing lock is still a resolve failure. A test result, including a non-zero main, is not.
    */
  private def testExit(error: ModuleTestError): CliExit =
    error match
      case ModuleTestError.Compile(
            GraphCompileError.Lock(_) | GraphCompileError.LockRead(_)
          ) =>
        CliExit.ResolveFailed
      case ModuleTestError.Lock(_) | ModuleTestError.LockRead(_) =>
        CliExit.ResolveFailed
      case _ =>
        CliExit.TestFailed

  private def say(text: String): UIO[Unit] =
    Console.printLine(text).orElseSucceed(())

  private def sayError(text: String): UIO[Unit] =
    Console.printLineError(text).orElseSucceed(())

  private def cachePath: URIO[ContentStore, Completed] =
    ZIO.serviceWith[ContentStore](store => Completed(CliExit.Success, store.root.toString, "", Playback.Held))

  private def cacheList: URIO[ContentStore, Completed] =
    ZIO
      .serviceWithZIO[ContentStore](_.list)
      .mapBoth(
        err => Completed(CliExit.LoadFailed, "", err.message, Playback.Held),
        entries =>
          val text =
            if entries.isEmpty then ""
            else
              entries
                .map(entry => s"${entry.digest.value} ${entry.bytes}")
                .mkString("\n")
          Completed(CliExit.Success, text, "", Playback.Held),
      )
      .merge

  private def cacheClean(dryRun: Boolean): URIO[ContentStore, Completed] =
    ZIO
      .serviceWithZIO[ContentStore]: store =>
        Clock.instant.flatMap(now => store.clean(now, ContentStore.Retention, dryRun))
      .mapBoth(
        err => Completed(CliExit.LoadFailed, "", err.message, Playback.Held),
        entries =>
          val verb = if dryRun then "would remove" else "removed"
          val text =
            if entries.isEmpty then s"$verb nothing"
            else
              entries
                .map(entry => s"$verb ${entry.digest.value}")
                .mkString("\n")
          Completed(CliExit.Success, text, "", Playback.Held),
      )
      .merge

  /** No names skip the graph. A live server still holds the compile permit. */
  private def clean(
      repo: Path,
      apiJar: Path,
      projects: List[String],
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
        error => Completed(CliExit.TaskFailed, "", error.message, Playback.Held),
        _ =>
          Completed(
            CliExit.Success,
            histos.clean.Products.line(Nil),
            "",
            Playback.Held,
          ),
      )

  private def cleanNamed(
      repo: Path,
      build: histos.load.LoadedBuild,
      projects: List[String],
  ): UIO[Completed] =
    histos.clean.Products
      .named(repo, build.graph, projects)
      .fold(
        error =>
          Completed(
            CliExit.TaskFailed,
            "",
            histos.clean.Products.fault(error),
            Playback.Held,
          ),
        names =>
          Completed(
            CliExit.Success,
            histos.clean.Products.line(names),
            "",
            Playback.Held,
          ),
      )

  private def cleanFull(repo: Path): URIO[ContentStore, Completed] =
    attach(repo, Call.CleanFull, cleanFullLocal(repo))

  /** Products go first. A store failure is exit 3 and does not claim success.
    */
  private def cleanFullLocal(repo: Path): URIO[ContentStore, Completed] =
    histos.clean.Products
      .wipe(repo)
      .foldZIO(
        error =>
          ZIO.succeed(
            Completed(CliExit.TaskFailed, "", error.message, Playback.Held)
          ),
        _ =>
          ZIO
            .serviceWithZIO[ContentStore]: store =>
              Clock.instant.flatMap(now => store.forget(histos.cache.CacheProject.of(repo), now))
            .fold(
              error => Completed(CliExit.LoadFailed, "", error.message, Playback.Held),
              entries =>
                Completed(
                  CliExit.Success,
                  histos.clean.Products.forgotten(entries),
                  "",
                  Playback.Held,
                ),
            ),
      )

  private def resolve(
      repo: Path,
      apiJar: Path,
      action: ResolveAction,
  ): URIO[Loader & Scope & ContentStore & Resolve & Machine, Completed] =
    attach(
      repo,
      Call.Resolve(Operation.resolve(action)),
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
              Completed(CliExit.LoadFailed, "", error.message, Playback.Held)
            case error: ResolveError =>
              Completed(CliExit.ResolveFailed, "", error.message, Playback.Held)
          },
          text => Completed(CliExit.Success, text, "", Playback.Held),
        )

  private def workflow(
      repo: Path,
      apiJar: Path,
      action: WorkflowAction,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    attach(
      repo,
      Call.Workflow(Operation.workflow(action)),
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
            Completed(CliExit.LoadFailed, "", error.message, Playback.Held)
          )
        case Right(loaded) =>
          Calls
            .workflowWork(repo, loaded.graph, action)
            .fold(
              fault =>
                Completed(
                  CliExit.WorkflowFailed,
                  "",
                  fault.message,
                  Playback.Held,
                ),
              file => Completed(CliExit.Success, file.toString, "", Playback.Held),
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
                err =>
                  Completed(
                    CliExit.ToolchainFailed,
                    "",
                    err.message,
                    Playback.Held,
                  ),
                found =>
                  Completed(
                    CliExit.Success,
                    Toolchain.text(found),
                    "",
                    Playback.Held,
                  ),
              )
              .merge
          case Left(error) =>
            ZIO.succeed(
              Completed(CliExit.LoadFailed, "", error.message, Playback.Held)
            )
          case Right(loaded) =>
            Survey
              .read(machine, loaded.graph)
              .mapBoth(
                err =>
                  Completed(
                    CliExit.ToolchainFailed,
                    Survey.shown(err),
                    err.message,
                    Playback.Held,
                  ),
                found =>
                  Completed(
                    CliExit.Success,
                    Survey.text(found),
                    "",
                    Playback.Held,
                  ),
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
          remote(socket, Call.Reload).foldZIO(
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
        error => Completed(CliExit.ResolveFailed, "", error.message, Playback.Held),
        pin =>
          Completed(
            CliExit.Success,
            s"${pin.revision} ${pin.sha256.value}",
            "",
            Playback.Held,
          ),
      )

  /** The id is the run id the server minted. No server has no runs. */
  private def cancel(repo: Path, id: String): UIO[Completed] =
    attach(
      repo,
      Call.Cancel(id),
      Ref
        .make(Map.empty[RunId, Fiber[ServerError, Unit]])
        .flatMap(runs => recorded(Calls.cancel(runs, id, _))),
    )

  private[cli] def recorded[R](
      run: Say => ZIO[R, ServerError, Unit]
  ): URIO[R, Completed] =
    Ref.make(Chunk.empty[Feed]).flatMap { feeds =>
      run(speaking(feeds)).foldCauseZIO(
        cause =>
          if cause.isInterruptedOnly then ZIO.succeed(Completed(CliExit.Interrupted, "", "", Playback.Heard))
          else
            cause.failureOrCause match
              case Left(error) =>
                ZIO.succeed(
                  Completed(
                    CliExit.LoadFailed,
                    "",
                    error.message,
                    Playback.Held,
                  )
                )
              case Right(rest) => ZIO.failCause(rest),
        _ =>
          feeds.get.flatMap: chunk =>
            val done = completed(chunk)
            if chunk.collectFirst { case Feed.Exit(_, _) => () }.isDefined then ZIO.succeed(done)
            else sayError("the call did not finish").as(done),
      )
    }

  /** Print, then append, so the line is on the console before the next message.
    */
  private def speaking(feeds: Ref[Chunk[Feed]]): Say =
    new Say({
      case Message.Out(text) =>
        say(text) *> feeds.update(_ :+ Feed.Out(text))
      case Message.Err(text) =>
        sayError(text) *> feeds.update(_ :+ Feed.Err(text))
      case Message.Exit(code, err) =>
        val shown = if err.isEmpty then ZIO.unit else sayError(err)
        shown *> feeds.update(_ :+ Feed.Exit(code, err))
      case _ => ZIO.unit
    })

  private def completed(feeds: Chunk[Feed]): Completed =
    val out      = feeds.collect { case Feed.Out(text) => text }.mkString("\n")
    val errLines = feeds.collect { case Feed.Err(text) => text }.toList
    val exits    = feeds.collect { case Feed.Exit(code, message) =>
      (code, message)
    }.toList
    exits.reverse match
      case (code, message) :: _ =>
        val err =
          if message.isEmpty then errLines.mkString("\n")
          else if errLines.isEmpty then message
          else s"${errLines.mkString("\n")}\n$message"
        Completed(CliExit.fromCode(code), out, err, Playback.Heard)
      case Nil =>
        Completed(
          CliExit.TaskFailed,
          out,
          "the call did not finish",
          Playback.Heard,
        )

  /** A running server replaces itself. No server starts one. */
  private[histos] def reboot(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Resolve & Machine, Completed] =
    Attach
      .decide(repo, VersionText)
      .flatMap:
        case Decision.Open(socket) =>
          remote(socket, Call.Reboot).foldZIO(
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

  private def serve(
      repo: Path,
      apiJar: Path,
  ): URIO[Loader & Scope & ContentStore & Resolve & Machine, Completed] =
    ZIO.environmentWithZIO[Machine & Resolve & ContentStore] { env =>
      val machine = env.get[Machine]
      ZIO.serviceWithZIO[Loader] { loader =>
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
                          Daemon
                            .open(
                              repo,
                              loaded.graph,
                              jdk,
                              VersionText,
                              apiJar,
                              classpath,
                              loader
                                .reportBuild(repo, apiJar)
                                .provideEnvironment(env),
                              loaded.plugins,
                              machine,
                            )
                            .flatMap { running =>
                              say(
                                running.record.socket.toString
                              ) *> running.done
                            }
                            .foldCauseZIO(
                              cause =>
                                cause.failureOrCause match
                                  case Left(error) =>
                                    ZIO.succeed(
                                      Completed(
                                        CliExit.LoadFailed,
                                        "",
                                        error.message,
                                        Playback.Held,
                                      )
                                    )
                                  case Right(rest) => ZIO.failCause(rest)
                              ,
                              _ =>
                                ZIO.succeed(
                                  Completed(
                                    CliExit.Success,
                                    "",
                                    "",
                                    Playback.Held,
                                  )
                                ),
                            ),
                      ),
                ),
          )
      }
    }

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
          remote(socket, call).foldZIO(
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
      socket: java.nio.file.Path,
      call: Call,
  ): ZIO[Any, ServerError, Completed] =
    zio.Ref.make(Completed(CliExit.Success, "", "", Playback.Held)).flatMap { result =>
      Client.talk(socket, call).foreach {
        case histos.server.Feed.Out(text)       => say(text)
        case histos.server.Feed.Err(text)       => sayError(text)
        case histos.server.Feed.Exit(code, err) =>
          result.set(
            Completed(CliExit.fromCode(code), "", err, Playback.Held)
          )
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

  private def speak(session: Bsp.Session): ZIO[Resolve, Frame.FrameError, Int] =
    val input                                     = java.lang.System.in
    val output                                    = java.lang.System.out
    def loop: ZIO[Resolve, Frame.FrameError, Int] =
      Frame
        .read(input)
        .flatMap:
          case None          => ZIO.succeed(1)
          case Some(payload) =>
            session
              .answer(payload)
              .flatMap:
                case Bsp.Answer.Reply(text) => Frame.write(output, text) *> loop
                case Bsp.Answer.Quiet       => loop
                case Bsp.Answer.Stop(code)  => ZIO.succeed(code)
    loop

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
  ): ZIO[Loader & Scope, String, Call] =
    tokens match
      case Nil =>
        ZIO.fail(CliParseError.WatchCommand("watch needs a command").message)
      case _ =>
        Grammar
          .parse(tokens)
          .either
          .flatMap:
            case Left(CliParseError.UnknownRoot) =>
              ZIO
                .serviceWithZIO[Loader](_.load(repo, apiJar))
                .mapError(_.message)
                .flatMap: loaded =>
                  taskFrom(loaded, tokens) match
                    case TaskParsed.Usage(message) => ZIO.fail(message)
                    case TaskParsed.Ready(task)    => ZIO.succeed(Call.Watch(task))
            case Left(error)    => ZIO.fail(error.message)
            case Right(command) =>
              ZIO.fromEither(
                Operation
                  .operation(command)
                  .map(Call.Watch(_))
                  .left
                  .map(_.message)
              )

  private def taskFrom(
      loaded: histos.load.LoadedBuild,
      tokens: List[String],
  ): TaskParsed =
    val commands = loaded.graph.plugins.flatMap: plugin =>
      plugin.contribution.commands.map(BoundCommand(plugin.id, _))
    PluginCommands.parse(commands, tokens) match
      case Left(_)                 => TaskParsed.Usage(PluginCommands.usage(commands))
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
