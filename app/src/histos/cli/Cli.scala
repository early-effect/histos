package histos.cli

import heddle.LinePipe
import histos.ApiClock
import histos.doctor.FaultText
import histos.bsp.{Bsp, Connection, Frame}
import histos.central.{PortalServer, ReleaseFeed}
import histos.build.{Beat, Build, BuildEvent}
import histos.load.LoadedBuild
import histos.server.{
  AssemblyMark,
  Attach,
  Call,
  Calls,
  Client,
  Daemon,
  Decision,
  Feed,
  LauncherAction,
  Message,
  Perform,
  ResolveAction,
  Say,
  ServerError,
  ServerIo,
  WatchPolicy,
  Watching,
}
import histos.load.BuildClasspath
import histos.cache.{CacheCleanMode, ContentStore}
import histos.schedule.{CacheConfig, CacheNamespace, NamespaceError}
import histos.jdk.{HostSupport, Machine, Os, Toolchain, ToolchainError}
import histos.load.{LoadError, Loader}
import histos.plugin.{FlagNameError, PluginArgs, PluginTaskError, PluginTaskId}
import histos.preview.{Plan, PreviewError}
import histos.program.ProgramError
import histos.publish.{LauncherError, LauncherPin, UpgradeNotice}
import histos.scaffold.{ImportSource, Provision, ProvisionError, ScaffoldKind, Skeleton}
import histos.report.{BuildFault, CallResult, Rejection, Report, Severity}
import histos.select.SelectError
import histos.toolchain.{Runtimes, Survey}
import histos.mcp.HistosMcp
import histos.resolve.{Resolve, ResolveError}
import histos.task.{RunId, TaskError, TaskId}
import histos.workflow.{Affected, WorkflowAction}
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
  case Test(projects: List[ProjectName], affected: AffectedChoice = AffectedChoice.Unset)
  case Coverage(
      projects: List[ProjectName],
      floors: histos.CoverageFloors,
      excludes: histos.coverage.Excludes,
  )
  case Affected(base: String)
  case TestOnly(modules: List[ProjectName], patterns: List[SuitePattern])
  case TestQuick(modules: List[ProjectName], patterns: List[SuitePattern])
  case Watch(command: Command, args: List[String])
  case Compile(projects: List[ProjectName])
  case Client
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
  case Fmt(targets: histos.format.FormatTargets, mode: histos.format.FormatMode)
  case Launcher(action: LauncherAction)
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
  case New(name: String, bare: Boolean, yes: Boolean, branch: Option[String])
  case Init(
      bare: Boolean,
      yes: Boolean,
      source: Option[ImportSource],
      branch: Option[String],
  )
  case Doctor(
      area: Option[String],
      json: Boolean,
      preflight: Option[String],
      fix: Boolean,
      yes: Boolean,
      verbose: Boolean,
      projects: List[ProjectName],
  )
  case Explain(id: String)
  case Deps(action: DepsAction)
  case Setup(from: Option[String], fix: Boolean, yes: Boolean)
  case Bump(part: histos.version.BumpPart, ships: List[histos.version.ShipName])
  case Help(words: List[String])

/** Why `histos fmt` arguments are not a format request. */
enum FmtUsage:
  case NoFiles
  case Flag(raw: String)
  case Absolute(raw: String)
  case BadPath(raw: String)

  def message: String = this match
    case NoFiles       => "fmt --file names at least one file"
    case Flag(raw)     => s"fmt does not take $raw\nhistos help fmt"
    case Absolute(raw) => s"fmt --file takes a path relative to the repository, not $raw"
    case BadPath(raw)  => s"'$raw' is not a path"
end FmtUsage

enum DepsAction:
  case Check
  case Update(yes: Boolean, security: Boolean)
  case Audit(report: Boolean)
  case Pr(security: Boolean, open: Boolean)
  case Alert
  case Snapshot(submit: Boolean)

enum CliParseError:
  case Help
  case UnknownCommand(token: String, near: List[String])
  case UnknownRoot(token: String)
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
  case Fmt(usage: FmtUsage)

  def message: String = this match
    case Help                        => histos.cli.Help.grouped
    case UnknownCommand(token, near) =>
      histos.cli.Help.unknown(token, if near.isEmpty then histos.cli.Help.names else near)
    case UnknownRoot(token)   => histos.cli.Help.unknown(token)
    case MissingArgument      => "run needs one name"
    case UnexpectedArguments  => "unexpected arguments\nhistos help"
    case BadTaskId            => "a task id is not empty"
    case BadPluginTask(error) => error.message
    case BadPlatform(error)   => error.message
    case TaskPlatform(name)   => s"run $name does not take a platform"
    case MissingPattern       => "testOnly needs a pattern"
    case Pattern(detail)      => detail
    case ProjectName(error)   => error.message
    case SuitePattern(error)  => error.message
    case RunId(error)         => error.message
    case Flag(error)          => error.message
    case Namespace(error)     => error.message
    case WatchCommand(detail) => detail
    case PublishForm          =>
      "publish is snapshots, release, assets, promote, or drop"
    case Rejected   => "usage: histos help"
    case Fmt(usage) => usage.message

  def diagnose: histos.doctor.Diagnostic = this match
    case Help =>
      histos.doctor.Noted.error(
        "cli.help",
        "histos help",
        histos.cli.Help.grouped,
        histos.doctor.Noted.command("histos help"),
      )
    case UnknownCommand(token, near) =>
      val summary = if token.isEmpty then "unknown command" else s"unknown command '$token'"
      val explain = near match
        case Nil        => "histos help lists the commands.\nhistos help <command> prints one."
        case one :: Nil => s"did you mean $one?\nhistos help\nhistos help $one"
        case many       => s"did you mean ${many.mkString(" or ")}?\nhistos help"
      val fix = near.headOption match
        case Some(name) => histos.doctor.Noted.command(s"histos help $name")
        case None       => histos.doctor.Noted.command("histos help")
      histos.doctor.Noted.error("cli.unknown-command", summary, explain, fix)
    case UnknownRoot(token) =>
      histos.doctor.Noted.error(
        "cli.unknown-command",
        if token.isEmpty then "unknown command" else s"unknown command '$token'",
        histos.cli.Help.unknown(token),
        histos.doctor.Noted.command("histos help"),
      )
    case Rejected =>
      histos.doctor.Noted.carry("cli.usage", message, "histos help lists the commands.", "histos help")
    case other =>
      histos.doctor.Noted.carry("cli.usage", other.message, other.message, "histos help")
end CliParseError

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
  /** The version this process runs, as `histos version` prints it. The shim passes the toolchain it resolved in
    * `HISTOS_VERSION`, and the native client compares the server record with that same text, so a server and a command
    * under one `-SNAPSHOT` pin agree. Without it, the pin line. A pin that does not parse still fails.
    */
  def versionText(repo: Path): IO[LauncherError, String] =
    LauncherPin
      .read(repo)
      .flatMap: pin =>
        zio.System
          .env("HISTOS_VERSION")
          .orElseSucceed(None)
          .map:
            case Some(passed) if passed.trim.nonEmpty => passed.trim
            case _                                    => s"histos ${pin.version}"

  /** A missing file is no run version. A bad line fails. */
  private def versionOf(repo: Path): IO[LauncherError, Option[String]] =
    versionText(repo)
      .map(Some(_))
      .catchSome:
        case LauncherError.Absent(_) => ZIO.succeed(None)

  val usageText: String =
    "usage: histos (version | graph | run <name> [jvm|js|native] | test [--affected <json>] [project ...] | coverage [--min-stmt <n>] [--min-branch <n>] [--min-stmt-file <n>] [--min-branch-file <n>] [--min-stmt-package <n>] [--min-branch-package <n>] [--exclude-class <regex>] [--exclude-file <regex>] [project ...] | affected --base <sha> | testOnly [--module project ...] <pattern> ... | testQuick [--module project ...] [pattern ...] | ~ <command> | compile [project ...] | client | clean [project ...] | cleanFull | assembly [path] | publishLocal [token ...] | publish snapshots [token ...] | publish release [token ...] [--automatic] | publish assets [token ...] | publish promote <deployment> | publish drop <deployment> | observe | mcp | bsp | server | portal | reload | reboot | cancel <id> | new [--bare] [--yes] [--branch <name>] <name> | init [--bare] [--yes] [--from sbt|zipx] [--branch <name>] | toolchain | cache path | cache list | cache clean [--dry-run] | cache promote --from <namespace> --to <namespace> | resolve [--update | --check] | workflow [--check | --update] | launcher (--update | --latest) | --explain <check-id> | doctor [project ...] [--area <area>] [--json] [--preflight <job>] [--verbose] [--fix] [--yes] | deps (check | update [--yes] [--security] | audit [--report] | pr [--security] [--open] | alert | snapshot [--submit]) | setup supply-chain [--from zipx] [--fix] [--yes] | preview compile [project ...] | preview test [project ...] | preview publishLocal [token ...] | preview publish snapshots [token ...] | preview clean [project ...] | preview cleanFull | preview run <name> [jvm|js|native])"

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
          case CliParseError.UnknownRoot(_) => contributed(args, repo, apiJar)
          case detail                       =>
            ZIO.succeed(
              Completed(CliExit.Usage, "", FaultText.show(detail.diagnose), Playback.Held)
            )
        },
        command => perform(repo, apiJar, command),
      )

  /** `new` and `init` write a project. They do not talk to the server. Full also loads the graph it just wrote. */
  private def fresh(
      parent: Path,
      apiJar: Path,
      name: Option[String],
      bare: Boolean,
      yes: Boolean,
      source: Option[ImportSource],
      branch: Option[String],
  ): URIO[Loader & ContentStore & Machine & Resolve, Completed] =
    ZIO.serviceWithZIO[Machine]: machine =>
      kindOf(bare, yes).flatMap: kind =>
        System.envs
          .orElseSucceed(Map.empty)
          .flatMap: env =>
            System
              .env("HISTOS_VERSION")
              .orElseSucceed(None)
              .flatMap: raw =>
                val request = Provision.Request(kind, source, branch, askBranch(yes, branch), env)
                val written =
                  ZIO.fromEither(Skeleton.versionOf(raw)).mapError(ProvisionError.Files(_)).flatMap { version =>
                    name match
                      case Some(project) => Provision.create(parent, project, version, apiJar, machine, request)
                      case None          => Provision.init(parent, version, apiJar, machine, request)
                  }
                written.fold(
                  error => Completed(CliExit.Usage, "", error.message, Playback.Held),
                  report => Completed(CliExit.Success, report.out, "", Playback.Held),
                )

  private def kindOf(bare: Boolean, yes: Boolean): UIO[ScaffoldKind] =
    if bare || yes || java.lang.System.console() == null then
      ZIO.succeed(if bare then ScaffoldKind.Bare else ScaffoldKind.Full)
    else
      ZIO
        .attemptBlocking:
          val console = java.lang.System.console()
          val read    =
            if console == null then ""
            else
              val line = console.readLine("Write CI, a lock, .gitignore, and ./histos? [Y/n] ")
              if line == null then "" else line
          val answer = read.trim.toLowerCase
          if answer == "n" || answer == "no" || answer == "bare" then ScaffoldKind.Bare else ScaffoldKind.Full
        .orElseSucceed(ScaffoldKind.Full)

  private def askBranch(yes: Boolean, branch: Option[String]): UIO[String] =
    branch match
      case Some(name)                                        => ZIO.succeed(name)
      case None if yes || java.lang.System.console() == null => ZIO.succeed(Provision.DefaultBranch)
      case None                                              =>
        ZIO
          .attemptBlocking:
            val console = java.lang.System.console()
            val read    =
              if console == null then ""
              else
                val line = console.readLine(s"CI branch [${Provision.DefaultBranch}]: ")
                if line == null then "" else line
            if read.trim.isEmpty then Provision.DefaultBranch else read.trim
          .orElseSucceed(Provision.DefaultBranch)

  private def explain(raw: String): Completed =
    histos.doctor.ToolchainExplain.text(raw) match
      case Some(body) => Completed(CliExit.Success, body, "", Playback.Held)
      case None       =>
        Completed(
          CliExit.Usage,
          "",
          s"no explanation for '$raw'. Known checks: ${histos.doctor.ToolchainExplain.known}.",
          Playback.Held,
        )

  private def stamped: UIO[Long] =
    ZIO.attempt(java.time.Instant.now().toEpochMilli).orDie

  private def printed(console: java.io.PrintStream)(line: String): UIO[Unit] =
    ZIO
      .attempt:
        console.println(line)
        console.flush()
      .ignore

  private def cover(
      repo: Path,
      apiJar: Path,
      machine: histos.jdk.Machine,
      asked: Command.Coverage,
      at: Long,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .either
      .flatMap:
        case Left(error) =>
          ZIO.succeed(Completed(CliExit.Failed, "", error.message, Playback.Held))
        case Right(loaded) =>
          histos.zinc.GraphCompile
            .jdk(machine)
            .either
            .flatMap:
              case Left(error) =>
                ZIO.succeed(Completed(CliExit.Failed, "", error.message, Playback.Held))
              case Right(jdk) =>
                histos.toolchain.Runtimes
                  .read(machine, loaded.graph.runnerTools.nodePath)
                  .flatMap: runtimes =>
                    coverageArgs(loaded.graph, asked.floors, asked.excludes) match
                      case Left(error) =>
                        ZIO.succeed(Completed(CliExit.Usage, "", error.message, Playback.Held))
                      case Right(floors, excludes) =>
                        histos.coverage.Run
                          .execute(
                            histos.coverage.CoverageRequest(
                              repo,
                              loaded.graph,
                              asked.projects,
                              jdk,
                              runtimes,
                              floors,
                              excludes,
                              at,
                            ),
                            histos.coverage.CoverageLines(
                              out = printed(java.lang.System.out),
                              err = printed(java.lang.System.err),
                            ),
                          )
                          .fold(
                            failure => Completed(coverageExit(failure), "", failure.message, Playback.Held),
                            {
                              case histos.coverage.CoverageResult.Met(text) =>
                                Completed(CliExit.Success, text, "", Playback.Held)
                              case histos.coverage.CoverageResult.Missed(text) =>
                                Completed(CliExit.Failed, text, "", Playback.Held)
                            },
                          )

  /** Build floors fill the flags this run omitted. Build excludes come first, then this run's regexes. */
  private[cli] def coverageArgs(
      graph: histos.ProjectGraph,
      floors: histos.CoverageFloors,
      excludes: histos.coverage.Excludes,
  ): Either[histos.coverage.CoverageError, (histos.CoverageFloors, histos.coverage.Excludes)] =
    val (chosen, raw) =
      graph.coverageRequest(floors, histos.CoverageExcludes(excludes.classlikes, excludes.files))
    histos.coverage.Excludes.check(raw.classlikes, raw.files).map(checked => (chosen, checked))

  private def coverageExit(failure: histos.coverage.CoverageFailure): CliExit =
    failure match
      case histos.coverage.CoverageFailure.Selected(_) => CliExit.Usage
      case histos.coverage.CoverageFailure.Resolved(_) | histos.coverage.CoverageFailure.Compiled(_) |
          histos.coverage.CoverageFailure.Test(_) | histos.coverage.CoverageFailure.Reported(_) |
          histos.coverage.CoverageFailure.Instrument(_) | histos.coverage.CoverageFailure.Io(_) =>
        CliExit.Failed

  /** Rewrite `.next` past the newest stable. The pin in `build.properties` stays. */
  private def bump(
      repo: Path,
      apiJar: Path,
      part: histos.version.BumpPart,
      ships: List[histos.version.ShipName],
  ): URIO[Loader & Scope, Completed] =
    val source = repo.resolve("histos.scala")
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .either
      .flatMap:
        case Left(error) =>
          ZIO.succeed(Completed(CliExit.Failed, "", error.message, Playback.Held))
        case Right(loaded) =>
          ZIO
            .attemptBlocking(java.nio.file.Files.readString(source))
            .mapError(_ => histos.version.BumpFault.Io(source.toString))
            .flatMap: text =>
              histos.version.ShipBumpRun.run(
                loaded.graph,
                part,
                ships,
                text,
                module => histos.version.CoursierStables.list(module, loaded.graph.resolvers),
              )
            .foldZIO(
              fault => ZIO.succeed(Completed(bumpExit(fault), "", fault.message, Playback.Held)),
              report =>
                report.source match
                  case None =>
                    ZIO.succeed(Completed(CliExit.Success, report.text, "", Playback.Held))
                  case Some(next) =>
                    ZIO
                      .attemptBlocking(java.nio.file.Files.writeString(source, next))
                      .mapError(_ => histos.version.BumpFault.Io(source.toString))
                      .foldZIO(
                        fault => ZIO.succeed(Completed(CliExit.Failed, "", fault.message, Playback.Held)),
                        _ => ZIO.succeed(Completed(CliExit.Success, report.text, "", Playback.Held)),
                      ),
            )

  private def bumpExit(fault: histos.version.BumpFault): CliExit =
    fault match
      case histos.version.BumpFault.MissingShip(_) => CliExit.Usage
      case _                                       => CliExit.Failed

  private def doctor(
      repo: Path,
      apiJar: Path,
      area: Option[String],
      json: Boolean,
      preflight: Option[String],
      fix: Boolean,
      yes: Boolean,
      verbose: Boolean,
      projects: List[ProjectName],
      machine: histos.jdk.Machine,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .either
      .flatMap:
        case Left(error) =>
          ZIO.succeed(Completed(CliExit.Failed, "", error.message, Playback.Held))
        case Right(loaded) =>
          histos.doctor.DoctorLaunch.modules(loaded.graph, projects) match
            case Left(error) =>
              ZIO.succeed(Completed(CliExit.Usage, "", FaultText.show(error.diagnose), Playback.Held))
            case Right(launch) =>
              histos.supply.DoctorSurface
                .run(
                  repo,
                  loaded.graph,
                  histos.supply.DoctorSurface.Request(area, json, preflight, fix, yes, launch, verbose),
                  machine,
                )
                .map: (code, text) =>
                  val exit = if code == 0 then CliExit.Success else CliExit.Failed
                  Completed(exit, text, "", Playback.Held)

  private def deps(
      repo: Path,
      apiJar: Path,
      action: DepsAction,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .either
      .flatMap:
        case Left(error) =>
          ZIO.succeed(Completed(CliExit.Failed, "", error.message, Playback.Held))
        case Right(loaded) =>
          val policy = loaded.graph.supply
          val net    = histos.supply.Net.live
          val today  = histos.supply.Date.trusted(java.time.LocalDate.now(java.time.ZoneOffset.UTC).toString)
          val token  = sys.env.get("GH_TOKEN").filter(_.nonEmpty).orElse(sys.env.get("GITHUB_TOKEN").filter(_.nonEmpty))
          val effect = action match
            case DepsAction.Check =>
              histos.supply.DepsSurface.check(repo, policy, net, today)
            case DepsAction.Update(yes, security) =>
              histos.supply.DepsSurface.update(repo, policy, net, today, yes, security)
            case DepsAction.Audit(report) =>
              histos.supply.DepsSurface.audit(repo, report, net)
            case DepsAction.Pr(security, open) =>
              histos.supply.DepsSurface.pullRequest(repo, policy, net, token, security, open)
            case DepsAction.Alert =>
              histos.supply.DepsSurface.alert(repo, policy, net, token)
            case DepsAction.Snapshot(submit) =>
              histos.supply.DepsSurface.snapshot(repo, net, token, submit)
          effect.map: result =>
            val exit =
              if result.code == 0 then CliExit.Success
              else if result.code == 2 then CliExit.Usage
              else CliExit.Failed
            Completed(exit, result.text, "", Playback.Held)

  private def setup(repo: Path, from: Option[String], fix: Boolean, yes: Boolean): Completed =
    val (code, text) = histos.supply.SetupSurface.run(repo, from, fix, yes)
    val exit         = if code == 0 then CliExit.Success else if code == 2 then CliExit.Usage else CliExit.Failed
    Completed(exit, text, "", Playback.Held)

  /** The process entry. A socket hit names none of the compile services. Each miss provides only what that miss uses.
    */
  private[histos] def launch(args: List[String], repo: Path): URIO[Scope, Completed] =
    System.property("os.name").orElseSucceed(None).flatMap {
      case Some(name) if Os.from(name) == Os.Windows =>
        ZIO.succeed(Completed(CliExit.Usage, "", HostSupport.refused, Playback.Held))
      case _ =>
        UpgradeNotice
          .advice(repo)
          .provideLayer(ReleaseFeed.live)
          .flatMap { lag =>
            ZIO.foreachDiscard(lag)(found => ZIO.succeed(java.lang.System.err.println(found.text)))
          } *>
          Connection.ensure(repo).ignore *>
          parse(args).foldZIO(
            {
              case CliParseError.UnknownRoot(_) =>
                inProcess(repo)((api, machine) => withLocal(machine)(contributed(args, repo, api)))
              case detail =>
                ZIO.succeed(Completed(CliExit.Usage, "", FaultText.show(detail.diagnose), Playback.Held))
            },
            command => route(repo, command),
          )
    }

  private def perform(
      repo: Path,
      apiJar: Path,
      command: Command,
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    command match
      case Command.Version =>
        versionText(repo).foldZIO(
          error => ZIO.succeed(held("", CallResult.Fault(BuildFault.Launcher(error)))),
          text =>
            ApiClock.versionLines(repo, text).flatMap { shown =>
              attach(
                repo,
                Call.Version,
                ZIO.succeed(held(shown, CallResult.reported())),
              )
            },
        )
      case Command.Graph                  => graph(repo, apiJar)
      case Command.Run(id, platform)      => run(repo, apiJar, id, platform)
      case Command.Plugin(id, pluginArgs) =>
        plugin(repo, apiJar, id, pluginArgs)
      case Command.Test(projects, affected) =>
        AffectedChoice.select(projects, affected) match
          case None =>
            ZIO.succeed(Completed(CliExit.Success, "", "", Playback.Held))
          case Some(names) =>
            test(repo, apiJar, names, Nil, TestMode.Run)
      case asked: Command.Coverage =>
        ZIO.serviceWithZIO[Machine]: machine =>
          stamped.flatMap(at => cover(repo, apiJar, machine, asked, at))
      case Command.Affected(base) =>
        affected(repo, apiJar, base)
      case Command.TestOnly(modules, patterns) =>
        test(repo, apiJar, modules, patterns, TestMode.Run)
      case Command.TestQuick(modules, patterns) =>
        test(repo, apiJar, modules, patterns, TestMode.Quick)
      case Command.Watch(inner, _)   => watch(repo, apiJar, inner)
      case Command.Compile(projects) => compile(repo, apiJar, projects)
      case Command.Client            =>
        attach(repo, Call.Client, locally(repo, apiJar, Call.Client))
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
      case Command.Resolve(action)    => resolve(repo, apiJar, action)
      case Command.Workflow(action)   => workflow(repo, apiJar, action)
      case Command.Fmt(targets, mode) =>
        val call = Call.Format(targets, mode)
        attach(repo, call, locally(repo, apiJar, call))
      case Command.Launcher(action) =>
        attach(repo, Call.Launcher(action), launcherLocal(repo, action))
      case Command.Reload                   => reload(repo)
      case Command.Reboot                   => reboot(repo)
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
      case Command.Observe                      => observe(repo)
      case Command.New(name, bare, yes, branch) =>
        fresh(repo, apiJar, Some(name), bare, yes, None, branch)
      case Command.Init(bare, yes, source, branch) =>
        fresh(repo, apiJar, None, bare, yes, source, branch)
      case Command.Doctor(area, json, preflight, fix, yes, verbose, projects) =>
        ZIO.serviceWithZIO[histos.jdk.Machine](machine =>
          doctor(repo, apiJar, area, json, preflight, fix, yes, verbose, projects, machine)
        )
      case Command.Bump(part, ships) =>
        bump(repo, apiJar, part, ships)
      case Command.Explain(id) =>
        ZIO.succeed(explain(id))
      case Command.Deps(action) =>
        deps(repo, apiJar, action)
      case Command.Setup(from, fix, yes) =>
        ZIO.succeed(setup(repo, from, fix, yes))
      case Command.Help(words) =>
        ZIO.succeed(answered(words))

  private def answered(words: List[String]): Completed =
    Help.render(words) match
      case Right(text) => Completed(CliExit.Success, s"$text\n", "", Playback.Held)
      case Left(text)  => Completed(CliExit.Usage, "", text, Playback.Held)

  /** Loopback rehearsal of the Central portal. Does not load the graph. */
  private def portal(repo: Path): URIO[Machine & Scope, Completed] =
    ZIO.serviceWithZIO[Machine] { machine =>
      PortalServer.credentials(machine.env) match
        case Left(error) =>
          ZIO.succeed(Completed(CliExit.Failed, "", FaultText.show(error.diagnose), Playback.Held))
        case Right(secret) =>
          PortalServer
            .serve(repo.resolve(".histos").resolve("portal"), secret)
            .foldZIO(
              error => ZIO.succeed(Completed(CliExit.Failed, "", FaultText.show(error.diagnose), Playback.Held)),
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
            .printLineError(FaultText.show(error.diagnose))
            .ignore
            .as(
              Completed(CliExit.Failed, "", FaultText.show(error.diagnose), Playback.Heard)
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
        ZIO.succeed(Completed(CliExit.Usage, "", FaultText.show(error.diagnose), Playback.Held))
      case Right(call) =>
        attach(repo, Call.Watch(call, WatchPolicy.After), watchLocal(repo, apiJar, call, WatchPolicy.After))

  private def policyOf(args: List[String]): WatchPolicy =
    if args.headOption.contains("--restart") then WatchPolicy.Restart else WatchPolicy.After

  private def watchLocal(
      repo: Path,
      apiJar: Path,
      inner: Call,
      policy: WatchPolicy,
  ): URIO[Local, Completed] =
    Ref.make(Completed(CliExit.Success, "", "", Playback.Held)).flatMap { result =>
      val printing = new Say({
        case Message.Event(event) =>
          CompileConsole.show(Show(repo, event), say, sayError)
        case Message.Done(done) =>
          val finished = done match
            case CallResult.Reported(_) =>
              Completed(CliExit.from(done), "", "", Playback.Heard)
            case CallResult.Fault(fault) =>
              Completed(
                CliExit.from(done),
                "",
                FaultText.show(fault.diagnose),
                Playback.Held,
              )
          result.set(finished)
        case _ => ZIO.unit
      })
      val roots = ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.map {
        case Left(_)       => Watching.Watched(List(repo.resolve("histos.scala")), Set.empty)
        case Right(loaded) => Watching.watched(repo, inner, loaded.graph)
      }
      val loop = policy match
        case WatchPolicy.Restart =>
          Watching.beside(
            printing,
            operate(repo, apiJar, inner, printing),
            roots,
            changed =>
              val compile = Watching.compileFor(inner)
              Ref
                .make(Option.empty[CallResult])
                .flatMap: finished =>
                  operate(repo, apiJar, compile, printing.dropExitInto(finished)) *>
                    finished.get.zip(roots).map((found, watched) => Watching.restarts(found, changed, watched)),
          )
        case WatchPolicy.After =>
          Watching.session(repo, printing, operate(repo, apiJar, inner, printing.dropExit), roots)
      loop
        .foldZIO(
          error =>
            ZIO.succeed(
              Completed(CliExit.Failed, "", error.message, Playback.Held)
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
        recorded(repo, toolchainSay(repo, apiJar, _))
      case other =>
        ZIO.serviceWithZIO[Machine] { machine =>
          Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]]).flatMap { runs =>
            versionText(repo).orElseSucceed("").flatMap { shown =>
              val build =
                Build.here[Local](
                  repo,
                  apiJar,
                  machine,
                  runs,
                  track = false,
                  shown,
                  adopted(repo, apiJar, _),
                  cliEdges(repo, _),
                  heard(repo, _),
                )
              build
                .perform(other)
                .runCollect
                .foldCauseZIO(
                  cause =>
                    if cause.isInterruptedOnly then ZIO.succeed(Completed(CliExit.Failed, "", "", Playback.Heard))
                    else
                      cause.failureOrCause match
                        case Left(error) =>
                          ZIO.succeed(Completed(CliExit.Failed, "", error.message, Playback.Held))
                        case Right(rest) => ZIO.failCause(rest),
                  beats =>
                    val feeds = beats.map(Build.feed)
                    val done  = completed(repo, feeds)
                    if finished(feeds) then ZIO.succeed(done)
                    else sayError("the call did not finish").as(done),
                )
            }
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
            versionText(repo).orElseSucceed("").flatMap { shown =>
              adopted(repo, apiJar, other).flatMap { loaded =>
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
                            runs,
                            track = false,
                            shown,
                            other,
                            say,
                            cliEdges(repo, say),
                          )
                          .provideSomeLayer(zio.ZLayer.succeed(report) ++ histos.format.Formatters.live)
                      )
                    )
                  }
              }
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

  private def cliEdges(repo: Path, say: Say): Perform.Edges[Local] =
    Perform.Edges(
      watch = (_, _, edge) =>
        Calls.finish(
          edge,
          CallResult.Fault(
            BuildFault.Rejected(Rejection.Watch(CliParseError.WatchCommand("watch does not nest").message))
          ),
        ),
      reload = ZIO.fail(ServerError.NoServer),
      reboot = bootCall(repo).flatMap { socket =>
        say(Message.Event(BuildEvent.Out(socket.toString))) *> Calls.finish(say, CallResult.reported())
      },
      bsp = Calls.finish(
        say,
        CallResult.Fault(BuildFault.Rejected(Rejection.Watch(CliParseError.Rejected.message))),
      ),
      compiled = ZIO.unit,
    )

  private def heard(repo: Path, beat: Beat): UIO[Unit] =
    beat match
      case Beat.Heard(event) =>
        CompileConsole.show(Show(repo, event), say, sayError)
      case Beat.Finished(CallResult.Fault(fault)) =>
        val text = FaultText.show(fault.diagnose)
        if text.isEmpty then ZIO.unit else sayError(text)
      case Beat.Finished(_) => ZIO.unit

  /** No server, so nothing was printed yet. Finish prints this once. */
  private def held(out: String, result: CallResult): Completed =
    val err = result match
      case CallResult.Fault(fault)     => FaultText.show(fault.diagnose)
      case CallResult.Reported(report) => report.err.mkString("\n")
    Completed(CliExit.from(result), out, err, Playback.Held)

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
                .read(machine)
                .flatMap:
                  case histos.jdk.JdkRead.Chosen(runtime, source) =>
                    Calls.tell(say, s"jdk ${runtime.version.value} ${runtime.home} from ${source.label}") *>
                      Calls.finish(say, CallResult.reported())
                  case histos.jdk.JdkRead.Failed(error, source) =>
                    Calls.tell(say, Toolchain.failure(error, source)) *>
                      Calls.finish(say, CallResult.Fault(BuildFault.Toolchain(error)))
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
          held("", CallResult.Fault(BuildFault.Load(err)))
        case Right(loaded) =>
          held(
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
      if id.builtIn then Call.Task(id.value, PluginArgs.empty)
      else Call.Run(id.value, platform)
    attach(repo, call, locally(repo, apiJar, call))

  private def compile(
      repo: Path,
      apiJar: Path,
      projects: List[ProjectName],
  ): URIO[Local, Completed] =
    compileAt(repo, projects, locally(repo, apiJar, Call.Compile(projects)))

  /** `miss` is forced only when the socket is missing or dead, so a hit never builds [[compileServices]]. */
  private[histos] def compileAt[R](
      repo: Path,
      projects: List[ProjectName],
      miss: => URIO[R, Completed],
  ): URIO[R, Completed] =
    val call = Call.Compile(projects)
    pinned(repo, miss)(text =>
      Attach
        .decide(repo, text)
        .flatMap {
          case Decision.Open(socket) =>
            remote(repo, socket, call).foldZIO(
              {
                case ServerError.Unreachable(_) => miss
                case error                      => ZIO.succeed(stopped(error))
              },
              done => ZIO.succeed(done),
            )
          case Decision.DifferentVersion(socket) =>
            supplant(repo, socket, started(repo, call, miss))(next =>
              remote(repo, next, call).foldZIO(
                {
                  case ServerError.Unreachable(_) => started(repo, call, miss)
                  case error                      => ZIO.succeed(stopped(error))
                },
                ZIO.succeed,
              )
            )
          case Decision.Absent =>
            started(repo, call, miss)
        }
    )

  /** No `./histos` means compile here. A script starts one server and this command attaches to it. */
  private def ensureServer(repo: Path): UIO[Either[ServerError, Option[Path]]] =
    val script = repo.resolve("histos")
    ZIO
      .attemptBlocking(Files.isRegularFile(script))
      .orDie
      .flatMap: present =>
        if present then startServer(repo).either.map(_.map(Some(_)))
        else ZIO.succeed(Right(None))

  private def startServer(repo: Path): IO[ServerError, Path] =
    val home = repo.resolve(".histos")
    val gate = home.resolve("server.starting")
    ZIO.acquireReleaseWith(
      ZIO
        .attemptBlocking {
          Files.createDirectories(home)
          java.nio.channels.FileChannel.open(
            gate,
            java.nio.file.StandardOpenOption.CREATE,
            java.nio.file.StandardOpenOption.WRITE,
          )
        }
        .mapError(_ => ServerError.Io(ServerIo.Bind(gate)))
    )(channel => ZIO.attemptBlocking(channel.close()).ignore) { channel =>
      ZIO
        .attemptBlocking(Option(channel.tryLock()))
        .mapError(_ => ServerError.Io(ServerIo.Bind(gate)))
        .flatMap {
          case None    => awaitServer(repo)
          case Some(_) => Daemon.spawn(repo) *> awaitServer(repo)
        }
    }

  private def awaitServer(repo: Path): IO[ServerError, Path] =
    val attempts                               = 1200
    def poll(left: Int): IO[ServerError, Path] =
      histos.server.Record.read(repo).flatMap {
        case Some(written)     => ZIO.succeed(written.socket)
        case None if left <= 0 =>
          histos.server.SocketPath.at(repo).flatMap(socket => ZIO.fail(ServerError.Unreachable(socket)))
        case None => ZIO.sleep(50.millis) *> poll(left - 1)
      }
    poll(attempts)

  private def stopped(error: ServerError): Completed =
    Completed(CliExit.Failed, "", error.message, Playback.Held)

  /** No `./histos` means `miss`. A script starts one server and this command attaches to it. */
  private def started[R](
      repo: Path,
      call: Call,
      miss: => URIO[R, Completed],
  ): URIO[R, Completed] =
    ensureServer(repo).flatMap {
      case Left(error)         => ZIO.succeed(stopped(error))
      case Right(None)         => miss
      case Right(Some(socket)) =>
        remote(repo, socket, call).foldZIO(
          error => ZIO.succeed(stopped(error)),
          done => ZIO.succeed(done),
        )
    }

  /** Reboot the recorded server, then run `use` on the replacement. `fallback` runs when it does not come back. */
  private def supplant[R](
      repo: Path,
      socket: Path,
      fallback: => URIO[R, Completed],
  )(use: Path => URIO[R, Completed]): URIO[R, Completed] =
    versionOf(repo).orElseSucceed(None).flatMap {
      case None       => fallback
      case Some(text) =>
        ApiClock.rebootLine(repo).flatMap { line =>
          sayError(line) *>
            Attach.replace(repo, text, socket).flatMap {
              case Some(next) => use(next)
              case None       =>
                ApiClock.restart(repo, socket).map { error =>
                  Completed(CliExit.Failed, "", error.message, Playback.Held)
                }
            }
        }
    }

  private def asked[R](
      repo: Path,
      socket: Path,
      miss: => URIO[R, Completed],
  ): URIO[R, Completed] =
    ApiClock.rebootLine(repo).flatMap { line =>
      sayError(line) *>
        remote(repo, socket, Call.Reboot).foldZIO(
          {
            case ServerError.Unreachable(_) | ServerError.DifferentVersion => miss
            case error                                                     => ZIO.succeed(stopped(error))
          },
          done => ZIO.succeed(done.copy(out = if done.out.isEmpty then line else s"$line\n${done.out}")),
        )
    }

  /** Only the local path loads. A matching server already holds the graph. */
  private def loaded[R](repo: Path, apiJar: Path)(
      use: histos.load.LoadedBuild => URIO[R, Completed]
  ): URIO[R & Loader & Scope & ContentStore & Resolve, Completed] =
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .foldZIO(
        error => ZIO.succeed(held("", CallResult.Fault(BuildFault.Load(error)))),
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
        error => held("", CallResult.Fault(BuildFault.Preview(error))),
        text => held(text, CallResult.reported()),
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
        error => held("", CallResult.Fault(BuildFault.Preview(error))),
        plan => held(Plan.renderTest(repo, plan), CallResult.reported()),
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
        loaded(repo, apiJar)(build => previewLocalPlan(repo, build, tokens, machine.env)),
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
        loaded(repo, apiJar)(build => previewSnapshotPlan(repo, build, tokens, machine.env)),
      )

  private def previewLocalPlan(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      tokens: List[String],
      env: Map[String, String],
  ): UIO[Completed] =
    Plan
      .publishLocal(repo, loaded.graph, tokens, env)
      .fold(
        error => held("", CallResult.Fault(BuildFault.Preview(error))),
        plan => held(Plan.renderPublish(plan), CallResult.reported()),
      )

  private def previewSnapshotPlan(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      tokens: List[String],
      env: Map[String, String],
  ): UIO[Completed] =
    Plan
      .publishSnapshots(repo, loaded.graph, tokens, env)
      .fold(
        error => held("", CallResult.Fault(BuildFault.Preview(error))),
        plan => held(Plan.renderPublish(plan), CallResult.reported()),
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
    ZIO.succeed(held(Plan.renderClean(repo, plan), CallResult.reported()))

  private def previewCleanLocal(
      repo: Path,
      loaded: histos.load.LoadedBuild,
      projects: List[ProjectName],
  ): UIO[Completed] =
    Plan
      .clean(repo, loaded.graph, projects)
      .fold(
        error => held("", CallResult.Fault(BuildFault.Preview(error))),
        plan => held(Plan.renderClean(repo, plan), CallResult.reported()),
      )

  private def previewCleanFull(
      repo: Path
  ): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    attach(repo, Call.PreviewCleanFull, previewCleanFullLocal(repo))

  private def previewCleanFullLocal(repo: Path): UIO[Completed] =
    val plan = Plan.cleanFull(repo)
    ZIO.succeed(held(Plan.renderClean(repo, plan), CallResult.reported()))

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
    ZIO.succeed:
      Plan.run(loaded.graph, id, platform) match
        case Left(error) =>
          held("", CallResult.Fault(BuildFault.Preview(error)))
        case Right(plan) =>
          held(Plan.renderRun(plan), CallResult.reported())

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
    Paint.render(text).flatMap(line => Console.printLine(line).orElseSucceed(()))

  private def sayError(text: String): UIO[Unit] =
    Paint.render(text).flatMap(line => Console.printLineError(line).orElseSucceed(()))

  private def cachePath: URIO[ContentStore, Completed] =
    ZIO.serviceWith[ContentStore](store => held(store.root.toString, CallResult.reported()))

  private def cacheList: URIO[ContentStore, Completed] =
    ZIO
      .serviceWithZIO[ContentStore](_.list)
      .mapBoth(
        err => held("", CallResult.Fault(BuildFault.Cache(err))),
        entries =>
          val text =
            if entries.isEmpty then ""
            else
              entries
                .map(entry => s"${entry.digest.value} ${entry.bytes}")
                .mkString("\n")
          held(text, CallResult.reported()),
      )
      .merge

  private def cacheClean(mode: CacheCleanMode): URIO[ContentStore, Completed] =
    ZIO
      .serviceWithZIO[ContentStore]: store =>
        Clock.instant.flatMap(now => store.clean(now, ContentStore.Retention, mode))
      .mapBoth(
        err => held("", CallResult.Fault(BuildFault.Cache(err))),
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
          held(text, CallResult.reported()),
      )
      .merge

  /** The remote URI is configuration. HTTP copies the index. gRPC has no list, so promote names that limit. */
  private def cachePromote(from: CacheNamespace, to: CacheNamespace): UIO[Completed] =
    ZIO
      .scoped(CacheConfig.processEnv.flatMap(env => CacheConfig.promote(env, from, to)))
      .fold(
        error => held("", CallResult.Fault(BuildFault.Remote(error))),
        _ => held("", CallResult.reported()),
      )

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
        error => held("", CallResult.Fault(BuildFault.Io(error))),
        _ => held(histos.clean.Products.line(Nil), CallResult.reported()),
      )

  private def cleanNamed(
      repo: Path,
      build: histos.load.LoadedBuild,
      projects: List[ProjectName],
  ): UIO[Completed] =
    histos.clean.Products
      .named(repo, build.graph, projects)
      .fold(
        error => held("", CallResult.Fault(cleanFault(error))),
        names => held(histos.clean.Products.line(names), CallResult.reported()),
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
        error => ZIO.succeed(held("", CallResult.Fault(BuildFault.Io(error)))),
        _ =>
          ZIO
            .serviceWithZIO[ContentStore]: store =>
              Clock.instant.flatMap(now => store.forget(histos.cache.CacheProject.of(repo), now))
            .fold(
              error => held("", CallResult.Fault(BuildFault.Cache(error))),
              entries => held(histos.clean.Products.forgotten(entries), CallResult.reported()),
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
              held("", CallResult.Fault(BuildFault.Load(error)))
            case error: ResolveError =>
              held("", CallResult.Fault(BuildFault.Resolve(error)))
          },
          text => held(text, CallResult.reported()),
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

  private def affected(
      repo: Path,
      apiJar: Path,
      base: String,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    ZIO
      .serviceWithZIO[Loader](_.load(repo, apiJar))
      .either
      .flatMap:
        case Left(error) =>
          ZIO.succeed(Completed(CliExit.Failed, "", error.message, Playback.Held))
        case Right(loaded) =>
          val text = Affected.report(loaded.graph, base, changed(repo, base))
          ZIO.succeed(Completed(CliExit.Success, s"$text\n", "", Playback.Held))

  private def affectedLocal(
      repo: Path,
      apiJar: Path,
      base: String,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    affected(repo, apiJar, base)

  private def changed(repo: Path, base: String): Option[List[String]] =
    if base == "all" then None
    else
      try
        val process = ProcessBuilder("git", "diff", "--name-only", s"$base...HEAD")
        process.directory(repo.toFile)
        process.redirectError(ProcessBuilder.Redirect.DISCARD)
        val running = process.start()
        val text    = String(running.getInputStream.readAllBytes, java.nio.charset.StandardCharsets.UTF_8)
        if running.waitFor() == 0 then Some(text.split('\n').toList.filter(_.nonEmpty)) else None
      catch case _: Throwable => None

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
            held("", CallResult.Fault(BuildFault.Load(error)))
          )
        case Right(loaded) =>
          Calls
            .sonaOf(loaded)
            .flatMap(sona => Calls.workflowWork(repo, loaded.graph, sona, action))
            .fold(
              fault => held("", CallResult.Fault(BuildFault.Workflow(fault))),
              file => held(file.toString, CallResult.reported()),
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
              .read(machine)
              .map:
                case histos.jdk.JdkRead.Chosen(runtime, source) =>
                  held(
                    s"jdk ${runtime.version.value} ${runtime.home} from ${source.label}",
                    CallResult.reported(),
                  )
                case histos.jdk.JdkRead.Failed(error, source) =>
                  held(
                    Toolchain.failure(error, source),
                    CallResult.Fault(BuildFault.Toolchain(error)),
                  )
          case Left(error) =>
            ZIO.succeed(held("", CallResult.Fault(BuildFault.Load(error))))
          case Right(loaded) =>
            Survey
              .read(machine, loaded.graph)
              .mapBoth(
                err => held(Survey.shown(err), CallResult.Fault(BuildFault.Survey(err))),
                found => held(Survey.text(found), CallResult.reported()),
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
          err => Completed(CliExit.Failed, "", err.message, Playback.Held),
          _ => Completed(CliExit.Success, "", "", Playback.Held),
        )

  private def launcherFailed(error: LauncherError): Completed =
    held("", CallResult.Fault(BuildFault.Launcher(error)))

  private def pinned[R](
      repo: Path,
      missing: => URIO[R, Completed],
  )(present: String => URIO[R, Completed]): URIO[R, Completed] =
    versionOf(repo).foldZIO(
      error => ZIO.succeed(launcherFailed(error)),
      {
        case None       => missing
        case Some(text) => present(text)
      },
    )

  private def bsp(repo: Path, apiJar: Path): URIO[Local, Completed] =
    bspAt(repo, bspLocal(repo, apiJar))

  private def bspAt[R](repo: Path, miss: => URIO[R, Completed]): URIO[R, Completed] =
    pinned(repo, miss)(text =>
      Attach
        .decide(repo, text)
        .flatMap {
          case Decision.Open(socket) =>
            Client
              .splice(socket)
              .foldZIO(
                {
                  case ServerError.Unreachable(_) => miss
                  case error                      => ZIO.succeed(stopped(error))
                },
                code =>
                  ZIO.succeed(
                    Completed(
                      if code == 0 then CliExit.Success else CliExit.Failed,
                      "",
                      "",
                      Playback.Held,
                    )
                  ),
              )
          case Decision.DifferentVersion(socket) =>
            supplant(repo, socket, miss)(next =>
              Client
                .splice(next)
                .foldZIO(
                  {
                    case ServerError.Unreachable(_) => miss
                    case error                      => ZIO.succeed(stopped(error))
                  },
                  code =>
                    ZIO.succeed(
                      Completed(
                        if code == 0 then CliExit.Success else CliExit.Failed,
                        "",
                        "",
                        Playback.Held,
                      )
                    ),
                )
            )
          case Decision.Absent => miss
        }
    )

  private def reload(repo: Path): UIO[Completed] =
    pinned(
      repo,
      ZIO.succeed(Completed(CliExit.Failed, "", ServerError.NoServer.message, Playback.Held)),
    )(text =>
      Attach
        .decide(repo, text)
        .flatMap {
          case Decision.DifferentVersion(socket) =>
            val quiet =
              ZIO.succeed(Completed(CliExit.Failed, "", ServerError.NoServer.message, Playback.Held))
            supplant(repo, socket, quiet)(next =>
              remote(repo, next, Call.Reload).foldZIO(
                {
                  case ServerError.Unreachable(_) => quiet
                  case error                      =>
                    ZIO.succeed(Completed(CliExit.Failed, "", error.message, Playback.Held))
                },
                ZIO.succeed,
              )
            )
          case Decision.Absent =>
            ZIO.succeed(
              Completed(
                CliExit.Failed,
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
                      CliExit.Failed,
                      "",
                      ServerError.NoServer.message,
                      Playback.Held,
                    )
                  )
                case error =>
                  ZIO.succeed(
                    Completed(
                      CliExit.Failed,
                      "",
                      error.message,
                      Playback.Held,
                    )
                  )
              },
              done => ZIO.succeed(done),
            )
        }
    )

  private def launcherLocal(repo: Path, action: LauncherAction): UIO[Completed] =
    val wrote = action match
      case LauncherAction.Update => histos.publish.LauncherPin.update(repo)
      case LauncherAction.Latest => histos.publish.LauncherPin.latest(repo)
    wrote.fold(
      error => held("", CallResult.Fault(BuildFault.Launcher(error))),
      pin => held(pin.shown, CallResult.reported()),
    )

  /** The id is the run id the server minted. No server has no runs. */
  private def cancel(repo: Path, id: RunId): UIO[Completed] =
    attach(
      repo,
      Call.Cancel(id),
      Ref
        .make(Map.empty[RunId, Fiber[ServerError, Unit]])
        .flatMap(runs => recorded(repo, Calls.cancel(runs, id, _))),
    )

  private[cli] def recorded[R](
      repo: Path,
      run: Say => ZIO[R, ServerError, Unit],
  ): URIO[R, Completed] =
    Ref.make(Chunk.empty[Feed]).flatMap { feeds =>
      run(speaking(repo, feeds)).foldCauseZIO(
        cause =>
          if cause.isInterruptedOnly then ZIO.succeed(Completed(CliExit.Failed, "", "", Playback.Heard))
          else
            cause.failureOrCause match
              case Left(error) =>
                ZIO.succeed(Completed(CliExit.Failed, "", error.message, Playback.Held))
              case Right(rest) => ZIO.failCause(rest),
        _ =>
          feeds.get.flatMap: chunk =>
            val done = completed(repo, chunk)
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
        val text  = FaultText.show(fault.diagnose)
        val shown = if text.isEmpty then ZIO.unit else sayError(text)
        shown *> feeds.update(_ :+ Feed.Done(CallResult.Fault(fault)))
      case Message.Done(result) =>
        feeds.update(_ :+ Feed.Done(result))
      case _ => ZIO.unit
    })

  private def completed(repo: Path, feeds: Chunk[Feed]): Completed =
    val events               = feeds.collect { case Feed.Event(event) => event }
    val (outLines, errLines) = Show.transcript(repo, events)
    val faults               = feeds.collect {
      case Feed.Done(CallResult.Fault(fault)) if FaultText.show(fault.diagnose).nonEmpty =>
        FaultText.show(fault.diagnose)
    }
    val out = outLines.mkString("\n")
    val err = (errLines ++ faults).mkString("\n")
    feeds.reverse.collectFirst { case Feed.Done(result) => result } match
      case Some(result) =>
        Completed(CliExit.from(result), out, err, Playback.Heard)
      case None =>
        Completed(CliExit.Failed, out, "the call did not finish", Playback.Heard)

  /** A running server replaces itself. No server starts one. The command returns when a server is listening. */
  private[histos] def reboot(repo: Path): URIO[Any, Completed] =
    rebootAt(repo, cold(repo))

  private def cold(repo: Path): URIO[Any, Completed] =
    bootCall(repo).fold(
      error => Completed(CliExit.Failed, "", error.message, Playback.Held),
      socket => Completed(CliExit.Success, socket.toString, "", Playback.Held),
    )

  private def rebootAt[R](repo: Path, miss: => URIO[R, Completed]): URIO[R, Completed] =
    pinned(repo, miss)(text =>
      Attach
        .decide(repo, text)
        .flatMap:
          case Decision.Absent       => miss
          case Decision.Open(socket) =>
            asked(repo, socket, miss)
          case Decision.DifferentVersion(socket) =>
            asked(repo, socket, miss)
    )

  private enum Boot:
    case Up
    case Load(error: LoadError)
    case Jdk(error: GraphCompileError)
    case Transport(error: ServerError)
    case Pin(error: LauncherError)

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
                          versionOf(repo).foldZIO(
                            error => ZIO.succeed(Boot.Pin(error)),
                            {
                              case None =>
                                ZIO.succeed(Boot.Pin(LauncherError.Absent(repo.resolve(LauncherPin.file))))
                              case Some(text) =>
                                AssemblyMark.read(repo).flatMap { mark =>
                                  Daemon
                                    .open(
                                      repo,
                                      loaded.graph,
                                      loaded.build,
                                      jdk,
                                      text,
                                      apiJar,
                                      classpath,
                                      loader.reportBuild(repo, apiJar).provideEnvironment(env),
                                      loaded.plugins,
                                      machine,
                                      assembly = mark,
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
                                    )
                                }
                            },
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
      case Boot.Load(error)      => held("", CallResult.Fault(BuildFault.Load(error)))
      case Boot.Jdk(error)       => held("", Perform.jdkMissed(Call.Reboot, error))
      case Boot.Pin(error)       => launcherFailed(error)
      case Boot.Transport(error) =>
        Completed(CliExit.Failed, "", error.message, Playback.Held)

  /** The socket a listening server recorded. A missing `./histos` is [[ServerError.NoServer]]. */
  private[histos] def bootCall(repo: Path): IO[ServerError, Path] =
    ensureServer(repo).flatMap {
      case Left(error)         => ZIO.fail(error)
      case Right(None)         => ZIO.fail(ServerError.NoServer)
      case Right(Some(socket)) => ZIO.succeed(socket)
    }

  /** A matching server runs the command. Another driver is rebooted first. `local` runs when that socket is missing or
    * dead.
    */
  private def attach[R](
      repo: Path,
      call: Call,
      local: => URIO[R, Completed],
  ): URIO[R, Completed] =
    ApiClock.admit(repo, call).flatMap {
      case ApiClock.Admit.Stop(fault) =>
        ZIO.succeed(held("", CallResult.Fault(fault)))
      case admitted =>
        val noted = admitted match
          case ApiClock.Admit.Warn(line) => sayError(line)
          case _                         => ZIO.unit
        noted *> ApiClock.drifted(repo).flatMap { stale =>
          val hold = stale && ApiClock.skipsReboot(call)
          pinned(repo, local)(text =>
            Attach
              .decide(repo, text)
              .flatMap {
                case Decision.Open(socket) =>
                  speak(repo, socket, call, local)
                case Decision.DifferentVersion(socket) if hold =>
                  speak(repo, socket, call, local)
                case Decision.DifferentVersion(socket) =>
                  supplant(repo, socket, local)(next => speak(repo, next, call, local))
                case Decision.Absent => local
              }
          )
        }
    }

  private def speak[R](
      repo: Path,
      socket: Path,
      call: Call,
      local: => URIO[R, Completed],
  ): URIO[R, Completed] =
    remote(repo, socket, call).foldZIO(
      {
        case ServerError.Unreachable(_) => local
        case error                      =>
          ZIO.succeed(
            Completed(
              CliExit.Failed,
              "",
              error.message,
              Playback.Held,
            )
          )
      },
      done => ZIO.succeed(done),
    )

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
              Completed(CliExit.from(done), "", "", Playback.Heard)
            case CallResult.Fault(fault) =>
              Completed(CliExit.from(done), "", FaultText.show(fault.diagnose), Playback.Held)
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
              Completed(CliExit.Failed, "", error.message, Playback.Held)
            ),
          loaded =>
            ZIO
              .serviceWithZIO[Machine](GraphCompile.jdk)
              .foldZIO(
                error =>
                  ZIO
                    .succeed(
                      Completed(
                        CliExit.Failed,
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
                            CliExit.Failed,
                            "",
                            error.message,
                            Playback.Held,
                          )
                        ),
                      classpath =>
                        val output = java.lang.System.out
                        val notify = (line: String) => histos.bsp.Frame.write(output, line).ignore
                        Runtimes
                          .read(env.get[Machine], loaded.graph.runnerTools.nodePath)
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
                                      CliExit.Failed,
                                      "",
                                      error.message,
                                      Playback.Held,
                                    )
                                  )
                                case Right(session) =>
                                  histos.resolve.ResolutionService
                                    .open(repo, loaded.graph)
                                    .mapError(ServerError.Resolve(_))
                                    .flatMap { report =>
                                      histos.zinc.CompileMemo
                                        .provide(
                                          histos.zinc.Compilers
                                            .provide(histos.schedule.Parallelism.default.cpu)(
                                              speak(session).provideSomeLayer(zio.ZLayer.succeed(report))
                                            )
                                        )
                                    }
                                    .foldZIO(
                                      error =>
                                        ZIO.succeed(
                                          Completed(
                                            CliExit.Failed,
                                            "",
                                            error.message,
                                            Playback.Held,
                                          )
                                        ),
                                      code =>
                                        ZIO.succeed(
                                          Completed(
                                            if code == 0 then CliExit.Success
                                            else CliExit.Failed,
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
  private def speak(
      session: Bsp.Session
  ): ZIO[
    Resolve & ContentStore & Scope & histos.zinc.Compilers & histos.zinc.CompileMemo & histos.resolve.ResolutionService,
    ServerError,
    Int,
  ] =
    val stdin  = Path.of("stdin")
    val stdout = Path.of("stdout")
    val send   =
      (line: String) => Frame.write(java.lang.System.out, line).mapError(_ => ServerError.Io(ServerIo.Write(stdout)))
    val next = Frame.read(java.lang.System.in).mapError(_ => ServerError.Io(ServerIo.Read(stdin)))
    session.follow *>
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
            Completed(CliExit.Failed, "", err.message, Playback.Held)
          )
        case Right(loaded) =>
          taskFrom(loaded, tokens) match
            case TaskParsed.Usage(message) =>
              ZIO.succeed(Completed(CliExit.Usage, "", message, Playback.Held))
            case TaskParsed.Ready(task) =>
              if watched then
                attach(repo, Call.Watch(task, WatchPolicy.After), watchLocal(repo, apiJar, task, WatchPolicy.After))
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
            case Left(CliParseError.UnknownRoot(_)) =>
              ZIO
                .serviceWithZIO[Loader](_.load(repo, apiJar))
                .mapError(error => BuildFault.Load(error))
                .flatMap: loaded =>
                  taskFrom(loaded, tokens) match
                    case TaskParsed.Usage(message) =>
                      ZIO.fail(BuildFault.Rejected(Rejection.Watch(message)))
                    case TaskParsed.Ready(task) => ZIO.succeed(Call.Watch(task, WatchPolicy.After))
            case Left(error) =>
              ZIO.fail(BuildFault.Rejected(Rejection.Watch(error.message)))
            case Right(command) =>
              ZIO.fromEither(
                Operation
                  .operation(command)
                  .map(call => Call.Watch(call, WatchPolicy.After))
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
        TaskParsed.Usage(FaultText.show(error.diagnose))
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

  /** The call a batch command sends. Other commands stay on the JVM route. */
  private[histos] def callOf(command: Command): Option[Call] =
    command match
      case Command.Compile(projects)        => Some(Call.Compile(projects))
      case Command.Test(projects, affected) =>
        AffectedChoice.select(projects, affected).map(names => Call.Test(names, Nil, TestMode.Run))
      case Command.Run(id, platform) =>
        Some(
          if id.builtIn then Call.Task(id.value, PluginArgs.empty)
          else Call.Run(id.value, platform)
        )
      case Command.Clean(projects) => Some(Call.Clean(projects))
      case Command.CleanFull       => Some(Call.CleanFull)
      case Command.Fmt(targets @ (histos.format.FormatTargets.Build | histos.format.FormatTargets.Projects(_)), mode) =>
        Some(Call.Format(targets, mode))
      case _ => None

  private def dispatched(command: Command)(use: Call => URIO[Scope, Completed]): URIO[Scope, Completed] =
    callOf(command) match
      case Some(call) => use(call)
      case None       => ZIO.succeed(Completed(CliExit.Usage, "", "histos could not build that call", Playback.Held))

  private def route(repo: Path, command: Command): URIO[Scope, Completed] =
    command match
      case Command.Version =>
        versionText(repo).foldZIO(
          error => ZIO.succeed(held("", CallResult.Fault(BuildFault.Launcher(error)))),
          text => attach(repo, Call.Version, ZIO.succeed(held(text, CallResult.reported()))),
        )
      case Command.Graph =>
        attach(repo, Call.Graph, inProcessReady(repo)((api, machine) => withLoader(machine)(graphLocal(repo, api))))
      case Command.Run(_, _) =>
        dispatched(command)(call => served(repo, call))
      case Command.Plugin(id, pluginArgs) =>
        served(repo, Call.Task(id.render, pluginArgs))
      case Command.Test(projects, affected) =>
        AffectedChoice.select(projects, affected) match
          case None =>
            ZIO.succeed(Completed(CliExit.Success, "", "", Playback.Held))
          case Some(names) =>
            dispatched(Command.Test(names))(call => served(repo, call))
      case asked: Command.Coverage =>
        stamped.flatMap: at =>
          inProcessReady(repo)((api, machine) => withLoader(machine)(cover(repo, api, machine, asked, at)))
      case Command.Affected(base) =>
        inProcessReady(repo)((api, machine) => withLoader(machine)(affectedLocal(repo, api, base)))
      case Command.TestOnly(modules, patterns) =>
        served(repo, Call.Test(modules, patterns, TestMode.Run))
      case Command.TestQuick(modules, patterns) =>
        served(repo, Call.Test(modules, patterns, TestMode.Quick))
      case Command.Watch(inner, args) =>
        Operation.operation(inner) match
          case Left(error) => ZIO.succeed(Completed(CliExit.Usage, "", error.message, Playback.Held))
          case Right(call) =>
            attach(
              repo,
              Call.Watch(call, policyOf(args)),
              inProcessReady(repo)((api, machine) => withLocal(machine)(watchLocal(repo, api, call, policyOf(args)))),
            )
      case Command.Compile(projects) =>
        dispatched(command)(call => compileAt(repo, projects, compileServices(repo, call)))
      case Command.Client =>
        served(repo, Call.Client)
      case Command.Fmt(targets, mode) =>
        served(repo, Call.Format(targets, mode))
      case Command.Clean(projects) =>
        dispatched(command)(call =>
          attach(
            repo,
            call,
            if projects.isEmpty then cleanLocal(repo)
            else
              inProcess(repo)((api, machine) => withLoader(machine)(loaded(repo, api)(cleanNamed(repo, _, projects)))),
          )
        )
      case Command.CleanFull =>
        dispatched(command)(call =>
          attach(repo, call, machineOf(repo)(machine => storeOf(machine)(cleanFullLocal(repo))))
        )
      case Command.Assembly(destination) =>
        served(repo, Call.Assembly(destination))
      case Command.PublishLocal(tokens) =>
        served(repo, Call.PublishLocal(tokens))
      case Command.PublishSnapshots(tokens) =>
        served(repo, Call.PublishSnapshots(tokens))
      case Command.PublishRelease(automatic, tokens) =>
        served(repo, Call.PublishRelease(tokens, automatic))
      case Command.PublishPromote(id) =>
        served(repo, Call.PublishPromote(id))
      case Command.PublishDrop(id) =>
        served(repo, Call.PublishDrop(id))
      case Command.PublishAssets(tokens) =>
        served(repo, Call.PublishAssets(tokens))
      case Command.Mcp =>
        inProcessReady(repo)((api, machine) => withLocal(machine)(mcp(repo, api)))
      case Command.Bsp =>
        bspAt(repo, inProcessReady(repo)((api, machine) => withLocal(machine)(bspLocal(repo, api))))
      case Command.Server =>
        inProcessReady(repo)((api, machine) => withLocal(machine)(serve(repo, api)))
      case Command.Portal =>
        machineReady(repo)(machine => portal(repo).provideSome[Scope](ZLayer.succeed(machine)))
      case Command.Toolchain =>
        attach(
          repo,
          Call.Toolchain,
          inProcess(repo)((api, machine) => withLocal(machine)(toolchainLocal(repo, api))),
        )
      case Command.CachePath =>
        attach(repo, Call.CachePath, machineReady(repo)(machine => storeOf(machine)(cachePath)))
      case Command.CacheList =>
        attach(repo, Call.CacheList, machineReady(repo)(machine => storeOf(machine)(cacheList)))
      case Command.CacheClean(mode) =>
        attach(repo, Call.CacheClean(mode), machineReady(repo)(machine => storeOf(machine)(cacheClean(mode))))
      case Command.CachePromote(from, to) =>
        cachePromote(from, to)
      case Command.Resolve(action) =>
        attach(
          repo,
          Call.Resolve(action),
          inProcessReady(repo)((api, machine) => withLocal(machine)(resolveLocal(repo, api, action))),
        )
      case Command.Workflow(action) =>
        attach(
          repo,
          Call.Workflow(action),
          inProcessReady(repo)((api, machine) => withLoader(machine)(workflowLocal(repo, api, action))),
        )
      case Command.Launcher(action) =>
        attach(repo, Call.Launcher(action), launcherLocal(repo, action))
      case Command.Reload =>
        reload(repo)
      case Command.Reboot =>
        reboot(repo)
      case Command.Cancel(id) =>
        cancel(repo, id)
      case Command.PreviewCompile(projects) =>
        attach(
          repo,
          Call.PreviewCompile(projects),
          loadedMiss(repo)(previewLocal(repo, _, projects)),
        )
      case Command.PreviewTest(projects) =>
        attach(repo, Call.PreviewTest(projects), loadedMiss(repo)(previewTestLocal(repo, _, projects)))
      case Command.PreviewPublishLocal(tokens) =>
        val call = Call.PreviewPublishLocal(tokens)
        attach(
          repo,
          call,
          inProcessReady(repo) { (api, machine) =>
            withLoader(machine)(loaded(repo, api)(build => previewLocalPlan(repo, build, tokens, machine.env)))
          },
        )
      case Command.PreviewPublishSnapshots(tokens) =>
        val call = Call.PreviewPublishSnapshots(tokens)
        attach(
          repo,
          call,
          inProcessReady(repo) { (api, machine) =>
            withLoader(machine)(
              loaded(repo, api)(build => previewSnapshotPlan(repo, build, tokens, machine.env))
            )
          },
        )
      case Command.PreviewClean(projects) =>
        attach(
          repo,
          Call.PreviewClean(projects),
          if projects.isEmpty then previewCleanAll(repo)
          else loadedMiss(repo)(previewCleanLocal(repo, _, projects)),
        )
      case Command.PreviewCleanFull =>
        attach(repo, Call.PreviewCleanFull, previewCleanFullLocal(repo))
      case Command.PreviewRun(id, platform) =>
        attach(
          repo,
          Call.PreviewRun(id.value, platform),
          loadedMiss(repo)(previewRunLocal(_, id.value, platform)),
        )
      case Command.PreviewPlugin(id) =>
        attach(repo, Call.PreviewRun(id.render, None), loadedMiss(repo)(previewRunLocal(_, id.render, None)))
      case Command.Observe =>
        observe(repo)
      case Command.New(name, bare, yes, branch) =>
        inProcessReady(repo)((api, machine) =>
          withLocal(machine)(fresh(repo, api, Some(name), bare, yes, None, branch))
        )
      case Command.Init(bare, yes, source, branch) =>
        inProcessReady(repo)((api, machine) => withLocal(machine)(fresh(repo, api, None, bare, yes, source, branch)))
      case Command.Doctor(area, json, preflight, fix, yes, verbose, projects) =>
        inProcessReady(repo)((api, machine) =>
          withLoader(machine)(doctor(repo, api, area, json, preflight, fix, yes, verbose, projects, machine))
        )
      case Command.Bump(part, ships) =>
        inProcessReady(repo)((api, machine) => withLoader(machine)(bump(repo, api, part, ships)))
      case Command.Explain(id) =>
        ZIO.succeed(explain(id))
      case Command.Deps(action) =>
        inProcessReady(repo)((api, machine) => withLoader(machine)(deps(repo, api, action)))
      case Command.Setup(from, fix, yes) =>
        ZIO.succeed(setup(repo, from, fix, yes))
      case Command.Help(words) =>
        ZIO.succeed(answered(words))

  /** A call the server can answer. The local build runs only when the socket does not. */
  private def served(repo: Path, call: Call): URIO[Scope, Completed] =
    attach(repo, call, compileServices(repo, call))

  /** Loader, resolve, the toolchain, and the compiler. Built only when this effect runs. */
  private def compileServices(repo: Path, call: Call): URIO[Scope, Completed] =
    inProcessReady(repo)((api, machine) => withLocal(machine)(locally(repo, api, call)))

  private def loadedMiss(repo: Path)(use: histos.load.LoadedBuild => UIO[Completed]): URIO[Scope, Completed] =
    inProcessReady(repo)((api, machine) => withLoader(machine)(loaded(repo, api)(use)))

  /** Jars and the machine, then `use`. Nothing here runs until the caller runs this effect. */
  private def inProcess(repo: Path)(use: (Path, Machine) => URIO[Scope, Completed]): URIO[Scope, Completed] =
    apiAndMachine(repo).flatMap:
      case Left(done)            => ZIO.succeed(done)
      case Right((api, machine)) => use(api, machine)

  private def inProcessReady(repo: Path)(use: (Path, Machine) => URIO[Scope, Completed]): URIO[Scope, Completed] =
    inProcess(repo) { (api, machine) =>
      Toolchain
        .prepare(machine)
        .foldZIO(
          err => ZIO.succeed(toolchainFailed(err)),
          _ => use(api, machine),
        )
    }

  private def machineOf(repo: Path)(use: Machine => URIO[Scope, Completed]): URIO[Scope, Completed] =
    Machine.load(repo).foldZIO(err => ZIO.succeed(toolchainFailed(err)), use)

  private def machineReady(repo: Path)(use: Machine => URIO[Scope, Completed]): URIO[Scope, Completed] =
    machineOf(repo): machine =>
      Toolchain
        .prepare(machine)
        .foldZIO(
          err => ZIO.succeed(toolchainFailed(err)),
          _ => use(machine),
        )

  private def withLocal(machine: Machine)(body: URIO[Local, Completed]): URIO[Scope, Completed] =
    body.provideSome[Scope](
      Loader.live,
      ZLayer.succeed(ContentStore.in(machine.cache)),
      ZLayer.succeed(machine),
      Resolve.coursier,
    )

  private def withLoader(
      machine: Machine
  )(body: URIO[Loader & ContentStore & Resolve & Scope, Completed]): URIO[Scope, Completed] =
    body.provideSome[Scope](
      Loader.live,
      ZLayer.succeed(ContentStore.in(machine.cache)),
      Resolve.coursier,
    )

  private def storeOf(machine: Machine)(body: URIO[ContentStore, Completed]): URIO[Scope, Completed] =
    body.provide(ZLayer.succeed(ContentStore.in(machine.cache)))

  private def apiAndMachine(repo: Path): UIO[Either[Completed, (Path, Machine)]] =
    jars.foldZIO(
      err => ZIO.succeed(Left(loadFailed(err))),
      api =>
        Machine
          .load(repo)
          .foldZIO(
            err => ZIO.succeed(Left(toolchainFailed(err))),
            machine => ZIO.succeed(Right((api, machine))),
          ),
    )

  private def toolchainFailed(err: ToolchainError): Completed =
    Completed(CliExit.Failed, "", err.message, Playback.Held)

  private def loadFailed(err: LoadError): Completed =
    Completed(CliExit.Failed, "", err.message, Playback.Held)

  private def jars: IO[LoadError, Path] =
    located.flatMap: program =>
      cacheHome.flatMap: cache =>
        BuildClasspath(program, cache)

  private def located: IO[LoadError, Path] =
    val location = Option(classOf[histos.HistosBuild].getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
    location match
      case None      => ZIO.fail(LoadError.MissingFile(Path.of("histos-api.jar")))
      case Some(url) =>
        ZIO
          .attempt(Path.of(url.toURI))
          .orElseFail(LoadError.Io(IoFailure(Path.of(url.toString), histos.workspace.IoFault.Read)))

  private def cacheHome: IO[LoadError, Path] =
    for
      xdg <- System
        .env("XDG_CACHE_HOME")
        .orElseFail(LoadError.Io(IoFailure(Path.of("XDG_CACHE_HOME"), histos.workspace.IoFault.Read)))
      home <- System
        .property("user.home")
        .orElseFail(LoadError.Io(IoFailure(Path.of("user.home"), histos.workspace.IoFault.Read)))
      root <- (xdg.filter(_.nonEmpty), home.filter(_.nonEmpty)) match
        case (Some(dir), _) => ZIO.succeed(Path.of(dir, "histos"))
        case (_, Some(dir)) => ZIO.succeed(Path.of(dir, ".cache", "histos"))
        case _              => ZIO.fail(LoadError.NoHome)
    yield root
end Cli

object Main extends ZIOAppDefault:
  override val bootstrap =
    zio.Runtime.enableLoomBasedExecutor ++ zio.Runtime.enableLoomBasedBlockingExecutor

  def run =
    for
      args <- getArgs.map(_.toList)
      _    <- Cli.launch(args, Path.of("").toAbsolutePath).flatMap(finish)
    yield ()

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
end Main
