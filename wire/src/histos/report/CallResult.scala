package histos.report

import histos.doctor.{Diagnostic as Finding, Noted}
import histos.cache.CacheCleanModeError
import histos.format.FormatError
import histos.assembly.AssembleError
import histos.jdk.ToolchainError
import histos.load.{LoadError, PluginFault}
import histos.plugin.{FlagNameError, ParamNameError, PluginTaskError}
import histos.preview.PreviewError
import histos.program.ProgramError
import histos.publish.{LauncherError, LocalPublishError, ScriptError}
import histos.resolve.ResolveError
import histos.schedule.{CacheError, RemoteError}
import histos.select.SelectError
import histos.task.TaskError
import histos.testrun.{ModuleTestError, SuitePatternError, TestModeError, TestedCell}
import histos.toolchain.SurveyError
import histos.version.VersionError
import histos.watch.WatchError
import histos.workflow.CiError
import histos.workspace.IoFailure
import histos.zinc.GraphCompileError
import histos.{PlatformError, ProjectNameError}

import zio.Chunk

/** A finished socket or tool call. A report is a run that happened. A fault is a request that never became one. */
enum CallResult:
  case Reported(report: Report)
  case Fault(fault: BuildFault)

  def message: String = this match
    case Reported(report) => report.err.mkString("\n")
    case Fault(fault)     => fault.message
end CallResult

object CallResult:
  /** A run that happened. `summary` is a live error line. An empty summary is a success. */
  def reported(
      summary: String = "",
      tests: Chunk[TestedCell] = Chunk.empty,
      program: Option[Int] = None,
  ): CallResult =
    val err = if summary.isEmpty then Chunk.empty else Chunk(summary)
    Reported(Report(Chunk.empty, err, None, None, Chunk.empty, tests, program))

  def fault(why: BuildFault): CallResult = Fault(why)
end CallResult

/** This request did not become a report. A compile or test that ran stays on the report. */
enum BuildFault:
  case Load(error: LoadError)
  case Resolve(error: ResolveError)
  case Toolchain(error: ToolchainError)
  case Survey(error: SurveyError)
  case Select(error: SelectError)
  case Pattern(error: SuitePatternError)
  case Compile(error: GraphCompileError)
  case Test(error: ModuleTestError)
  case Publish(error: LocalPublishError)
  case Release(detail: String)
  case Workflow(error: WorkflowFault)
  case Program(error: ProgramError)
  case Platform(error: PlatformError)
  case Task(error: TaskError)
  case Plugin(error: PluginFault)
  case PluginTask(error: PluginTaskError)
  case Preview(error: PreviewError)
  case Assemble(error: AssembleError)
  case Launcher(error: LauncherError)
  case Cache(error: CacheError)
  case Remote(error: RemoteError)
  case Store(failure: IoFailure)
  case Io(failure: IoFailure)
  case Version(error: VersionError)
  case Watch(error: WatchError)
  case Format(error: FormatError)
  case Interrupted
  case Rejected(rejection: Rejection)

  def message: String = this match
    case Load(error)         => error.message
    case Resolve(error)      => error.message
    case Toolchain(error)    => error.message
    case Survey(error)       => error.message
    case Select(error)       => error.message
    case Pattern(error)      => error.message
    case Compile(error)      => error.message
    case Test(error)         => error.message
    case Publish(error)      => error.message
    case Release(detail)     => detail
    case Workflow(error)     => error.message
    case Program(error)      => error.message
    case Platform(error)     => error.message
    case Task(error)         => error.message
    case Plugin(error)       => error.message
    case PluginTask(error)   => error.message
    case Preview(error)      => error.message
    case Assemble(error)     => error.message
    case Launcher(error)     => error.message
    case Cache(error)        => error.message
    case Remote(error)       => error.message
    case Store(failure)      => failure.message
    case Io(failure)         => failure.message
    case Version(error)      => error.message
    case Watch(error)        => error.message
    case Format(error)       => error.message
    case Interrupted         => "interrupted"
    case Rejected(rejection) => rejection.message

  def diagnose: Finding = this match
    case Load(error)      => error.diagnose
    case Resolve(error)   => error.diagnose
    case Select(error)    => error.diagnose
    case Compile(error)   => error.diagnose
    case Test(error)      => error.diagnose
    case Publish(error)   => error.diagnose
    case Workflow(error)  => error.diagnose
    case Program(error)   => error.diagnose
    case Task(error)      => error.diagnose
    case Preview(error)   => error.diagnose
    case Format(error)    => error.diagnose
    case Rejected(reason) => reason.diagnose
    case Release(detail)  => Noted.carry("publish.release", detail, detail, "histos help publish")
    case Interrupted      => Noted.carry("build.interrupted", message, "The run was interrupted.", "histos help")
    case other            => Noted.carry("build.failed", other.message, other.message, "histos help")
end BuildFault

/** A request the grammar refused. Tokens only. The CLI renders a usage banner for [[Rejection.CommandHelp]]. */
enum Rejection:
  case ResolveAction(raw: String)
  case WorkflowAction(raw: String)
  case LauncherAction(raw: String)
  case AssemblyDestination(raw: String)
  case TaskArguments(id: String)
  case NotATool
  case Watch(detail: String)
  case RunFailed
  case WatchStopped
  case PreviewProjects
  case PreviewOperation(raw: String)
  case MissingRunName
  case Parameter(error: ParamNameError)
  case ProjectName(error: ProjectNameError)
  case SuitePattern(error: SuitePatternError)
  case RunId(error: TaskError)
  case TestMode(error: TestModeError)
  case CacheClean(error: CacheCleanModeError)
  case Flag(error: FlagNameError)
  case TaskPlatform(name: String)
  case CommandHelp(names: List[String])
  case FormatMode(raw: String)
  case FormatTargets
  case FormatPath(raw: String)

  def message: String = this match
    case ResolveAction(raw)     => s"'$raw' is not a resolve action"
    case WorkflowAction(raw)    => s"'$raw' is not a workflow action"
    case LauncherAction(raw)    => s"'$raw' is not a launcher action"
    case AssemblyDestination(_) => "assembly path is not a path"
    case TaskArguments(id)      => s"run $id does not take arguments"
    case NotATool               => "not a tool"
    case Watch(detail)          => detail
    case RunFailed              => "the run failed"
    case WatchStopped           => "watch stopped"
    case PreviewProjects        => "cleanFull does not take a project"
    case PreviewOperation(raw)  => s"preview does not run $raw"
    case MissingRunName         => "run needs one name"
    case Parameter(error)       => error.message
    case ProjectName(error)     => error.message
    case SuitePattern(error)    => error.message
    case RunId(error)           => error.message
    case TestMode(error)        => error.message
    case CacheClean(error)      => error.message
    case Flag(error)            => error.message
    case TaskPlatform(name)     => s"run $name does not take a platform"
    case FormatMode(raw)        => s"'$raw' is not a fmt mode. It is write or check"
    case FormatTargets          => "fmt takes projects or files, not both"
    case FormatPath(raw)        => s"'$raw' is not a path relative to the repository"
    case CommandHelp(names)     =>
      if names.isEmpty then "unknown command" else s"unknown command: ${names.mkString(", ")}"

  def diagnose: Finding = this match
    case CommandHelp(names) =>
      val token   = names.headOption.getOrElse("")
      val summary = if token.isEmpty then "unknown command" else s"unknown command '$token'"
      Noted.error(
        "cli.unknown-command",
        summary,
        "histos help lists the commands.\nhistos help <command> prints one.",
        Noted.command("histos help"),
      )
    case MissingRunName =>
      Noted.carry("cli.run", message, "run needs the task name.", "histos help run")
    case other =>
      Noted.carry("cli.usage", other.message, other.message, "histos help")
end Rejection

/** Workflow generation failed before a build report existed. */
enum WorkflowFault:
  case Files(error: CiError)
  case Scripts(error: ScriptError)

  def message: String = this match
    case Files(error)   => error.message
    case Scripts(error) => error.message

  def diagnose: Finding = this match
    case Files(error)   => error.diagnose
    case Scripts(error) => Noted.carry("workflow.script", error.message, error.message, "histos workflow --update")
end WorkflowFault
