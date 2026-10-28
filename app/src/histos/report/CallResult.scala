package histos.report

import histos.cache.CacheCleanModeError
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
    case Interrupted         => "interrupted"
    case Rejected(rejection) => rejection.message
end BuildFault

object BuildFault:
  /** A compile that never started. A cell that ran stays on the report, so this is empty then. */
  def compileStopped(error: GraphCompileError): Option[BuildFault] =
    error match
      case GraphCompileError.Failed(_, _, _, _) | GraphCompileError.Cells(_) => None
      case GraphCompileError.Lock(found)                                     => Some(Resolve(found))
      case other                                                             => Some(Compile(other))

  /** A test request that never became a run. `None` means the compile or the selector ran. */
  def testStopped(error: ModuleTestError): Option[BuildFault] =
    error match
      case ModuleTestError.Pattern(found) => Some(Pattern(found))
      case ModuleTestError.Target(found)  => Some(Select(found))
      case ModuleTestError.Lock(found)    => Some(Resolve(found))
      case ModuleTestError.Compile(inner) => compileStopped(inner)
      case ModuleTestError.NoneMatched(_) => None
      case other                          => Some(Test(other))

  /** A program request that never started. A compile that ran stays on the report. */
  def programStopped(error: ProgramError): Option[BuildFault] =
    error match
      case ProgramError.Compile(inner) => compileStopped(inner)
      case ProgramError.Lock(found)    => Some(Resolve(found))
      case other                       => Some(Program(other))

  /** An assembly that never produced a jar. A compile that ran stays on the report. */
  def assembleStopped(error: AssembleError): Option[BuildFault] =
    error match
      case AssembleError.Compile(inner) => compileStopped(inner)
      case AssembleError.Resolve(found) => Some(Resolve(found))
      case other                        => Some(Assemble(other))
end BuildFault

/** A request the grammar refused. Tokens only. The CLI renders a usage banner for [[Rejection.CommandHelp]]. */
enum Rejection:
  case ResolveAction(raw: String)
  case WorkflowAction(raw: String)
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

  def message: String = this match
    case ResolveAction(raw)     => s"'$raw' is not a resolve action"
    case WorkflowAction(raw)    => s"'$raw' is not a workflow action"
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
    case CommandHelp(names)     =>
      if names.isEmpty then "unknown command" else s"unknown command: ${names.mkString(", ")}"
end Rejection

/** Workflow generation failed before a build report existed. */
enum WorkflowFault:
  case Files(error: CiError)
  case Scripts(error: ScriptError)

  def message: String = this match
    case Files(error)   => error.message
    case Scripts(error) => error.message
end WorkflowFault
