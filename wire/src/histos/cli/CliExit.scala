package histos.cli

import histos.format.FormatError
import histos.program.ProgramError
import histos.report.{BuildFault, CallResult, Rejection, Report, Severity, WorkflowFault}
import histos.server.Call
import histos.workflow.CiError
import histos.testrun.{ModuleTestError, TestStatus}
import histos.zinc.GraphCompileError

enum CliExit:
  case Success, TaskFailed, Usage, LoadFailed, ToolchainFailed, ResolveFailed,
    WorkflowFailed, TestFailed, CompileFailed, PublishFailed
  case Program(status: Int)
  case Interrupted

  /** Process status is 0, 1, or 2. The case still names which request failed. */
  def code: Int = this match
    case Success         => 0
    case Usage           => 2
    case Program(0)      => 0
    case Program(2)      => 2
    case Program(_)      => 1
    case TaskFailed      => 1
    case Interrupted     => 1
    case LoadFailed      => 1
    case ToolchainFailed => 1
    case ResolveFailed   => 1
    case WorkflowFailed  => 1
    case TestFailed      => 1
    case CompileFailed   => 1
    case PublishFailed   => 1
end CliExit

object CliExit:
  /** A child status. 0 is ok, 2 is bad usage, and every other non-zero status is a failure. */
  def fromCode(code: Int): CliExit =
    code match
      case 0 => Success
      case 2 => Usage
      case _ => TaskFailed

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

  /** A report that already failed. Publish, test, and compile stay distinct cases. The status is 1. */
  private def failedCall(call: Call): CliExit =
    call match
      case Call.Test(_, _, _) | Call.PreviewTest(_) => TestFailed
      case Call.PublishLocal(_) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PublishPromote(_) |
          Call.PublishDrop(_) | Call.PublishAssets(_) | Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) =>
        PublishFailed
      case Call.Format(_, _) => TaskFailed
      case _                 => CompileFailed

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
      case BuildFault.Workflow(fault)                    => workflowExit(fault)
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
      case BuildFault.Format(FormatError.Resolved(_))    => ResolveFailed
      case BuildFault.Format(FormatError.Selected(_))    => Usage
      case BuildFault.Format(_)                          => TaskFailed
      case BuildFault.Interrupted                        => Interrupted
      case BuildFault.Rejected(rejection)                => rejectedExit(rejection)

  /** A missing default branch is a failed command. Other workflow faults stay on their own status. */
  private def workflowExit(fault: WorkflowFault): CliExit =
    fault match
      case WorkflowFault.Files(CiError.NoDefaultBranch) => TaskFailed
      case _                                            => WorkflowFailed

  /** No lock is a compile, publish, or resolve failure. The status is 1. */
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

  /** A missing lock on test or run is resolve. Cache is load. Clean is a failed task. The status is 1. */
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
