package histos.cli

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
      case BuildFault.Interrupted                        => Interrupted
      case BuildFault.Rejected(rejection)                => rejectedExit(rejection)

  /** A missing default branch is a failed command. Other workflow faults stay on their own status. */
  private def workflowExit(fault: WorkflowFault): CliExit =
    fault match
      case WorkflowFault.Files(CiError.NoDefaultBranch) => TaskFailed
      case _                                            => WorkflowFailed

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
