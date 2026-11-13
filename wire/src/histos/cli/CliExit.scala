package histos.cli

import histos.format.FormatError
import histos.report.{BuildFault, CallResult, Rejection, Report, Severity}
import histos.testrun.{ModuleTestError, TestStatus}

/** The process status. The message names which request failed; the status is only success, failure, or usage. */
enum CliExit:
  case Success, Failed, Usage

  def code: Int = this match
    case Success => 0
    case Failed  => 1
    case Usage   => 2
end CliExit

object CliExit:
  /** A child status. 0 is ok, 2 is bad usage, and every other status is a failure. */
  def fromCode(code: Int): CliExit =
    code match
      case 0 => Success
      case 2 => Usage
      case _ => Failed

  /** Histos's own process status. The report does not store it. */
  def from(result: CallResult): CliExit =
    result match
      case CallResult.Reported(report) => fromReport(report)
      case CallResult.Fault(fault)     => fromFault(fault)

  private def fromReport(report: Report): CliExit =
    report.program match
      case Some(code) if code != 0 => fromCode(code)
      case _                       =>
        if report.tests.exists(failedTest) || failedFacts(report) then Failed else Success

  private def failedTest(cell: histos.testrun.TestedCell): Boolean =
    cell.status match
      case TestStatus.Failed(_)                                          => true
      case TestStatus.NoTests | TestStatus.Unmatched | TestStatus.Passed => false

  private def failedFacts(report: Report): Boolean =
    report.err.nonEmpty || report.diagnostics.exists(_.severity == Severity.Error)

  /** A bad test pattern and a fmt project selection are usage, and a refusal is [[rejectedExit]]. Every other fault is
    * a failure.
    */
  private def fromFault(fault: BuildFault): CliExit =
    fault match
      case BuildFault.Pattern(_) | BuildFault.Test(ModuleTestError.Pattern(_)) |
          BuildFault.Format(FormatError.Selected(_)) =>
        Usage
      case BuildFault.Rejected(rejection) => rejectedExit(rejection)
      case BuildFault.Load(_) | BuildFault.Resolve(_) | BuildFault.Toolchain(_) | BuildFault.Survey(_) |
          BuildFault.Select(_) | BuildFault.Compile(_) | BuildFault.Test(_) | BuildFault.Publish(_) |
          BuildFault.Release(_) | BuildFault.Workflow(_) | BuildFault.Program(_) | BuildFault.Platform(_) |
          BuildFault.Task(_) | BuildFault.Plugin(_) | BuildFault.PluginTask(_) | BuildFault.Preview(_) |
          BuildFault.Assemble(_) | BuildFault.Launcher(_) | BuildFault.Cache(_) | BuildFault.Remote(_) |
          BuildFault.Store(_) | BuildFault.Io(_) | BuildFault.Version(_) | BuildFault.Watch(_) | BuildFault.Format(_) |
          BuildFault.Interrupted | BuildFault.StaleApi(_, _) =>
        Failed

  /** A run that failed is a failure. Every other refusal is usage. */
  private def rejectedExit(rejection: Rejection): CliExit =
    rejection match
      case Rejection.TaskArguments(_) | Rejection.RunFailed | Rejection.WatchStopped =>
        Failed
      case _ => Usage
end CliExit
