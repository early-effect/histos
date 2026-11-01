package histos.server

import histos.assembly.AssembleError
import histos.program.ProgramError
import histos.report.BuildFault
import histos.testrun.ModuleTestError
import histos.zinc.GraphCompileError

/** Which failures never became a run. A cell that ran stays on the report. */
object Faults:
  /** A compile that never started. A cell that ran stays on the report, so this is empty then. */
  def compileStopped(error: GraphCompileError): Option[BuildFault] =
    error match
      case GraphCompileError.Failed(_, _, _, _) | GraphCompileError.Cells(_) => None
      case GraphCompileError.Lock(found)                                     => Some(BuildFault.Resolve(found))
      case other                                                             => Some(BuildFault.Compile(other))

  /** A test request that never became a run. `None` means the compile or the selector ran. */
  def testStopped(error: ModuleTestError): Option[BuildFault] =
    error match
      case ModuleTestError.Pattern(found) => Some(BuildFault.Pattern(found))
      case ModuleTestError.Target(found)  => Some(BuildFault.Select(found))
      case ModuleTestError.Lock(found)    => Some(BuildFault.Resolve(found))
      case ModuleTestError.Compile(inner) => compileStopped(inner)
      case ModuleTestError.NoneMatched(_) => None
      case other                          => Some(BuildFault.Test(other))

  /** A program request that never started. A compile that ran stays on the report. */
  def programStopped(error: ProgramError): Option[BuildFault] =
    error match
      case ProgramError.Compile(inner) => compileStopped(inner)
      case ProgramError.Lock(found)    => Some(BuildFault.Resolve(found))
      case other                       => Some(BuildFault.Program(other))

  /** An assembly that never produced a jar. A compile that ran stays on the report. */
  def assembleStopped(error: AssembleError): Option[BuildFault] =
    error match
      case AssembleError.Compile(inner) => compileStopped(inner)
      case AssembleError.Resolve(found) => Some(BuildFault.Resolve(found))
      case other                        => Some(BuildFault.Assemble(other))
end Faults
