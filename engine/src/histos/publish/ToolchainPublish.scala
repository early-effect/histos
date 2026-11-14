package histos.publish

import histos.assembly.AssembleError
import histos.program.ProgramError
import histos.workspace.IoFailure

/** Pack the launcher and compile the link kit, then install or upload. Publish does not link a client. */
enum ToolchainPublishError:
  case Pack(error: AssembleError)
  case Kit(error: ProgramError)
  case Upload(detail: String)
  case Install(failure: IoFailure)

  def message: String = this match
    case Pack(error)      => error.message
    case Kit(error)       => error.message
    case Upload(detail)   => detail
    case Install(failure) => failure.message
end ToolchainPublishError
