package histos.testrun

import java.nio.file.Path
import histos.workspace.IoFailure

/** Why a suite digest could not be built. A missing suite class is not one of these: that digest is absent and the
  * suite runs.
  */
enum StampError:
  case Io(failure: IoFailure)
  case Class(path: Path)
  case Missing(name: String)

  def message: String = this match
    case Io(failure)   => failure.message
    case Class(path)   => s"could not read class $path"
    case Missing(name) => s"stamp index has no $name"
end StampError
