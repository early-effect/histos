package histos.testrun

import java.nio.file.Path
import histos.jdk.Sha256Error
import histos.workspace.IoFailure

/** A stored success could not be written, read, or removed, or the text was not a success digest. A missing record is
  * not one of these: [[QuickRecord.hit]] treats that as a miss.
  */
enum QuickError:
  case Save(failure: IoFailure)
  case Clear(failure: IoFailure)
  case Read(failure: IoFailure)
  case Shape(path: Path)
  case Hash(path: Path, error: Sha256Error)
  case Suite(error: SuiteNameError)

  def message: String = this match
    case Save(failure)     => failure.message
    case Clear(failure)    => failure.message
    case Read(failure)     => failure.message
    case Shape(path)       => s"quick record $path is not a stored success"
    case Hash(path, error) => s"quick record $path ${error.message}"
    case Suite(error)      => error.message
end QuickError
