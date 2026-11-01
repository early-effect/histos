package histos.publish

import histos.workspace.{IoFailure}
import java.nio.file.{Path}

enum ScriptError:
  case Drift(path: Path)
  case Missing(path: Path)
  case Io(failure: IoFailure)
  case Revision
  case Pin(error: LauncherError)

  def message: String = this match
    case Drift(path)   => s"launcher script does not match the ship: $path"
    case Missing(path) => s"no launcher script at $path"
    case Io(failure)   => failure.message
    case Pin(error)    => error.message
    case Revision      => "the launcher script needs one manual ship version"
end ScriptError
