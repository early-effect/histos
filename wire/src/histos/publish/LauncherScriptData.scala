package histos.publish

import histos.workspace.IoFailure
import java.nio.file.Path

enum ScriptError:
  case Drift(path: Path)
  case Missing(path: Path)
  case Io(failure: IoFailure)

  /** The launcher jar has no `install/histos` digest. */
  case Shim

  def message: String = this match
    case Drift(path)   => s"launcher script does not match the rendering ship: $path"
    case Missing(path) => s"no launcher script at $path"
    case Io(failure)   => failure.message
    case Shim          => "the rendering ship has no shim digest"
end ScriptError
