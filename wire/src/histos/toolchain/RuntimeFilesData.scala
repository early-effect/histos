package histos.toolchain

import histos.jdk.Sha256
import histos.workspace.IoFailure
import java.nio.file.Path

enum RuntimeIdentity:
  /** `salt` is the canonical home and the build the runtime already published. */
  case Local(home: Path, salt: Sha256)

enum RuntimeLocationError:
  case InvalidPath(raw: String)
  case Io(failure: IoFailure)

  def message: String = this match
    case InvalidPath(raw) => s"invalid runtime search path: $raw"
    case Io(failure)      => failure.message
end RuntimeLocationError
