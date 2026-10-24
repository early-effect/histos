package histos.node

import java.nio.file.Path
import histos.workspace.IoFailure
import histos.toolchain.RuntimeLocationError

enum NodeOverrideError:
  case Empty
  case Multiple
  case Relative(raw: String)
  case Invalid(raw: String)

  def message: String = this match
    case Empty         => "node.local is empty"
    case Multiple      => "node.local has more than one path"
    case Relative(raw) => s"node.local path is not absolute: $raw"
    case Invalid(raw)  => s"node.local path is invalid: $raw"

/** The `node` a Js run or test launches. */
final case class NodeBinary(path: Path)

/** Why no Node can be chosen for a Js launch. */
enum NodeMiss:
  case Unresolved(error: NodeError)
  case Rejected(path: Path, reason: NodeReject)
  case TooOld(required: NodeVersion, found: NodeVersion, home: Path)
  case Absent(required: NodeVersion)

  def message: String = this match
    case Unresolved(error)      => error.message
    case Rejected(path, reason) =>
      s"Node at $path was rejected: ${reason.message}"
    case TooOld(required, found, home) =>
      s"Node ${found.value} at $home is older than ${required.value}"
    case Absent(required) =>
      s"no Node found; Histos needs ${required.value} or newer"

enum NodeError:
  case BadMajor(raw: Int)
  case Unreadable(path: Path)
  case Locations(error: RuntimeLocationError)
  case Version(path: Path, error: NodeVersionError)
  case Override(error: NodeOverrideError)
  case Io(failure: IoFailure)

  def message: String = this match
    case BadMajor(raw)        => s"$raw is not a Node major"
    case Unreadable(path)     => s"could not read $path"
    case Locations(error)     => error.message
    case Version(path, error) =>
      s"could not read the Node version at $path: ${error.message}"
    case Override(error) => error.message
    case Io(failure)     => failure.message
