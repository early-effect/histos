package histos.node

import java.nio.file.Path
import histos.workspace.IoFailure
import histos.toolchain.RuntimeLocationError
import histos.NodePathError

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
end NodeOverrideError

/** The `node` a Js run or test launches. */
final case class NodeBinary(path: Path)

/** Where a Node binary was named. */
enum NodeSource:
  case Environment
  case Config
  case Path
  case Local

  def label: String = this match
    case Environment => "HISTOS_NODE"
    case Config      => "config"
    case Path        => "PATH"
    case Local       => "node.local"
end NodeSource

/** A named binary that cannot be launched. */
enum NodeFileFault:
  case Missing
  case NotExecutable
  case NotAFile

  def message: String = this match
    case Missing       => "is missing"
    case NotExecutable => "is not executable"
    case NotAFile      => "is not a file"
end NodeFileFault

/** Why no Node can be chosen for a Js launch. */
enum NodeMiss:
  case Unresolved(error: NodeError)
  case Rejected(path: Path, reason: NodeReject, source: NodeSource)
  case TooOld(required: NodeVersion, found: NodeVersion, home: Path, source: NodeSource)
  case Absent(required: NodeVersion, source: NodeSource)
  case Unusable(path: Path, source: NodeSource, fault: NodeFileFault)
  case Refused(source: NodeSource, error: NodePathError)

  def message: String = this match
    case Unresolved(error)              => error.message
    case Rejected(path, reason, source) =>
      s"Node from ${source.label} at $path was rejected: ${reason.message}"
    case TooOld(required, found, home, source) =>
      s"Node ${found.value} from ${source.label} at $home is older than ${required.value}"
    case Absent(required, source) =>
      s"no Node on ${source.label}; Histos needs ${required.value} or newer"
    case Unusable(path, source, fault) =>
      s"${source.label} names $path, which ${fault.message}"
    case Refused(source, error) =>
      s"${source.label} ${error.message}"
end NodeMiss

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
end NodeError
