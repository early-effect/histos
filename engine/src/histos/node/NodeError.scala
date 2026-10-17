package histos.node

import java.nio.file.Path

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

enum NodeError:
  case BadMajor(raw: Int)
  case Unreadable(path: Path)
  case PathScan
  case Version(path: Path, error: NodeVersionError)
  case Lock(error: NodeLockError)
  case Stage(error: NodeStageError)
  case Override(error: NodeOverrideError)
  case NoLock(path: Path)

  def message: String = this match
    case BadMajor(raw)    => s"$raw is not a Node major"
    case Unreadable(path) => s"could not read $path"
    case PathScan         => "could not read PATH"
    case Version(path, error) => s"could not read the Node version at $path: ${error.message}"
    case Lock(error)      => error.message
    case Stage(error)     => error.message
    case Override(error)  => error.message
    case NoLock(path)     => s"no node lock at $path"
