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
  case Install(hint: NodeInstall)

  def message: String = this match
    case Unresolved(error)      => error.message
    case Rejected(path, reason) => s"$path was rejected: ${reason.message}"
    case Install(hint)          =>
      s"no installed Node matches the lock. Install it: ${hint.command}"

enum NodeError:
  case Archive(error: histos.toolchain.ToolArchiveError)
  case BadMajor(raw: Int)
  case Unreadable(path: Path)
  case PathScan
  case Locations(error: RuntimeLocationError)
  case Version(path: Path, error: NodeVersionError)
  case Lock(error: NodeLockError)
  case Stage(error: NodeStageError)
  case Override(error: NodeOverrideError)
  case Io(failure: IoFailure)
  case NoLock(path: Path)
  case NoRuntime

  def message: String = this match
    case Archive(error)       => error.message
    case BadMajor(raw)        => s"$raw is not a Node major"
    case Unreadable(path)     => s"could not read $path"
    case PathScan             => "could not read PATH"
    case Locations(error)     => error.message
    case Version(path, error) =>
      s"could not read the Node version at $path: ${error.message}"
    case Lock(error)     => error.message
    case Stage(error)    => error.message
    case Override(error) => error.message
    case Io(failure)     => failure.message
    case NoRuntime       => "no Node is available on PATH without a lock"
    case NoLock(path)    => s"no node lock at $path"
