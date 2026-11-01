package histos.jdk

import histos.toolchain.RuntimeIdentity
import histos.workspace.IoFailure
import java.nio.file.Path
import zio.Config

enum OverrideError:
  case Empty
  case Multiple
  case Relative(raw: String)
  case Invalid(raw: String)

  def message: String = this match
    case Empty         => "toolchain.local is empty"
    case Multiple      => "toolchain.local has more than one path"
    case Relative(raw) => s"toolchain.local path is not absolute: $raw"
    case Invalid(raw)  => s"toolchain.local path is invalid: $raw"
end OverrideError

enum ToolchainError:
  case Probe(error: JdkError)
  case Override(error: OverrideError)
  case TooOld(required: JdkVersion, found: JdkVersion, home: Path)
  case Absent(required: JdkVersion)
  case NoUserHome
  case NoArchitecture
  case Denied
  case Config(error: zio.Config.Error)
  case Io(failure: IoFailure)
  case Rejected(path: Path, reason: Reject)

  def message: String = this match
    case Probe(error)                  => error.message
    case Override(error)               => error.message
    case TooOld(required, found, home) =>
      s"JDK ${found.value} at $home is older than ${required.value}"
    case Absent(required) =>
      s"no JDK found; Histos needs ${required.value} or newer"
    case NoArchitecture         => "os.arch is not set"
    case NoUserHome             => "user.home is not set"
    case Denied                 => "the process environment could not be read"
    case Config(error)          => s"configuration: ${configText(error)}"
    case Io(failure)            => failure.message
    case Rejected(path, reason) =>
      s"JDK at $path was rejected: ${reason.message}"

  private def configText(error: zio.Config.Error): String =
    error.fold(
      (path, message) => s"${place(path)} $message",
      (path, message) => s"${place(path)} $message",
      (path, message, _) => s"${place(path)} $message",
      (path, message) => s"${place(path)} $message",
    )((left, right) => s"$left; $right", (left, right) => s"$left or $right")

  private def place(path: zio.Chunk[String]): String =
    if path.isEmpty then "config" else path.mkString(".")
end ToolchainError

/** The JDK this command is running on. */
final case class JdkSurvey(
    home: Path,
    version: JdkVersion,
    identity: RuntimeIdentity,
)
