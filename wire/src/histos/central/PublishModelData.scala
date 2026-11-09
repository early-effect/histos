package histos.central

import histos.doctor.{Diagnostic, Noted}

import java.nio.file.Path

/** Why a publish filesystem call failed. The path stays on `PublishError.Io`. */
enum PublishFault:
  case Stat, Write

  def word: String = this match
    case Stat  => "stat"
    case Write => "write"
end PublishFault

enum PublishError:
  case EmptyJar
  case NotAJar
  case NotADirectory(path: Path)
  case Escaped(path: Path)
  case Io(path: Path, fault: PublishFault)
  case Unauthorized
  case NotSnapshot(error: SnapshotRevisionError)
  case Status(code: Int)
  case Transport(reason: TransportFault)
  case Metadata(reason: MetadataFault)

  def message: String = this match
    case EmptyJar            => "a published jar is not empty"
    case NotAJar             => "a published jar is a zip"
    case NotADirectory(path) => s"$path is not a directory"
    case Escaped(path)       => s"$path leaves the repository"
    case Io(path, fault)     => s"could not ${fault.word} $path"
    case Unauthorized        => "sonatype rejected the credentials"
    case NotSnapshot(error)  => error.message
    case Status(code)        => s"publish failed with HTTP $code"
    case Transport(reason)   => reason.message
    case Metadata(reason)    => reason.message

  def diagnose: Diagnostic = this match
    case Unauthorized =>
      Noted.error(
        "publish.rejected",
        message,
        "The username and password were sent. The repository answered 401 or 403.",
        Noted.steps("Check the token at central.sonatype.com.", "A missing SONATYPE_USERNAME is a different error."),
      )
    case Status(code) =>
      Noted.carry(
        "publish.status",
        message,
        s"HTTP $code is a repository status, not a transport failure.",
        "histos help publish",
      )
    case Transport(reason) =>
      Noted.carry(
        "publish.transport",
        reason.message,
        "No HTTP status arrived. The repository did not reject the publish.",
        "histos help publish",
      )
    case Metadata(reason) =>
      Noted.carry("publish.metadata", reason.message, reason.message, "histos help publish")
    case other =>
      Noted.carry("publish.failed", other.message, other.message, "histos help publish")
end PublishError
