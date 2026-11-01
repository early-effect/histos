package histos.central

import java.nio.file.{Path}

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
end PublishError
