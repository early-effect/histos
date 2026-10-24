package histos.archive

import histos.workspace.IoFailure

import java.nio.file.Path

import zio.IO
import zio.stream.ZStream

enum ArchiveKind:
  case Zip, TarGz, TarXz

enum ArchiveEntryKind:
  case File, Directory
  case SymbolicLink(target: String)
  case HardLink(target: String)

final case class DecodedEntry(
    name: String,
    kind: ArchiveEntryKind,
    mode: Option[Int],
    body: ZStream[Any, ExtractFailure, Byte]
)

enum ExtractFailure:
  case UnknownArchive
  case Malformed(kind: ArchiveKind)
  case UnsupportedEntry(name: String)
  case UnsupportedData(name: String)
  case MissingMetadata
  case Io(failure: IoFailure)
  case Budget(error: ArchiveBudgetError)
  case UnconsumedEntry(name: String)
  case EntryClosed(name: String)
  case EntryAlreadyRead(name: String)

  def message(archive: Path): String = this match
    case UnknownArchive  => s"$archive is not a zip, a tar.gz, or a tar.xz"
    case Malformed(kind) => s"$archive is a malformed $kind archive"
    case UnsupportedEntry(name) => s"$archive has an unsupported entry: $name"
    case UnsupportedData(name)  => s"$archive has unsupported data: $name"
    case MissingMetadata        => s"$archive has missing entry metadata"
    case Io(failure)            => failure.message
    case Budget(error)          => error.message
    case UnconsumedEntry(name)  => s"archive body was not consumed: $name"
    case EntryClosed(name)      => s"archive entry scope is closed: $name"
    case EntryAlreadyRead(name) => s"archive entry body was already read: $name"

trait ArchiveDecoder:
  def fold[A, E](archive: Path, budget: ArchiveBudget, initial: A)(
      consume: (A, DecodedEntry) => IO[E, A]
  ): IO[ExtractFailure | E, A]
