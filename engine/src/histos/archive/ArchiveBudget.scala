package histos.archive

import zio.ZIO
import zio.stream.ZStream

enum ArchiveLimit:
  case DownloadBytes, DecompressedBytes, ExtractedBytes, EntryBytes, Entries,
    MetadataBytes, DecoderMemoryKiB

enum ArchiveBudgetError:
  case Invalid(limit: ArchiveLimit, value: Long)
  case Exceeded(limit: ArchiveLimit, maximum: Long)

  def message: String = this match
    case Invalid(limit, value)    => s"invalid archive budget $limit: $value"
    case Exceeded(limit, maximum) => s"archive exceeded $limit budget $maximum"

final class ArchiveBudget private (val settings: ArchiveBudget.Settings):
  def downloadBytes: Long = settings.downloadBytes
  def decompressedBytes: Long = settings.decompressedBytes
  def extractedBytes: Long = settings.extractedBytes
  def entryBytes: Long = settings.entryBytes
  def entries: Int = settings.entries
  def metadataBytes: Long = settings.metadataBytes
  def decoderMemoryKiB: Int = settings.decoderMemoryKiB

  def maximum(limit: ArchiveLimit): Long = limit match
    case ArchiveLimit.DownloadBytes     => downloadBytes
    case ArchiveLimit.DecompressedBytes => decompressedBytes
    case ArchiveLimit.ExtractedBytes    => extractedBytes
    case ArchiveLimit.EntryBytes        => entryBytes
    case ArchiveLimit.Entries           => entries.toLong
    case ArchiveLimit.MetadataBytes     => metadataBytes
    case ArchiveLimit.DecoderMemoryKiB  => decoderMemoryKiB.toLong

  def download[E](
      stream: ZStream[Any, E, Byte]
  ): ZStream[Any, E | ArchiveBudgetError, Byte] =
    stream.chunks
      .mapAccumZIO(0L): (count, bytes) =>
        if bytes.length.toLong > downloadBytes - count then
          ZIO.fail(
            ArchiveBudgetError
              .Exceeded(ArchiveLimit.DownloadBytes, downloadBytes)
          )
        else ZIO.succeed((count + bytes.length) -> bytes)
      .flattenChunks

object ArchiveBudget:
  final case class Settings(
      downloadBytes: Long,
      decompressedBytes: Long,
      extractedBytes: Long,
      entryBytes: Long,
      entries: Int,
      metadataBytes: Long,
      decoderMemoryKiB: Int
  )

  private val GiB = 1024L * 1024 * 1024
  val default: ArchiveBudget = new ArchiveBudget(
    Settings(GiB, 8 * GiB, 4 * GiB, GiB, 100000, 16L * 1024 * 1024, 256 * 1024)
  )

  def from(settings: Settings): Either[ArchiveBudgetError, ArchiveBudget] =
    val candidate = new ArchiveBudget(settings)
    for _ <- ArchiveLimit.values.foldLeft[Either[ArchiveBudgetError, Unit]](
        Right(())
      )((acc, field) =>
        acc.flatMap(_ => valid(field, candidate.maximum(field)))
      )
    yield candidate

  private def valid(
      limit: ArchiveLimit,
      value: Long
  ): Either[ArchiveBudgetError, Unit] =
    Either.cond(
      value > 0 && value <= Long.MaxValue / 2 && (limit != ArchiveLimit.Entries || value < Int.MaxValue.toLong),
      (),
      ArchiveBudgetError.Invalid(limit, value)
    )
