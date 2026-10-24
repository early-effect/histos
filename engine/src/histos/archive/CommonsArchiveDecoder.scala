package histos.archive

import histos.workspace.{IoFailure, IoFault}
import org.apache.commons.compress.archivers.tar.{
  TarArchiveEntry,
  TarArchiveInputStream,
  TarConstants
}
import org.apache.commons.compress.archivers.zip.{
  UnixStat,
  ZipArchiveEntry,
  ZipArchiveInputStream,
  ZipFile
}
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.commons.compress.compressors.xz.XZCompressorInputStream
import org.apache.commons.io.input.BoundedInputStream

import java.io.{BufferedInputStream, InputStream}
import java.nio.file.{Files, Path}
import java.util.zip.{CRC32, ZipEntry}
import java.util.concurrent.atomic.AtomicReference
import org.apache.commons.compress.MemoryLimitException

import zio.{IO, Ref, Scope, ZIO, ZLayer}
import zio.stream.ZStream

final case class CommonsArchiveDecoder() extends ArchiveDecoder:
  def fold[A, E](archive: Path, budget: ArchiveBudget, initial: A)(
      consume: (A, DecodedEntry) => IO[E, A]
  ): IO[ExtractFailure | E, A] =
    entries(archive, budget).zipWithIndex.runFoldZIO(initial):
      (state, indexed) =>
        val (entry, index) = indexed
        if index >= budget.entries then
          ZIO.fail(
            ExtractFailure.Budget(
              ArchiveBudgetError
                .Exceeded(ArchiveLimit.Entries, budget.entries.toLong)
            )
          )
        else owned(entry)(consume(state, _))

  private enum BodyState:
    case Available, Reading, Consumed, Closed

  private def owned[A, E](entry: DecodedEntry)(
      consume: DecodedEntry => IO[E, A]
  ): IO[ExtractFailure | E, A] =
    ZIO.scoped:
      for
        state <- Ref.make(BodyState.Available)
        _ <- ZIO.acquireRelease(ZIO.unit)(_ => state.set(BodyState.Closed))
        body = ZStream.unwrap(
          state
            .modify:
              case BodyState.Available => Right(()) -> BodyState.Reading
              case BodyState.Closed    =>
                Left(ExtractFailure.EntryClosed(entry.name)) -> BodyState.Closed
              case found =>
                Left(ExtractFailure.EntryAlreadyRead(entry.name)) -> found
            .flatMap(ZIO.fromEither(_))
            .as(
              entry.body ++ ZStream.fromZIO(state.set(BodyState.Consumed)).drain
            )
        )
        result <- consume(entry.copy(body = body))
        finished <- state.get
        _ <- ZIO
          .fail(ExtractFailure.UnconsumedEntry(entry.name))
          .when(
            entry.kind == ArchiveEntryKind.File && finished != BodyState.Consumed
          )
      yield result

  private def entries(
      archive: Path,
      budget: ArchiveBudget
  ): ZStream[Any, ExtractFailure, DecodedEntry] =
    ZStream.unwrapScoped:
      for
        raw <- ZIO.acquireRelease(
          ZIO
            .attemptBlocking(
              new BufferedInputStream(Files.newInputStream(archive))
            )
            .mapError(_ => ExtractFailure.Io(IoFailure(archive, IoFault.Read)))
        )(input => ZIO.attemptBlocking(input.close()).ignore)
        signature <- ZIO
          .attemptBlocking:
            raw.mark(12)
            val bytes = raw.readNBytes(12)
            raw.reset()
            bytes
          .mapError(_ => ExtractFailure.Io(IoFailure(archive, IoFault.Read)))
        stream <-
          if ZipArchiveInputStream.matches(signature, signature.length) then
            zip(archive, budget)
          else if GzipCompressorInputStream.matches(signature, signature.length)
          then tar(ArchiveKind.TarGz, raw, budget)
          else if XZCompressorInputStream.matches(signature, signature.length)
          then tar(ArchiveKind.TarXz, raw, budget)
          else ZIO.fail(ExtractFailure.UnknownArchive)
      yield stream

  private def zip(
      archive: Path,
      budget: ArchiveBudget
  ): ZIO[Scope, ExtractFailure, ZStream[Any, ExtractFailure, DecodedEntry]] =
    for
      channel <- ZIO.acquireRelease(
        ZIO
          .attemptBlocking(Files.newByteChannel(archive))
          .mapError(_ => ExtractFailure.Io(IoFailure(archive, IoFault.Read)))
      )(channel => ZIO.attemptBlocking(channel.close()).ignore)
      file <- ZIO.acquireRelease(
        decode(ArchiveKind.Zip)(
          ZipFile.builder().setSeekableByteChannel(channel).get()
        )
      )(file => ZIO.attemptBlocking(file.close()).ignore)
      entries <- decode(ArchiveKind.Zip)(file.getEntries)
      inflated <- Ref.make(0L)
    yield ZStream.repeatZIOOption:
      decode(ArchiveKind.Zip)(if entries.hasMoreElements then
        Option(entries.nextElement())
      else None)
        .mapError(Some(_))
        .flatMap(entry => ZIO.fromOption(entry))
        .flatMap(entry =>
          zipEntry(file, entry, budget, inflated).mapError(Some(_))
        )

  private def zipEntry(
      file: ZipFile,
      entry: ZipArchiveEntry,
      budget: ArchiveBudget,
      inflated: Ref[Long]
  ): IO[ExtractFailure, DecodedEntry] =
    for
      name <- metadata(entry.getName)
      _ <- ZIO
        .fail(ExtractFailure.UnsupportedData(name))
        .unless(
          file.canReadEntryData(entry) && Set(
            ZipEntry.STORED,
            ZipEntry.DEFLATED
          ).contains(entry.getMethod)
        )
      kind <-
        if entry.isUnixSymlink then
          for
            _ <- ZIO
              .fail(ExtractFailure.Malformed(ArchiveKind.Zip))
              .when(entry.getSize > 4096)
            _ <- zipBody(file, entry, budget, inflated).runDrain
            target <- decode(ArchiveKind.Zip)(
              Option(file.getUnixSymlink(entry))
            )
              .someOrFail(ExtractFailure.MissingMetadata)
          yield ArchiveEntryKind.SymbolicLink(target)
        else
          val fileType = entry.getUnixMode & UnixStat.FILE_TYPE_FLAG
          if fileType != 0 && fileType != UnixStat.FILE_FLAG && fileType != UnixStat.DIR_FLAG
          then ZIO.fail(ExtractFailure.UnsupportedEntry(name))
          else
            ZIO.succeed(if entry.isDirectory then ArchiveEntryKind.Directory
            else ArchiveEntryKind.File)
      _ <- ZIO
        .fail(ExtractFailure.Malformed(ArchiveKind.Zip))
        .when(kind == ArchiveEntryKind.Directory && entry.getSize != 0)
      mode = Option.when(entry.getUnixMode != 0)(
        entry.getUnixMode & UnixStat.PERM_MASK
      )
    yield DecodedEntry(
      name,
      kind,
      mode,
      if kind == ArchiveEntryKind.File then
        zipBody(file, entry, budget, inflated)
      else ZStream.empty
    )

  private def zipBody(
      file: ZipFile,
      entry: ZipArchiveEntry,
      budget: ArchiveBudget,
      inflated: Ref[Long]
  ): ZStream[Any, ExtractFailure, Byte] =
    ZStream.unwrapScoped:
      for
        input <- ZIO.acquireRelease(
          decode(ArchiveKind.Zip)(Option(file.getInputStream(entry)))
            .someOrFail(ExtractFailure.MissingMetadata)
        )(input => ZIO.attemptBlocking(input.close()).ignore)
        count <- Ref.make(0L)
        crc <- ZIO
          .attemptBlocking(new CRC32)
          .mapError(_ => ExtractFailure.Malformed(ArchiveKind.Zip))
      yield
        val bytes = ZStream
          .fromInputStream(input)
          .mapError(_ => ExtractFailure.Malformed(ArchiveKind.Zip))
          .mapChunksZIO(chunk =>
            (inflated
              .modify(total =>
                if chunk.length.toLong > budget.decompressedBytes - total then
                  Left(
                    ExtractFailure.Budget(
                      ArchiveBudgetError.Exceeded(
                        ArchiveLimit.DecompressedBytes,
                        budget.decompressedBytes
                      )
                    )
                  ) -> total
                else Right(()) -> (total + chunk.length)
              )
              .flatMap(ZIO.fromEither(_)) *> decode(ArchiveKind.Zip)(
              crc.update(chunk.toArray)
            ) *> count.update(_ + chunk.length)).as(chunk)
          )
        bytes ++ ZStream
          .fromZIO(
            for
              actual <- decode(ArchiveKind.Zip)(crc.getValue)
              size <- count.get
              _ <- ZIO
                .fail(ExtractFailure.Malformed(ArchiveKind.Zip))
                .unless(actual == entry.getCrc && size == entry.getSize)
            yield ()
          )
          .drain

  private def tar(
      kind: ArchiveKind,
      raw: InputStream,
      budget: ArchiveBudget
  ): ZIO[Scope, ExtractFailure, ZStream[Any, ExtractFailure, DecodedEntry]] =
    for
      compressed <- ZIO.acquireRelease(compression(kind, budget):
        kind match
          case ArchiveKind.TarGz =>
            GzipCompressorInputStream
              .builder()
              .setInputStream(raw)
              .setDecompressConcatenated(true)
              .get()
          case ArchiveKind.TarXz =>
            XZCompressorInputStream
              .builder()
              .setInputStream(raw)
              .setDecompressConcatenated(true)
              .setMemoryLimitKiB(budget.decoderMemoryKiB)
              .get()
          case ArchiveKind.Zip => raw)(input =>
        ZIO.attemptBlocking(input.close()).ignore
      )
      bounded <- ZIO.acquireRelease(
        decode(kind)(
          BoundedInputStream
            .builder()
            .setInputStream(compressed)
            .setMaxCount(budget.decompressedBytes + 1)
            .get()
        )
      )(input => ZIO.attemptBlocking(input.close()).ignore)
      switched <- decode(kind)(new EntryInput(bounded))
      input <- ZIO.acquireRelease(
        tarRead(kind, bounded, budget)(
          new TarArchiveInputStream(switched, "UTF-8")
        )
      )(input => ZIO.attemptBlocking(input.close()).ignore)
    yield
      val entries = ZStream.repeatZIOOption:
        nextTarEntry(input, switched, kind, bounded, budget)
          .mapError(Some(_))
          .flatMap(entry => ZIO.fromOption(entry))
          .flatMap(entry =>
            tarEntry(input, entry, kind, bounded, budget).mapError(Some(_))
          )
      entries ++ ZStream
        .fromInputStream(bounded)
        .mapError(error => tarFailure(kind, bounded, budget, Some(error)))
        .mapChunksZIO(chunk => tarRead(kind, bounded, budget)(chunk))
        .drain

  private final class EntryInput(source: InputStream) extends InputStream:
    private val current = new AtomicReference[InputStream](source)
    def use(input: InputStream): Unit = current.set(input)
    override def read(): Int = current.get().read()
    override def read(bytes: Array[Byte], offset: Int, length: Int): Int =
      current.get().read(bytes, offset, length)
    override def skip(count: Long): Long = current.get().skip(count)
    override def available(): Int = current.get().available()
    override def close(): Unit = source.close()

  private def nextTarEntry(
      input: TarArchiveInputStream,
      switched: EntryInput,
      kind: ArchiveKind,
      bounded: BoundedInputStream,
      budget: ArchiveBudget
  ): IO[ExtractFailure, Option[TarArchiveEntry]] =
    ZIO.scoped:
      for
        metadata <- ZIO.acquireRelease(
          decode(kind)(
            BoundedInputStream
              .builder()
              .setInputStream(bounded)
              .setMaxCount(budget.metadataBytes + 1)
              .setPropagateClose(false)
              .get()
          )
        )(stream => ZIO.attemptBlocking(stream.close()).ignore)
        _ <- decode(kind)(switched.use(metadata))
        entry <- tarRead(kind, bounded, budget)(Option(input.getNextEntry))
          .mapError(error =>
            if metadata.getCount > budget.metadataBytes then
              ExtractFailure.Budget(
                ArchiveBudgetError
                  .Exceeded(ArchiveLimit.MetadataBytes, budget.metadataBytes)
              )
            else error
          )
          .ensuring(ZIO.succeed(switched.use(bounded)))
        _ <- ZIO
          .fail(
            ExtractFailure.Budget(
              ArchiveBudgetError
                .Exceeded(ArchiveLimit.MetadataBytes, budget.metadataBytes)
            )
          )
          .when(metadata.getCount > budget.metadataBytes)
      yield entry

  private def compression[A](kind: ArchiveKind, budget: ArchiveBudget)(
      value: => A
  ): IO[ExtractFailure, A] =
    ZIO
      .attemptBlocking(value)
      .mapError:
        case _: MemoryLimitException =>
          ExtractFailure.Budget(
            ArchiveBudgetError.Exceeded(
              ArchiveLimit.DecoderMemoryKiB,
              budget.decoderMemoryKiB.toLong
            )
          )
        case _ => ExtractFailure.Malformed(kind)

  private def tarEntry(
      input: TarArchiveInputStream,
      entry: TarArchiveEntry,
      kind: ArchiveKind,
      bounded: BoundedInputStream,
      budget: ArchiveBudget
  ): IO[ExtractFailure, DecodedEntry] =
    for
      name <- metadata(entry.getName)
      _ <- ZIO.fail(ExtractFailure.Malformed(kind)).unless(entry.isCheckSumOK)
      _ <- ZIO
        .fail(ExtractFailure.UnsupportedData(name))
        .unless(input.canReadEntryData(entry))
      entryKind <- entry.getLinkFlag match
        case TarConstants.LF_SYMLINK =>
          metadata(entry.getLinkName).map(ArchiveEntryKind.SymbolicLink(_))
        case TarConstants.LF_LINK =>
          metadata(entry.getLinkName).map(ArchiveEntryKind.HardLink(_))
        case TarConstants.LF_DIR => ZIO.succeed(ArchiveEntryKind.Directory)
        case TarConstants.LF_NORMAL | TarConstants.LF_OLDNORM |
            TarConstants.LF_CONTIG if !entry.isSparse =>
          ZIO.succeed(ArchiveEntryKind.File)
        case _ => ZIO.fail(ExtractFailure.UnsupportedEntry(name))
      _ <- ZIO
        .fail(ExtractFailure.Malformed(kind))
        .when(entryKind != ArchiveEntryKind.File && entry.getSize != 0)
    yield DecodedEntry(
      name,
      entryKind,
      Some(entry.getMode),
      ZStream
        .fromInputStream(input)
        .mapError(error => tarFailure(kind, bounded, budget, Some(error)))
        .mapChunksZIO(chunk => tarRead(kind, bounded, budget)(chunk))
    )

  private def tarFailure(
      kind: ArchiveKind,
      input: BoundedInputStream,
      budget: ArchiveBudget,
      error: Option[Throwable] = None
  ): ExtractFailure =
    if input.getCount > budget.decompressedBytes then
      ExtractFailure.Budget(
        ArchiveBudgetError
          .Exceeded(ArchiveLimit.DecompressedBytes, budget.decompressedBytes)
      )
    else
      error match
        case Some(_: MemoryLimitException) =>
          ExtractFailure.Budget(
            ArchiveBudgetError.Exceeded(
              ArchiveLimit.DecoderMemoryKiB,
              budget.decoderMemoryKiB.toLong
            )
          )
        case _ => ExtractFailure.Malformed(kind)

  private def tarRead[A](
      kind: ArchiveKind,
      input: BoundedInputStream,
      budget: ArchiveBudget
  )(value: => A): IO[ExtractFailure, A] =
    ZIO
      .attemptBlocking(value)
      .mapError(error => tarFailure(kind, input, budget, Some(error)))
      .flatMap(result =>
        if input.getCount > budget.decompressedBytes then
          ZIO.fail(tarFailure(kind, input, budget))
        else ZIO.succeed(result)
      )

  private def metadata(value: String): IO[ExtractFailure, String] =
    ZIO.fromOption(Option(value)).orElseFail(ExtractFailure.MissingMetadata)

  private def decode[A](kind: ArchiveKind)(value: => A): IO[ExtractFailure, A] =
    ZIO.attemptBlocking(value).mapError(_ => ExtractFailure.Malformed(kind))

object CommonsArchiveDecoder:
  val layer: ZLayer[Any, Nothing, ArchiveDecoder] =
    ZLayer.scoped(ZIO.succeed(CommonsArchiveDecoder()))
