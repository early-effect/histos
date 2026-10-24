package histos.archive

import histos.workspace.{IoFailure, IoFault}
import org.apache.commons.compress.archivers.tar.{
  TarArchiveEntry,
  TarArchiveOutputStream,
  TarConstants
}
import org.apache.commons.compress.archivers.zip.{
  ZipArchiveEntry,
  ZipArchiveOutputStream
}
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream
import org.apache.commons.compress.compressors.xz.XZCompressorOutputStream

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.file.Path

import zio.{Chunk, IO, ZIO}

private[histos] object ArchiveFixture:
  enum Kind:
    case File, Directory, Symbolic, Hard, Fifo

    def flag: Byte = this match
      case File      => TarConstants.LF_NORMAL
      case Directory => TarConstants.LF_DIR
      case Symbolic  => TarConstants.LF_SYMLINK
      case Hard      => TarConstants.LF_LINK
      case Fifo      => TarConstants.LF_FIFO

  enum Names:
    case Pax, Gnu

  final case class Entry(
      name: String,
      body: Chunk[Byte] = Chunk.empty,
      mode: Int = 420,
      kind: Kind = Kind.File,
      link: String = ""
  )

  def zip(entries: (String, Chunk[Byte])*): IO[IoFailure, Chunk[Byte]] =
    zipEntries(entries.toList.map((name, bytes) => Entry(name, bytes)))

  def zipEntries(entries: List[Entry]): IO[IoFailure, Chunk[Byte]] =
    ZIO.scoped:
      for
        buffer <- make(new ByteArrayOutputStream)
        output <- ZIO.acquireRelease(make(new ZipArchiveOutputStream(buffer)))(
          close
        )
        _ <- ZIO.foreachDiscard(entries): value =>
          make:
            val entry = new ZipArchiveEntry(value.name)
            entry.setTime(0L)
            if value.kind == Kind.Symbolic then
              entry.setUnixMode(40960 | value.mode)
            output.putArchiveEntry(entry)
            output.write(if value.kind == Kind.Symbolic then
              value.link.getBytes(java.nio.charset.StandardCharsets.UTF_8)
            else value.body.toArray)
            output.closeArchiveEntry()
        _ <- make(output.finish())
        bytes <- make(Chunk.fromArray(buffer.toByteArray))
      yield bytes

  def tarGz(
      entries: List[Entry],
      names: Names = Names.Pax
  ): IO[IoFailure, Chunk[Byte]] =
    packed(entries, names)(buffer =>
      make(new GzipCompressorOutputStream(buffer))
    )

  def tarXz(entries: List[Entry]): IO[IoFailure, Chunk[Byte]] =
    packed(entries, Names.Pax)(buffer =>
      make(new XZCompressorOutputStream(buffer))
    )

  private def packed(entries: List[Entry], names: Names)(
      compress: ByteArrayOutputStream => IO[IoFailure, OutputStream]
  ): IO[IoFailure, Chunk[Byte]] =
    ZIO.scoped:
      for
        buffer <- make(new ByteArrayOutputStream)
        compressed <- ZIO.acquireRelease(compress(buffer))(close)
        output <- ZIO.acquireRelease(
          make(new TarArchiveOutputStream(compressed, "UTF-8"))
        )(close)
        _ <- make(output.setLongFileMode(names match
          case Names.Pax => TarArchiveOutputStream.LONGFILE_POSIX
          case Names.Gnu => TarArchiveOutputStream.LONGFILE_GNU))
        _ <- ZIO.foreachDiscard(entries): value =>
          make:
            val entry = new TarArchiveEntry(value.name, value.kind.flag, true)
            entry.setMode(value.mode)
            entry.setSize(value.body.length.toLong)
            entry.setLinkName(value.link)
            entry.setModTime(0L)
            output.putArchiveEntry(entry)
            output.write(value.body.toArray)
            output.closeArchiveEntry()
        _ <- make(output.finish())
        _ <- make(output.close())
        bytes <- make(Chunk.fromArray(buffer.toByteArray))
      yield bytes

  private def make[A](value: => A): IO[IoFailure, A] =
    ZIO
      .attemptBlocking(value)
      .mapError(_ => IoFailure(Path.of("archive-fixture"), IoFault.Write))

  private def close(output: OutputStream): IO[Nothing, Unit] =
    ZIO.attemptBlocking(output.close()).ignore
