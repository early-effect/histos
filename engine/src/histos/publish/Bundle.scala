package histos.publish

import java.io.{BufferedOutputStream, IOException}
import java.nio.file.{AtomicMoveNotSupportedException, Files, LinkOption, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, UIO, ZIO}

/** A path inside a bundle. Forward slashes, relative, with no empty or parent segment. */
opaque type BundlePath = String

object BundlePath:
  def from(raw: String): Either[BundleError, BundlePath] =
    val name = raw.replace('\\', '/')
    if refused(name) then Left(BundleError.Escaped(raw)) else Right(name)

  private def refused(name: String): Boolean =
    name.isEmpty ||
      name.startsWith("/") ||
      name.split("/", -1).exists(part => part.isEmpty || part == "." || part == "..")

  extension (path: BundlePath) def name: String = path
end BundlePath

/** One file the zipper will write. `bytes` is the declared size. `body` is read only after the cap accepts the total.
  */
final case class Staged(path: BundlePath, bytes: Long, body: IO[BundleError, Chunk[Byte]])

object Bundle:
  /** Central rejects a bundle over 1GB. A size equal to the cap is still a bundle. */
  val Cap: Long = 1024L * 1024L * 1024L

  def zip(stage: Path, to: Path): IO[BundleError, Unit] =
    ZIO
      .attemptBlocking(prepare(stage, to))
      .mapError(_ => BundleError.Io(stage.toString))
      .flatMap(ZIO.fromEither)
      .flatMap: files =>
        store(files, to)

  def zip(files: Chunk[Staged], to: Path): IO[BundleError, Unit] =
    ZIO.fromEither(accept(files.map(_.bytes), files.map(_.path.name))).flatMap(_ => write(files, to))

  private def prepare(stage: Path, to: Path): Either[BundleError, Chunk[Listed]] =
    if contained(stage, to) then Left(BundleError.Contained(to.toString))
    else
      inspect(stage).flatMap: files =>
        accept(files.map(_.bytes), files.map(_.path.name)).map(_ => files)

  private def contained(stage: Path, to: Path): Boolean =
    to.toAbsolutePath.normalize.startsWith(stage.toAbsolutePath.normalize)

  private def inspect(stage: Path): Either[BundleError, Chunk[Listed]] =
    if !Files.isDirectory(stage) then Left(BundleError.Missing(stage.toString))
    else
      val opened =
        try Right(Files.walk(stage))
        catch case _: IOException => Left(BundleError.Io(stage.toString))
      opened.flatMap: walk =>
        val paths =
          try Right(walk.iterator().asScala.toList)
          catch case _: IOException => Left(BundleError.Io(stage.toString))
          finally walk.close()
        paths.flatMap: found =>
          found
            .filter(path => Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS))
            .sortBy(_.toString)
            .foldLeft[Either[BundleError, Chunk[Listed]]](Right(Chunk.empty)):
              case (left @ Left(_), _) => left
              case (Right(acc), file)  => listed(stage, file).map(acc :+ _)

  private def listed(stage: Path, file: Path): Either[BundleError, Listed] =
    val relative = stage.relativize(file).iterator().asScala.map(_.toString).mkString("/")
    try
      val size = Files.size(file)
      BundlePath
        .from(relative)
        .flatMap: path =>
          if size < 0 then Left(BundleError.Size(size)) else Right(Listed(path, size, file))
    catch case _: IOException => Left(BundleError.Io(file.toString))

  private def accept(sizes: Chunk[Long], names: Chunk[String]): Either[BundleError, Unit] =
    gate(sizes).flatMap(_ => distinct(names))

  private def gate(sizes: Chunk[Long]): Either[BundleError, Long] =
    if sizes.isEmpty then Left(BundleError.Empty)
    else
      sizes
        .foldLeft[Either[BundleError, Long]](Right(0L)):
          case (left @ Left(_), _)                              => left
          case (Right(_), size) if size < 0                     => Left(BundleError.Size(size))
          case (Right(sum), size) if size > Long.MaxValue - sum => Left(BundleError.TooLarge(Long.MaxValue))
          case (Right(sum), size)                               => Right(sum + size)
        .flatMap: total =>
          if total > Cap then Left(BundleError.TooLarge(total)) else Right(total)

  private def distinct(names: Chunk[String]): Either[BundleError, Unit] =
    names
      .foldLeft[Either[BundleError, Set[String]]](Right(Set.empty)):
        case (left @ Left(_), _)               => left
        case (Right(seen), name) if seen(name) => Left(BundleError.Duplicate(name))
        case (Right(seen), name)               => Right(seen + name)
      .map(_ => ())

  private def store(files: Chunk[Listed], to: Path): IO[BundleError, Unit] =
    commit(to): stream =>
      files.foreach: file =>
        open(stream, file.path)
        Files.copy(file.file, stream)
        stream.closeEntry()

  private def write(files: Chunk[Staged], to: Path): IO[BundleError, Unit] =
    val temp = temporary(to)
    ZIO.acquireReleaseWith(ZIO.succeed(temp))(drop): _ =>
      for
        _ <- ZIO.attemptBlocking(Files.createDirectories(parent(to))).mapError(_ => BundleError.Io(to.toString))
        _ <- ZIO.scoped:
          for
            stream <- ZIO.acquireRelease(
              ZIO
                .attemptBlocking(new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(temp))))
                .mapError(_ => BundleError.Io(to.toString))
            )(stream => ZIO.attemptBlocking(stream.close()).ignore)
            _ <- ZIO.foreachDiscard(files): file =>
              file.body.flatMap: bytes =>
                ZIO
                  .attemptBlocking:
                    open(stream, file.path)
                    val array = bytes.toArray
                    stream.write(array)
                    stream.closeEntry()
                  .mapError(_ => BundleError.Io(to.toString))
          yield ()
        _ <- ZIO.attemptBlocking(move(temp, to)).mapError(_ => BundleError.Io(to.toString))
      yield ()

  private def commit(to: Path)(body: ZipOutputStream => Unit): IO[BundleError, Unit] =
    val temp = temporary(to)
    ZIO.acquireReleaseWith(ZIO.succeed(temp))(drop): _ =>
      ZIO
        .attemptBlocking:
          Files.createDirectories(parent(to))
          val stream = new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(temp)))
          try body(stream)
          finally stream.close()
          move(temp, to)
        .mapError(_ => BundleError.Io(to.toString))

  private def open(stream: ZipOutputStream, path: BundlePath): Unit =
    val entry = new ZipEntry(path.name)
    // The portal reads names and bytes. A moving timestamp would change the zip when the stage did not.
    entry.setTime(0L)
    stream.putNextEntry(entry)

  private def parent(to: Path): Path =
    Option(to.getParent).getOrElse(to)

  private def temporary(to: Path): Path =
    val fileName = Option(to.getFileName).map(_.toString).getOrElse("bundle.zip")
    parent(to).resolve(s".$fileName.tmp")

  private def move(temp: Path, to: Path): Unit =
    try Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch case _: AtomicMoveNotSupportedException => Files.move(temp, to, StandardCopyOption.REPLACE_EXISTING)

  private def drop(path: Path): UIO[Unit] =
    ZIO.attemptBlocking(Files.deleteIfExists(path)).ignore

  private final case class Listed(path: BundlePath, bytes: Long, file: Path)
end Bundle

enum BundleError:
  case TooLarge(bytes: Long)
  case Empty
  case Size(bytes: Long)
  case Escaped(name: String)
  case Duplicate(name: String)
  case Contained(path: String)
  case Missing(path: String)
  case Io(path: String)

  def message: String = this match
    case TooLarge(bytes) => s"bundle is $bytes bytes, over the 1GB cap"
    case Empty           => "the stage has nothing to zip"
    case Size(bytes)     => s"a staged file is $bytes bytes"
    case Escaped(name)   => s"'$name' leaves the bundle"
    case Duplicate(name) => s"'$name' is in the bundle twice"
    case Contained(path) => s"'$path' is inside the stage"
    case Missing(path)   => s"'$path' is not a stage"
    case Io(path)        => s"could not write '$path'"
end BundleError
