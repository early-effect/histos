package histos.pack

import java.io.BufferedOutputStream
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

enum JarIo:
  case Read, Write, Move

enum JarError:
  case NotADirectory(path: Path)
  case Empty(path: Path)
  case Escaped(name: String)
  case DestinationInside(path: Path)
  case Io(op: JarIo)

  def message: String = this match
    case NotADirectory(path)   => s"$path is not a directory of classes"
    case Empty(path)           => s"$path has no class files"
    case Escaped(name)         => s"'$name' leaves the classes directory"
    case DestinationInside(path) => s"$path is inside the classes directory"
    case Io(op)                => s"jar failed while ${op.toString.toLowerCase}"

final case class Jarred(path: Path, entries: Int)

object ClassJar:
  /** Zip the class files. Entry names are relative and sorted. A file that leaves the directory is refused. */
  def write(classes: Path, destination: Path): IO[JarError, Jarred] =
    for
      files <- entries(classes, destination)
      _     <- place(destination, files)
    yield Jarred(destination, files.length)

  private def entries(classes: Path, destination: Path): IO[JarError, List[(String, Path)]] =
    ZIO.attemptBlocking {
      val base = classes.toAbsolutePath.normalize
      val dest = destination.toAbsolutePath.normalize
      if !Files.isDirectory(classes) then Left(JarError.NotADirectory(classes))
      else if dest.startsWith(base) then Left(JarError.DestinationInside(destination))
      else
        val walk = Files.walk(classes)
        val files =
          try walk.iterator.asScala.filter(path => Files.isRegularFile(path)).toList
          finally walk.close()
        files.sortBy(_.toString).foldLeft[Either[JarError, List[(String, Path)]]](Right(Nil)): (acc, file) =>
          acc.flatMap: seen =>
            val absolute = file.toAbsolutePath.normalize
            val relative = base.relativize(absolute)
            val name = relative.iterator.asScala.map(_.toString).mkString("/")
            if !absolute.startsWith(base) || relative.startsWith("..") then Left(JarError.Escaped(name))
            else Right(seen :+ (name, file))
        .flatMap: named =>
          if named.isEmpty then Left(JarError.Empty(classes)) else Right(named)
    }.mapError(_ => JarError.Io(JarIo.Read)).flatMap(ZIO.fromEither)

  private def place(destination: Path, files: List[(String, Path)]): IO[JarError, Unit] =
    val parent = Option(destination.getParent).getOrElse(destination)
    val fileName = Option(destination.getFileName).map(_.toString).getOrElse("classes.jar")
    val temp = parent.resolve(s"$fileName.tmp")
    ZIO.acquireReleaseWith(ZIO.succeed(temp))(leftover => ZIO.attemptBlocking(Files.deleteIfExists(leftover)).ignore) { _ =>
      ZIO.attemptBlocking {
        Files.createDirectories(parent)
        val stream = new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(temp)))
        try
          files.foreach: (name, file) =>
            stream.putNextEntry(new ZipEntry(name))
            Files.copy(file, stream)
            stream.closeEntry()
        finally stream.close()
        try Files.move(temp, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch
          case _: AtomicMoveNotSupportedException =>
            Files.move(temp, destination, StandardCopyOption.REPLACE_EXISTING)
        ()
      }.mapError(_ => JarError.Io(JarIo.Write))
    }
