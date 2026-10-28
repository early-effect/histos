package histos.pack

import java.io.BufferedOutputStream
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, ZIO}

enum JarIo:
  case Read(path: Path)
  case Write(path: Path)
  case Move(path: Path)

  def word: String = this match
    case Read(_)  => "read"
    case Write(_) => "write"
    case Move(_)  => "move"

  def location: Path = this match
    case Read(path)  => path
    case Write(path) => path
    case Move(path)  => path
end JarIo

enum JarError:
  case NotADirectory(path: Path)
  case Empty(path: Path)
  case Escaped(name: String)
  case Duplicate(name: String)
  case DestinationInside(path: Path)
  case Io(op: JarIo)

  def message: String = this match
    case NotADirectory(path)     => s"$path is not a directory of classes"
    case Empty(path)             => s"$path has nothing to jar"
    case Escaped(name)           => s"'$name' leaves the jar"
    case Duplicate(name)         => s"'$name' is already a jar entry"
    case DestinationInside(path) => s"$path is inside the classes directory"
    case Io(op)                  => s"jar failed while ${op.word} ${op.location}"
end JarError

final case class Jarred(path: Path, entries: Int)

object ClassJar:
  /** Zip the class files. Entry names are relative and sorted. A file that leaves the directory is refused. */
  def write(classes: Path, destination: Path): IO[JarError, Jarred] =
    write(classes, Nil, destination)

  /** Class files plus named extras. A repeated entry name is refused. */
  def write(
      classes: Path,
      extra: List[(String, Path)],
      destination: Path,
  ): IO[JarError, Jarred] =
    for
      files <- entries(classes, destination)
      _     <- accept(extra, files.map(_._1))
      named = files ++ extra
      _ <- place(destination, named.sortBy(_._1))
    yield Jarred(destination, named.length)

  def writeNamed(files: List[(String, Path)], destination: Path): IO[JarError, Jarred] =
    for
      _ <- checked(files.map(_._1), destination)
      _ <- place(destination, files.sortBy(_._1))
    yield Jarred(destination, files.length)

  def writeBytes(entries: List[(String, Chunk[Byte])], destination: Path): IO[JarError, Jarred] =
    for
      _ <- checked(entries.map(_._1), destination)
      _ <- commit(destination): stream =>
        entries
          .sortBy(_._1)
          .foreach: (name, bytes) =>
            stream.putNextEntry(new ZipEntry(name))
            stream.write(bytes.toArray)
            stream.closeEntry()
    yield Jarred(destination, entries.length)

  private def entries(classes: Path, destination: Path): IO[JarError, List[(String, Path)]] =
    ZIO
      .attemptBlocking {
        val base = classes.toAbsolutePath.normalize
        val dest = destination.toAbsolutePath.normalize
        if !Files.isDirectory(classes) then Left(JarError.NotADirectory(classes))
        else if dest.startsWith(base) then Left(JarError.DestinationInside(destination))
        else
          val walk  = Files.walk(classes)
          val files =
            try walk.iterator.asScala.filter(path => Files.isRegularFile(path)).toList
            finally walk.close()
          files
            .sortBy(_.toString)
            .foldLeft[Either[JarError, List[(String, Path)]]](Right(Nil)): (acc, file) =>
              acc.flatMap: seen =>
                val absolute = file.toAbsolutePath.normalize
                val relative = base.relativize(absolute)
                val name     = relative.iterator.asScala.map(_.toString).mkString("/")
                if !absolute.startsWith(base) || relative.startsWith("..") then Left(JarError.Escaped(name))
                else Right(seen :+ (name, file))
            .flatMap: named =>
              if named.isEmpty then Left(JarError.Empty(classes)) else Right(named)
      }
      .mapError(_ => JarError.Io(JarIo.Read(classes)))
      .flatMap(ZIO.fromEither)

  private def accept(extra: List[(String, Path)], have: List[String]): IO[JarError, Unit] =
    extra match
      case Nil => ZIO.unit
      case _   =>
        extra.map(_._1).collectFirst { case name if rejected(name) => name } match
          case Some(name) => ZIO.fail(JarError.Escaped(name))
          case None       =>
            (have ++ extra.map(_._1)).groupBy(identity).collectFirst {
              case (name, found) if found.lengthIs > 1 => name
            } match
              case Some(name) => ZIO.fail(JarError.Duplicate(name))
              case None       => ZIO.unit

  private def checked(names: List[String], destination: Path): IO[JarError, Unit] =
    names match
      case Nil => ZIO.fail(JarError.Empty(destination))
      case _   =>
        names.collectFirst { case name if rejected(name) => name } match
          case Some(name) => ZIO.fail(JarError.Escaped(name))
          case None       =>
            names.groupBy(identity).collectFirst { case (name, found) if found.lengthIs > 1 => name } match
              case Some(name) => ZIO.fail(JarError.Duplicate(name))
              case None       => ZIO.unit

  private def rejected(name: String): Boolean =
    name.isEmpty ||
      name.startsWith("/") ||
      name.contains('\\') ||
      name.split("/", -1).exists(part => part.isEmpty || part == "." || part == "..")

  private def place(destination: Path, files: List[(String, Path)]): IO[JarError, Unit] =
    commit(destination): stream =>
      files.foreach: (name, file) =>
        stream.putNextEntry(new ZipEntry(name))
        Files.copy(file, stream)
        stream.closeEntry()

  private def commit(destination: Path)(body: ZipOutputStream => Unit): IO[JarError, Unit] =
    val parent   = Option(destination.getParent).getOrElse(destination)
    val fileName = Option(destination.getFileName).map(_.toString).getOrElse("classes.jar")
    val temp     = parent.resolve(s"$fileName.tmp")
    ZIO.acquireReleaseWith(ZIO.succeed(temp))(leftover => ZIO.attemptBlocking(Files.deleteIfExists(leftover)).ignore) {
      _ =>
        ZIO
          .attemptBlocking {
            Files.createDirectories(parent)
            val stream = new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(temp)))
            try body(stream)
            finally stream.close()
            try Files.move(temp, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
            catch
              case _: AtomicMoveNotSupportedException =>
                Files.move(temp, destination, StandardCopyOption.REPLACE_EXISTING)
            ()
          }
          .mapError(_ => JarError.Io(JarIo.Write(temp)))
    }
end ClassJar
