package histos.native

import java.io.{IOException, UncheckedIOException}
import java.nio.file.{AccessDeniedException, DirectoryIteratorException, Files, NoSuchFileException, Path}
import java.nio.file.attribute.BasicFileAttributes
import java.util.stream.Stream
import java.util.zip.{ZipEntry, ZipFile}

import scala.jdk.CollectionConverters.*

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode

import zio.{IO, URIO, ZIO}

/** Superclass names from `.class` files. Native tests are not loaded on the JVM. */
object ClassShapes:
  final case class Shape(name: String, module: Boolean, parents: List[String])

  def read(classpath: List[Path]): IO[ClassShapeError, List[Shape]] =
    ZIO
      .foldLeft(classpath)(Map.empty[String, Shape]): (seen, path) =>
        scan(path).map: found =>
          found.foldLeft(seen)((have, shape) => have.updated(shape.name, shape))
      .map(_.values.toList.sortBy(_.name))

  private def scan(path: Path): IO[ClassShapeError, List[Shape]] =
    kind(path).flatMap:
      case Kind.Directory => directory(path)
      case Kind.Jar       => jar(path)
      case Kind.Other     => ZIO.succeed(Nil)

  private def kind(path: Path): IO[ClassShapeError, Kind] =
    ZIO
      .attemptBlocking(Files.readAttributes(path, classOf[BasicFileAttributes]))
      .foldZIO(
        thrown =>
          ioKind(thrown) match
            case IoKind.Missing => ZIO.succeed(Kind.Other)
            case IoKind.Read    => ZIO.fail(ClassShapeError.Read(path))
            case IoKind.Die     => ZIO.die(thrown)
        ,
        attrs =>
          val found =
            if attrs.isDirectory then Kind.Directory
            else if attrs.isRegularFile && fileName(path).endsWith(".jar") then Kind.Jar
            else Kind.Other
          ZIO.succeed(found),
      )

  private def directory(root: Path): IO[ClassShapeError, List[Shape]] =
    walk(root, List.empty[Shape]): opened =>
      ZIO
        .attemptBlocking(opened.iterator.asScala)
        .foldZIO(ioMiss(root, Nil), paths => step(root, paths, Nil))
        .map(_.reverse)

  private def walk[A](root: Path, missing: A)(
      use: Stream[Path] => IO[ClassShapeError, A]
  ): IO[ClassShapeError, A] =
    ZIO.scoped:
      ZIO
        .acquireRelease(ZIO.attemptBlocking(Files.walk(root)))(closeStream)
        .foldZIO(ioMiss(root, missing), use)

  private def step(
      root: Path,
      paths: Iterator[Path],
      acc: List[Shape],
  ): IO[ClassShapeError, List[Shape]] =
    ZIO
      .attemptBlocking(Option.when(paths.hasNext)(paths.next()))
      .foldZIO(
        ioMiss(root, acc),
        {
          case None       => ZIO.succeed(acc)
          case Some(path) =>
            one(root, path).flatMap:
              case None        => step(root, paths, acc)
              case Some(found) => step(root, paths, found :: acc)
        },
      )

  private def one(root: Path, path: Path): IO[ClassShapeError, Option[Shape]] =
    if !fileName(path).endsWith(".class") then ZIO.succeed(None)
    else
      ZIO
        .attemptBlocking(Files.readAttributes(path, classOf[BasicFileAttributes]))
        .foldZIO(
          ioMiss(root, None),
          attrs =>
            if !attrs.isRegularFile then ZIO.succeed(None)
            else
              ZIO
                .attemptBlocking(Files.readAllBytes(path))
                .foldZIO(ioMiss(root, None), bytes => parse(path.toString, bytes)),
        )

  private def jar(path: Path): IO[ClassShapeError, List[Shape]] =
    withZip(path, List.empty[Shape]): zip =>
      ZIO
        .attemptBlocking(zip.entries.asScala.toList)
        .foldZIO(ioMiss(path, Nil), entries => readEntries(path, zip, entries))

  private def withZip[A](path: Path, missing: A)(use: ZipFile => IO[ClassShapeError, A]): IO[ClassShapeError, A] =
    ZIO.scoped:
      ZIO
        .acquireRelease(ZIO.attemptBlocking(new ZipFile(path.toFile)))(closeZip)
        .foldZIO(ioMiss(path, missing), use)

  private def readEntries(path: Path, zip: ZipFile, entries: List[ZipEntry]): IO[ClassShapeError, List[Shape]] =
    ZIO
      .foldLeft(entries)(List.empty[Shape]): (acc, entry) =>
        Option(entry.getName) match
          case Some(name) if !entry.isDirectory && name.endsWith(".class") =>
            readEntry(path, zip, entry, s"${path.toString}:$name").map:
              case None        => acc
              case Some(found) => found :: acc
          case _ =>
            ZIO.succeed(acc)
      .map(_.reverse)

  private def readEntry(
      path: Path,
      zip: ZipFile,
      entry: ZipEntry,
      label: String,
  ): IO[ClassShapeError, Option[Shape]] =
    ZIO
      .attemptBlocking(Option(zip.getInputStream(entry)))
      .foldZIO(
        ioMiss(path, None),
        {
          case None        => ZIO.fail(ClassShapeError.Read(path))
          case Some(input) =>
            ZIO.scoped:
              ZIO
                .acquireRelease(ZIO.succeed(input))(closeInput)
                .flatMap: opened =>
                  ZIO.attemptBlocking(opened.readAllBytes()).foldZIO(ioMiss(path, None), bytes => parse(label, bytes))
        },
      )

  private def parse(label: String, bytes: Array[Byte]): IO[ClassShapeError, Option[Shape]] =
    ZIO.attempt(parsed(bytes)).foldZIO(classFail(label), ZIO.succeed)

  private def parsed(bytes: Array[Byte]): Option[Shape] =
    val reader = new ClassReader(bytes)
    val node   = new ClassNode()
    reader.accept(node, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
    val binary = Option(node.name).map(_.replace('/', '.')).getOrElse("")
    if binary.isEmpty || binary.contains("$$") then None
    else
      val interfaces = Option(node.interfaces).toList.flatMap(_.asScala)
      val parents    = (Option(node.superName).toList ++ interfaces)
        .map(_.replace('/', '.'))
        .filter(name => name.nonEmpty && name != "java.lang.Object")
      Some(Shape(binary, binary.endsWith("$"), parents))

  private def classFail[A](label: String)(thrown: Throwable): IO[ClassShapeError, A] =
    thrown match
      case _: IllegalArgumentException | _: IndexOutOfBoundsException =>
        ZIO.fail(ClassShapeError.ClassFile(label))
      case _ => ZIO.die(thrown)

  private def ioMiss[A](path: Path, missing: A)(thrown: Throwable): IO[ClassShapeError, A] =
    ioKind(thrown) match
      case IoKind.Missing => ZIO.succeed(missing)
      case IoKind.Read    => ZIO.fail(ClassShapeError.Read(path))
      case IoKind.Die     => ZIO.die(thrown)

  private def ioKind(thrown: Throwable): IoKind =
    cause(thrown) match
      case _: NoSuchFileException   => IoKind.Missing
      case _: AccessDeniedException => IoKind.Read
      case _: IOException           => IoKind.Read
      case _                        => IoKind.Die

  private def cause(thrown: Throwable): Throwable =
    thrown match
      case unchecked: UncheckedIOException =>
        Option(unchecked.getCause) match
          case Some(inner) => inner
          case None        => unchecked
      case iterated: DirectoryIteratorException =>
        Option(iterated.getCause) match
          case Some(inner) => inner
          case None        => iterated
      case other => other

  private def closeStream(opened: Stream[Path]): URIO[Any, Unit] =
    ZIO.attemptBlocking(opened.close()).ignore

  private def closeZip(zip: ZipFile): URIO[Any, Unit] =
    ZIO.attemptBlocking(zip.close()).ignore

  private def closeInput(input: java.io.InputStream): URIO[Any, Unit] =
    ZIO.attemptBlocking(input.close()).ignore

  private def fileName(path: Path): String =
    Option(path.getFileName).map(_.toString).getOrElse("")

  private enum Kind:
    case Directory, Jar, Other

  private enum IoKind:
    case Missing, Read, Die
end ClassShapes

enum ClassShapeError:
  case Failed(kind: String)
  case Read(path: Path)
  case ClassFile(label: String)

  def message: String = this match
    case Failed(kind)     => s"could not read class files ($kind)"
    case Read(path)       => s"could not read class files under $path"
    case ClassFile(label) => s"could not read the class file $label"
end ClassShapeError
