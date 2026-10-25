package histos.native

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode

import zio.{IO, ZIO}

/** Superclass names from `.class` files. Native tests are not loaded on the JVM. */
object ClassShapes:
  final case class Shape(name: String, module: Boolean, parents: List[String])

  def read(classpath: List[Path]): IO[ClassShapeError, List[Shape]] =
    ZIO
      .attemptBlocking(collect(classpath))
      .mapError(thrown => ClassShapeError.Failed(thrown.getClass.getName))
      .flatMap(ZIO.fromEither)

  private def collect(classpath: List[Path]): Either[ClassShapeError, List[Shape]] =
    classpath
      .foldLeft[Either[ClassShapeError, Map[String, Shape]]](Right(Map.empty)): (acc, path) =>
        acc.flatMap: seen =>
          entries(path).flatMap: found =>
            found.foldLeft[Either[ClassShapeError, Map[String, Shape]]](Right(seen)): (shapes, entry) =>
              shapes.flatMap: have =>
                shape(entry).map:
                  case Some(next) => have.updated(next.name, next)
                  case None       => have
      .map(_.values.toList.sortBy(_.name))

  private def entries(path: Path): Either[ClassShapeError, List[ClassBytes]] =
    if !Files.exists(path) then Right(Nil)
    else if Files.isDirectory(path) then directory(path)
    else if Files.isRegularFile(path) && nameOf(path).endsWith(".jar") then jar(path)
    else Right(Nil)

  private def directory(root: Path): Either[ClassShapeError, List[ClassBytes]] =
    val walk = Files.walk(root)
    try
      walk.iterator.asScala.foldLeft[Either[ClassShapeError, List[ClassBytes]]](Right(Nil)): (acc, path) =>
        acc.flatMap: seen =>
          if Files.isRegularFile(path) && nameOf(path).endsWith(".class") then bytes(path.toString, path).map(seen :+ _)
          else Right(seen)
    catch case NonFatal(_) => Left(ClassShapeError.Read(root))
    finally walk.close()

  private def jar(path: Path): Either[ClassShapeError, List[ClassBytes]] =
    open(path).flatMap: zip =>
      try
        zip.entries.asScala.foldLeft[Either[ClassShapeError, List[ClassBytes]]](Right(Nil)): (acc, entry) =>
          acc.flatMap: seen =>
            if entry.isDirectory || !entry.getName.endsWith(".class") then Right(seen)
            else
              val input = zip.getInputStream(entry)
              try
                val label = s"${path.toString}:${entry.getName}"
                Right(seen :+ ClassBytes(label, input.readAllBytes()))
              catch case NonFatal(_) => Left(ClassShapeError.Read(path))
              finally input.close()
      finally zip.close()

  private def open(path: Path): Either[ClassShapeError, ZipFile] =
    try Right(new ZipFile(path.toFile))
    catch case NonFatal(_) => Left(ClassShapeError.Read(path))

  private def bytes(label: String, path: Path): Either[ClassShapeError, ClassBytes] =
    try Right(ClassBytes(label, Files.readAllBytes(path)))
    catch case NonFatal(_) => Left(ClassShapeError.Read(path))

  private def shape(entry: ClassBytes): Either[ClassShapeError, Option[Shape]] =
    try
      val reader = new ClassReader(entry.bytes)
      val node   = new ClassNode()
      reader.accept(node, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
      val binary = Option(node.name).map(_.replace('/', '.')).getOrElse("")
      if binary.isEmpty || binary.contains("$$") then Right(None)
      else
        val interfaces = Option(node.interfaces).toList.flatMap(_.asScala)
        val parents    = (Option(node.superName).toList ++ interfaces)
          .map(_.replace('/', '.'))
          .filter(name => name.nonEmpty && name != "java.lang.Object")
        Right(Some(Shape(binary, binary.endsWith("$"), parents)))
    catch case NonFatal(_) => Left(ClassShapeError.ClassFile(entry.label))

  private def nameOf(path: Path): String =
    Option(path.getFileName).map(_.toString).getOrElse("")

  private final case class ClassBytes(label: String, bytes: Array[Byte])
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
