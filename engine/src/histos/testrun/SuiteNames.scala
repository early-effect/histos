package histos.testrun

import java.io.{IOException, UncheckedIOException}
import java.nio.file.{AccessDeniedException, DirectoryIteratorException, Files, NoSuchFileException, Path}
import java.nio.file.attribute.BasicFileAttributes
import java.util.zip.{ZipEntry, ZipFile}

import scala.jdk.CollectionConverters.*

import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes}
import org.objectweb.asm.tree.ClassNode

import histos.native.{ClassShapeError, ClassShapes}

import zio.{IO, URIO, ZIO}

/** Suites a provider would run, read from class files. The engine does not load the framework. */
object SuiteNames:
  final case class Mark(superclass: String, module: Boolean)

  enum Found:
    case Suites(names: List[String])
    case Unseen

  def discover(testClasses: Path, classpath: List[Path], framework: String): IO[ClassShapeError, Found] =
    ClassShapes
      .read(List(testClasses))
      .flatMap: local =>
        if local.isEmpty then sjsir(testClasses).map(seen => if seen then Found.Unseen else Found.Suites(Nil))
        else
          for
            marks <- fingerprints(testClasses, classpath, framework)
            world <- parents(local, classpath)
          yield Found.Suites(choose(local, world, marks))

  private[testrun] def choose(
      local: List[ClassShapes.Shape],
      world: Map[String, ClassShapes.Shape],
      marks: List[Mark],
  ): List[String] =
    local
      .flatMap { shape =>
        val name = shown(shape.name)
        if name.startsWith("subprocess.") || name.contains("$$") then None
        else if marks.exists(mark =>
            mark.module == shape.module && reaches(shape.name, mark.superclass, world, Set.empty)
          )
        then Some(name)
        else None
      }
      .distinct
      .sorted

  private def shown(name: String): String =
    if name.endsWith("$") && !name.endsWith("$$") then name.dropRight(1) else name

  private def reaches(name: String, target: String, world: Map[String, ClassShapes.Shape], seen: Set[String]): Boolean =
    if name == target then true
    else if seen.contains(name) then false
    else
      world.get(name) match
        case None        => false
        case Some(shape) => shape.parents.exists(parent => reaches(parent, target, world, seen + name))

  private def parents(
      local: List[ClassShapes.Shape],
      classpath: List[Path],
  ): IO[ClassShapeError, Map[String, ClassShapes.Shape]] =
    val seed = local.map(shape => shape.name -> shape).toMap
    grow(seed, local.flatMap(_.parents).distinct, classpath)

  private def grow(
      have: Map[String, ClassShapes.Shape],
      wanted: List[String],
      classpath: List[Path],
  ): IO[ClassShapeError, Map[String, ClassShapes.Shape]] =
    wanted.filterNot(have.contains) match
      case Nil     => ZIO.succeed(have)
      case missing =>
        ZIO
          .foreach(missing)(name => load(name, classpath))
          .flatMap: loaded =>
            val found = loaded.flatten
            if found.isEmpty then ZIO.succeed(have)
            else
              val next = found.foldLeft(have)((acc, shape) => acc.updated(shape.name, shape))
              grow(next, found.flatMap(_.parents).distinct, classpath)

  private def load(name: String, classpath: List[Path]): IO[ClassShapeError, Option[ClassShapes.Shape]] =
    val entry = name.replace('.', '/') + ".class"
    first(classpath, entry).flatMap:
      case None        => ZIO.succeed(None)
      case Some(found) => shape(found.label, found.bytes)

  private def first(classpath: List[Path], entry: String): IO[ClassShapeError, Option[Loaded]] =
    classpath match
      case Nil          => ZIO.succeed(None)
      case path :: rest =>
        bytes(path, entry).flatMap:
          case Some(found) => ZIO.succeed(Some(found))
          case None        => first(rest, entry)

  private def bytes(path: Path, entry: String): IO[ClassShapeError, Option[Loaded]] =
    kind(path).flatMap:
      case Kind.Directory =>
        val file = path.resolve(entry)
        readFile(file).map(_.map(body => Loaded(file.toString, body)))
      case Kind.Jar =>
        zipBytes(path, entry).map(_.map(body => Loaded(s"${path.toString}:$entry", body)))
      case Kind.Other =>
        ZIO.succeed(None)

  private def fingerprints(
      testClasses: Path,
      classpath: List[Path],
      framework: String,
  ): IO[ClassShapeError, List[Mark]] =
    val known =
      if framework == "zio.test.sbt.ZTestFramework" then List(Mark("zio.test.ZIOSpecAbstract", module = true))
      else Nil
    ZIO
      .foreach(classpath)(path => owns(path, framework).map(path -> _))
      .flatMap: hits =>
        val home = hits.collect { case (path, true) => path }
        for
          local <- scan(testClasses)
          extra <- ZIO.foreach(home)(scan)
        yield (known ++ local ++ extra.flatten).distinct

  private def owns(path: Path, framework: String): IO[ClassShapeError, Boolean] =
    bytes(path, framework.replace('.', '/') + ".class").map(_.isDefined)

  private def scan(path: Path): IO[ClassShapeError, List[Mark]] =
    kind(path).flatMap:
      case Kind.Directory => scanDirectory(path)
      case Kind.Jar       => scanJar(path)
      case Kind.Other     => ZIO.succeed(Nil)

  private def scanDirectory(root: Path): IO[ClassShapeError, List[Mark]] =
    walk(root, Nil)(foldMarks(root, _))

  private def foldMarks(
      root: Path,
      opened: java.util.stream.Stream[Path],
  ): IO[ClassShapeError, List[Mark]] =
    ZIO.attemptBlocking(opened.iterator).foldZIO(ioMiss(root, Nil), stepMarks(root, _, Nil))

  private def stepMarks(
      root: Path,
      paths: java.util.Iterator[Path],
      marks: List[Mark],
  ): IO[ClassShapeError, List[Mark]] =
    ZIO
      .attemptBlocking(paths.hasNext)
      .foldZIO(
        ioMiss(root, marks),
        has =>
          if !has then ZIO.succeed(marks)
          else
            ZIO
              .attemptBlocking(paths.next())
              .foldZIO(
                ioMiss(root, marks),
                path =>
                  one(path).flatMap:
                    case None        => stepMarks(root, paths, marks)
                    case Some(found) => stepMarks(root, paths, marks :+ found),
              ),
      )

  private def one(path: Path): IO[ClassShapeError, Option[Mark]] =
    if !fileName(path).endsWith(".class") then ZIO.succeed(None)
    else
      readFile(path).flatMap:
        case None        => ZIO.succeed(None)
        case Some(bytes) => mark(path.toString, bytes)

  private def scanJar(path: Path): IO[ClassShapeError, List[Mark]] =
    withZip(path, Nil): zip =>
      ZIO.attemptBlocking(zip.entries.asScala.toList).foldZIO(ioMiss(path, Nil), readMarks(path, zip, _, Nil))

  private def readMarks(
      path: Path,
      zip: ZipFile,
      entries: List[ZipEntry],
      marks: List[Mark],
  ): IO[ClassShapeError, List[Mark]] =
    entries match
      case Nil           => ZIO.succeed(marks)
      case entry :: rest =>
        if entry.isDirectory || !entry.getName.endsWith(".class") then readMarks(path, zip, rest, marks)
        else
          streamBytes(path, zip, entry).flatMap:
            case None       => readMarks(path, zip, rest, marks)
            case Some(body) =>
              mark(s"${path.toString}:${entry.getName}", body).flatMap:
                case None        => readMarks(path, zip, rest, marks)
                case Some(found) => readMarks(path, zip, rest, marks :+ found)

  private def mark(label: String, bytes: Array[Byte]): IO[ClassShapeError, Option[Mark]] =
    ZIO.attempt(readMark(bytes)).foldZIO(classFail(label), ZIO.succeed)

  private def readMark(bytes: Array[Byte]): Option[Mark] =
    var superclass: Option[String] = None
    var module: Option[Boolean]    = None
    var fingerprint                = false
    val reader                     = new ClassReader(bytes)
    reader.accept(
      new ClassVisitor(Opcodes.ASM9):
        override def visit(
            version: Int,
            access: Int,
            name: String,
            signature: String,
            superName: String,
            interfaces: Array[String],
        ): Unit =
          if interfaces != null && interfaces.contains("sbt/testing/SubclassFingerprint") then fingerprint = true

        override def visitMethod(
            access: Int,
            name: String,
            descriptor: String,
            signature: String,
            exceptions: Array[String],
        ): MethodVisitor =
          if name == "superclassName" then
            new MethodVisitor(Opcodes.ASM9):
              override def visitLdcInsn(value: Object): Unit =
                value match
                  case text: String => superclass = Some(text)
                  case _            => ()
          else if name == "isModule" then
            new MethodVisitor(Opcodes.ASM9):
              override def visitInsn(opcode: Int): Unit =
                if opcode == Opcodes.ICONST_1 then module = Some(true)
                else if opcode == Opcodes.ICONST_0 then module = Some(false)
          else null
      ,
      ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES,
    )
    superclass.filter(_ => fingerprint).map(name => Mark(name, module.getOrElse(true)))
  end readMark

  private def shape(label: String, bytes: Array[Byte]): IO[ClassShapeError, Option[ClassShapes.Shape]] =
    ZIO.attempt(parsed(bytes)).foldZIO(classFail(label), ZIO.succeed)

  private def parsed(bytes: Array[Byte]): Option[ClassShapes.Shape] =
    val reader = new ClassReader(bytes)
    val node   = new ClassNode()
    reader.accept(node, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
    val binary = Option(node.name).map(_.replace('/', '.')).getOrElse("")
    if binary.isEmpty || binary.contains("$$") then None
    else
      val interfaces = Option(node.interfaces).toList.flatMap(_.asScala)
      val supers     = (Option(node.superName).toList ++ interfaces)
        .map(_.replace('/', '.'))
        .filter(name => name.nonEmpty && name != "java.lang.Object")
      Some(ClassShapes.Shape(binary, binary.endsWith("$"), supers))

  private def sjsir(root: Path): IO[ClassShapeError, Boolean] =
    kind(root).flatMap:
      case Kind.Directory =>
        walk(root, false): opened =>
          ZIO
            .attemptBlocking:
              opened.iterator.asScala.exists(path => !classFile(path) && fileName(path).endsWith(".sjsir"))
            .foldZIO(ioMiss(root, false), ZIO.succeed)
      case Kind.Jar | Kind.Other => ZIO.succeed(false)

  /** The directory stream closes when the fold fails or the fiber is interrupted. */
  private def walk[A](root: Path, missing: A)(
      use: java.util.stream.Stream[Path] => IO[ClassShapeError, A]
  ): IO[ClassShapeError, A] =
    ZIO
      .attemptBlocking(Files.walk(root))
      .foldZIO(
        ioMiss(root, missing),
        opened => ZIO.acquireReleaseWith(ZIO.succeed(opened))(closeStream)(use),
      )

  private def withZip[A](path: Path, missing: A)(use: ZipFile => IO[ClassShapeError, A]): IO[ClassShapeError, A] =
    ZIO
      .attemptBlocking(new ZipFile(path.toFile))
      .foldZIO(
        ioMiss(path, missing),
        zip => ZIO.acquireReleaseWith(ZIO.succeed(zip))(closeZip)(use),
      )

  private def zipBytes(path: Path, entry: String): IO[ClassShapeError, Option[Array[Byte]]] =
    withZip(path, None): zip =>
      ZIO
        .attemptBlocking(Option(zip.getEntry(entry)))
        .foldZIO(
          ioMiss(path, None),
          {
            case None        => ZIO.succeed(None)
            case Some(found) => streamBytes(path, zip, found)
          },
        )

  private def streamBytes(path: Path, zip: ZipFile, entry: ZipEntry): IO[ClassShapeError, Option[Array[Byte]]] =
    ZIO
      .attemptBlocking(Option(zip.getInputStream(entry)))
      .foldZIO(
        ioMiss(path, None),
        {
          case None        => ZIO.fail(ClassShapeError.Read(path))
          case Some(input) =>
            ZIO.acquireReleaseWith(ZIO.succeed(input))(closeInput): opened =>
              ZIO.attemptBlocking(opened.readAllBytes()).foldZIO(ioMiss(path, None), bytes => ZIO.succeed(Some(bytes)))
        },
      )

  private def readFile(path: Path): IO[ClassShapeError, Option[Array[Byte]]] =
    ZIO
      .attemptBlocking(Files.readAttributes(path, classOf[BasicFileAttributes]))
      .foldZIO(
        ioMiss(path, None),
        attrs =>
          if !attrs.isRegularFile then ZIO.succeed(None)
          else
            ZIO.attemptBlocking(Files.readAllBytes(path)).foldZIO(ioMiss(path, None), bytes => ZIO.succeed(Some(bytes))),
      )

  private def kind(path: Path): IO[ClassShapeError, Kind] =
    ZIO
      .attemptBlocking(Files.readAttributes(path, classOf[BasicFileAttributes]))
      .foldZIO(
        thrown =>
          ioKind(thrown) match
            case IoKind.Missing    => ZIO.succeed(Kind.Other)
            case IoKind.Failed     => ZIO.fail(ClassShapeError.Read(path))
            case IoKind.Unexpected => ZIO.die(thrown)
        ,
        attrs =>
          val found =
            if attrs.isDirectory then Kind.Directory
            else if attrs.isRegularFile && fileName(path).endsWith(".jar") then Kind.Jar
            else Kind.Other
          ZIO.succeed(found),
      )

  private def classFail[A](label: String)(thrown: Throwable): IO[ClassShapeError, A] =
    thrown match
      case _: IllegalArgumentException | _: IndexOutOfBoundsException =>
        ZIO.fail(ClassShapeError.ClassFile(label))
      case _ => ZIO.die(thrown)

  private def ioMiss[A](path: Path, missing: A)(thrown: Throwable): IO[ClassShapeError, A] =
    ioKind(thrown) match
      case IoKind.Missing    => ZIO.succeed(missing)
      case IoKind.Failed     => ZIO.fail(ClassShapeError.Read(path))
      case IoKind.Unexpected => ZIO.die(thrown)

  private def ioKind(thrown: Throwable): IoKind =
    cause(thrown) match
      case _: NoSuchFileException   => IoKind.Missing
      case _: AccessDeniedException => IoKind.Failed
      case _: IOException           => IoKind.Failed
      case _                        => IoKind.Unexpected

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

  private def closeStream(opened: java.util.stream.Stream[Path]): URIO[Any, Unit] =
    ZIO.attemptBlocking(opened.close()).ignore

  private def closeZip(zip: ZipFile): URIO[Any, Unit] =
    ZIO.attemptBlocking(zip.close()).ignore

  private def closeInput(input: java.io.InputStream): URIO[Any, Unit] =
    ZIO.attemptBlocking(discardClose(input)).ignore

  private def discardClose(input: java.io.InputStream): Unit =
    try input.close()
    catch case _: IOException => ()

  private def classFile(path: Path): Boolean =
    Files.isRegularFile(path) && fileName(path).endsWith(".class")

  private def fileName(path: Path): String =
    Option(path.getFileName).map(_.toString).getOrElse("")

  private enum Kind:
    case Directory, Jar, Other

  private enum IoKind:
    case Missing, Failed, Unexpected

  private final case class Loaded(label: String, bytes: Array[Byte])
end SuiteNames
