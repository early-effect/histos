package histos.testrun

import java.nio.file.{Files, Path}
import java.nio.file.attribute.BasicFileAttributes

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import org.objectweb.asm.{
  ClassReader,
  ClassVisitor,
  MethodVisitor,
  Opcodes,
  Type
}

import histos.jdk.Sha256
import histos.workspace.{IoFailure, IoFault}

import zio.{Chunk, IO, ZIO}

/** Why a suite digest could not be built. A missing suite class is not one of
  * these: that digest is absent and the suite runs.
  */
enum StampError:
  case Io(failure: IoFailure)
  case Class(path: Path)
  case Missing(name: String)

  def message: String = this match
    case Io(failure)   => failure.message
    case Class(path)   => s"could not read class $path"
    case Missing(name) => s"stamp index has no $name"

/** Digest of one suite, the classes it references, the locked jars, the
  * upstream jars, the JDK, and the scalac flags. Each class is a fingerprint
  * and the names it points at. The bytes are not kept. A class this suite does
  * not reference is not part of the digest. Tool and runtime hashes supplied by
  * [[histos.testrun.TestContext]] are jar lines. This digest adds the upstream
  * jars, the JDK token, and the flags.
  */
object SuiteStamp:
  /** Fingerprint of one class file, and the binary names it mentions. */
  private[testrun] final case class Print(
      fingerprint: String,
      refs: Set[String]
  )

  /** Identity besides the referenced class files. `jars` are this module's
    * locked artifacts. `upstream` are compile jars of upstream modules. `jdk`
    * is the accepted runtime token. `flags` are the scalac arguments.
    */
  private[testrun] final case class Cover(
      jars: List[String],
      upstream: List[String],
      jdk: String,
      flags: List[String]
  )

  def digest(
      suite: String,
      directories: List[Path],
      cover: Cover
  ): IO[StampError, Option[String]] =
    index(directories).flatMap(tree =>
      ZIO.fromEither(digest(suite, tree, cover))
    )

  /** One walk of the class directories. `remember` stamps every suite from
    * this.
    */
  private[testrun] def index(
      directories: List[Path]
  ): IO[StampError, Map[String, Print]] =
    ZIO
      .foreach(directories)(readDirectory)
      .map: groups =>
        groups.flatten.foldLeft(Map.empty[String, Print]):
          case (acc, (name, print)) => acc.updated(name, print)

  private[testrun] def digest(
      suite: String,
      indexed: Map[String, Print],
      cover: Cover
  ): Either[StampError, Option[String]] =
    val start = List(suite, s"$suite$$").filter(indexed.contains)
    if start.isEmpty then Right(None)
    else
      val used = closure(start, indexed).toList.sorted
      fingerprints(used, indexed).map: lines =>
        Some(hash((lines ++ covered(cover)).mkString("\n")))

  private def readDirectory(root: Path): IO[StampError, List[(String, Print)]] =
    ZIO
      .attemptBlocking(scan(root))
      .mapError(_ => StampError.Io(IoFailure(root, IoFault.Walk)))
      .flatMap(ZIO.fromEither)

  private def scan(root: Path): Either[StampError, List[(String, Print)]] =
    attributes(root).flatMap: attrs =>
      if !attrs.isDirectory then Right(Nil)
      else
        val opened =
          try Right(Files.walk(root))
          catch
            case NonFatal(_) =>
              Left(StampError.Io(IoFailure(root, IoFault.Walk)))
        opened.flatMap: walk =>
          try foldClasses(root, walk)
          finally walk.close()

  private def attributes(root: Path): Either[StampError, BasicFileAttributes] =
    try Right(Files.readAttributes(root, classOf[BasicFileAttributes]))
    catch case NonFatal(_) => Left(StampError.Io(IoFailure(root, IoFault.Stat)))

  private def foldClasses(
      root: Path,
      walk: java.util.stream.Stream[Path]
  ): Either[StampError, List[(String, Print)]] =
    walk.iterator.asScala
      .foldLeft[Either[StampError, List[(String, Print)]]](Right(Nil)):
        case (Left(error), _)    => Left(error)
        case (Right(rows), path) =>
          readClass(root, path).map:
            case Some(row) => rows :+ row
            case None      => rows

  private def readClass(
      root: Path,
      path: Path
  ): Either[StampError, Option[(String, Print)]] =
    if !Files.isRegularFile(path) then Right(None)
    else
      val relative = root.relativize(path).toString.replace('\\', '/')
      if !relative.endsWith(".class") then Right(None)
      else
        val binary = relative.stripSuffix(".class").replace('/', '.')
        if binary.isEmpty then Right(None)
        else
          readBytes(path).flatMap: bytes =>
            references(path, bytes).map: refs =>
              Some(
                binary -> Print(Sha256.of(Chunk.fromArray(bytes)).value, refs)
              )

  private def readBytes(path: Path): Either[StampError, Array[Byte]] =
    try Right(Files.readAllBytes(path))
    catch case NonFatal(_) => Left(StampError.Io(IoFailure(path, IoFault.Read)))

  private def fingerprints(
      names: List[String],
      index: Map[String, Print]
  ): Either[StampError, List[String]] =
    names.foldLeft[Either[StampError, List[String]]](Right(Nil)):
      case (Left(error), _)     => Left(error)
      case (Right(lines), name) =>
        index.get(name) match
          case Some(print) => Right(lines :+ s"$name ${print.fingerprint}")
          case None        => Left(StampError.Missing(name))

  private def covered(cover: Cover): List[String] =
    cover.jars.sorted.map(jar => s"jar $jar") ++
      cover.upstream.sorted.map(jar => s"upstream $jar") ++
      List(s"jdk ${cover.jdk}") ++
      cover.flags.map(flag => s"flag $flag")

  private def hash(text: String): String =
    Sha256
      .of(
        Chunk.fromArray(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      )
      .value

  private def closure(
      start: List[String],
      index: Map[String, Print]
  ): Set[String] =
    def step(seen: Set[String], queue: List[String]): Set[String] =
      queue match
        case Nil                                 => seen
        case name :: rest if seen.contains(name) => step(seen, rest)
        case name :: rest                        =>
          index.get(name) match
            case None        => step(seen, rest)
            case Some(print) =>
              val next =
                print.refs
                  .flatMap(alias)
                  .filter(index.contains)
                  .filterNot(seen.contains)
              step(seen + name, rest ++ next)
    step(Set.empty, start.distinct)

  private def alias(name: String): List[String] =
    if name.endsWith("$") && !name.endsWith("$$") then
      List(name, name.dropRight(1))
    else List(name, s"$name$$")

  private def references(
      path: Path,
      bytes: Array[Byte]
  ): Either[StampError, Set[String]] =
    val found = scala.collection.mutable.Set.empty[String]
    val faults = scala.collection.mutable.ListBuffer.empty[StampError]
    def fail(error: StampError): Unit =
      if faults.isEmpty then faults += error
    def add(name: String): Unit =
      if name.nonEmpty then found += name.replace('/', '.')
    def collect(kind: Type): Unit =
      kind.getSort match
        case Type.OBJECT =>
          val name = kind.getInternalName
          if name != null then add(name)
        case Type.ARRAY  => collect(kind.getElementType)
        case Type.METHOD =>
          kind.getArgumentTypes.foreach(collect)
          collect(kind.getReturnType)
        case _ => ()
    def note(text: String): Unit =
      if text != null && text.nonEmpty then
        try collect(Type.getType(text))
        catch case NonFatal(_) => fail(StampError.Class(path))
    try
      val reader = new ClassReader(bytes)
      reader.accept(
        new ClassVisitor(Opcodes.ASM9):
          override def visit(
              version: Int,
              access: Int,
              name: String,
              signature: String,
              superName: String,
              interfaces: Array[String]
          ): Unit =
            if superName != null then add(superName)
            if interfaces != null then interfaces.foreach(add)

          override def visitField(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              value: Object
          ): org.objectweb.asm.FieldVisitor =
            note(descriptor)
            null

          override def visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              exceptions: Array[String]
          ): MethodVisitor =
            note(descriptor)
            new MethodVisitor(Opcodes.ASM9):
              override def visitTypeInsn(opcode: Int, typeName: String): Unit =
                add(typeName)
              override def visitFieldInsn(
                  opcode: Int,
                  owner: String,
                  name: String,
                  descriptor: String
              ): Unit =
                add(owner)
                note(descriptor)
              override def visitMethodInsn(
                  opcode: Int,
                  owner: String,
                  name: String,
                  descriptor: String,
                  isInterface: Boolean
              ): Unit =
                add(owner)
                note(descriptor)
              override def visitLdcInsn(value: Object): Unit =
                value match
                  case kind: Type => collect(kind)
                  case _          => ()
        ,
        ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES
      )
    catch case NonFatal(_) => fail(StampError.Class(path))
    faults.headOption match
      case Some(error) => Left(error)
      case None        => Right(found.toSet)
