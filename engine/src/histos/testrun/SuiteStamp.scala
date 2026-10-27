package histos.testrun

import java.io.{IOException, UncheckedIOException}
import java.nio.file.{AccessDeniedException, Files, NoSuchFileException, Path}
import java.nio.file.attribute.BasicFileAttributes

import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes, Type}

import histos.jdk.Sha256
import histos.workspace.{IoFailure, IoFault}

import zio.{Chunk, IO, ZIO}

/** Why a suite digest could not be built. A missing suite class is not one of these: that digest is absent and the
  * suite runs.
  */
enum StampError:
  case Io(failure: IoFailure)
  case Class(path: Path)
  case Missing(name: String)

  def message: String = this match
    case Io(failure)   => failure.message
    case Class(path)   => s"could not read class $path"
    case Missing(name) => s"stamp index has no $name"
end StampError

/** Digest of one suite, the classes it references, the locked jars, the upstream jars, the JDK, and the scalac flags.
  * Each class is a fingerprint and the names it points at. The bytes are not kept. A class this suite does not
  * reference is not part of the digest. Tool fingerprints and runtime-token hashes from [[SuiteCover]] are jar lines.
  * This digest adds the upstream jars, the JDK token, and the flags.
  */
object SuiteStamp:
  /** Fingerprint of one class file, and the binary names it mentions. */
  private[testrun] final case class Print(
      fingerprint: String,
      refs: Set[String],
  )

  /** Identity besides the referenced class files. `jars` are locked shas, tool fingerprints, and runtime-token hashes.
    * `upstream` are compile jars of upstream modules. `jdk` is the accepted runtime token. `flags` are the scalac
    * arguments.
    */
  private[histos] final case class Cover(
      jars: List[String],
      upstream: List[String],
      jdk: String,
      flags: List[String],
  )

  def digest(
      suite: String,
      directories: List[Path],
      cover: Cover,
  ): IO[StampError, Option[Sha256]] =
    index(directories).flatMap(tree => ZIO.fromEither(digest(suite, tree, cover)))

  /** One class walk, then a digest per named suite. A missing suite class is `None`, not an error. */
  def digests(
      suites: List[String],
      directories: List[Path],
      cover: Cover,
  ): IO[StampError, Map[String, Option[Sha256]]] =
    index(directories).flatMap(tree => ZIO.fromEither(foldDigests(suites, tree, cover)))

  private def foldDigests(
      suites: List[String],
      indexed: Map[String, Print],
      cover: Cover,
  ): Either[StampError, Map[String, Option[Sha256]]] =
    suites.foldLeft[Either[StampError, Map[String, Option[Sha256]]]](Right(Map.empty)):
      case (Left(error), _)     => Left(error)
      case (Right(seen), suite) =>
        digest(suite, indexed, cover).map(found => seen.updated(suite, found))

  /** One walk of the class directories. `remember` stamps every suite from this.
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
      cover: Cover,
  ): Either[StampError, Option[Sha256]] =
    val start = List(suite, s"$suite$$").filter(indexed.contains)
    if start.isEmpty then Right(None)
    else
      val used = closure(start, indexed).toList.sorted
      fingerprints(used, indexed).map: lines =>
        Some(hash((lines ++ covered(cover)).mkString("\n")))

  private def readDirectory(root: Path): IO[StampError, List[(String, Print)]] =
    attributes(root).flatMap: attrs =>
      if !attrs.isDirectory then ZIO.succeed(Nil)
      else scan(root)

  private def attributes(root: Path): IO[StampError, BasicFileAttributes] =
    ZIO
      .attemptBlocking(Files.readAttributes(root, classOf[BasicFileAttributes]))
      .foldZIO(failIo(root, IoFault.Stat), attrs => ZIO.succeed(attrs))

  /** The directory stream closes when the fold fails or the fiber is interrupted. */
  private def scan(root: Path): IO[StampError, List[(String, Print)]] =
    ZIO.acquireReleaseWith(
      ZIO
        .attemptBlocking(Files.walk(root))
        .foldZIO(failIo(root, IoFault.Walk), walk => ZIO.succeed(walk))
    )(walk => ZIO.attemptBlocking(walk.close()).ignore)(foldClasses(root, _))

  private def foldClasses(
      root: Path,
      walk: java.util.stream.Stream[Path],
  ): IO[StampError, List[(String, Print)]] =
    ZIO
      .attemptBlocking(walk.iterator)
      .foldZIO(failIo(root, IoFault.Walk), step(root, _, Nil))

  private def step(
      root: Path,
      paths: java.util.Iterator[Path],
      rows: List[(String, Print)],
  ): IO[StampError, List[(String, Print)]] =
    ZIO
      .attemptBlocking(paths.hasNext)
      .foldZIO(
        failIo(root, IoFault.Walk),
        next =>
          if !next then ZIO.succeed(rows)
          else
            ZIO
              .attemptBlocking(paths.next())
              .foldZIO(
                failIo(root, IoFault.Walk),
                path =>
                  ZIO
                    .fromEither(readClass(root, path))
                    .flatMap:
                      case Some(row) => step(root, paths, rows :+ row)
                      case None      => step(root, paths, rows),
              ),
      )

  private def readClass(
      root: Path,
      path: Path,
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
    catch
      case thrown: Exception if platformIo(thrown) =>
        Left(StampError.Io(IoFailure(path, IoFault.Read)))

  private def failIo(path: Path, fault: IoFault)(thrown: Throwable): IO[StampError, Nothing] =
    if platformIo(thrown) then ZIO.fail(StampError.Io(IoFailure(path, fault)))
    else ZIO.die(thrown)

  /** Platform IO, including the unchecked wrapper a directory stream throws from `next`. */
  private def platformIo(thrown: Throwable): Boolean =
    thrown match
      case _: NoSuchFileException          => true
      case _: AccessDeniedException        => true
      case _: IOException                  => true
      case unchecked: UncheckedIOException =>
        Option(unchecked.getCause) match
          case Some(cause) => platformIo(cause)
          case None        => false
      case _ => false

  private def fingerprints(
      names: List[String],
      index: Map[String, Print],
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

  private def hash(text: String): Sha256 =
    Sha256.of(
      Chunk.fromArray(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    )

  private def closure(
      start: List[String],
      index: Map[String, Print],
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
    if name.endsWith("$") && !name.endsWith("$$") then List(name, name.dropRight(1))
    else List(name, s"$name$$")

  private def references(
      path: Path,
      bytes: Array[Byte],
  ): Either[StampError, Set[String]] =
    val found                         = scala.collection.mutable.Set.empty[String]
    val faults                        = scala.collection.mutable.ListBuffer.empty[StampError]
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
        catch case _: IllegalArgumentException => fail(StampError.Class(path))
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
              interfaces: Array[String],
          ): Unit =
            if superName != null then add(superName)
            if interfaces != null then interfaces.foreach(add)

          override def visitField(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              value: Object,
          ): org.objectweb.asm.FieldVisitor =
            note(descriptor)
            null

          override def visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              exceptions: Array[String],
          ): MethodVisitor =
            note(descriptor)
            new MethodVisitor(Opcodes.ASM9):
              override def visitTypeInsn(opcode: Int, typeName: String): Unit =
                add(typeName)
              override def visitFieldInsn(
                  opcode: Int,
                  owner: String,
                  name: String,
                  descriptor: String,
              ): Unit =
                add(owner)
                note(descriptor)
              override def visitMethodInsn(
                  opcode: Int,
                  owner: String,
                  name: String,
                  descriptor: String,
                  isInterface: Boolean,
              ): Unit =
                add(owner)
                note(descriptor)
              override def visitLdcInsn(value: Object): Unit =
                value match
                  case kind: Type => collect(kind)
                  case _          => ()
        ,
        ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES,
      )
    // ClassReader indexes off the end of a short class file.
    catch
      case _: IllegalArgumentException | _: IndexOutOfBoundsException | _: org.objectweb.asm.LimitExceededException =>
        fail(StampError.Class(path))
    end try
    faults.headOption match
      case Some(error) => Left(error)
      case None        => Right(found.toSet)
  end references
end SuiteStamp
