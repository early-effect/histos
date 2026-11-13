package histos.testrun

import java.io.{IOException, UncheckedIOException}
import java.nio.file.{AccessDeniedException, Files, NoSuchFileException, Path}
import java.nio.file.attribute.BasicFileAttributes

import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes, Type}

import histos.jdk.{Sha256, Sha256Digest}
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.ProductDigest

import zio.{Chunk, IO, ZIO}

/** Digest of one suite, the classes it references, the locked jars, the upstream jars, the JDK, the scalac flags, and
  * each upstream API digest. Each class in that closure is the fingerprint of its [[ProductDigest.positionFree]] bytes,
  * so a comment in a referenced class keeps the stamp and a body change moves it. The bytes are not kept. A class this
  * suite does not reference is not part of that closure, and a class no suite reaches is not fingerprinted. The API
  * digest still is: a public signature outside the closure moves the stamp, and a comment does not. Tool fingerprints
  * and runtime-token hashes from [[SuiteCover]] are jar lines.
  */
object SuiteStamp:
  /** One class file, and the binary names it mentions. */
  private final case class Print(
      path: Path,
      refs: Set[String],
  )

  /** Identity besides the referenced class files. `jars` are locked shas, tool fingerprints, and runtime-token hashes.
    * `upstream` are compile jars of upstream modules. `jdk` is the accepted runtime token. `flags` are the scalac
    * arguments. `apis` are `module digest` lines for upstream API files that exist.
    */
  private[histos] final case class Cover(
      jars: List[String],
      upstream: List[String],
      jdk: String,
      flags: List[String],
      apis: List[String],
  )

  def digest(
      suite: String,
      directories: List[Path],
      cover: Cover,
  ): IO[StampError, Option[Sha256]] =
    digests(List(suite), directories, cover).map(_.get(suite).flatten)

  /** One class walk, then a digest per named suite. A missing suite class is `None`, not an error. A class no named
    * suite reaches is not read again.
    */
  def digests(
      suites: List[String],
      directories: List[Path],
      cover: Cover,
  ): IO[StampError, Map[String, Option[Sha256]]] =
    index(directories).flatMap: tree =>
      val reached = suites.map(suite => suite -> reach(suite, tree)).toMap
      fingerprinted(reached.values.flatten.flatten.toSet, tree).flatMap: prints =>
        ZIO.fromEither(stamped(reached, prints, cover))

  /** One walk of the class directories. */
  private def index(
      directories: List[Path]
  ): IO[StampError, Map[String, Print]] =
    ZIO
      .foreach(directories)(readDirectory)
      .map: groups =>
        groups.flatten.foldLeft(Map.empty[String, Print]):
          case (acc, (name, print)) => acc.updated(name, print)

  /** The suite's closure, or `None` when neither the suite class nor its module class is in the tree. */
  private def reach(suite: String, indexed: Map[String, Print]): Option[Set[String]] =
    val start = List(suite, s"$suite$$").filter(indexed.contains)
    if start.isEmpty then None else Some(closure(start, indexed))

  /** Each reached class read again, without its positions. */
  private def fingerprinted(
      names: Set[String],
      indexed: Map[String, Print],
  ): IO[StampError, Map[String, String]] =
    ZIO
      .blocking:
        ZIO.foreach(names.toList.flatMap(name => indexed.get(name).map(print => name -> print.path))): (name, path) =>
          ZIO.fromEither(fingerprint(path)).map(name -> _)
      .map(_.toMap)

  private def fingerprint(path: Path): Either[StampError, String] =
    readBytes(path).flatMap: bytes =>
      ProductDigest
        .positionFree(bytes)
        .left
        .map(_ => StampError.Class(path))
        .map(clean => Sha256Digest.of(Chunk.fromArray(clean)).value)

  private def stamped(
      reached: Map[String, Option[Set[String]]],
      prints: Map[String, String],
      cover: Cover,
  ): Either[StampError, Map[String, Option[Sha256]]] =
    reached.foldLeft[Either[StampError, Map[String, Option[Sha256]]]](Right(Map.empty)):
      case (Left(error), _)                   => Left(error)
      case (Right(seen), (suite, None))       => Right(seen.updated(suite, None))
      case (Right(seen), (suite, Some(used))) =>
        fingerprints(used.toList.sorted, prints).map: lines =>
          seen.updated(suite, Some(hash((lines ++ covered(cover)).mkString("\n"))))

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
      .map(_.reverse)

  /** `rows` is newest first. */
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
                      case Some(row) => step(root, paths, row :: rows)
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
            references(path, bytes).map(refs => Some(binary -> Print(path, refs)))

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
      prints: Map[String, String],
  ): Either[StampError, List[String]] =
    names
      .foldLeft[Either[StampError, List[String]]](Right(Nil)):
        case (Left(error), _)     => Left(error)
        case (Right(lines), name) =>
          prints.get(name) match
            case Some(print) => Right(s"$name $print" :: lines)
            case None        => Left(StampError.Missing(name))
      .map(_.reverse)

  private def covered(cover: Cover): List[String] =
    cover.jars.sorted.map(jar => s"jar $jar") ++
      cover.upstream.sorted.map(jar => s"upstream $jar") ++
      List(s"jdk ${cover.jdk}") ++
      cover.flags.map(flag => s"flag $flag") ++
      cover.apis.sorted.map(api => s"api $api")

  private def hash(text: String): Sha256 =
    Sha256Digest.of(
      Chunk.fromArray(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    )

  /** Every class reachable from `start`. The walk is depth first, and the set does not depend on the order. */
  private def closure(
      start: List[String],
      index: Map[String, Print],
  ): Set[String] =
    val seen                              = scala.collection.mutable.HashSet.empty[String]
    def step(pending: List[String]): Unit =
      pending match
        case Nil                                 => ()
        case name :: rest if seen.contains(name) => step(rest)
        case name :: rest                        =>
          index.get(name) match
            case None        => step(rest)
            case Some(print) =>
              seen += name
              step(print.refs.foldLeft(rest)((stack, ref) => alias(ref).filter(index.contains) ++ stack))
    step(start.distinct)
    seen.toSet

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
