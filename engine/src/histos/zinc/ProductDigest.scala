package histos.zinc

import histos.jdk.{Sha256, Sha256Digest}
import histos.workspace.{IoFailure, IoFault}

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  DataInputStream,
  DataOutputStream,
  IOException,
  UncheckedIOException,
}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import org.objectweb.asm.tree.{ClassNode, LdcInsnNode, LineNumberNode}
import org.objectweb.asm.{ClassReader, ClassWriter}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

import zio.Chunk

/** A class file this digest could not read. The bytes are not a hash input.
  */
enum ClassBytesError:
  case Unreadable

  def message: String = this match
    case Unreadable => "class bytes are not a class file"
end ClassBytesError

/** Line-number tables and the `TASTY` attribute are not part of a product. `TASTY` is a hash of positions and comments.
  * A jar entry's timestamp is not part of a product either. The only path removed is `checkout`, and only where that
  * absolute root sits on a path boundary. Every other string stays as written.
  */
object ProductDigest:
  val fileName: String = "product.sha256"

  def normalize(bytes: Array[Byte], checkout: Path): Either[ClassBytesError, Array[Byte]] =
    val root = checkoutRoot(checkout)
    rewrite(bytes): node =>
      dropPositions(node)
      dropPaths(node, root)
    .flatMap(rewritten => rewritePool(rewritten, root))

  /** The class without its positions: line-number tables, the source debug extension, and the `TASTY` attribute. A
    * comment moves only these. A path constant stays as written, so a changed directory in it is a changed class.
    */
  def positionFree(bytes: Array[Byte]): Either[ClassBytesError, Array[Byte]] =
    rewrite(bytes)(dropPositions)

  def ofDirectory(root: Path, checkout: Path): Either[CompileError, Sha256] =
    classFiles(root).flatMap: found =>
      found
        .foldLeft[Either[CompileError, List[Array[Byte]]]](Right(Nil)):
          case (Left(error), _)           => Left(error)
          case (Right(acc), (name, path)) =>
            readBytes(path).flatMap: bytes =>
              normalize(bytes, checkout) match
                case Right(clean) => Right(acc :+ piece(name, clean))
                case Left(_)      => Left(CompileError.Io(IoFailure(path, IoFault.Read)))
        .map(parts => Sha256Digest.of(Chunk.fromArray(parts.flatten.toArray)))

  def ofJar(path: Path, checkout: Path): Either[CompileError, Sha256] =
    try
      val zip = new ZipFile(path.toFile)
      try
        val rows = zip.entries.asScala.toList.flatMap: entry =>
          if entry.isDirectory then None
          else
            val stream = zip.getInputStream(entry)
            try Option(entry.getName).map(name => name -> stream.readAllBytes())
            finally stream.close()
        hashed(rows.sortBy(_._1), checkout)
      finally zip.close()
    catch case _: IOException => Left(CompileError.Io(IoFailure(path, IoFault.Read)))

  def read(dir: Path): Either[CompileError, Sha256] =
    val path = dir.resolve(fileName)
    try
      Sha256
        .from(Files.readString(path, StandardCharsets.UTF_8))
        .left
        .map: _ =>
          CompileError.Io(IoFailure(path, IoFault.Read))
    catch case _: IOException => Left(CompileError.Io(IoFailure(path, IoFault.Read)))

  def write(dir: Path, classes: Path, checkout: Path): Either[CompileError, Sha256] =
    ofDirectory(classes, checkout).flatMap: digest =>
      val path = dir.resolve(fileName)
      try
        Files.createDirectories(dir)
        Files.writeString(path, digest.value + "\n", StandardCharsets.UTF_8)
        Right(digest)
      catch case _: IOException => Left(CompileError.Io(IoFailure(path, IoFault.Write)))

  private def hashed(rows: List[(String, Array[Byte])], checkout: Path): Either[CompileError, Sha256] =
    val root = checkoutRoot(checkout)
    rows
      .foldLeft[Either[CompileError, List[Array[Byte]]]](Right(Nil)):
        case (Left(error), _)          => Left(error)
        case (Right(acc), (name, raw)) =>
          val body =
            if name.endsWith(".class") then
              normalize(raw, checkout) match
                case Right(clean) => Right(clean)
                case Left(_)      => Left(CompileError.Io(IoFailure(Path.of(name), IoFault.Read)))
            else Right(scrubBytes(raw, root))
          body.map(clean => acc :+ piece(stripRoot(name, root), clean))
      .map(parts => Sha256Digest.of(Chunk.fromArray(parts.flatten.toArray)))

  private def classFiles(root: Path): Either[CompileError, List[(String, Path)]] =
    if !Files.isDirectory(root) then Left(CompileError.Io(IoFailure(root, IoFault.Stat)))
    else
      val opened =
        try Right(Files.walk(root))
        catch case _: IOException => Left(CompileError.Io(IoFailure(root, IoFault.Walk)))
      opened.flatMap: walk =>
        try
          val found = walk.iterator.asScala.flatMap: path =>
            val file = Option(path.getFileName).map(_.toString)
            file match
              case Some(name) if Files.isRegularFile(path) && name.endsWith(".class") =>
                val relative = root.relativize(path).iterator.asScala.map(_.toString).mkString("/")
                if relative.startsWith("..") || relative.startsWith("/") then None
                else Some(relative -> path)
              case _ => None
          Right(found.toList.sortBy(_._1))
        catch
          case _: IOException | _: UncheckedIOException =>
            Left(CompileError.Io(IoFailure(root, IoFault.Walk)))
        finally walk.close()

  private def readBytes(path: Path): Either[CompileError, Array[Byte]] =
    try Right(Files.readAllBytes(path))
    catch case _: IOException => Left(CompileError.Io(IoFailure(path, IoFault.Read)))

  private def piece(name: String, bytes: Array[Byte]): Array[Byte] =
    val head = s"$name\n".getBytes(StandardCharsets.UTF_8)
    val len  = Array[Byte](
      ((bytes.length >>> 24) & 0xff).toByte,
      ((bytes.length >>> 16) & 0xff).toByte,
      ((bytes.length >>> 8) & 0xff).toByte,
      (bytes.length & 0xff).toByte,
    )
    head ++ len ++ bytes

  private def rewrite(bytes: Array[Byte])(edit: ClassNode => Unit): Either[ClassBytesError, Array[Byte]] =
    try
      val node = new ClassNode()
      new ClassReader(bytes).accept(node, 0)
      edit(node)
      val writer = new ClassWriter(0)
      node.accept(writer)
      Right(writer.toByteArray)
    catch
      case _: IllegalArgumentException | _: IndexOutOfBoundsException =>
        Left(ClassBytesError.Unreadable)

  private def dropPositions(node: ClassNode): Unit =
    node.sourceDebug = null
    if node.attrs != null then
      val kept = node.attrs.asScala.filterNot(_.`type` == "TASTY").asJava
      node.attrs.clear()
      node.attrs.addAll(kept)
    node.methods.asScala.foreach: method =>
      val insns = method.instructions
      insns.toArray.foreach:
        case line: LineNumberNode => insns.remove(line)
        case _                    => ()

  private def dropPaths(node: ClassNode, root: String): Unit =
    if node.sourceFile != null then node.sourceFile = stripRoot(node.sourceFile, root)
    node.methods.asScala.foreach: method =>
      method.instructions.toArray.foreach:
        case ldc: LdcInsnNode =>
          ldc.cst match
            case text: String => ldc.cst = stripRoot(text, root)
            case _            => ()
        case _ => ()

  private enum PoolEntry:
    case Utf8(raw: Array[Byte], text: String)
    case Copied(raw: Array[Byte], slots: Int)

  private def rewritePool(bytes: Array[Byte], root: String): Either[ClassBytesError, Array[Byte]] =
    if bytes.length < 10 || !magic(bytes) then Left(ClassBytesError.Unreadable)
    else
      val count = u2(bytes, 8)
      readPool(bytes, 10, 1, count, Nil).map: (offset, entries) =>
        val out = new ByteArrayOutputStream
        out.write(bytes, 0, 10)
        entries.foreach:
          case PoolEntry.Utf8(raw, text) =>
            val cleaned = stripRoot(text, root)
            if cleaned == text then out.write(raw)
            else out.write(encodeUtf8(cleaned))
          case PoolEntry.Copied(raw, _) => out.write(raw)
        if offset < bytes.length then out.write(bytes, offset, bytes.length - offset)
        out.toByteArray

  private def readPool(
      bytes: Array[Byte],
      offset: Int,
      index: Int,
      count: Int,
      acc: List[PoolEntry],
  ): Either[ClassBytesError, (Int, List[PoolEntry])] =
    if index >= count then Right((offset, acc.reverse))
    else if offset >= bytes.length then Left(ClassBytesError.Unreadable)
    else
      val tag = bytes(offset) & 0xff
      entry(bytes, offset, tag) match
        case None                     => Left(ClassBytesError.Unreadable)
        case Some((next, slots, row)) =>
          readPool(bytes, next, index + slots, count, row :: acc)

  private def entry(
      bytes: Array[Byte],
      offset: Int,
      tag: Int,
  ): Option[(Int, Int, PoolEntry)] =
    def slice(length: Int, slots: Int): Option[(Int, Int, PoolEntry)] =
      if offset + length > bytes.length then None
      else Some((offset + length, slots, PoolEntry.Copied(bytes.slice(offset, offset + length), slots)))
    tag match
      case 1 =>
        if offset + 3 > bytes.length then None
        else
          val length = u2(bytes, offset + 1)
          val end    = offset + 3 + length
          if end > bytes.length then None
          else
            val raw = bytes.slice(offset, end)
            decode(raw.slice(3, raw.length)).map(text => (end, 1, PoolEntry.Utf8(raw, text)))
      case 7 | 8 | 16 | 19 | 20               => slice(3, 1)
      case 15                                 => slice(4, 1)
      case 3 | 4 | 9 | 10 | 11 | 12 | 17 | 18 => slice(5, 1)
      case 5 | 6                              => slice(9, 2)
      case _                                  => None

  private def magic(bytes: Array[Byte]): Boolean =
    (bytes(0) & 0xff) == 0xca && (bytes(1) & 0xff) == 0xfe && (bytes(2) & 0xff) == 0xba && (bytes(3) & 0xff) == 0xbe

  private def u2(bytes: Array[Byte], offset: Int): Int =
    ((bytes(offset) & 0xff) << 8) | (bytes(offset + 1) & 0xff)

  private def decode(raw: Array[Byte]): Option[String] =
    if raw.forall(byte => (byte & 0xff) < 128) then Some(new String(raw, StandardCharsets.US_ASCII))
    else
      try
        val prefix = Array[Byte](((raw.length >> 8) & 0xff).toByte, (raw.length & 0xff).toByte)
        val input  = new DataInputStream(new ByteArrayInputStream(prefix ++ raw))
        Some(input.readUTF())
      catch case _: IOException => None

  private def encodeUtf8(text: String): Array[Byte] =
    val body =
      if text.forall(_ < 128) then text.getBytes(StandardCharsets.US_ASCII)
      else
        val out  = new ByteArrayOutputStream
        val data = new DataOutputStream(out)
        data.writeUTF(text)
        data.flush()
        val prefixed = out.toByteArray
        prefixed.slice(2, prefixed.length)
    val length = body.length
    Array[Byte](1, ((length >> 8) & 0xff).toByte, (length & 0xff).toByte) ++ body

  /** The absolute checkout, with no trailing separator. A filesystem root is empty: stripping `/` would rewrite every
    * absolute path.
    */
  private def checkoutRoot(checkout: Path): String =
    val trimmed = trimSeparators(checkout.toAbsolutePath.normalize.toString)
    if isFilesystemRoot(trimmed) then "" else trimmed

  private def isFilesystemRoot(text: String): Boolean =
    text.length <= 1 || (text.length == 2 && text.charAt(1) == ':' && text.charAt(0).isLetter)

  private def trimSeparators(text: String): String =
    val end = text.lastIndexWhere(char => !isSep(char))
    if end < 0 then "" else text.substring(0, end + 1)

  private def stripRoot(text: String, root: String): String =
    if root.isEmpty then text
    else stripFrom(text, root, 0, new StringBuilder(text.length))

  @tailrec
  private def stripFrom(text: String, root: String, at: Int, out: StringBuilder): String =
    if at >= text.length then out.toString
    else if rootedAt(text, root, at) then
      val next = at + root.length
      val skip = if next < text.length && isSep(text.charAt(next)) then next + 1 else next
      stripFrom(text, root, skip, out)
    else stripFrom(text, root, at + 1, out.append(text.charAt(at)))

  private def rootedAt(text: String, root: String, at: Int): Boolean =
    val end = at + root.length
    end <= text.length &&
    text.regionMatches(at, root, 0, root.length) &&
    (at == 0 || !isPathChar(text.charAt(at - 1))) &&
    (end == text.length || isSep(text.charAt(end)))

  private def isPathChar(char: Char): Boolean =
    char != '/' && char != '\\' && char != '"' && char != '\u0000' && !char.isWhitespace

  private def isSep(char: Char): Boolean =
    char == '/' || char == '\\'

  private def scrubBytes(bytes: Array[Byte], root: String): Array[Byte] =
    val text = new String(bytes, StandardCharsets.ISO_8859_1)
    stripRoot(text, root).getBytes(StandardCharsets.ISO_8859_1)
end ProductDigest
