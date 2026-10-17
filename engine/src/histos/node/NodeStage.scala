package histos.node

import histos.jdk.{ArchiveSource, HttpsUrl, Sha256}

import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest
import java.util.zip.ZipInputStream

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

enum NodeStageOp:
  case Read, Write, Move, Unpack, Delete

  def label: String = this match
    case Read   => "read"
    case Write  => "write"
    case Move   => "move"
    case Unpack => "unpack"
    case Delete => "delete"

enum NodeStageError:
  case Remote(url: HttpsUrl)
  case NotZip
  case Checksum(expected: Sha256, actual: Sha256)
  case UnsafePath(entry: String)
  case NoHome
  case Io(op: NodeStageOp)

  def message: String = this match
    case Remote(url)                => s"will not fetch ${url.value}"
    case NotZip                     => "node archive is not a zip"
    case Checksum(expected, actual) => s"node checksum was ${actual.value}, lock has ${expected.value}"
    case UnsafePath(entry)          => s"node archive entry escapes the cache: $entry"
    case NoHome                     => "node archive has no bin/node of the locked major"
    case Io(op)                     => s"node stage failed while ${op.label}"

/** Stage a locked zip into `cache/nodes/<sha256>`. `https` urls are refused. There is no network here. */
object NodeStage:
  def stage(lock: NodeLock, cache: Path): IO[NodeStageError, NodeHome] =
    lock.source match
      case ArchiveSource.Remote(url) => ZIO.fail(NodeStageError.Remote(url))
      case ArchiveSource.Local(source) =>
        val root    = cache.resolve(lock.sha256.value)
        val archive = root.resolve("archive")
        val unpack  = root.resolve("unpack")
        for
          _    <- place(source, archive, lock.sha256)
          _    <- requireZip(archive)
          _    <- unpackIfNeeded(unpack, archive, lock)
          home <- homeOf(unpack, lock)
        yield home

  private def place(source: Path, archive: Path, expected: Sha256): IO[NodeStageError, Unit] =
    hashIfPresent(archive).flatMap:
      case Some(found) if found == expected => ZIO.unit
      case Some(found)                      => ZIO.fail(NodeStageError.Checksum(expected, found))
      case None =>
        for
          tmp    <- tempBeside(archive)
          _      <- copy(source, tmp)
          actual <- hash(tmp)
          _      <-
            if actual == expected then move(tmp, archive)
            else delete(tmp) *> ZIO.fail(NodeStageError.Checksum(expected, actual))
        yield ()

  private def unpackIfNeeded(unpack: Path, archive: Path, lock: NodeLock): IO[NodeStageError, Unit] =
    homeOf(unpack, lock).either.flatMap:
      case Right(_) => ZIO.unit
      case Left(_)  => deleteTree(unpack) *> unpackZip(archive, unpack)

  private def homeOf(unpack: Path, lock: NodeLock): IO[NodeStageError, NodeHome] =
    NodeProbe.homes(unpack).mapError(stageProbe).flatMap: homes =>
      homes.find(home => home.hasBinary && home.version.exists(_.major == lock.major)) match
        case Some(home) => ZIO.succeed(home)
        case None       => ZIO.fail(NodeStageError.NoHome)

  private def stageProbe(error: NodeError): NodeStageError =
    error match
      case NodeError.Unreadable(_) | NodeError.BadMajor(_) | NodeError.PathScan | NodeError.Version(_, _) |
          NodeError.Lock(_) | NodeError.Stage(_) | NodeError.Override(_) | NodeError.NoLock(_) =>
        NodeStageError.Io(NodeStageOp.Unpack)

  private def hashIfPresent(archive: Path): IO[NodeStageError, Option[Sha256]] =
    ZIO.attempt(Files.isRegularFile(archive)).mapError(_ => NodeStageError.Io(NodeStageOp.Read)).flatMap:
      case false => ZIO.none
      case true  => hash(archive).map(Some(_))

  private def tempBeside(archive: Path): IO[NodeStageError, Path] =
    Option(archive.getParent) match
      case None => ZIO.fail(NodeStageError.Io(NodeStageOp.Write))
      case Some(parent) =>
        ZIO.attempt {
          Files.createDirectories(parent)
          parent.resolve("archive.partial")
        }.mapError(_ => NodeStageError.Io(NodeStageOp.Write))

  private def copy(from: Path, to: Path): IO[NodeStageError, Unit] =
    ZIO.attempt(Files.copy(from, to, StandardCopyOption.REPLACE_EXISTING)).mapError(_ => NodeStageError.Io(NodeStageOp.Write)).unit

  private def hash(path: Path): IO[NodeStageError, Sha256] =
    ZIO.attempt {
      val digest = MessageDigest.getInstance("SHA-256")
      val input  = Files.newInputStream(path)
      try pump(input, digest)
      finally input.close()
      Sha256.fromDigest(digest.digest)
    }.mapError(_ => NodeStageError.Io(NodeStageOp.Read))

  private def pump(input: java.io.InputStream, digest: MessageDigest): Unit =
    val buffer = new Array[Byte](8192)
    val read   = input.read(buffer)
    if read > 0 then
      digest.update(buffer, 0, read)
      pump(input, digest)
    else if read == 0 then pump(input, digest)

  private def move(from: Path, to: Path): IO[NodeStageError, Unit] =
    ZIO.attempt {
      Option(to.getParent).foreach(Files.createDirectories(_))
      try Files.move(from, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      catch
        case _: java.nio.file.AtomicMoveNotSupportedException =>
          Files.move(from, to, StandardCopyOption.REPLACE_EXISTING)
    }.mapError(_ => NodeStageError.Io(NodeStageOp.Move)).unit

  private def delete(path: Path): IO[NodeStageError, Unit] =
    ZIO.attempt(Files.deleteIfExists(path)).mapError(_ => NodeStageError.Io(NodeStageOp.Delete)).unit

  private def deleteTree(root: Path): IO[NodeStageError, Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.mapError(_ => NodeStageError.Io(NodeStageOp.Delete))

  private def requireZip(archive: Path): IO[NodeStageError, Unit] =
    ZIO.attempt {
      val input = Files.newInputStream(archive)
      try
        val first  = input.read()
        val second = input.read()
        first == 0x50 && second == 0x4b
      finally input.close()
    }.mapError(_ => NodeStageError.Io(NodeStageOp.Read)).flatMap:
      case true  => ZIO.unit
      case false => ZIO.fail(NodeStageError.NotZip)

  private def unpackZip(archive: Path, dest: Path): IO[NodeStageError, Unit] =
    ZIO.attempt(unpackEntries(archive, dest.normalize)).mapError(_ => NodeStageError.Io(NodeStageOp.Unpack)).flatMap:
      case Left(error) => ZIO.fail(error)
      case Right(_)    => ZIO.unit

  private def unpackEntries(archive: Path, root: Path): Either[NodeStageError, Unit] =
    Files.createDirectories(root)
    val input = new ZipInputStream(Files.newInputStream(archive))
    try readEntries(input, root)
    finally input.close()

  private def readEntries(input: ZipInputStream, root: Path): Either[NodeStageError, Unit] =
    Option(input.getNextEntry) match
      case None => Right(())
      case Some(entry) =>
        val name = entry.getName
        val step =
          if entry.isDirectory || name.endsWith("/") then Right(())
          else
            safe(root, name).flatMap: path =>
              try
                Option(path.getParent).foreach(Files.createDirectories(_))
                Files.copy(input, path, StandardCopyOption.REPLACE_EXISTING)
                Right(())
              catch case _: java.io.IOException => Left(NodeStageError.Io(NodeStageOp.Unpack))
        step.flatMap: _ =>
          input.closeEntry()
          readEntries(input, root)

  private def safe(root: Path, name: String): Either[NodeStageError, Path] =
    val cleaned = name.replace('\\', '/')
    val parts   = cleaned.split("/").toList.filter(part => part.nonEmpty && part != ".")
    val escaped = cleaned.startsWith("/") || parts.exists(part => part == ".." || part.contains(":"))
    if escaped then Left(NodeStageError.UnsafePath(name))
    else
      val dest = parts.foldLeft(root)(_.resolve(_)).normalize
      if dest.startsWith(root) then Right(dest) else Left(NodeStageError.UnsafePath(name))
