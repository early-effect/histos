package histos.jdk

import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

enum StageOp:
  case Read, Write, Move, Unpack, Delete

enum StageError:
  case Remote(url: HttpsUrl)
  case NotZip
  case Checksum(expected: Sha256, actual: Sha256)
  case UnsafePath(entry: String)
  case NoHome
  case Io(op: StageOp)

  def message: String = this match
    case Remote(url)                 => s"will not fetch ${url.value}"
    case NotZip                      => "jdk archive is not a zip"
    case Checksum(expected, actual)  => s"jdk checksum was ${actual.value}, lock has ${expected.value}"
    case UnsafePath(entry)           => s"jdk archive entry escapes the cache: $entry"
    case NoHome                      => "jdk archive has no bin/javac of the locked major"
    case Io(op)                      => s"jdk stage failed while ${op.toString.toLowerCase}"

/** Stage a locked zip into `cache/jdks/<sha256>`. `https` urls are refused. There is no network here. */
object JdkStage:
  def stage(lock: JdkLock, cache: Path): IO[StageError, JdkHome] =
    lock.source match
      case ArchiveSource.Remote(url) => ZIO.fail(StageError.Remote(url))
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

  private def place(source: Path, archive: Path, expected: Sha256): IO[StageError, Unit] =
    hashIfPresent(archive).flatMap:
      case Some(found) if found == expected => ZIO.unit
      case Some(found)                      => ZIO.fail(StageError.Checksum(expected, found))
      case None =>
        for
          tmp    <- tempBeside(archive)
          _      <- copy(source, tmp)
          actual <- hash(tmp)
          _      <-
            if actual == expected then move(tmp, archive)
            else delete(tmp) *> ZIO.fail(StageError.Checksum(expected, actual))
        yield ()

  private def unpackIfNeeded(unpack: Path, archive: Path, lock: JdkLock): IO[StageError, Unit] =
    homeOf(unpack, lock).either.flatMap:
      case Right(_) => ZIO.unit
      case Left(_)  => deleteTree(unpack) *> unpackZip(archive, unpack)

  private def homeOf(unpack: Path, lock: JdkLock): IO[StageError, JdkHome] =
    Jdk.homes(unpack).mapError(stageProbe).flatMap: homes =>
      homes.find(home => home.hasJavac && home.version.exists(_.major == lock.major)) match
        case Some(home) => ZIO.succeed(home)
        case None       => ZIO.fail(StageError.NoHome)

  private def stageProbe(error: JdkError): StageError =
    error match
      case JdkError.Unreadable(_) | JdkError.BadMajor(_) | JdkError.PathScan => StageError.Io(StageOp.Unpack)

  private def hashIfPresent(archive: Path): IO[StageError, Option[Sha256]] =
    ZIO.attempt(Files.isRegularFile(archive)).mapError(_ => StageError.Io(StageOp.Read)).flatMap:
      case false => ZIO.none
      case true  => hash(archive).map(Some(_))

  private def tempBeside(archive: Path): IO[StageError, Path] =
    Option(archive.getParent) match
      case None => ZIO.fail(StageError.Io(StageOp.Write))
      case Some(parent) =>
        ZIO.attempt {
          Files.createDirectories(parent)
          parent.resolve("archive.partial")
        }.mapError(_ => StageError.Io(StageOp.Write))

  private def copy(from: Path, to: Path): IO[StageError, Unit] =
    ZIO.attempt(Files.copy(from, to, StandardCopyOption.REPLACE_EXISTING)).mapError(_ => StageError.Io(StageOp.Write)).unit

  private def hash(path: Path): IO[StageError, Sha256] =
    ZIO.attempt {
      val digest = MessageDigest.getInstance("SHA-256")
      val input  = Files.newInputStream(path)
      try
        val buffer = new Array[Byte](8192)
        var read   = input.read(buffer)
        while read >= 0 do
          if read > 0 then digest.update(buffer, 0, read)
          read = input.read(buffer)
      finally input.close()
      Sha256.fromDigest(digest.digest)
    }.mapError(_ => StageError.Io(StageOp.Read))

  private def move(from: Path, to: Path): IO[StageError, Unit] =
    ZIO.attempt {
      Option(to.getParent).foreach(Files.createDirectories(_))
      try Files.move(from, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      catch case _: java.nio.file.AtomicMoveNotSupportedException =>
        Files.move(from, to, StandardCopyOption.REPLACE_EXISTING)
    }.mapError(_ => StageError.Io(StageOp.Move)).unit

  private def delete(path: Path): IO[StageError, Unit] =
    ZIO.attempt(Files.deleteIfExists(path)).mapError(_ => StageError.Io(StageOp.Delete)).unit

  private def deleteTree(root: Path): IO[StageError, Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.mapError(_ => StageError.Io(StageOp.Delete))

  private def requireZip(archive: Path): IO[StageError, Unit] =
    ZIO.attempt {
      val input = Files.newInputStream(archive)
      try
        val first  = input.read()
        val second = input.read()
        first == 0x50 && second == 0x4b
      finally input.close()
    }.mapError(_ => StageError.Io(StageOp.Read)).flatMap:
      case true  => ZIO.unit
      case false => ZIO.fail(StageError.NotZip)

  private def unpackZip(archive: Path, dest: Path): IO[StageError, Unit] =
    ZIO.attempt(unpackEntries(archive, dest.normalize)).mapError(_ => StageError.Io(StageOp.Unpack)).flatMap:
      case Left(error) => ZIO.fail(error)
      case Right(_)    => ZIO.unit

  private def unpackEntries(archive: Path, root: Path): Either[StageError, Unit] =
    Files.createDirectories(root)
    val input = new java.util.zip.ZipInputStream(Files.newInputStream(archive))
    try
      var entry  = input.getNextEntry
      var failed = Option.empty[StageError]
      while entry != null && failed.isEmpty do
        val current = entry
        val name    = current.getName
        failed =
          if current.isDirectory || name.endsWith("/") then None
          else
            safe(root, name) match
              case Left(error) => Some(error)
              case Right(path) =>
                try
                  Option(path.getParent).foreach(Files.createDirectories(_))
                  Files.copy(input, path, StandardCopyOption.REPLACE_EXISTING)
                  None
                catch case _: java.io.IOException => Some(StageError.Io(StageOp.Unpack))
        if failed.isEmpty then
          input.closeEntry()
          entry = input.getNextEntry
      failed.toLeft(())
    finally input.close()

  private def safe(root: Path, name: String): Either[StageError, Path] =
    val cleaned = name.replace('\\', '/')
    val parts   = cleaned.split("/").toList.filter(part => part.nonEmpty && part != ".")
    val escaped = cleaned.startsWith("/") || parts.exists(part => part == ".." || part.contains(":"))
    if escaped then Left(StageError.UnsafePath(name))
    else
      val dest = parts.foldLeft(root)(_.resolve(_)).normalize
      if dest.startsWith(root) then Right(dest) else Left(StageError.UnsafePath(name))
