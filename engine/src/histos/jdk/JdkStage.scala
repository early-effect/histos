package histos.jdk

import histos.cache.{Placed, Scratch}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

enum StageError:
  case Remote(url: HttpsUrl)
  case UnknownArchive
  case Checksum(expected: Sha256, actual: Sha256)
  case UnsafePath(entry: String)
  case NoHome
  case Extract(archive: Path, failure: ExtractFailure)
  case Io(failure: IoFailure)

  def message: String = this match
    case Remote(url)                 => s"will not fetch ${url.value}"
    case UnknownArchive              => "jdk archive is not a zip, a tar.gz, or a tar.xz"
    case Checksum(expected, actual)  => s"jdk checksum was ${actual.value}, lock has ${expected.value}"
    case UnsafePath(entry)           => s"jdk archive entry escapes the cache: $entry"
    case NoHome                      => "jdk archive has no bin/javac of the locked major"
    case Extract(archive, failure)   => failure.message(archive)
    case Io(failure)                 => failure.message

/**
 * Stage a locked archive into `cache/jdks/<sha256>`. `https` urls are refused. There is no network here.
 * A zip is the Windows archive. A tar.gz or tar.xz is unpacked by the system `tar`.
 * The archive and the unpack are built under scratch names and moved into place, so a reader never sees half of one.
 */
object JdkStage:
  def stage(lock: JdkLock, machine: Machine): IO[StageError, JdkHome] =
    lock.source match
      case ArchiveSource.Remote(url) => ZIO.fail(StageError.Remote(url))
      case ArchiveSource.Local(source) =>
        val root    = machine.cache.resolve("jdks").resolve(lock.sha256.value)
        val archive = root.resolve("archive")
        val unpack  = root.resolve("unpack")
        for
          _    <- place(source, archive, lock.sha256)
          kind <- ArchiveKind.of(archive, StageError.Io(IoFailure(archive, IoFault.Read))).someOrFail(StageError.UnknownArchive)
          _    <- unpackIfNeeded(kind, archive, unpack, lock, machine)
          home <- homeOf(unpack, lock)
        yield home

  private def place(source: Path, archive: Path, expected: Sha256): IO[StageError, Unit] =
    hashIfPresent(archive).flatMap:
      case Some(found) if found == expected => ZIO.unit
      case Some(found)                      => ZIO.fail(StageError.Checksum(expected, found))
      case None =>
        ZIO.scoped:
          for
            tmp    <- Scratch.file(archive, StageError.Io(IoFailure(archive, IoFault.Write)))
            _      <- copy(source, tmp)
            actual <- hash(tmp)
            _      <- ZIO.unless(actual == expected)(ZIO.fail(StageError.Checksum(expected, actual)))
            _      <- Scratch.place(tmp, archive, StageError.Io(IoFailure(archive, IoFault.Write)))
          yield ()

  private def unpackIfNeeded(kind: ArchiveKind, archive: Path, unpack: Path, lock: JdkLock, machine: Machine): IO[StageError, Unit] =
    homeOf(unpack, lock).either.flatMap:
      case Right(_) => ZIO.unit
      case Left(_) =>
        ZIO.scoped:
          for
            fresh <- Scratch.directory(unpack, StageError.Io(IoFailure(unpack, IoFault.Create)))
            _     <- kind match
                       case ArchiveKind.Zip                     => unpackZip(archive, fresh)
                       case ArchiveKind.TarGz | ArchiveKind.TarXz =>
                         Tar.extract(archive, fresh, machine).mapError(StageError.Extract(archive, _))
            _     <- homeOf(fresh, lock)
            _     <- install(fresh, unpack, lock)
          yield ()

  /** A complete unpack already in place came from a stager that won the race, and it stays. An incomplete one predates atomic staging. */
  private def install(fresh: Path, unpack: Path, lock: JdkLock): IO[StageError, Unit] =
    Scratch.place(fresh, unpack, StageError.Io(IoFailure(unpack, IoFault.Write))).flatMap:
      case Placed.Moved => ZIO.unit
      case Placed.Occupied =>
        homeOf(unpack, lock).unit.orElse:
          deleteTree(unpack) *> Scratch.place(fresh, unpack, StageError.Io(IoFailure(unpack, IoFault.Write))) *> homeOf(unpack, lock).unit

  private def homeOf(unpack: Path, lock: JdkLock): IO[StageError, JdkHome] =
    Jdk.homes(unpack).mapError(stageProbe(unpack)).flatMap: homes =>
      homes.find(home => home.hasJavac && home.version.exists(_.major == lock.major)) match
        case Some(home) => ZIO.succeed(home)
        case None       => ZIO.fail(StageError.NoHome)

  private def stageProbe(root: Path)(error: JdkError): StageError =
    error match
      case JdkError.Unreadable(path) => StageError.Io(IoFailure(path, IoFault.Read))
      case JdkError.BadMajor(_) | JdkError.PathScan => StageError.Io(IoFailure(root, IoFault.Walk))

  private def hashIfPresent(archive: Path): IO[StageError, Option[Sha256]] =
    ZIO.attemptBlocking(Files.isRegularFile(archive)).mapError(_ => StageError.Io(IoFailure(archive, IoFault.Stat))).flatMap:
      case false => ZIO.none
      case true  => hash(archive).map(Some(_))

  private def copy(from: Path, to: Path): IO[StageError, Unit] =
    ZIO.attemptBlocking(Files.copy(from, to, StandardCopyOption.REPLACE_EXISTING)).mapError(_ => StageError.Io(IoFailure(to, IoFault.Write))).unit

  private def hash(path: Path): IO[StageError, Sha256] =
    ZIO.attemptBlocking {
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
    }.mapError(_ => StageError.Io(IoFailure(path, IoFault.Read)))

  private def deleteTree(root: Path): IO[StageError, Unit] =
    Workspace.delete(root).mapError(StageError.Io(_))

  private def unpackZip(archive: Path, dest: Path): IO[StageError, Unit] =
    ZIO.attemptBlocking(unpackEntries(archive, dest.normalize)).mapError(_ => StageError.Io(IoFailure(dest, IoFault.Write))).flatMap:
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
                catch case _: java.io.IOException => Some(StageError.Io(IoFailure(path, IoFault.Write)))
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
