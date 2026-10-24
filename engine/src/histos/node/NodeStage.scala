package histos.node

import histos.cache.{Placed, Scratch}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.jdk.{ArchiveKind, ArchiveSource, ExtractFailure, HttpsUrl, Machine, Sha256, Tar}

import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest
import java.util.zip.{ZipException, ZipInputStream}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

enum NodeStageError:
  case Remote(url: HttpsUrl)
  case UnknownArchive
  case Checksum(expected: Sha256, actual: Sha256)
  case UnsafePath(entry: String)
  case NoHome
  case Version(path: Path, error: NodeVersionError)
  case Extract(archive: Path, failure: ExtractFailure)
  case Io(failure: IoFailure)

  def message: String = this match
    case Remote(url)                => s"will not fetch ${url.value}"
    case UnknownArchive             => "node archive is not a zip, a tar.gz, or a tar.xz"
    case Extract(archive, failure)  => failure.message(archive)
    case Checksum(expected, actual) => s"node checksum was ${actual.value}, lock has ${expected.value}"
    case UnsafePath(entry)          => s"node archive entry escapes the cache: $entry"
    case NoHome                     => "node archive has no bin/node of the locked major"
    case Version(path, error)       => s"could not read the Node version at $path: ${error.message}"
    case Io(failure)                => failure.message

/**
 * Stage a locked archive into `cache/nodes/<sha256>`. `https` urls are refused. There is no network here.
 * A zip is the Windows archive. A tar.gz or tar.xz, which nodejs.org ships for Linux and macOS, is unpacked by the system `tar`.
 * The archive and the unpack are built under scratch names and moved into place, so a reader never sees half of one.
 */
object NodeStage:
  def stage(lock: NodeLock, machine: Machine): IO[NodeStageError, NodeHome] =
    lock.source match
      case ArchiveSource.Remote(url) => ZIO.fail(NodeStageError.Remote(url))
      case ArchiveSource.Local(source) =>
        val root    = machine.cache.resolve("nodes").resolve(lock.sha256.value)
        val archive = root.resolve("archive")
        val unpack  = root.resolve("unpack")
        for
          _    <- place(source, archive, lock.sha256)
          kind <- ArchiveKind.of(archive, NodeStageError.Io(IoFailure(archive, IoFault.Read))).someOrFail(NodeStageError.UnknownArchive)
          _    <- unpackIfNeeded(kind, archive, unpack, lock, machine)
          home <- homeOf(unpack, lock)
        yield home

  private def place(source: Path, archive: Path, expected: Sha256): IO[NodeStageError, Unit] =
    hashIfPresent(archive).flatMap:
      case Some(found) if found == expected => ZIO.unit
      case Some(found)                      => ZIO.fail(NodeStageError.Checksum(expected, found))
      case None =>
        ZIO.scoped:
          for
            tmp    <- Scratch.file(archive, NodeStageError.Io(IoFailure(archive, IoFault.Write)))
            _      <- copy(source, tmp)
            actual <- hash(tmp)
            _      <- ZIO.unless(actual == expected)(ZIO.fail(NodeStageError.Checksum(expected, actual)))
            _      <- Scratch.place(tmp, archive, NodeStageError.Io(IoFailure(archive, IoFault.Write)))
          yield ()

  private def unpackIfNeeded(
      kind: ArchiveKind,
      archive: Path,
      unpack: Path,
      lock: NodeLock,
      machine: Machine,
  ): IO[NodeStageError, Unit] =
    homeOf(unpack, lock).either.flatMap:
      case Right(_) => ZIO.unit
      case Left(_) =>
        ZIO.scoped:
          for
            fresh <- Scratch.directory(unpack, NodeStageError.Io(IoFailure(unpack, IoFault.Create)))
            _     <- kind match
                       case ArchiveKind.Zip                     => unpackZip(archive, fresh)
                       case ArchiveKind.TarGz | ArchiveKind.TarXz =>
                         Tar.extract(archive, fresh, machine).mapError(NodeStageError.Extract(archive, _))
            _     <- homeOf(fresh, lock)
            _     <- install(fresh, unpack, lock)
          yield ()

  /** A complete unpack already in place came from a stager that won the race, and it stays. An incomplete one predates atomic staging. */
  private def install(fresh: Path, unpack: Path, lock: NodeLock): IO[NodeStageError, Unit] =
    Scratch.place(fresh, unpack, NodeStageError.Io(IoFailure(unpack, IoFault.Write))).flatMap:
      case Placed.Moved => ZIO.unit
      case Placed.Occupied =>
        homeOf(unpack, lock).unit.orElse:
          deleteTree(unpack) *> Scratch.place(fresh, unpack, NodeStageError.Io(IoFailure(unpack, IoFault.Write))) *> homeOf(unpack, lock).unit

  private def homeOf(unpack: Path, lock: NodeLock): IO[NodeStageError, NodeHome] =
    NodeProbe.homes(unpack).mapError(stageProbe(unpack)).flatMap: homes =>
      homes.find(home => home.hasBinary && home.version.exists(_.major == lock.major)) match
        case Some(home) => ZIO.succeed(home)
        case None       => ZIO.fail(NodeStageError.NoHome)

  private def stageProbe(root: Path)(error: NodeError): NodeStageError =
    error match
      case NodeError.Unreadable(path)    => NodeStageError.Io(IoFailure(path, IoFault.Read))
      case NodeError.Version(path, error) => NodeStageError.Version(path, error)
      case NodeError.NoLock(path)        => NodeStageError.Io(IoFailure(path, IoFault.Read))
      case NodeError.Stage(stage)        => stage
      case NodeError.BadMajor(_) | NodeError.PathScan | NodeError.Lock(_) | NodeError.Override(_) =>
        NodeStageError.Io(IoFailure(root, IoFault.Walk))

  private def hashIfPresent(archive: Path): IO[NodeStageError, Option[Sha256]] =
    ZIO.attemptBlocking(Files.isRegularFile(archive)).mapError(_ => NodeStageError.Io(IoFailure(archive, IoFault.Stat))).flatMap:
      case false => ZIO.none
      case true  => hash(archive).map(Some(_))

  private def copy(from: Path, to: Path): IO[NodeStageError, Unit] =
    ZIO.attemptBlocking(Files.copy(from, to, StandardCopyOption.REPLACE_EXISTING)).mapError(_ => NodeStageError.Io(IoFailure(to, IoFault.Write))).unit

  private def hash(path: Path): IO[NodeStageError, Sha256] =
    ZIO.attemptBlocking {
      val digest = MessageDigest.getInstance("SHA-256")
      val input  = Files.newInputStream(path)
      try pump(input, digest)
      finally input.close()
      Sha256.fromDigest(digest.digest)
    }.mapError(_ => NodeStageError.Io(IoFailure(path, IoFault.Read)))

  private def pump(input: java.io.InputStream, digest: MessageDigest): Unit =
    val buffer = new Array[Byte](8192)
    val read   = input.read(buffer)
    if read > 0 then
      digest.update(buffer, 0, read)
      pump(input, digest)
    else if read == 0 then pump(input, digest)

  private def deleteTree(root: Path): IO[NodeStageError, Unit] =
    Workspace.delete(root).mapError(NodeStageError.Io(_))

  private def unpackZip(archive: Path, dest: Path): IO[NodeStageError, Unit] =
    val root = dest.normalize
    for
      _ <- ZIO
             .attemptBlocking(Files.createDirectories(root))
             .mapError(_ => NodeStageError.Io(IoFailure(root, IoFault.Create)))
      result <- ZIO
                  .attemptBlocking(unpackEntries(archive, root))
                  .mapError:
                    case _: ZipException => NodeStageError.Extract(archive, ExtractFailure.Malformed)
                    case _ => NodeStageError.Extract(archive, ExtractFailure.Io(IoFailure(archive, IoFault.Read)))
      _ <- result match
             case Left(error) => ZIO.fail(error)
             case Right(_)    => ZIO.unit
    yield ()

  private def unpackEntries(archive: Path, root: Path): Either[NodeStageError, Unit] =
    val input = new ZipInputStream(Files.newInputStream(archive))
    try readEntries(input, root, archive)
    finally input.close()

  private def readEntries(input: ZipInputStream, root: Path, archive: Path): Either[NodeStageError, Unit] =
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
                if launcher(path) then path.toFile.setExecutable(true)
                Right(())
              catch
                case _: ZipException => Left(NodeStageError.Extract(archive, ExtractFailure.Malformed))
                case _: java.io.IOException => Left(NodeStageError.Io(IoFailure(path, IoFault.Write)))
        step.flatMap: _ =>
          input.closeEntry()
          readEntries(input, root, archive)

  /** A zip entry read here carries no mode, and the staged `bin/node` has to run. */
  private def launcher(path: Path): Boolean =
    Option(path.getFileName).exists(_.toString == "node") &&
      Option(path.getParent).flatMap(parent => Option(parent.getFileName)).exists(_.toString == "bin")

  private def safe(root: Path, name: String): Either[NodeStageError, Path] =
    val cleaned = name.replace('\\', '/')
    val parts   = cleaned.split("/").toList.filter(part => part.nonEmpty && part != ".")
    val escaped = cleaned.startsWith("/") || parts.exists(part => part == ".." || part.contains(":"))
    if escaped then Left(NodeStageError.UnsafePath(name))
    else
      val dest = parts.foldLeft(root)(_.resolve(_)).normalize
      if dest.startsWith(root) then Right(dest) else Left(NodeStageError.UnsafePath(name))
