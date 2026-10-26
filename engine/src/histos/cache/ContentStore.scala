package histos.cache

import histos.jdk.Sha256
import histos.schedule.{ActionCache, CacheError, Digest}
import histos.workspace.{IoFailure, IoFault}

import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}
import java.time.{Duration, Instant}
import scala.jdk.CollectionConverters.*

import zio.*

/** One project's keep set. The key is the sha256 of its absolute path, so the file name is safe. */
opaque type CacheProject = String

object CacheProject:
  def of(repo: Path): CacheProject =
    Sha256.of(Chunk.fromArray(repo.toAbsolutePath.normalize.toString.getBytes(StandardCharsets.UTF_8))).value

  extension (project: CacheProject) def value: String = project
end CacheProject

final case class CacheEntry(digest: Digest, path: Path, bytes: Long, lastUsed: Instant)

/** Content-addressed bytes. Writes land under a temporary name and move after the bytes read back. */
trait ContentStore:
  def root: Path
  def put(digest: Digest, bytes: Chunk[Byte], at: Instant): IO[CacheError, Unit]
  def read(digest: Digest): IO[CacheError, Option[Chunk[Byte]]]
  def list: IO[CacheError, List[CacheEntry]]

  /** Replace this project's keep set. Other projects are left alone. */
  def replace(project: CacheProject, digests: Set[Digest]): IO[CacheError, Unit]
  def kept: IO[CacheError, Set[Digest]]
  def clean(now: Instant, retention: Duration, dryRun: Boolean): IO[CacheError, List[CacheEntry]]

  /** Drop this project's keep file, then delete `cas` and `plugins` entries no remaining keep set names. Age does not
    * matter. A temp stamped at `now` stays.
    */
  def forget(project: CacheProject, now: Instant): IO[CacheError, List[CacheEntry]]
end ContentStore

object ContentStore:
  val Retention: Duration = Duration.ofDays(30)

  def in(root: Path): ContentStore = DiskStore(root)

  /** Plugin manifests are UTF-8 blobs in this store. The stamp is the fiber's clock. */
  def actions(store: ContentStore): ActionCache = new ActionCache:
    def get(digest: Digest): IO[CacheError, Option[String]] =
      store.read(digest).map(_.map(bytes => new String(bytes.toArray, StandardCharsets.UTF_8)))

    override def put(digest: Digest, value: String): IO[CacheError, Unit] =
      val bytes = Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8))
      Clock.instant.flatMap(at => store.put(digest, bytes, at))

  def home(xdgCache: Option[String], userHome: String): Path =
    xdgCache.filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir, "histos")
      case None      => Path.of(userHome, ".cache", "histos")
end ContentStore

private final class DiskStore(val root: Path) extends ContentStore:
  /** One JVM can lock a file only once. Fibers take this first, then the file lock covers other processes. */
  private val gate = java.util.concurrent.locks.ReentrantLock()
  def put(digest: Digest, bytes: Chunk[Byte], at: Instant): IO[CacheError, Unit] =
    val dest = objectPath(digest)
    ZIO.scoped:
      ZIO
        .acquireRelease(tempFile)(delete(_).ignore)
        .flatMap: tmp =>
          for
            _ <- write(tmp, bytes)
            _ <- readBack(tmp, bytes, digest)
            _ <- move(tmp, dest)
            _ <- stamp(dest, at)
          yield ()

  def read(digest: Digest): IO[CacheError, Option[Chunk[Byte]]] =
    val dest = objectPath(digest)
    ZIO
      .attemptBlocking:
        if Files.notExists(dest) then Right(None)
        else if !Files.isRegularFile(dest) then Left(CacheError.Io(IoFailure(dest, IoFault.Stat)))
        else Right(Some(Chunk.fromArray(Files.readAllBytes(dest))))
      .mapError(io(dest, IoFault.Read))
      .flatMap:
        case Left(error)   => ZIO.fail(error)
        case Right(result) => ZIO.succeed(result)

  def list: IO[CacheError, List[CacheEntry]] =
    ZIO.attemptBlocking(casEntries ++ pluginEntries).mapError(io(root, IoFault.Walk))

  def replace(project: CacheProject, digests: Set[Digest]): IO[CacheError, Unit] =
    withKeep(IoFault.Write): dir =>
      val file  = dir.resolve(project.value)
      val tmp   = Files.createTempFile(dir, s".${project.value}.", ".tmp")
      val lines = digests.map(_.value).toList.sorted
      val text  = if lines.isEmpty then "" else lines.mkString("\n") + "\n"
      try
        Files.writeString(tmp, text)
        try Files.move(tmp, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch
          case _: java.nio.file.AtomicMoveNotSupportedException =>
            Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING)
      finally Files.deleteIfExists(tmp)
      ()

  def kept: IO[CacheError, Set[Digest]] =
    withKeep(IoFault.Read): dir =>
      val listed = Files.list(dir)
      try
        listed.iterator.asScala.filter(Files.isRegularFile(_)).flatMap(linesOf).flatMap(Digest.from).toSet
      finally listed.close()

  def clean(now: Instant, retention: Duration, dryRun: Boolean): IO[CacheError, List[CacheEntry]] =
    val cutoff = now.minus(retention)
    for
      entries <- list
      keep    <- kept
      stale = entries.filter(entry => entry.lastUsed.isBefore(cutoff) && !keep.contains(entry.digest))
      _ <- ZIO.unless(dryRun)(ZIO.foreachDiscard(stale)(entry => delete(entry.path)) *> sweepTemps(cutoff))
    yield stale

  def forget(project: CacheProject, now: Instant): IO[CacheError, List[CacheEntry]] =
    for
      _       <- dropKeep(project)
      entries <- list
      keep    <- kept
      gone = entries.filter(entry => !keep.contains(entry.digest))
      _ <- ZIO.foreachDiscard(gone)(entry => delete(entry.path))
      _ <- sweepTemps(now)
    yield gone

  private def dropKeep(project: CacheProject): IO[CacheError, Unit] =
    withKeep(IoFault.Delete): dir =>
      Files.deleteIfExists(dir.resolve(project.value))
      ()

  private def objectPath(digest: Digest): Path =
    val hex = digest.value
    root.resolve("cas").resolve(hex.take(2)).resolve(hex.drop(2))

  private def tempFile: IO[CacheError, Path] =
    ZIO
      .randomWith(_.nextUUID)
      .flatMap: id =>
        ZIO
          .attemptBlocking {
            val dir = root.resolve("tmp")
            Files.createDirectories(dir)
            dir.resolve(id.toString)
          }
          .mapError(io(root.resolve("tmp"), IoFault.Temp))

  private def write(path: Path, bytes: Chunk[Byte]): IO[CacheError, Unit] =
    ZIO.attemptBlocking(Files.write(path, bytes.toArray)).mapError(io(path, IoFault.Write)).unit

  private def readBack(path: Path, bytes: Chunk[Byte], digest: Digest): IO[CacheError, Unit] =
    ZIO
      .attemptBlocking(Chunk.fromArray(Files.readAllBytes(path)))
      .mapError(io(path, IoFault.Read))
      .flatMap: found =>
        if found == bytes then ZIO.unit
        else ZIO.fail(CacheError.Corrupt(digest))

  private def move(from: Path, to: Path): IO[CacheError, Unit] =
    ZIO
      .attemptBlocking {
        Files.createDirectories(to.getParent)
        try Files.move(from, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch
          case _: java.nio.file.AtomicMoveNotSupportedException =>
            Files.move(from, to, StandardCopyOption.REPLACE_EXISTING)
      }
      .mapError(io(to, IoFault.Write))
      .unit

  private def stamp(path: Path, at: Instant): IO[CacheError, Unit] =
    ZIO
      .attemptBlocking(Files.setLastModifiedTime(path, java.nio.file.attribute.FileTime.from(at)))
      .mapError(io(path, IoFault.Write))
      .unit

  private def delete(path: Path): IO[CacheError, Unit] =
    ZIO
      .attemptBlocking {
        if Files.isDirectory(path) then
          val walk  = Files.walk(path)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(file => Files.deleteIfExists(file))
        else Files.deleteIfExists(path)
        ()
      }
      .mapError(io(path, IoFault.Delete))

  private def casEntries: List[CacheEntry] =
    val cas = root.resolve("cas")
    if !Files.isDirectory(cas) then Nil
    else
      val walk = Files.walk(cas)
      try walk.iterator.asScala.filter(Files.isRegularFile(_)).flatMap(entryOf).toList
      finally walk.close()

  /** Complete and incomplete plugin class directories. A `.partial-` directory is not a digest. */
  private def pluginEntries: List[CacheEntry] =
    val plugins = root.resolve("plugins")
    if !Files.isDirectory(plugins) then Nil
    else
      val listed = Files.list(plugins)
      try listed.iterator.asScala.flatMap(pluginEntry).toList
      finally listed.close()

  private def pluginEntry(path: Path): Option[CacheEntry] =
    val name = Option(path.getFileName).map(_.toString).getOrElse("")
    if !Files.isDirectory(path) then None
    else
      Digest
        .from(name)
        .map: digest =>
          CacheEntry(digest, path, treeBytes(path), Files.getLastModifiedTime(path).toInstant)

  private def treeBytes(path: Path): Long =
    val walk = Files.walk(path)
    try walk.iterator.asScala.filter(Files.isRegularFile(_)).map(Files.size).sum
    finally walk.close()

  private def entryOf(path: Path): Option[CacheEntry] =
    val parent = Option(path.getParent).flatMap(dir => Option(dir.getFileName)).map(_.toString)
    val name   = Option(path.getFileName).map(_.toString)
    (parent, name) match
      case (Some(shard), Some(rest)) =>
        Digest
          .from(shard + rest)
          .map: digest =>
            CacheEntry(digest, path, Files.size(path), Files.getLastModifiedTime(path).toInstant)
      case _ => None

  /** Temps older than the cutoff are leftovers. A put that is still running stamped its file just now. */
  private def sweepTemps(cutoff: Instant): IO[CacheError, Unit] =
    ZIO
      .attemptBlocking {
        val dir = root.resolve("tmp")
        if Files.isDirectory(dir) then
          val listed = Files.list(dir)
          val old    =
            try
              listed.iterator.asScala
                .filter(Files.isRegularFile(_))
                .filter { path =>
                  Files.getLastModifiedTime(path).toInstant.isBefore(cutoff)
                }
                .toList
            finally listed.close()
          old.foreach(path => Files.deleteIfExists(path))
        ()
      }
      .mapError(io(root.resolve("tmp"), IoFault.Delete))

  private def withKeep[A](fault: IoFault)(use: Path => A): IO[CacheError, A] =
    ZIO
      .attemptBlockingInterrupt {
        gate.lockInterruptibly()
        try
          Files.createDirectories(root)
          val channel = FileChannel.open(
            root.resolve("keep.lock"),
            StandardOpenOption.CREATE,
            StandardOpenOption.WRITE,
          )
          try
            val lock = channel.lock()
            try use(keepDir())
            finally lock.close()
          finally channel.close()
        finally gate.unlock()
      }
      .mapError(io(root.resolve("keep"), fault))

  /** `keep` used to be one file. A directory holds one file per project, and that old file becomes `legacy`. */
  private def keepDir(): Path =
    val path = root.resolve("keep")
    if Files.isRegularFile(path) then
      val text = Files.readString(path)
      Files.delete(path)
      Files.createDirectories(path)
      if text.nonEmpty then Files.writeString(path.resolve("legacy"), text)
      path
    else
      Files.createDirectories(path)
      path

  private def linesOf(path: Path): List[String] =
    Files.readString(path).split('\n').toList.map(_.trim).filter(_.nonEmpty)

  private def io(path: Path, fault: IoFault): Throwable => CacheError =
    _ => CacheError.Io(IoFailure(path, fault))
end DiskStore
