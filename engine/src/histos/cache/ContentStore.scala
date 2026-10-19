package histos.cache

import histos.schedule.{ActionCache, CacheError, CacheOp, Digest}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.{Duration, Instant}
import scala.jdk.CollectionConverters.*

import zio.*

final case class CacheEntry(digest: Digest, path: Path, bytes: Long, lastUsed: Instant)

/** Content-addressed bytes. Writes land under a temporary name and move after the bytes read back. */
trait ContentStore:
  def root: Path
  def put(digest: Digest, bytes: Chunk[Byte], at: Instant): IO[CacheError, Unit]
  def read(digest: Digest): IO[CacheError, Option[Chunk[Byte]]]
  def list: IO[CacheError, List[CacheEntry]]
  def remember(digest: Digest): IO[CacheError, Unit]
  def kept: IO[CacheError, Set[Digest]]
  def clean(now: Instant, retention: Duration, dryRun: Boolean): IO[CacheError, List[CacheEntry]]

object ContentStore:
  val Retention: Duration = Duration.ofDays(30)

  def in(root: Path): ContentStore = DiskStore(root)

  /** Plugin manifests are UTF-8 blobs in this store. The stamp is wall time. `ActionCache.put` has no clock. */
  def actions(store: ContentStore): ActionCache = new ActionCache:
    def get(digest: Digest): IO[CacheError, Option[String]] =
      store.read(digest).map(_.map(bytes => new String(bytes.toArray, StandardCharsets.UTF_8)))

    override def put(digest: Digest, value: String): IO[CacheError, Unit] =
      val bytes = Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8))
      store.put(digest, bytes, Instant.now)

  def home(xdgCache: Option[String], userHome: String): Path =
    xdgCache.filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir, "histos")
      case None      => Path.of(userHome, ".cache", "histos")

private final class DiskStore(val root: Path) extends ContentStore:
  def put(digest: Digest, bytes: Chunk[Byte], at: Instant): IO[CacheError, Unit] =
    val dest = objectPath(digest)
    for
      tmp <- tempFile
      _   <- write(tmp, bytes)
      _   <- readBack(tmp, bytes, digest)
      _   <- move(tmp, dest)
      _   <- stamp(dest, at)
    yield ()

  def read(digest: Digest): IO[CacheError, Option[Chunk[Byte]]] =
    val dest = objectPath(digest)
    ZIO.attempt(Files.isRegularFile(dest)).mapError(io(CacheOp.Read)).flatMap:
      case false => ZIO.none
      case true  =>
        ZIO.attempt {
          val found = Chunk.fromArray(Files.readAllBytes(dest))
          found
        }.mapError(io(CacheOp.Read)).map(Some(_))

  def list: IO[CacheError, List[CacheEntry]] =
    ZIO.attempt(casEntries ++ pluginEntries).mapError(io(CacheOp.List))

  def remember(digest: Digest): IO[CacheError, Unit] =
    ZIO.attempt {
      Files.createDirectories(root)
      val file = root.resolve("keep")
      val have = if Files.isRegularFile(file) then Files.readString(file) else ""
      if !have.split('\n').contains(digest.value) then
        Files.writeString(file, have + digest.value + "\n")
    }.mapError(io(CacheOp.Keep)).unit

  def kept: IO[CacheError, Set[Digest]] =
    ZIO.attempt {
      val file = root.resolve("keep")
      if !Files.isRegularFile(file) then Set.empty[String]
      else Files.readString(file).split('\n').toSet.filter(_.nonEmpty)
    }.mapError(io(CacheOp.Keep)).map(_.flatMap(Digest.from))

  def clean(now: Instant, retention: Duration, dryRun: Boolean): IO[CacheError, List[CacheEntry]] =
    val cutoff = now.minus(retention)
    for
      entries <- list
      keep    <- kept
      stale    = entries.filter(entry => entry.lastUsed.isBefore(cutoff) && !keep.contains(entry.digest))
      _       <- ZIO.unless(dryRun)(ZIO.foreachDiscard(stale)(entry => delete(entry.path)))
    yield stale

  private def objectPath(digest: Digest): Path =
    val hex = digest.value
    root.resolve("cas").resolve(hex.take(2)).resolve(hex.drop(2))

  private def tempFile: IO[CacheError, Path] =
    ZIO.randomWith(_.nextUUID).flatMap: id =>
      ZIO.attempt {
        val dir = root.resolve("tmp")
        Files.createDirectories(dir)
        dir.resolve(id.toString)
      }.mapError(io(CacheOp.CreateTemp))

  private def write(path: Path, bytes: Chunk[Byte]): IO[CacheError, Unit] =
    ZIO.attempt(Files.write(path, bytes.toArray)).mapError(io(CacheOp.Write)).unit

  private def readBack(path: Path, bytes: Chunk[Byte], digest: Digest): IO[CacheError, Unit] =
    ZIO.attempt(Chunk.fromArray(Files.readAllBytes(path))).mapError(io(CacheOp.Read)).flatMap: found =>
      if found == bytes then ZIO.unit
      else delete(path) *> ZIO.fail(CacheError.Corrupt(digest))

  private def move(from: Path, to: Path): IO[CacheError, Unit] =
    ZIO.attempt {
      Files.createDirectories(to.getParent)
      try Files.move(from, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      catch
        case _: java.nio.file.AtomicMoveNotSupportedException =>
          Files.move(from, to, StandardCopyOption.REPLACE_EXISTING)
    }.mapError(io(CacheOp.Move)).unit

  private def stamp(path: Path, at: Instant): IO[CacheError, Unit] =
    ZIO.attempt(Files.setLastModifiedTime(path, java.nio.file.attribute.FileTime.from(at))).mapError(io(CacheOp.Stamp)).unit

  private def delete(path: Path): IO[CacheError, Unit] =
    ZIO.attempt {
      if Files.isDirectory(path) then
        val walk = Files.walk(path)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(file => Files.deleteIfExists(file))
      else Files.deleteIfExists(path)
      ()
    }.mapError(io(CacheOp.Delete))

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
      Digest.from(name).map: digest =>
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
        Digest.from(shard + rest).map: digest =>
          CacheEntry(digest, path, Files.size(path), Files.getLastModifiedTime(path).toInstant)
      case _ => None

  private def io(op: CacheOp): Throwable => CacheError = _ => CacheError.Io(op)
end DiskStore
