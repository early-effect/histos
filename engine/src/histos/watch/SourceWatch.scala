package histos.watch

import java.nio.file.{Files, Path}
import java.nio.file.attribute.BasicFileAttributes

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import histos.Module

import zio.{durationInt, IO, ZIO}

/** Source files a command recompiles. Output under `.histos` is not a source.
  */
object SourceWatch:
  final case class Entry(path: String, modified: Long, bytes: Long)
  final case class Snapshot(entries: List[Entry])

  val Poll: zio.Duration   = 200.millis
  val Settle: zio.Duration = 300.millis

  def roots(repo: Path, modules: List[Module]): List[Path] =
    val owned = modules.flatMap { module =>
      val base = repo.resolve(module.baseDirectory)
      List(base.resolve("src"), base.resolve("test"))
    }
    val shared = if modules.length == 1 then List(repo.resolve("src"), repo.resolve("test"))
    else Nil
    (repo.resolve("histos.scala") :: owned ::: shared).distinct

  def take(roots: List[Path]): IO[WatchError, Snapshot] =
    ZIO.foreach(roots)(one).map(found => Snapshot(found.flatten.sortBy(_.path)))

  private def one(root: Path): IO[WatchError, List[Entry]] =
    ZIO.attemptBlocking(entries(root)).mapError(_ => WatchError.Read(root))

  /** Wait until a snapshot differs from one taken now, and until a burst of writes has settled. */
  def changed(roots: List[Path]): IO[WatchError, List[Path]] =
    take(roots).flatMap(since(roots, _))

  /** Wait until a snapshot differs from `start`, and until a burst of writes has settled. The paths are the ones that
    * differ from `start`.
    */
  def since(roots: List[Path], start: Snapshot): IO[WatchError, List[Path]] =
    def poll(seen: Snapshot): IO[WatchError, Snapshot] =
      ZIO.sleep(Poll) *> take(roots).flatMap { now =>
        if now == seen then poll(seen) else settle(now)
      }
    def settle(seen: Snapshot): IO[WatchError, Snapshot] =
      ZIO.sleep(Settle) *> take(roots).flatMap { now =>
        if now == seen then ZIO.succeed(now) else settle(now)
      }
    poll(start).map(done => woke(start, done))

  private def woke(before: Snapshot, after: Snapshot): List[Path] =
    val previous = before.entries.map(entry => entry.path -> entry).toMap
    val next     = after.entries.map(entry => entry.path -> entry).toMap
    (previous.keySet ++ next.keySet).toList.sorted.flatMap { path =>
      (previous.get(path), next.get(path)) match
        case (Some(left), Some(right)) if left.modified == right.modified && left.bytes == right.bytes =>
          None
        case _ => Some(Path.of(path))
    }

  private def entries(root: Path): List[Entry] =
    if !Files.exists(root) then Nil
    else if Files.isRegularFile(root) then List(entry(root)).flatten
    else
      val walk = Files.walk(root)
      try walk.iterator.asScala.flatMap(entry).toList
      finally walk.close()

  private def entry(path: Path): Option[Entry] =
    if !Files.isRegularFile(path) then None
    else
      try
        val attributes =
          Files.readAttributes(path, classOf[BasicFileAttributes])
        Some(
          Entry(
            path.toAbsolutePath.normalize.toString,
            attributes.lastModifiedTime.toMillis,
            attributes.size,
          )
        )
      catch case NonFatal(_) => None
end SourceWatch

enum WatchError:
  case Read(path: Path)

  def message: String = this match
    case Read(path) => s"could not read sources under $path"
end WatchError
