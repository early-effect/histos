package histos.workspace

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, Scope, ZIO}

/** Why a filesystem call failed. The path stays on `IoFailure`. */
enum IoFault:
  case Read, Write, Stat, Delete, Create, Walk, Temp

  def word: String = this match
    case Read   => "read"
    case Write  => "write"
    case Stat   => "stat"
    case Delete => "delete"
    case Create => "create"
    case Walk   => "walk"
    case Temp   => "create"
end IoFault

final case class IoFailure(path: Path, fault: IoFault):
  def message: String = s"could not ${fault.word} $path"

/** Files the build owns: the dependency lock text, a tree delete, and a scoped work directory.
  */
object Workspace:
  def readString(path: Path): IO[IoFailure, String] =
    ZIO
      .attemptBlocking(Files.readString(path))
      .mapError(_ => IoFailure(path, IoFault.Read))

  def writeString(path: Path, text: String): IO[IoFailure, Unit] =
    ZIO
      .attemptBlocking:
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, text)
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit

  def recreate(path: Path): IO[IoFailure, Path] =
    delete(path) *> createDirectories(path).as(path)

  /** `None` when `path` is not a regular file. A failed read is `IoFault.Read`.
    */
  def readIfPresent(path: Path): IO[IoFailure, Option[String]] =
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(path) then Some(Files.readString(path)) else None
      }
      .mapError(_ => IoFailure(path, IoFault.Read))

  def createDirectories(path: Path): IO[IoFailure, Path] =
    ZIO
      .attemptBlocking(Files.createDirectories(path))
      .mapError(_ => IoFailure(path, IoFault.Create))

  def delete(root: Path): IO[IoFailure, Unit] =
    ZIO
      .attemptBlocking(deleteSync(root))
      .mapError(_ => IoFailure(root, IoFault.Delete))

  /** The same walk `delete` runs. A caller already inside one blocking effect uses this.
    */
  def deleteSync(root: Path): Unit =
    if Files.exists(root) then
      val walk  = Files.walk(root)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths
        .sortBy(_.getNameCount)
        .reverse
        .foreach(path => Files.deleteIfExists(path))

  /** Removed when the scope closes, including on interrupt. */
  def work(prefix: String): ZIO[Scope, IoFailure, Path] =
    ZIO.acquireRelease(
      ZIO
        .attemptBlocking(Files.createTempDirectory(prefix))
        .mapError(_ => IoFailure(Path.of(prefix), IoFault.Temp))
    )(path => delete(path).ignore)
end Workspace
