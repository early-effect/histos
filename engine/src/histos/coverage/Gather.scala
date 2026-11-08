package histos.coverage

import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** One cell's inventory and the measurement files beside it. A directory with no class files and no inventory is an
  * empty report. Class files without an inventory are an error.
  */
object Gather:
  val InventoryName: String = "scoverage.coverage"

  def read(dir: Path): IO[CoverageError | IoFailure, (List[Statement], Set[Int])] =
    directory(dir).flatMap:
      case false => ZIO.succeed((Nil, Set.empty))
      case true  =>
        Workspace
          .readIfPresent(dir.resolve(InventoryName))
          .flatMap:
            case None =>
              hasClass(dir).flatMap:
                case true  => ZIO.fail(CoverageError.MissingInventory(dir))
                case false => ZIO.succeed((Nil, Set.empty))
            case Some(text) =>
              for
                rows <- ZIO.fromEither(Inventory.parse(text))
                ids  <- measurements(dir)
              yield (rows, ids)

  private def directory(dir: Path): IO[IoFailure, Boolean] =
    ZIO.attemptBlocking(Files.isDirectory(dir)).mapError(_ => IoFailure(dir, IoFault.Stat))

  private def measurements(dir: Path): IO[CoverageError | IoFailure, Set[Int]] =
    listed(dir, Measurements.Prefix).flatMap: paths =>
      ZIO.foldLeft(paths)(Set.empty[Int]): (acc, path) =>
        Workspace
          .readString(path)
          .flatMap: text =>
            ZIO.fromEither(Measurements.parse(text)).map(ids => acc ++ ids)

  private def hasClass(dir: Path): IO[IoFailure, Boolean] =
    ZIO.scoped:
      for
        stream <- ZIO.acquireRelease(
          ZIO.attemptBlocking(Files.walk(dir)).mapError(_ => IoFailure(dir, IoFault.Walk))
        )(open => ZIO.attemptBlocking(open.close()).ignore)
        found <- ZIO
          .attemptBlocking(
            stream.iterator.asScala.exists: path =>
              Files.isRegularFile(path) && fileName(path).exists(_.endsWith(".class"))
          )
          .mapError(_ => IoFailure(dir, IoFault.Walk))
      yield found

  private def listed(dir: Path, prefix: String): IO[IoFailure, List[Path]] =
    ZIO.scoped:
      for
        stream <- ZIO.acquireRelease(
          ZIO.attemptBlocking(Files.list(dir)).mapError(_ => IoFailure(dir, IoFault.Walk))
        )(open => ZIO.attemptBlocking(open.close()).ignore)
        paths <- ZIO
          .attemptBlocking(
            stream.iterator.asScala.filter(path => fileName(path).exists(_.startsWith(prefix))).toList
          )
          .mapError(_ => IoFailure(dir, IoFault.Walk))
      yield paths

  private def fileName(path: Path): Option[String] =
    Option(path.getFileName).map(_.toString)
end Gather
