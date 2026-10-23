package histos.cache

import histos.workspace.Workspace

import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}

import zio.{IO, Scope, UIO, ZIO}

/** What a move into place found. */
enum Placed:
  case Moved
  case Occupied

/** A file or directory beside its destination, so a move into place stays on
  * one file system. The scope removes whatever was not moved: a failure, an
  * interrupt, or a lost race.
  */
object Scratch:
  def file[E](beside: Path, failed: E): ZIO[Scope, E, Path] =
    made(beside, failed)((parent, name) =>
      Files.createTempFile(parent, name, ".partial")
    )

  def directory[E](beside: Path, failed: E): ZIO[Scope, E, Path] =
    made(beside, failed)((parent, name) =>
      Files.createTempDirectory(parent, name)
    )

  /** `Occupied` means something was already at `to`: `from` stays where it is,
    * for its scope to remove.
    */
  def place[E](from: Path, to: Path, failed: E): IO[E, Placed] =
    ZIO
      .attemptBlocking(Files.move(from, to, StandardCopyOption.ATOMIC_MOVE))
      .foldZIO(
        _ =>
          ZIO
            .attemptBlocking(Files.exists(to, LinkOption.NOFOLLOW_LINKS))
            .orElseFail(failed)
            .flatMap: present =>
              if present then ZIO.succeed(Placed.Occupied)
              else ZIO.fail(failed),
        _ => ZIO.succeed(Placed.Moved)
      )

  private def made[E](beside: Path, failed: E)(
      create: (Path, String) => Path
  ): ZIO[Scope, E, Path] =
    (Option(beside.getParent), Option(beside.getFileName)) match
      case (Some(parent), Some(name)) =>
        ZIO.acquireRelease(
          ZIO
            .attemptBlocking {
              Files.createDirectories(parent)
              create(parent, s".${name.toString}-")
            }
            .orElseFail(failed)
        )(remove)
      case _ => ZIO.fail(failed)

  private def remove(path: Path): UIO[Unit] =
    Workspace.delete(path).ignore
