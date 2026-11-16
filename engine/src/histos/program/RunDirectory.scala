package histos.program

import java.nio.file.{Files, NoSuchFileException, NotDirectoryException, Path}

import zio.{IO, ZIO}

/** A tool directory must already exist, be a directory, and stay inside the repo after symlinks resolve. */
object RunDirectory:
  def accept(repo: Path, asked: Path): IO[RunDirectoryFault, Path] =
    ZIO.attemptBlocking(inside(repo, asked)).orDie.flatMap(ZIO.fromEither)

  /** A shell directory may sit outside the repo. It still has to exist and be a directory. */
  def existing(asked: Path): IO[RunDirectoryFault, Path] =
    ZIO.attemptBlocking(present(asked)).orDie.flatMap(ZIO.fromEither)

  private def inside(repo: Path, asked: Path): Either[RunDirectoryFault, Path] =
    val candidate = if asked.isAbsolute then asked else repo.resolve(asked)
    present(candidate).flatMap: real =>
      try
        val root = repo.toRealPath()
        if real.startsWith(root) then Right(real)
        else Left(RunDirectoryFault.Outside(asked))
      catch
        case _: NoSuchFileException   => Left(RunDirectoryFault.Missing(repo))
        case _: NotDirectoryException => Left(RunDirectoryFault.NotDirectory(repo))
        case _: java.io.IOException   => Left(RunDirectoryFault.Unreadable(repo))

  private def present(asked: Path): Either[RunDirectoryFault, Path] =
    if !Files.exists(asked) then Left(RunDirectoryFault.Missing(asked))
    else
      try
        val real = asked.toRealPath()
        if Files.isDirectory(real) then Right(real)
        else Left(RunDirectoryFault.NotDirectory(asked))
      catch
        case _: NoSuchFileException   => Left(RunDirectoryFault.Missing(asked))
        case _: NotDirectoryException => Left(RunDirectoryFault.NotDirectory(asked))
        case _: java.io.IOException   => Left(RunDirectoryFault.Unreadable(asked))
end RunDirectory
