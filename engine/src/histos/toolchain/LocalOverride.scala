package histos.toolchain

import histos.workspace.IoFailure

import java.nio.file.Path

import scala.util.Try

import zio.{IO, ZIO}

enum LocalOverrideError:
  case Empty, Multiple
  case Relative(raw: String)
  case Invalid(raw: String)

object LocalOverride:
  def parse(text: String): Either[LocalOverrideError, Path] =
    text.linesIterator.map(_.strip).filter(_.nonEmpty).toList match
      case Nil        => Left(LocalOverrideError.Empty)
      case raw :: Nil =>
        Try(Path.of(raw)).toEither.left
          .map(_ => LocalOverrideError.Invalid(raw))
          .flatMap: path =>
            Either.cond(
              path.isAbsolute,
              path,
              LocalOverrideError.Relative(raw),
            )
      case _ => Left(LocalOverrideError.Multiple)

  def read(path: Path): IO[IoFailure | LocalOverrideError, Option[Path]] =
    RuntimeFiles
      .readIfPresent(path)
      .flatMap(text => ZIO.foreach(text)(value => ZIO.fromEither(parse(value))))
end LocalOverride
