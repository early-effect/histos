package histos.report

import histos.clean.Products
import histos.workspace.Workspace

import java.nio.file.Path

import zio.json.*
import zio.{IO, ZIO}

import WireJson.given

/** `.histos/observation.json`. One file, replaced, not a log. */
object Stored:
  def read(repo: Path): IO[ObserveError, Observation] =
    val path  = Products.observation(repo)
    val shown = path.toString
    Workspace
      .readIfPresent(path)
      .mapError(_ => ObserveError.Unreadable(shown))
      .flatMap:
        case None       => ZIO.fail(ObserveError.Missing(shown))
        case Some(text) =>
          text.fromJson[Observation] match
            case Right(found) => ZIO.succeed(found)
            case Left(_)      => ZIO.fail(ObserveError.Malformed(shown))

  def keep(
      repo: Path,
      observation: Option[Observation],
  ): IO[histos.workspace.IoFailure, Unit] =
    val path = Products.observation(repo)
    observation match
      case None        => Workspace.delete(path)
      case Some(found) => Workspace.writeString(path, found.toJson)
end Stored
