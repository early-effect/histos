package histos.process

import java.nio.file.Path

import zio.{IO, ZLayer}
import zio.stream.ZStream

/** The child a run starts. The live layer spawns. */
trait Process:
  def events(request: ChildRequest): ZStream[Any, ChildError, ChildEvent]

  def command(
      argv: List[String],
      work: Path,
      environment: Map[String, String],
  ): ZStream[Any, ChildError, ChildEvent]

  def run(request: ChildRequest): IO[ChildError, ChildOutcome]
end Process

object Process:
  val live: ZLayer[Any, Nothing, Process] =
    ZLayer.succeed(ChildProcess)
