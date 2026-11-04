package histos.task

import histos.ProjectGraph
import histos.plugin.PluginOutput
import histos.version.Decision

import zio.*

enum TaskResult:
  case Inspect(projects: List[histos.ProjectName])
  case Versions(decision: Decision)
  case Plugin(output: PluginOutput)
  case Doc

  def render: String = this match
    case Inspect(projects)  => projects.map(_.value).mkString(",")
    case Versions(decision) => Decision.text(decision)
    case Plugin(output)     => output.text
    case Doc                => "scaladoc"
end TaskResult

/** A started run. `await` is interrupted when the run is cancelled. */
final case class Run(id: RunId, await: IO[TaskError, TaskResult])

trait Tasks:
  def run(id: TaskId): ZIO[Scope, TaskError, Run]
  def cancel(id: RunId): IO[TaskError, Unit]

object Tasks:
  def live(graph: ProjectGraph): ULayer[Tasks] =
    live(graph, Map.empty)

  def live(
      graph: ProjectGraph,
      extra: Map[TaskId, IO[TaskError, TaskResult]],
  ): ULayer[Tasks] =
    ZLayer.scoped:
      for
        registry <- Ref.make(Map.empty[RunId, Fiber[TaskError, TaskResult]])
        _        <- ZIO.addFinalizer(
          registry.get.flatMap(fibers => ZIO.foreachDiscard(fibers.values)(_.interrupt))
        )
      yield TasksLive(inspect(graph) ++ extra, registry)

  private def inspect(
      graph: ProjectGraph
  ): Map[TaskId, IO[TaskError, TaskResult]] =
    Map(
      TaskId.Inspect -> ZIO.succeed(
        TaskResult.Inspect(graph.projects.map(_.name))
      )
    )
end Tasks

private final class TasksLive(
    runners: Map[TaskId, IO[TaskError, TaskResult]],
    registry: Ref[Map[RunId, Fiber[TaskError, TaskResult]]],
) extends Tasks:
  def run(id: TaskId): ZIO[Scope, TaskError, Run] =
    runners.get(id) match
      case None         => ZIO.fail(TaskError.NoRunner(id))
      case Some(effect) =>
        for
          uuid <- Random.nextUUID
          runId = RunId.trusted(uuid.toString)
          fiber <- effect.forkScoped
          _     <- registry.update(_ + (runId -> fiber))
          _     <- ZIO.addFinalizer(registry.update(_ - runId))
        yield Run(runId, fiber.join)

  def cancel(id: RunId): IO[TaskError, Unit] =
    registry
      .modify(runs => (runs.get(id), runs - id))
      .flatMap:
        case Some(fiber) => fiber.interrupt.unit
        case None        => ZIO.fail(TaskError.UnknownRun(id))
end TasksLive
