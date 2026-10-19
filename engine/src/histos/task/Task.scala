package histos.task

import histos.ProjectGraph

import zio.*

enum TaskKind:
  case Inspect
  case Compile
  case Versions
  case Plugin

opaque type TaskId = String

object TaskId:
  val Inspect: TaskId = "inspect"
  val Versions: TaskId = "versions"

  given CanEqual[TaskId, TaskId] = CanEqual.derived

  def from(raw: String): Either[TaskError, TaskId] =
    if raw.isEmpty then Left(TaskError.EmptyId) else Right(raw)

  extension (id: TaskId) def value: String = id

opaque type RunId = String

object RunId:
  given CanEqual[RunId, RunId] = CanEqual.derived

  def from(raw: String): Either[TaskError, RunId] =
    if raw.isEmpty then Left(TaskError.EmptyId) else Right(raw)

  /** Built from `Random.nextUUID`. Not a parser. */
  private[task] def trusted(raw: String): RunId = raw

  extension (id: RunId) def value: String = id

enum TaskError:
  case EmptyId
  case NoRunner(id: TaskId)
  case UnknownRun(id: RunId)
  case Version(error: histos.version.VersionError)
  case Plugin(error: histos.plugin.PluginRunError)

  def message: String = this match
    case EmptyId        => "a task id is not empty"
    case NoRunner(id)   => s"no runner for '${id.value}'"
    case UnknownRun(id) => s"no run '${id.value}'"
    case Version(error) => error.message
    case Plugin(error)  => error.message

final case class TaskResult(kind: TaskKind, text: String)

/** A started run. `await` finishes with the result, or dies if the run is cancelled. */
final case class Run(id: RunId, await: IO[TaskError, TaskResult])

trait Tasks:
  def run(id: TaskId): ZIO[Scope, TaskError, Run]
  def cancel(id: RunId): IO[TaskError, Unit]

object Tasks:
  def live(graph: ProjectGraph): ULayer[Tasks] =
    live(graph, Map.empty)

  def live(graph: ProjectGraph, extra: Map[TaskId, IO[TaskError, TaskResult]]): ULayer[Tasks] =
    ZLayer.scoped:
      for
        registry <- Ref.make(Map.empty[RunId, Fiber[TaskError, TaskResult]])
        _        <- ZIO.addFinalizer(registry.set(Map.empty))
      yield TasksLive(inspect(graph) ++ extra, registry)

  private def inspect(graph: ProjectGraph): Map[TaskId, IO[TaskError, TaskResult]] =
    Map(TaskId.Inspect -> ZIO.succeed(TaskResult(TaskKind.Inspect, graph.projects.map(_.name.value).mkString(","))))

private final class TasksLive(
    runners: Map[TaskId, IO[TaskError, TaskResult]],
    registry: Ref[Map[RunId, Fiber[TaskError, TaskResult]]],
) extends Tasks:
  def run(id: TaskId): ZIO[Scope, TaskError, Run] =
    runners.get(id) match
      case None => ZIO.fail(TaskError.NoRunner(id))
      case Some(effect) =>
        for
          uuid  <- Random.nextUUID
          runId  = RunId.trusted(uuid.toString)
          fiber <- effect.forkScoped
          _     <- registry.update(_ + (runId -> fiber))
          _     <- ZIO.addFinalizer(registry.update(_ - runId))
        yield Run(runId, fiber.join)

  def cancel(id: RunId): IO[TaskError, Unit] =
    registry.modify(runs => (runs.get(id), runs - id)).flatMap:
      case Some(fiber) => fiber.interrupt.unit
      case None        => ZIO.fail(TaskError.UnknownRun(id))
