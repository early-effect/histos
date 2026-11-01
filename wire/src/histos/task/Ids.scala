package histos.task

import histos.plugin.PluginRunError
import histos.version.VersionError

opaque type TaskId = String

object TaskId:
  val Inspect: TaskId  = "inspect"
  val Versions: TaskId = "versions"

  given CanEqual[TaskId, TaskId] = CanEqual.derived

  def from(raw: String): Either[TaskError, TaskId] =
    if raw.isEmpty then Left(TaskError.EmptyId) else Right(raw)

  extension (id: TaskId) def value: String = id
end TaskId

opaque type RunId = String

object RunId:
  given CanEqual[RunId, RunId] = CanEqual.derived

  def from(raw: String): Either[TaskError, RunId] =
    if raw.isEmpty then Left(TaskError.EmptyId) else Right(raw)

  /** Built from `Random.nextUUID`. Not a parser. */
  private[histos] def trusted(raw: String): RunId = raw

  extension (id: RunId) def value: String = id
end RunId

enum TaskError:
  case EmptyId
  case NoRunner(id: TaskId)
  case UnknownRun(id: RunId)
  case Version(error: VersionError)
  case Plugin(error: PluginRunError)

  def message: String = this match
    case EmptyId        => "a task id is not empty"
    case NoRunner(id)   => s"no runner for '${id.value}'"
    case UnknownRun(id) => s"no run '${id.value}'"
    case Version(error) => error.message
    case Plugin(error)  => error.message
end TaskError
