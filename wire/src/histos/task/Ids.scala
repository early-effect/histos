package histos.task

import histos.doctor.{Diagnostic, Noted}
import histos.plugin.PluginRunError
import histos.version.VersionError

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

import zio.NonEmptyChunk

opaque type TaskId = String

object TaskId:
  val Inspect: TaskId  = "inspect"
  val Versions: TaskId = "versions"
  val Doc: TaskId      = "doc"

  given CanEqual[TaskId, TaskId] = CanEqual.derived

  def from(raw: String): Either[TaskError, TaskId] =
    if raw.isEmpty then Left(TaskError.EmptyId) else Right(raw)

  extension (id: TaskId)
    def value: String = id

    /** `inspect`, `versions`, and `doc` are tasks. A platform after the name is a module main. */
    def builtIn: Boolean =
      id == Inspect || id == Versions || id == Doc
  end extension
end TaskId

opaque type RunId = String

object RunId:
  given CanEqual[RunId, RunId] = CanEqual.derived

  inline def apply(inline raw: String): RunId = ${
    RunIdMacro.literal('raw)
  }

  def from(raw: String): Either[TaskError, RunId] =
    if raw.isEmpty then Left(TaskError.EmptyId) else Right(raw)

  /** Built from `Random.nextUUID`, or from a literal the macro checked. Not a parser. */
  @publicInBinary private[histos] def trusted(raw: String): RunId = raw

  extension (id: RunId) def value: String = id
end RunId

private object RunIdMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[RunId] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "RunId(...) takes a literal. Use RunId.from at runtime.",
          raw,
        )
      case Some(value) =>
        RunId.from(value) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ RunId.trusted(${ Expr(value) }) }

enum TaskError:
  case EmptyId
  case NoRunner(id: TaskId)
  case UnknownRun(id: RunId)
  case Version(error: VersionError)
  case Plugin(error: PluginRunError)
  case Doc(lines: NonEmptyChunk[String])

  def message: String = this match
    case EmptyId        => "a task id is not empty"
    case NoRunner(id)   => s"no runner for '${id.value}'"
    case UnknownRun(id) => s"no run '${id.value}'"
    case Version(error) => error.message
    case Plugin(error)  => error.message
    case Doc(lines)     => lines.mkString("\n")

  def diagnose: Diagnostic = this match
    case UnknownRun(id) =>
      Noted.carry("run.unknown", message, s"No live run is named ${id.value}.", "histos help cancel")
    case other =>
      Noted.carry("task.failed", other.message, other.message, "histos help run")
end TaskError
