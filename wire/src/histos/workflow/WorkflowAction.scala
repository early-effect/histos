package histos.workflow

/** What `histos workflow` does. The command token is `check` or `update`. */
enum WorkflowAction:
  case Check, Update

  def token: String = this match
    case Check  => "check"
    case Update => "update"
end WorkflowAction

object WorkflowAction:
  private val known: List[WorkflowAction] = List(Check, Update)

  def from(raw: String): Either[WorkflowActionError, WorkflowAction] =
    known.find(_.token == raw).toRight(WorkflowActionError.Unknown(raw))
end WorkflowAction

enum WorkflowActionError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a workflow action"
end WorkflowActionError
