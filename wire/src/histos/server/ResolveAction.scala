package histos.server

/** What `histos resolve` does. The command token is `verify`, `update`, or `check`. */
enum ResolveAction:
  case Verify, Update, Check

  def token: String = this match
    case Verify => "verify"
    case Update => "update"
    case Check  => "check"
end ResolveAction

object ResolveAction:
  private val known: List[ResolveAction] = List(Verify, Update, Check)

  def from(raw: String): Either[ResolveActionError, ResolveAction] =
    known.find(_.token == raw).toRight(ResolveActionError.Unknown(raw))
end ResolveAction

enum ResolveActionError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a resolve action"
end ResolveActionError
