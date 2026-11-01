package histos.server

/** What `histos launcher` writes. `update` freezes the channel. `latest` follows it. */
enum LauncherAction:
  case Update, Latest

  def token: String = this match
    case Update => "update"
    case Latest => "latest"
end LauncherAction

object LauncherAction:
  private val known: List[LauncherAction] = List(Update, Latest)

  def from(raw: String): Either[LauncherActionError, LauncherAction] =
    known.find(_.token == raw).toRight(LauncherActionError.Unknown(raw))
end LauncherAction

enum LauncherActionError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a launcher action"
end LauncherActionError
