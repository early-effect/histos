package histos.cache

/** What `histos cache clean` does. The command token is `remove` or `dry-run`. */
enum CacheCleanMode:
  case Remove, DryRun

  def token: String = this match
    case Remove => "remove"
    case DryRun => "dry-run"
end CacheCleanMode

object CacheCleanMode:
  private val known: List[CacheCleanMode] = List(Remove, DryRun)

  def from(raw: String): Either[CacheCleanModeError, CacheCleanMode] =
    known.find(_.token == raw).toRight(CacheCleanModeError.Unknown(raw))
end CacheCleanMode

enum CacheCleanModeError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a cache clean mode"
end CacheCleanModeError
