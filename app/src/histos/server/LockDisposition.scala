package histos.server

import histos.lock.{LockMode, LockPrecedence, LockTransactionError}
import histos.resolve.ResolveError
import histos.ProjectName

/** Which explicit `--lock` mode this call actually applies. Anything else fails before a write. */
object LockDisposition:
  /** `None` means the call may run. `Some` is a mode this call would ignore. */
  def unapplied(call: Call, lock: Option[LockMode]): Option[LockMode] =
    lock match
      case None       => None
      case Some(mode) =>
        call match
          case Call.Resolve(ResolveAction.Update) if mode == LockMode.Update => None
          case Call.Resolve(ResolveAction.Refresh(_))                        =>
            mode match
              case LockMode.Offer | LockMode.Update => None
              case LockMode.Frozen | LockMode.Local => Some(mode)
          case Call.Watch(inner, _) => unapplied(inner, Some(mode))
          case _                    => Some(mode)

  def refused(mode: LockMode): ResolveError =
    ResolveError.Transaction(LockTransactionError.Unapplied(mode))

  /** The refresh request was recorded. No snapshot was revalidated and the lock was not written. */
  def refreshText(selectors: List[ProjectName], lock: Option[LockMode]): String =
    val listed = selectors match
      case Nil   => "all"
      case names => names.map(_.value).mkString(",")
    val disposition = lock match
      case Some(mode) => LockPrecedence.token(mode)
      case None       => "absent"
    s"snapshot refresh was not applied\ndisposition $disposition\nselectors $listed\nNo lock write."
end LockDisposition
