package histos.lock

import histos.server.ResolveAction

/** What `--lock` is allowed to change. A plain command is not preparation. */
enum DispositionSubject:
  case Prepare
  case Resolve(action: ResolveAction)
  case Plain

/** Why a lock flag is not this command. The text is the usage line. */
enum PrecedenceFault:
  case NotPreparation
  case NotRefresh
  case Validation(action: ResolveAction)
  case UpdateConflict(mode: LockMode)
  case RefreshConflict(mode: LockMode)
  case RefreshCombined
  case RefreshRepeated
  case UnknownLock(raw: String)
  case RepeatedLock

  def message: String = this match
    case NotPreparation       => "--lock is only valid when the command prepares dependencies"
    case NotRefresh           => "--refresh belongs to resolve"
    case Validation(action)   => s"resolve ${action.token} does not take --lock"
    case UpdateConflict(mode) =>
      s"resolve --update does not take --lock=${LockPrecedence.token(mode)}"
    case RefreshConflict(mode) =>
      s"resolve --refresh does not take --lock=${LockPrecedence.token(mode)}"
    case RefreshCombined  => "resolve --refresh cannot be combined with --update or --check"
    case RefreshRepeated  => "resolve --refresh is already set"
    case UnknownLock(raw) =>
      if raw.isEmpty then "--lock needs offer, update, local, or frozen"
      else s"'$raw' is not a lock mode"
    case RepeatedLock => "--lock is already set"
end PrecedenceFault

/** Explicit `--lock` wins. Verify and check never take it. Update and refresh reject frozen and local. */
object LockPrecedence:
  def token(mode: LockMode): String = mode match
    case LockMode.Offer  => "offer"
    case LockMode.Update => "update"
    case LockMode.Local  => "local"
    case LockMode.Frozen => "frozen"

  def fromToken(raw: String): Either[PrecedenceFault, LockMode] =
    raw match
      case "offer"  => Right(LockMode.Offer)
      case "update" => Right(LockMode.Update)
      case "local"  => Right(LockMode.Local)
      case "frozen" => Right(LockMode.Frozen)
      case other    => Left(PrecedenceFault.UnknownLock(other))

  /** `None` is the phase default. A present mode is the explicit disposition. */
  def admit(subject: DispositionSubject, lock: Option[LockMode]): Either[PrecedenceFault, Option[LockMode]] =
    lock match
      case None       => Right(None)
      case Some(mode) =>
        subject match
          case DispositionSubject.Plain           => Left(PrecedenceFault.NotPreparation)
          case DispositionSubject.Prepare         => Right(Some(mode))
          case DispositionSubject.Resolve(action) =>
            action match
              case ResolveAction.Verify | ResolveAction.Check =>
                Left(PrecedenceFault.Validation(action))
              case ResolveAction.Update =>
                mode match
                  case LockMode.Frozen | LockMode.Local => Left(PrecedenceFault.UpdateConflict(mode))
                  case LockMode.Offer | LockMode.Update => Right(Some(mode))
              case ResolveAction.Refresh(_) =>
                mode match
                  case LockMode.Frozen | LockMode.Local => Left(PrecedenceFault.RefreshConflict(mode))
                  case LockMode.Offer | LockMode.Update => Right(Some(mode))
end LockPrecedence
