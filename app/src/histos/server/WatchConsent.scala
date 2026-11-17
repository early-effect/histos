package histos.server

import histos.lock.{LockReply, LockTransactionError}
import histos.plugin.{Interaction, ProposalId}

import java.nio.file.Path

import zio.{Ref, UIO, ZIO}

/** One outstanding watch proposal. Accept resumes that iteration once. A later change cancels it. */
final class WatchConsent(state: Ref[WatchConsent.Phase], resumes: Ref[Int]):
  def offer(id: ProposalId, token: String, interaction: Interaction): UIO[WatchOffer] =
    state.modify:
      case WatchConsent.Phase.Pending(current, held) if held == token && interaction == Interaction.Unattended =>
        (WatchOffer.Suppressed, WatchConsent.Phase.Pending(current, held))
      case WatchConsent.Phase.Pending(current, _) =>
        (WatchOffer.Replaced(current, id), WatchConsent.Phase.Pending(id, token))
      case WatchConsent.Phase.Empty | WatchConsent.Phase.Done(_) =>
        (WatchOffer.Fresh(id), WatchConsent.Phase.Pending(id, token))

  /** The first accept of the pending id increments. A second accept of that id does not. */
  def accept(reply: LockReply): UIO[WatchAnswer] =
    reply match
      case LockReply.Decline(id) =>
        state.modify:
          case WatchConsent.Phase.Pending(current, _) if current == id =>
            (WatchAnswer.Declined, WatchConsent.Phase.Empty)
          case other =>
            (WatchAnswer.Stale(LockTransactionError.StaleProposal(id)), other)
      case LockReply.Accept(id, _) =>
        state
          .modify:
            case WatchConsent.Phase.Pending(current, _) if current == id =>
              (WatchStep.First, WatchConsent.Phase.Done(id))
            case WatchConsent.Phase.Done(current) if current == id =>
              (WatchStep.Again, WatchConsent.Phase.Done(current))
            case other =>
              (WatchStep.Missed, other)
          .flatMap:
            case WatchStep.First  => resumes.update(_ + 1).as(WatchAnswer.Resumed)
            case WatchStep.Again  => ZIO.succeed(WatchAnswer.Repeated)
            case WatchStep.Missed => ZIO.succeed(WatchAnswer.Stale(LockTransactionError.StaleProposal(id)))

  def invalidate: UIO[Option[ProposalId]] =
    state.modify:
      case WatchConsent.Phase.Pending(id, _) => (Some(id), WatchConsent.Phase.Empty)
      case other                             => (None, other)

  def resumeCount: UIO[Int] = resumes.get
end WatchConsent

object WatchConsent:
  enum Phase:
    case Empty
    case Pending(id: ProposalId, token: String)
    case Done(id: ProposalId)

  def make: UIO[WatchConsent] =
    for
      state   <- Ref.make[Phase](Phase.Empty)
      resumes <- Ref.make(0)
    yield WatchConsent(state, resumes)

  /** A non-empty source change cancels the proposal before the next iteration. An empty wake does not. */
  def onChange(consent: WatchConsent, woke: List[Path]): UIO[Unit] =
    if woke.isEmpty then ZIO.unit else consent.invalidate.unit
end WatchConsent

private enum WatchStep:
  case First
  case Again
  case Missed

enum WatchOffer:
  case Fresh(id: ProposalId)
  case Suppressed
  case Replaced(cancelled: ProposalId, id: ProposalId)

enum WatchAnswer:
  case Resumed
  case Repeated
  case Declined
  case Stale(error: LockTransactionError)
