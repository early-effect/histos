package histos.lock

import histos.plugin.{ClientId, CommandIdentity, ContentHash, InvocationContext, InvocationId, LockRevision, ProposalId}
import histos.server.Call

/** The call and the caller, as data. The running socket still accepts a bare [[Call]]. */
final case class InvocationEnvelope(context: InvocationContext, call: Call)

/** One complete repair, offered once. Consent is not stdin and not the server environment. */
final case class LockOffer(
    proposal: ProposalId,
    caller: ClientId,
    base: LockRevisionState,
    declarations: ContentHash,
    candidate: ContentHash,
    scope: NecessaryScope,
    command: CommandIdentity,
)

enum LockReply:
  /** `base` is the revision the caller consented to. An absent lock is that revision. */
  case Accept(proposal: ProposalId, base: LockRevisionState)
  case Decline(proposal: ProposalId)

final case class LockCommitted(revision: LockRevision, invocation: InvocationId, client: ClientId)

/** Frozen or rejected before a solve. No lock write is implied. */
final case class LockBlocked(refusal: LockRefusal, scope: NecessaryScope)

/** A lock transaction failed. This is not a resolver miss and not a checksum rewrite. */
enum LockTransactionError:
  case StaleProposal(id: ProposalId)
  case RefreshRequired
  case ConsentRequired(id: ProposalId)
  case AtomicUnavailable

  def message: String = this match
    case StaleProposal(_)   => "the lock proposal does not match the current revision"
    case RefreshRequired    => "the repair needs an explicit snapshot refresh"
    case ConsentRequired(_) => "the lock proposal is waiting for consent"
    case AtomicUnavailable  => "the lock file cannot be replaced atomically"
end LockTransactionError

/** Lock facts for the event channel. P8 publishes them on the live build feed. */
enum LockEvent:
  case Offered(offer: LockOffer)
  case Replied(reply: LockReply)
  case Committed(committed: LockCommitted)
  case Blocked(blocked: LockBlocked)
