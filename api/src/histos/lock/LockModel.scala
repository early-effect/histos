package histos.lock

import histos.plugin.{
  ClientId,
  CommandIdentity,
  ContentHash,
  InvocationContext,
  InvocationId,
  LockRevision,
  PluginDetail,
  PluginEvents,
  ProposalId,
  ResolutionIntent,
}

/** Where this invocation runs. A strategy reads it from the call, not from a constructor. */
enum ExecutionKind:
  case Local
  case Ci

/** Built-in lock disposition. An invocation override uses the same cases. */
enum LockMode:
  case Offer
  case Update
  case Local
  case Frozen

/** Core-safe defaults. Custom policy uses its own typed configuration. */
final case class LockDefaults(local: LockMode, ci: LockMode)

object LockDefaults:
  val core: LockDefaults = LockDefaults(local = LockMode.Offer, ci = LockMode.Frozen)

enum LockPhase:
  case Seed
  case Plugins
  case Graph

enum LockStage:
  case Discovery
  case Acceptance

/** No lock file is [[Absent]]. That is a revision, not a missing read. */
enum LockRevisionState:
  case Absent
  case Present(revision: LockRevision)

enum DeclarationDrift:
  case Same
  case Changed(declarations: ContentHash)

final case class NecessaryScope(summary: PluginDetail)

final case class ProposalSummary(id: ProposalId, base: LockRevisionState, candidate: ContentHash)

/** Facts a strategy may read. It does not commit the lock or own the generation. */
final case class LockContext(
    phase: LockPhase,
    stage: LockStage,
    execution: ExecutionKind,
    intent: ResolutionIntent,
    revision: LockRevisionState,
    drift: DeclarationDrift,
    scope: NecessaryScope,
    proposal: Option[ProposalSummary],
    invocation: InvocationContext,
    events: PluginEvents.Invocation,
)

enum LockDecision:
  case UsePinned
  case OfferRepair
  case Update
  case LocalSolve
  case Frozen
  case Reject(refusal: LockRefusal)

enum LockRefusal:
  case FrozenDrift
  case Policy(detail: PluginDetail)

enum LockStrategyError:
  case Configuration(detail: PluginDetail)
  case Adapter(detail: PluginDetail)
  case Policy(detail: PluginDetail)

  def message: String = this match
    case Configuration(detail) => detail.value
    case Adapter(detail)       => detail.value
    case Policy(detail)        => detail.value
end LockStrategyError

/** One consent for the whole necessary diff. */
final case class LockProposal(
    id: ProposalId,
    invocation: InvocationId,
    client: ClientId,
    command: CommandIdentity,
    base: LockRevisionState,
    candidate: ContentHash,
    scope: NecessaryScope,
)
