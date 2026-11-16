package histos.resolve

import histos.lock.{DeclarationDrift, LockDecision, LockMode, LockPhase, LockRefusal}

/** One phase's disposition. Aggregation sees only the phases that need repair. */
final case class PhaseDecision(phase: LockPhase, decision: LockDecision, drift: DeclarationDrift)

/** The single repair this invocation is allowed to perform. */
enum RepairAggregate:
  case Pinned
  case Blocked(refusal: LockRefusal)
  case Ephemeral
  case Consent
  case AutoUpdate

/** Where one invocation is in the repair machine. Payloads live beside this phase. */
enum RepairPhase:
  case Captured
  case Discovering
  case CandidateReady
  case Offered
  case AutoAccepted
  case LocalReady
  case FrozenValidated
  case Pinned
  case Committing
  case Adopted
  case Executing
  case Completed
  case Blocked
  case Declined
  case Discarded

enum RepairStep:
  case BeginDiscovery
  case Candidate
  case Offer
  case AutoAccept
  case UseLocal
  case ValidateFrozen
  case Keep
  case Block
  case Decline
  case BeginCommit
  case Renamed
  case BeginExecute
  case Finish
  case Discard

/** Necessary-closure aggregation and the legal repair transitions. */
object RepairScope:
  def builtin(mode: LockMode, drift: DeclarationDrift): LockDecision =
    mode match
      case LockMode.Offer =>
        drift match
          case DeclarationDrift.Same       => LockDecision.UsePinned
          case DeclarationDrift.Changed(_) => LockDecision.OfferRepair
      case LockMode.Update => LockDecision.Update
      case LockMode.Local  => LockDecision.LocalSolve
      case LockMode.Frozen => LockDecision.Frozen

  /** Reject and frozen drift block. One local solve is ephemeral. One offer is one consent. Update needs every repaired
    * phase.
    */
  def aggregate(phases: List[PhaseDecision]): RepairAggregate =
    val repairing = phases.filter(requiresRepair)
    val rejection =
      repairing.collectFirst:
        case PhaseDecision(_, LockDecision.Reject(refusal), _) => refusal
    val frozen  = repairing.exists(_.decision == LockDecision.Frozen)
    val local   = repairing.exists(_.decision == LockDecision.LocalSolve)
    val offer   = repairing.exists(_.decision == LockDecision.OfferRepair)
    val updates = repairing.forall(_.decision == LockDecision.Update)
    rejection match
      case Some(refusal)                         => RepairAggregate.Blocked(refusal)
      case None if frozen                        => RepairAggregate.Blocked(LockRefusal.FrozenDrift)
      case None if local                         => RepairAggregate.Ephemeral
      case None if offer                         => RepairAggregate.Consent
      case None if repairing.nonEmpty && updates => RepairAggregate.AutoUpdate
      case None                                  => RepairAggregate.Pinned

  def advance(state: RepairPhase, step: RepairStep): Option[RepairPhase] =
    state match
      case RepairPhase.Captured        => captured(step)
      case RepairPhase.Discovering     => discovering(step)
      case RepairPhase.CandidateReady  => candidate(step)
      case RepairPhase.Offered         => offered(step)
      case RepairPhase.AutoAccepted    => accepted(step)
      case RepairPhase.LocalReady      => ready(step)
      case RepairPhase.FrozenValidated => ready(step)
      case RepairPhase.Pinned          => ready(step)
      case RepairPhase.Committing      => committing(step)
      case RepairPhase.Adopted         => adopted(step)
      case RepairPhase.Executing       => executing(step)
      case RepairPhase.Completed       => deny(step)
      case RepairPhase.Blocked         => deny(step)
      case RepairPhase.Declined        => deny(step)
      case RepairPhase.Discarded       => deny(step)

  private def requiresRepair(phase: PhaseDecision): Boolean =
    phase.decision match
      case LockDecision.UsePinned => false
      case LockDecision.Frozen    =>
        phase.drift match
          case DeclarationDrift.Same       => false
          case DeclarationDrift.Changed(_) => true
      case LockDecision.Reject(_)   => true
      case LockDecision.OfferRepair => true
      case LockDecision.Update      => true
      case LockDecision.LocalSolve  => true

  private def captured(step: RepairStep): Option[RepairPhase] =
    step match
      case RepairStep.BeginDiscovery => Some(RepairPhase.Discovering)
      case RepairStep.Candidate | RepairStep.Offer | RepairStep.AutoAccept | RepairStep.UseLocal |
          RepairStep.ValidateFrozen | RepairStep.Keep | RepairStep.Block | RepairStep.Decline | RepairStep.BeginCommit |
          RepairStep.Renamed | RepairStep.BeginExecute | RepairStep.Finish | RepairStep.Discard =>
        None

  private def discovering(step: RepairStep): Option[RepairPhase] =
    step match
      case RepairStep.Candidate      => Some(RepairPhase.CandidateReady)
      case RepairStep.ValidateFrozen => Some(RepairPhase.FrozenValidated)
      case RepairStep.Keep           => Some(RepairPhase.Pinned)
      case RepairStep.Block          => Some(RepairPhase.Blocked)
      case RepairStep.Discard        => Some(RepairPhase.Discarded)
      case RepairStep.BeginDiscovery | RepairStep.Offer | RepairStep.AutoAccept | RepairStep.UseLocal |
          RepairStep.Decline | RepairStep.BeginCommit | RepairStep.Renamed | RepairStep.BeginExecute |
          RepairStep.Finish =>
        None

  private def candidate(step: RepairStep): Option[RepairPhase] =
    step match
      case RepairStep.Offer      => Some(RepairPhase.Offered)
      case RepairStep.AutoAccept => Some(RepairPhase.AutoAccepted)
      case RepairStep.UseLocal   => Some(RepairPhase.LocalReady)
      case RepairStep.Keep       => Some(RepairPhase.Pinned)
      case RepairStep.Block      => Some(RepairPhase.Blocked)
      case RepairStep.Discard    => Some(RepairPhase.Discarded)
      case RepairStep.BeginDiscovery | RepairStep.Candidate | RepairStep.ValidateFrozen | RepairStep.Decline |
          RepairStep.BeginCommit | RepairStep.Renamed | RepairStep.BeginExecute | RepairStep.Finish =>
        None

  private def offered(step: RepairStep): Option[RepairPhase] =
    step match
      case RepairStep.Decline     => Some(RepairPhase.Declined)
      case RepairStep.BeginCommit => Some(RepairPhase.Committing)
      case RepairStep.BeginDiscovery | RepairStep.Candidate | RepairStep.Offer | RepairStep.AutoAccept |
          RepairStep.UseLocal | RepairStep.ValidateFrozen | RepairStep.Keep | RepairStep.Block | RepairStep.Renamed |
          RepairStep.BeginExecute | RepairStep.Finish | RepairStep.Discard =>
        None

  private def accepted(step: RepairStep): Option[RepairPhase] =
    step match
      case RepairStep.BeginCommit => Some(RepairPhase.Committing)
      case RepairStep.BeginDiscovery | RepairStep.Candidate | RepairStep.Offer | RepairStep.AutoAccept |
          RepairStep.UseLocal | RepairStep.ValidateFrozen | RepairStep.Keep | RepairStep.Block | RepairStep.Decline |
          RepairStep.Renamed | RepairStep.BeginExecute | RepairStep.Finish | RepairStep.Discard =>
        None

  private def ready(step: RepairStep): Option[RepairPhase] =
    step match
      case RepairStep.BeginExecute => Some(RepairPhase.Executing)
      case RepairStep.BeginDiscovery | RepairStep.Candidate | RepairStep.Offer | RepairStep.AutoAccept |
          RepairStep.UseLocal | RepairStep.ValidateFrozen | RepairStep.Keep | RepairStep.Block | RepairStep.Decline |
          RepairStep.BeginCommit | RepairStep.Renamed | RepairStep.Finish | RepairStep.Discard =>
        None

  private def committing(step: RepairStep): Option[RepairPhase] =
    step match
      case RepairStep.Renamed => Some(RepairPhase.Adopted)
      case RepairStep.Discard => Some(RepairPhase.Discarded)
      case RepairStep.BeginDiscovery | RepairStep.Candidate | RepairStep.Offer | RepairStep.AutoAccept |
          RepairStep.UseLocal | RepairStep.ValidateFrozen | RepairStep.Keep | RepairStep.Block | RepairStep.Decline |
          RepairStep.BeginCommit | RepairStep.BeginExecute | RepairStep.Finish =>
        None

  private def adopted(step: RepairStep): Option[RepairPhase] =
    step match
      case RepairStep.BeginExecute => Some(RepairPhase.Executing)
      case RepairStep.BeginDiscovery | RepairStep.Candidate | RepairStep.Offer | RepairStep.AutoAccept |
          RepairStep.UseLocal | RepairStep.ValidateFrozen | RepairStep.Keep | RepairStep.Block | RepairStep.Decline |
          RepairStep.BeginCommit | RepairStep.Renamed | RepairStep.Finish | RepairStep.Discard =>
        None

  private def executing(step: RepairStep): Option[RepairPhase] =
    step match
      case RepairStep.Finish => Some(RepairPhase.Completed)
      case RepairStep.BeginDiscovery | RepairStep.Candidate | RepairStep.Offer | RepairStep.AutoAccept |
          RepairStep.UseLocal | RepairStep.ValidateFrozen | RepairStep.Keep | RepairStep.Block | RepairStep.Decline |
          RepairStep.BeginCommit | RepairStep.Renamed | RepairStep.BeginExecute | RepairStep.Discard =>
        None

  private def deny(step: RepairStep): Option[RepairPhase] =
    step match
      case RepairStep.BeginDiscovery | RepairStep.Candidate | RepairStep.Offer | RepairStep.AutoAccept |
          RepairStep.UseLocal | RepairStep.ValidateFrozen | RepairStep.Keep | RepairStep.Block | RepairStep.Decline |
          RepairStep.BeginCommit | RepairStep.Renamed | RepairStep.BeginExecute | RepairStep.Finish |
          RepairStep.Discard =>
        None
end RepairScope
