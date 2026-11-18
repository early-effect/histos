package histos.resolve

import histos.lock.{
  LockBlocked,
  LockCommitted,
  LockContext,
  LockDecision,
  LockMode,
  LockOffer,
  LockProposal,
  LockRefusal,
  LockReply,
  LockRevisionState,
  LockStage,
  LockStrategy,
  LockStrategyError,
  LockTransactionError,
  NecessaryScope,
  ProposalSummary,
}
import histos.plugin.{
  ClientId,
  CommandIdentity,
  ContentHash,
  Digests,
  GenerationId,
  InvocationContext,
  InvocationId,
  LockRevision,
  PluginDetail,
  PluginDetailError,
  PluginEvents,
  ProposalId,
  ResolutionIntent,
}

import java.nio.charset.StandardCharsets

import zio.{IO, Promise, Ref, UIO, ZIO}

/** The command suspended until the selected generation is adopted. */
trait SuspendedCommand:
  def run(snapshot: ResolutionSnapshot): IO[LockRepairError, Unit]

/** Discovers drift and, when policy allows, solves one uncommitted candidate. */
trait LockPreparer:
  def inspect(
      request: PrepareRequest,
      revision: LockRevisionState,
      document: LockDocument,
  ): IO[PrepareFault, Inspection]
  def solve(request: PrepareRequest, inspection: Inspection): IO[PrepareFault, Solved]

final case class PrepareRequest(
    context: InvocationContext,
    intent: ResolutionIntent,
    events: PluginEvents.Invocation,
    overrideMode: Option[LockMode],
    refresh: Boolean,
)

final case class PhaseInspection(phase: histos.lock.LockPhase, drift: histos.lock.DeclarationDrift)

final case class Inspection(
    declarations: ContentHash,
    phases: List[PhaseInspection],
    preview: histos.lock.LockDiff = histos.lock.LockDiff(Nil),
    refreshed: List[SnapshotLocator] = Nil,
)

enum PrepareFault:
  case Solve(detail: PluginDetail)
  case Transaction(error: LockTransactionError)

enum PrepareOutcome:
  case Offered(offer: LockOffer)
  case Committed(committed: LockCommitted, snapshot: ResolutionSnapshot)
  case Blocked(blocked: LockBlocked)
  case Selected(snapshot: ResolutionSnapshot)

final case class AcceptOutcome(committed: LockCommitted, snapshot: ResolutionSnapshot)

/** Restart after a rename. Adoption is not a rollback, and it does not run the suspended command. */
enum LockRecovery:
  case Idle
  case Adopted(revision: LockRevision, snapshot: ResolutionSnapshot)
  case Retained(revision: LockRevisionState)
  case Diverged(observed: LockRevisionState)

enum LockRepairError:
  case Transaction(error: LockTransactionError)
  case Solve(detail: PluginDetail)
  case Strategy(error: LockStrategyError)
  case Store(fault: LockStoreFault)
  case Transition(phase: RepairPhase, step: RepairStep)

  def message: String = this match
    case Transaction(error)      => error.message
    case Solve(detail)           => detail.value
    case Strategy(error)         => error.message
    case Store(fault)            => fault.toString
    case Transition(phase, step) => s"illegal lock transition from $phase on $step"
end LockRepairError

/** One invocation's repair: discover, consent or commit, then run the suspended command once. */
final class LockCoordinator private (
    preparer: LockPreparer,
    strategy: LockStrategy,
    store: LockStore,
    generations: GenerationOwner,
    commands: SuspendedCommand,
    phases: Ref[Map[InvocationId, RepairPhase]],
    opens: Ref[Map[ProposalId, OpenRepair]],
):
  def prepare(request: PrepareRequest): IO[LockRepairError, PrepareOutcome] =
    val id = request.context.id
    for
      _          <- phases.update(_.updated(id, RepairPhase.Captured))
      _          <- advance(id, RepairStep.BeginDiscovery)
      loaded     <- store.read.mapError(liftStore)
      inspection <- preparer.inspect(request, loaded.revision, loaded.document).mapError(liftPrepare)
      preview = previewScope(inspection)
      discovery <- decisions(request, loaded, inspection, LockStage.Discovery, preview, None)
      outcome   <- RepairScope.aggregate(discovery) match
        case RepairAggregate.Blocked(refusal) => block(id, refusal, blockedScope(refusal, preview))
        case RepairAggregate.Pinned           => selectCurrent(id, request, loaded, inspection, discovery)
        case RepairAggregate.Ephemeral | RepairAggregate.Consent | RepairAggregate.AutoUpdate =>
          repair(request, loaded, inspection, discovery)
    yield outcome

  def accept(reply: LockReply.Accept): IO[LockRepairError, AcceptOutcome] =
    for
      promise <- Promise.make[LockRepairError, AcceptOutcome]
      role    <- opens.modify: map =>
        map.get(reply.proposal) match
          case None       => (AcceptRole.Missing, map)
          case Some(open) =>
            open.slot match
              case RepairSlot.Settled(result)  => (AcceptRole.Settled(result), map)
              case RepairSlot.Running(waiting) => (AcceptRole.Waiting(waiting), map)
              case RepairSlot.Interrupted      => (AcceptRole.Interrupted, map)
              case RepairSlot.Idle             =>
                (AcceptRole.Lead(open), map.updated(reply.proposal, open.copy(slot = RepairSlot.Running(promise))))
      outcome <- role match
        case AcceptRole.Missing =>
          ZIO.fail(LockRepairError.Transaction(LockTransactionError.StaleProposal(reply.proposal)))
        case AcceptRole.Settled(Right(done)) => ZIO.succeed(done)
        case AcceptRole.Settled(Left(error)) => ZIO.fail(error)
        case AcceptRole.Waiting(waiting)     => waiting.await
        case AcceptRole.Interrupted          => ZIO.interrupt
        case AcceptRole.Lead(open)           => lead(open, reply, promise)
    yield outcome

  def decline(reply: LockReply.Decline): IO[LockRepairError, Unit] =
    opens
      .modify { map =>
        map.get(reply.proposal) match
          case Some(open) =>
            open.slot match
              case RepairSlot.Idle        => (Some(open), map.removed(reply.proposal))
              case RepairSlot.Running(_)  => (None, map)
              case RepairSlot.Settled(_)  => (None, map)
              case RepairSlot.Interrupted => (None, map)
          case None => (None, map)
      }
      .flatMap:
        case None       => ZIO.fail(LockRepairError.Transaction(LockTransactionError.StaleProposal(reply.proposal)))
        case Some(open) => advance(open.prepared.proposal.invocation, RepairStep.Decline).unit

  def recover: IO[LockRepairError, LockRecovery] =
    store.recover
      .mapError(liftStore)
      .flatMap:
        case StoreRecovery.Idle                      => ZIO.succeed(LockRecovery.Idle)
        case StoreRecovery.Retained(revision)        => ZIO.succeed(LockRecovery.Retained(revision))
        case StoreRecovery.Diverged(observed)        => ZIO.succeed(LockRecovery.Diverged(observed))
        case StoreRecovery.Adopted(intent, document) =>
          val snapshot = snapshotOf(
            intent.intent,
            LockRevisionState.Present(intent.next),
            intent.declarations,
            document,
            SnapshotProvenance(intent.invocation, intent.client, intent.command),
            SnapshotSelection.Committed,
            intent.generation,
          )
          generations.publish(snapshot).mapError(liftStore).as(LockRecovery.Adopted(intent.next, snapshot))

  private def repair(
      request: PrepareRequest,
      loaded: LoadedLock,
      inspection: Inspection,
      discovery: List[PhaseDecision],
  ): IO[LockRepairError, PrepareOutcome] =
    val id = request.context.id
    for
      solved <- preparer.solve(request, inspection).mapError(liftPrepare).tapError(_ => discard(id))
      merged <- ZIO
        .fromEither(LockDocument.merge(loaded.document, solved, request.refresh))
        .mapError(liftMerge)
        .tapError(_ => discard(id))
      _     <- advance(id, RepairStep.Candidate)
      built <- candidate(request, loaded, inspection, solved, merged)
      (prepared, scope) = built
      summary           = ProposalSummary(
        prepared.proposal.id,
        prepared.proposal.base,
        prepared.proposal.declarations,
        prepared.proposal.candidate,
        scope,
      )
      acceptance <- decisions(request, loaded, inspection, LockStage.Acceptance, scope, Some(summary))
      outcome    <- RepairScope.aggregate(discovery ++ acceptance) match
        case RepairAggregate.Blocked(refusal) => block(id, refusal, scope)
        case RepairAggregate.Ephemeral        => ephemeral(request, loaded, inspection, prepared)
        case RepairAggregate.Consent          => offer(prepared, scope)
        case RepairAggregate.AutoUpdate       =>
          advance(id, RepairStep.AutoAccept) *> commit(request, prepared).map: done =>
            PrepareOutcome.Committed(done.committed, done.snapshot)
        case RepairAggregate.Pinned =>
          advance(id, RepairStep.Keep) *> publishCurrent(request, loaded, inspection.declarations)
    yield outcome

  private def selectCurrent(
      id: InvocationId,
      request: PrepareRequest,
      loaded: LoadedLock,
      inspection: Inspection,
      discovery: List[PhaseDecision],
  ): IO[LockRepairError, PrepareOutcome] =
    val step =
      if discovery.exists(_.decision == LockDecision.Frozen) then RepairStep.ValidateFrozen else RepairStep.Keep
    advance(id, step) *> publishCurrent(request, loaded, inspection.declarations)

  private def publishCurrent(
      request: PrepareRequest,
      loaded: LoadedLock,
      declarations: ContentHash,
  ): IO[LockRepairError, PrepareOutcome] =
    val snapshot = snapshotOf(
      request.intent,
      loaded.revision,
      declarations,
      loaded.document,
      provenance(request),
      SnapshotSelection.Committed,
      generationOf(loaded.revision, loaded.document),
    )
    for
      _ <- generations.publish(snapshot).mapError(liftStore)
      _ <- execute(request.context.id, snapshot)
    yield PrepareOutcome.Selected(snapshot)

  private def ephemeral(
      request: PrepareRequest,
      loaded: LoadedLock,
      inspection: Inspection,
      prepared: Prepared,
  ): IO[LockRepairError, PrepareOutcome] =
    val snapshot = snapshotOf(
      request.intent,
      loaded.revision,
      inspection.declarations,
      prepared.document,
      provenance(request),
      SnapshotSelection.Ephemeral,
      prepared.generation,
    )
    for
      _ <- advance(request.context.id, RepairStep.UseLocal)
      _ <- generations.publish(snapshot).mapError(liftStore)
      _ <- execute(request.context.id, snapshot)
    yield PrepareOutcome.Selected(snapshot)

  private def offer(prepared: Prepared, scope: NecessaryScope): IO[LockRepairError, PrepareOutcome] =
    val proposal = prepared.proposal
    val offer    = LockOffer(
      proposal.id,
      proposal.client,
      proposal.base,
      proposal.declarations,
      proposal.candidate,
      scope,
      proposal.command,
    )
    for
      _ <- advance(proposal.invocation, RepairStep.Offer)
      _ <- opens.update(_.updated(proposal.id, OpenRepair(prepared, RepairSlot.Idle)))
    yield PrepareOutcome.Offered(offer)

  private def lead(
      open: OpenRepair,
      reply: LockReply.Accept,
      promise: Promise[LockRepairError, AcceptOutcome],
  ): IO[LockRepairError, AcceptOutcome] =
    val run =
      for
        _      <- quoted(open, reply)
        result <- commit(open.prepared.request, open.prepared)
      yield result
    run.onExit(exit => finish(open.prepared.proposal.id, promise, exit))

  private def commit(request: PrepareRequest, prepared: Prepared): IO[LockRepairError, AcceptOutcome] =
    val proposal = prepared.proposal
    val intent   = CommitIntent(
      proposal.id,
      proposal.base,
      LockDocument.revision(prepared.document),
      prepared.generation,
      proposal.declarations,
      proposal.invocation,
      proposal.client,
      proposal.command,
      request.intent,
      proposal.candidate,
    )
    for
      _        <- advance(proposal.invocation, RepairStep.BeginCommit)
      revision <- store
        .commit(proposal.base, prepared.document, intent)
        .mapError(liftCommit(proposal.id))
        .tapError(_ => discard(proposal.invocation))
      _ <- advance(proposal.invocation, RepairStep.Renamed)
      snapshot = snapshotOf(
        request.intent,
        LockRevisionState.Present(revision),
        proposal.declarations,
        prepared.document,
        provenance(request),
        SnapshotSelection.Committed,
        prepared.generation,
      )
      _ <- generations.publish(snapshot).mapError(liftStore)
      _ <- execute(proposal.invocation, snapshot)
      _ <- store.forget.mapError(liftStore)
    yield AcceptOutcome(LockCommitted(revision, proposal.invocation, proposal.client), snapshot)

  private def execute(id: InvocationId, snapshot: ResolutionSnapshot): IO[LockRepairError, Unit] =
    for
      _ <- advance(id, RepairStep.BeginExecute)
      _ <- commands.run(snapshot)
      _ <- advance(id, RepairStep.Finish)
    yield ()

  private def block(
      id: InvocationId,
      refusal: LockRefusal,
      scope: NecessaryScope,
  ): IO[LockRepairError, PrepareOutcome] =
    advance(id, RepairStep.Block).as(PrepareOutcome.Blocked(LockBlocked(refusal, scope)))

  private def candidate(
      request: PrepareRequest,
      loaded: LoadedLock,
      inspection: Inspection,
      solved: Solved,
      merged: LockDocument,
  ): IO[LockRepairError, (Prepared, NecessaryScope)] =
    val diff = LockDocument.closureDiff(loaded.document, merged, solved.closure)
    ZIO
      .fromEither(LockDocument.scope(diff))
      .mapError(liftSummary)
      .map: scope =>
        val digest     = LockDocument.candidate(merged)
        val generation = GenerationId.trusted(digest.value)
        val id         = proposalIdentity(request.context.id, loaded.revision, inspection.declarations, digest, scope)
        val proposal   = LockProposal(
          id,
          request.context.id,
          request.context.client,
          request.context.command,
          loaded.revision,
          inspection.declarations,
          digest,
          scope,
        )
        (Prepared(request, proposal, merged, generation), scope)

  private def decisions(
      request: PrepareRequest,
      loaded: LoadedLock,
      inspection: Inspection,
      stage: LockStage,
      scope: NecessaryScope,
      proposal: Option[ProposalSummary],
  ): IO[LockRepairError, List[PhaseDecision]] =
    ZIO.foreach(inspection.phases): phase =>
      val decided =
        request.overrideMode match
          case Some(mode) => ZIO.succeed(RepairScope.builtin(mode, phase.drift))
          case None       =>
            strategy
              .decide(
                LockContext(
                  phase.phase,
                  stage,
                  request.context.execution,
                  request.intent,
                  loaded.revision,
                  phase.drift,
                  scope,
                  proposal,
                  request.context,
                  request.events,
                )
              )
              .mapError(LockRepairError.Strategy(_))
      decided.map(decision => PhaseDecision(phase.phase, decision, phase.drift))

  private def quoted(open: OpenRepair, reply: LockReply.Accept): IO[LockRepairError, Unit] =
    if open.prepared.proposal.base == reply.base then ZIO.unit
    else ZIO.fail(LockRepairError.Transaction(LockTransactionError.StaleProposal(reply.proposal)))

  private def finish(
      id: ProposalId,
      promise: Promise[LockRepairError, AcceptOutcome],
      exit: zio.Exit[LockRepairError, AcceptOutcome],
  ): UIO[Unit] =
    exit match
      case zio.Exit.Success(outcome) => settle(id, promise, Right(outcome))
      case zio.Exit.Failure(cause)   =>
        cause.failureOption match
          case Some(error) => settle(id, promise, Left(error))
          case None        => abandon(id, promise)

  private def abandon(id: ProposalId, promise: Promise[LockRepairError, AcceptOutcome]): UIO[Unit] =
    opens.update { map =>
      map.get(id) match
        case Some(open) => map.updated(id, open.copy(slot = RepairSlot.Interrupted))
        case None       => map
    } *> promise.interrupt.unit

  private def settle(
      id: ProposalId,
      promise: Promise[LockRepairError, AcceptOutcome],
      result: Either[LockRepairError, AcceptOutcome],
  ): UIO[Unit] =
    opens.update { map =>
      map.get(id) match
        case Some(open) => map.updated(id, open.copy(slot = RepairSlot.Settled(result)))
        case None       => map
    } *> complete(promise, result)

  private def complete(
      promise: Promise[LockRepairError, AcceptOutcome],
      result: Either[LockRepairError, AcceptOutcome],
  ): UIO[Unit] =
    result match
      case Left(error)    => promise.fail(error).unit
      case Right(outcome) => promise.succeed(outcome).unit

  private def advance(id: InvocationId, step: RepairStep): IO[LockRepairError, RepairPhase] =
    phases
      .modify: map =>
        map.get(id) match
          case None        => (Left(LockRepairError.Transition(RepairPhase.Captured, step)), map)
          case Some(state) =>
            RepairScope.advance(state, step) match
              case Some(next) => (Right(next), map.updated(id, next))
              case None       => (Left(LockRepairError.Transition(state, step)), map)
      .flatMap(ZIO.fromEither)

  private def discard(id: InvocationId): UIO[Unit] =
    advance(id, RepairStep.Discard).ignore

  private def snapshotOf(
      intent: ResolutionIntent,
      revision: LockRevisionState,
      declarations: ContentHash,
      document: LockDocument,
      provenance: SnapshotProvenance,
      selection: SnapshotSelection,
      generation: GenerationId,
  ): ResolutionSnapshot =
    ResolutionSnapshot(
      intent,
      revision,
      declarations,
      ResolutionSnapshot.cells(document),
      provenance,
      selection,
      generation,
    )

  private def provenance(request: PrepareRequest): SnapshotProvenance =
    SnapshotProvenance(request.context.id, request.context.client, request.context.command)

  private def generationOf(revision: LockRevisionState, document: LockDocument): GenerationId =
    revision match
      case LockRevisionState.Absent => GenerationId.trusted(Digests.sha256("absent".getBytes(StandardCharsets.UTF_8)))
      case LockRevisionState.Present(_) => GenerationId.trusted(LockDocument.candidate(document).value)

  private def previewScope(inspection: Inspection): NecessaryScope =
    NecessaryScope(PluginDetail("lock preview"), inspection.preview)

  private def blockedScope(refusal: LockRefusal, preview: NecessaryScope): NecessaryScope =
    refusal match
      case LockRefusal.FrozenDrift    => NecessaryScope(PluginDetail("frozen drift"), preview.diff)
      case LockRefusal.Policy(detail) => NecessaryScope(detail, preview.diff)

  private def proposalIdentity(
      invocation: InvocationId,
      base: LockRevisionState,
      declarations: ContentHash,
      candidate: ContentHash,
      scope: NecessaryScope,
  ): ProposalId =
    val rows = scope.diff.entries.map: entry =>
      s"${entry.coordinate.render} ${entry.change} ${entry.subject}"
    val material = List(
      invocation.value,
      LockCodec.renderRevision(base),
      declarations.value,
      candidate.value,
      rows.mkString("\n"),
    ).mkString("\n")
    ProposalId.trusted(Digests.sha256(material.getBytes(StandardCharsets.UTF_8)))

  private def liftPrepare(fault: PrepareFault): LockRepairError =
    fault match
      case PrepareFault.Solve(detail)      => LockRepairError.Solve(detail)
      case PrepareFault.Transaction(error) => LockRepairError.Transaction(error)

  private def liftMerge(fault: MergeFault): LockRepairError =
    fault match
      case MergeFault.RefreshRequired   => LockRepairError.Transaction(LockTransactionError.RefreshRequired)
      case MergeFault.MissingRefresh(_) =>
        LockRepairError.Store(LockStoreFault.Corrupt(PluginDetail("missing snapshot refresh")))

  private def liftSummary(error: PluginDetailError): LockRepairError =
    error match
      case PluginDetailError.Empty  => LockRepairError.Store(LockStoreFault.Corrupt(PluginDetail("lock summary")))
      case PluginDetailError.Bad(_) => LockRepairError.Store(LockStoreFault.Corrupt(PluginDetail("lock summary")))

  private def liftStore(fault: LockStoreFault): LockRepairError =
    fault match
      case LockStoreFault.AtomicUnavailable => LockRepairError.Transaction(LockTransactionError.AtomicUnavailable)
      case LockStoreFault.StaleBase         => LockRepairError.Store(fault)
      case LockStoreFault.Unreadable        => LockRepairError.Store(fault)
      case LockStoreFault.Unwritable        => LockRepairError.Store(fault)
      case LockStoreFault.Corrupt(_)        => LockRepairError.Store(fault)

  private def liftCommit(id: ProposalId)(fault: LockStoreFault): LockRepairError =
    fault match
      case LockStoreFault.StaleBase         => LockRepairError.Transaction(LockTransactionError.StaleProposal(id))
      case LockStoreFault.AtomicUnavailable => LockRepairError.Transaction(LockTransactionError.AtomicUnavailable)
      case other                            => liftStore(other)
end LockCoordinator

object LockCoordinator:
  def open(
      preparer: LockPreparer,
      strategy: LockStrategy,
      store: LockStore,
      generations: GenerationOwner,
      commands: SuspendedCommand,
  ): UIO[LockCoordinator] =
    for
      phases <- Ref.make(Map.empty[InvocationId, RepairPhase])
      opens  <- Ref.make(Map.empty[ProposalId, OpenRepair])
    yield LockCoordinator(preparer, strategy, store, generations, commands, phases, opens)

private final case class Prepared(
    request: PrepareRequest,
    proposal: LockProposal,
    document: LockDocument,
    generation: GenerationId,
)

private final case class OpenRepair(prepared: Prepared, slot: RepairSlot)

private enum RepairSlot:
  case Idle
  case Running(promise: Promise[LockRepairError, AcceptOutcome])
  case Settled(result: Either[LockRepairError, AcceptOutcome])
  case Interrupted

private enum AcceptRole:
  case Missing
  case Lead(open: OpenRepair)
  case Waiting(promise: Promise[LockRepairError, AcceptOutcome])
  case Settled(result: Either[LockRepairError, AcceptOutcome])
  case Interrupted
