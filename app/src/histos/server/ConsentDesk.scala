package histos.server

import histos.lock.{
  ExecutionKind,
  LockContext,
  LockDecision,
  LockDefaults,
  LockEvent,
  LockMode,
  LockReply,
  LockStrategy,
  LockStrategyError,
}
import histos.plugin.{PluginDetail, ProposalId}
import histos.resolve.{
  DocumentPreparer,
  GenerationOwner,
  LockCoordinator,
  LockPreparer,
  LockRepairError,
  LockStore,
  LockStoreFault,
  PrepareOutcome,
  PrepareRequest,
  RepairScope,
  ResolutionSnapshot,
  ResolveError,
  SuspendedCommand,
}

import java.nio.file.Path

import zio.{IO, Ref, UIO, ZIO}

/** One session's coordinator. A reply is [[histos.lock.LockReply]], never [[histos.server.RunInput]], and the server
  * environment is not a source. [[histos.lock.LockEvent.Offered]] is the build feed. Corrupt lock bytes are a refusal
  * before an offer.
  */
final class ConsentDesk private (
    coordinator: LockCoordinator,
    generations: GenerationOwner,
    consent: WatchConsent,
    pending: Ref[Option[ProposalId]],
    retained: Ref[Option[WatchAnswer]],
    feed: LockEvent => UIO[Unit],
):
  /** Discover, then publish an offer. A replacement declines the cancelled proposal before the new offer is published.
    * An unattended repeat of the same caller is not published. A corrupt store fails here and does not publish.
    */
  def prepare(request: PrepareRequest): IO[LockRepairError, PrepareOutcome] =
    coordinator
      .prepare(request)
      .flatMap:
        case PrepareOutcome.Offered(offer) =>
          consent
            .offer(offer.proposal, offer.caller.value, request.context.interaction)
            .flatMap:
              case WatchOffer.Fresh(_) =>
                publish(offer)
              case WatchOffer.Replaced(cancelled, _) =>
                coordinator.decline(LockReply.Decline(cancelled)) *> publish(offer)
              case WatchOffer.Suppressed =>
                coordinator.decline(LockReply.Decline(offer.proposal)).as(PrepareOutcome.Offered(offer))
        case other @ (PrepareOutcome.Committed(_, _) | PrepareOutcome.Blocked(_) | PrepareOutcome.Selected(_)) =>
          ZIO.succeed(other)

  /** Accept or decline, then [[histos.server.WatchConsent.accept]]. The caller keeps the answer for the watch. */
  def answer(reply: LockReply): IO[LockRepairError, WatchAnswer] =
    reply match
      case accept: LockReply.Accept =>
        coordinator.accept(accept) *> consent.accept(reply)
      case decline: LockReply.Decline =>
        coordinator.decline(decline) *> consent.accept(reply)

  /** The watch loop reads the answer [[answer]] produced. */
  def keep(answer: WatchAnswer): UIO[Unit] =
    retained.set(Some(answer)) *> pending.set(None)

  def iteration: UIO[Option[WatchAnswer]] =
    retained.get

  def onChange(woke: List[Path]): UIO[Unit] =
    if woke.isEmpty then ZIO.unit
    else
      pending
        .modify(current => (current, None))
        .flatMap:
          case Some(id) => coordinator.decline(LockReply.Decline(id)).ignore
          case None     => ZIO.unit
      *> WatchConsent.onChange(consent, woke)
        *> retained.set(None)

  private def publish(offer: histos.lock.LockOffer): UIO[PrepareOutcome] =
    pending.set(Some(offer.proposal)) *> feed(LockEvent.Offered(offer)).as(PrepareOutcome.Offered(offer))

  /** The client left before accept. An open proposal is declined and the command does not run. */
  def discard: UIO[Unit] =
    pending
      .modify(current => (current, None))
      .flatMap:
        case Some(id) => coordinator.decline(LockReply.Decline(id)).ignore *> consent.invalidate.unit
        case None     => consent.invalidate.unit

  def resumes: UIO[Int] =
    consent.resumeCount

  def adopted: UIO[Option[ResolutionSnapshot]] =
    generations.current
end ConsentDesk

object ConsentDesk:
  def open(
      root: Path,
      strategy: LockStrategy,
      command: SuspendedCommand,
      preparer: LockPreparer,
      feed: LockEvent => UIO[Unit],
  ): UIO[ConsentDesk] =
    for
      store       <- LockStore.open(root)
      generations <- GenerationOwner.memory(root)
      coordinator <- LockCoordinator.open(preparer, strategy, store, generations, command)
      consent     <- WatchConsent.make
      pending     <- Ref.make(Option.empty[ProposalId])
      retained    <- Ref.make(Option.empty[WatchAnswer])
    yield ConsentDesk(coordinator, generations, consent, pending, retained, feed)

  /** The invocation's mode. Absent uses [[histos.lock.LockDefaults.core]]. Drift still chooses the decision. */
  def strategy(execution: ExecutionKind, lock: Option[LockMode]): LockStrategy =
    val mode = lock match
      case Some(found) => found
      case None        =>
        execution match
          case ExecutionKind.Local => LockDefaults.core.local
          case ExecutionKind.Ci    => LockDefaults.core.ci
    new LockStrategy:
      def decide(context: LockContext): IO[LockStrategyError, LockDecision] =
        ZIO.succeed(RepairScope.builtin(mode, context.drift))

  /** Runs after the generation is published. It does not write the lock. */
  val command: SuspendedCommand = new SuspendedCommand:
    def run(snapshot: ResolutionSnapshot): IO[LockRepairError, Unit] =
      ZIO.unit

  /** Inspects a document the store already parsed. A dependency lock never reaches this. */
  def preparer(root: Path): LockPreparer =
    DocumentPreparer(root)

  def feed(say: Say): LockEvent => UIO[Unit] =
    event => say(Message.Event(histos.build.BuildEvent.Lock(event))).ignore

  /** [[histos.resolve.ResolveError.Transaction]] is the transaction. Corrupt stays the detail, not `toString`. */
  def reportOf(error: LockRepairError): ConsentReport =
    error match
      case LockRepairError.Transaction(found) =>
        ConsentReport.Transaction(ResolveError.Transaction(found))
      case LockRepairError.Store(LockStoreFault.Corrupt(detail)) =>
        ConsentReport.Corrupt(detail)
      case LockRepairError.Solve(_) | LockRepairError.Strategy(_) | LockRepairError.Store(_) |
          LockRepairError.Transition(_, _) =>
        ConsentReport.Unreported(error)
end ConsentDesk

enum ConsentReport:
  case Transaction(error: ResolveError.Transaction)
  case Corrupt(detail: PluginDetail)
  case Unreported(error: LockRepairError)
