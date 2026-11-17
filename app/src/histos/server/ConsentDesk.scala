package histos.server

import histos.lock.{
  DeclarationDrift,
  ExecutionKind,
  LockContext,
  LockDecision,
  LockDefaults,
  LockDiff,
  LockEvent,
  LockMode,
  LockPhase,
  LockReply,
  LockStrategy,
  LockStrategyError,
}
import histos.plugin.{Digests, PluginCoordinate, PluginDetail, ProposalId}
import histos.resolve.{
  GenerationOwner,
  Inspection,
  LockCoordinator,
  LockDocument,
  LockPreparer,
  LockRepairError,
  LockStore,
  LockStoreFault,
  PhaseInspection,
  PrepareFault,
  PrepareOutcome,
  PrepareRequest,
  RepairScope,
  ResolutionSnapshot,
  ResolveError,
  SnapshotLocator,
  Solved,
  SuspendedCommand,
}

import java.nio.file.{Files, Path}

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

/** The parsed document is the solve. Refresh re-reads retained files named in that document and does not fetch. */
private final class DocumentPreparer(root: Path) extends LockPreparer:
  def inspect(
      request: PrepareRequest,
      revision: histos.lock.LockRevisionState,
      document: LockDocument,
  ): IO[PrepareFault, Inspection] =
    val same =
      revision match
        case histos.lock.LockRevisionState.Absent | histos.lock.LockRevisionState.Present(_) =>
          DeclarationDrift.Same
    ZIO.succeed(
      Inspection(
        LockDocument.candidate(document),
        List(phase(LockPhase.Seed, same), phase(LockPhase.Plugins, same), phase(LockPhase.Graph, same)),
        LockDiff(Nil),
      )
    )

  def solve(request: PrepareRequest, inspection: Inspection): IO[PrepareFault, Solved] =
    LockStore
      .open(root)
      .flatMap(_.read)
      .mapError(unread)
      .flatMap: loaded =>
        if request.refresh then refreshed(loaded.document)
        else ZIO.succeed(Solved(closure(loaded.document), loaded.document.pins, Nil, Nil))

  private def phase(known: LockPhase, drift: DeclarationDrift): PhaseInspection =
    known match
      case LockPhase.Seed    => PhaseInspection(LockPhase.Seed, drift)
      case LockPhase.Plugins => PhaseInspection(LockPhase.Plugins, drift)
      case LockPhase.Graph   => PhaseInspection(LockPhase.Graph, drift)

  private def closure(document: LockDocument): List[PluginCoordinate] =
    document.pins.foldLeft(List.empty[PluginCoordinate]): (seen, pin) =>
      if seen.contains(pin.coordinate) then seen else seen :+ pin.coordinate

  private def refreshed(document: LockDocument): IO[PrepareFault, Solved] =
    ZIO.foreach(document.retained)(reread).map { located =>
      Solved(closure(document), document.pins, document.retained.map(_.name), located)
    }

  /** The locator name is the file in the repository root. */
  private def reread(locator: SnapshotLocator): IO[PrepareFault, SnapshotLocator] =
    val path = root.resolve(locator.name.value)
    ZIO
      .attemptBlocking(Files.isRegularFile(path))
      .mapError(_ => PrepareFault.Solve(PluginDetail("a retained snapshot could not be read")))
      .flatMap:
        case false => ZIO.fail(PrepareFault.Solve(PluginDetail("a retained snapshot is not a file")))
        case true  =>
          ZIO
            .attemptBlocking(Files.readAllBytes(path))
            .mapBoth(
              _ => PrepareFault.Solve(PluginDetail("a retained snapshot could not be read")),
              bytes => SnapshotLocator(locator.name, Digests.content(bytes)),
            )

  private def unread(fault: LockStoreFault): PrepareFault =
    fault match
      case LockStoreFault.Corrupt(detail) => PrepareFault.Solve(detail)
      case LockStoreFault.StaleBase | LockStoreFault.AtomicUnavailable | LockStoreFault.Unreadable |
          LockStoreFault.Unwritable =>
        PrepareFault.Solve(PluginDetail("the lock could not be read"))
end DocumentPreparer
