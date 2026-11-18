package histos.resolve

import histos.lock.{DeclarationDrift, LockPhase, LockRevisionState}
import histos.plugin.{Digests, PluginCoordinate, PluginDetail}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** The parsed document is the solve. Refresh re-reads retained files in the repository root and does not fetch. */
final class DocumentPreparer(root: Path) extends LockPreparer:
  def inspect(
      request: PrepareRequest,
      revision: LockRevisionState,
      document: LockDocument,
  ): IO[PrepareFault, Inspection] =
    if !request.refresh then
      val same = revision match
        case LockRevisionState.Absent | LockRevisionState.Present(_) => DeclarationDrift.Same
      ZIO.succeed(Inspection(LockDocument.candidate(document), phases(same), histos.lock.LockDiff(Nil)))
    else
      revalidate(request, document).map: found =>
        Inspection(
          LockDocument.candidate(document),
          phases(SnapshotRefresh.drift(document, found.changed)),
          SnapshotRefresh.preview(document, found.changed),
          found.changed,
        )

  def solve(request: PrepareRequest, inspection: Inspection): IO[PrepareFault, Solved] =
    if request.refresh then
      val changed = inspection.refreshed
      ZIO.succeed(Solved(Nil, Nil, changed.map(_.name), changed))
    else
      LockStore
        .open(root)
        .flatMap(_.read)
        .mapError(unread)
        .map: loaded =>
          val document = loaded.document
          Solved(closure(document), document.pins, Nil, Nil)

  private def revalidate(
      request: PrepareRequest,
      document: LockDocument,
  ): IO[PrepareFault, SnapshotRefresh.Revalidated] =
    SnapshotRefresh.select(document, SnapshotRefresh.selectors(request.context.command)) match
      case Left(error)   => ZIO.fail(PrepareFault.Transaction(error))
      case Right(chosen) =>
        ZIO.foreach(chosen)(readChanged).map(found => SnapshotRefresh.Revalidated(found.flatten))

  private def phases(drift: DeclarationDrift): List[PhaseInspection] =
    List(LockPhase.Seed, LockPhase.Plugins, LockPhase.Graph).map(phase(_, drift))

  private def phase(known: LockPhase, drift: DeclarationDrift): PhaseInspection =
    known match
      case LockPhase.Seed    => PhaseInspection(LockPhase.Seed, drift)
      case LockPhase.Plugins => PhaseInspection(LockPhase.Plugins, drift)
      case LockPhase.Graph   => PhaseInspection(LockPhase.Graph, drift)

  private def closure(document: LockDocument): List[PluginCoordinate] =
    document.pins.foldLeft(List.empty[PluginCoordinate]): (seen, pin) =>
      if seen.contains(pin.coordinate) then seen else seen :+ pin.coordinate

  /** `None` keeps the retained record. `Some` is the locator whose bytes hashed to a different digest. */
  private def readChanged(locator: SnapshotLocator): IO[PrepareFault, Option[SnapshotLocator]] =
    reread(locator).map: next =>
      if next.digest == locator.digest then None else Some(next)

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
