package histos.resolve

import histos.lock.{
  DeclarationDrift,
  LockChange,
  LockDecision,
  LockDiffEntry,
  LockMode,
  LockPhase,
  LockRefusal,
  LockSubject,
}
import histos.plugin.{
  CommandIdentity,
  ContentHash,
  Digests,
  GenerationId,
  PluginCoordinate,
  PluginDetail,
  ResolutionIntent,
}
import histos.workspace.Workspace
import histos.{ProjectName, Resolution}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Scope, ZIO}
import zio.test.*

object LockSnapshotSpec extends ZIOSpecDefault:
  private val app   = PluginCoordinate.Module(ProjectName("app"))
  private val tools = PluginCoordinate.Module(ProjectName("tools"))
  private val shots = LocatorName("shots")

  def spec = suite("lock snapshot and necessary closure")(
    test("a document round-trips and the closure diff keeps unrelated pins out") {
      val before = sample
      val after  = LockDocument(List(pin(app, "app-2"), pin(tools, "tools-1")), before.retained)
      val diff   = LockDocument.closureDiff(before, after, List(app))
      assertTrue(
        LockDocument.parse(LockDocument.render(before)) == Right(before),
        diff.entries == List(
          LockDiffEntry(app, LockChange.Changed, LockSubject.Artifact, Some(hash("app-1")), Some(hash("app-2")))
        ),
      )
    },
    test("persisted commands retain platform, filtered versus named intent, affected mask and run main") {
      val name      = ProjectName("app")
      val platforms = histos.PlatformSelection.only(histos.Platform.Jvm, histos.Platform.Js)
      val commands  = List(
        CommandIdentity.Compile(List(name), platforms),
        CommandIdentity.Test(histos.TestSelection(histos.TestTargets.Filtered(List(name)), platforms, Some(Nil))),
        CommandIdentity.Test(histos.TestSelection(histos.TestTargets.Named(name, Nil), platforms, None)),
        CommandIdentity.Run(
          histos.plugin.CommandToken("app"),
          Some(histos.PlatformSelection.only(histos.Platform.Jvm)),
          Some(histos.MainClass("demo.Main")),
        ),
      )
      assertTrue(commands.forall(command => LockCodec.parseCommand(LockCodec.renderCommand(command)) == Right(command)))
    },
    test("added and removed closure pins are rows and an unrelated pin is not") {
      val extra  = PluginCoordinate.Module(ProjectName("extra"))
      val before = LockDocument(List(pin(app, "app-1"), pin(extra, "extra-1")), Nil)
      val after  = LockDocument(List(pin(extra, "extra-1"), pin(tools, "tools-1")), Nil)
      val diff   = LockDocument.closureDiff(before, after, List(app, tools, extra))
      assertTrue(
        diff.entries == List(
          LockDiffEntry(app, LockChange.Removed, LockSubject.Artifact, Some(hash("app-1")), None),
          LockDiffEntry(tools, LockChange.Added, LockSubject.Artifact, None, Some(hash("tools-1"))),
        )
      )
    },
    test("replacing a retained locator without refresh is refresh required") {
      val updated = SnapshotLocator(shots, hash("snap-2"))
      val refused = Solved(List(app), List(pin(app, "app-2")), List(shots), Nil)
      val allowed = refused.copy(refreshed = List(updated))
      val merged  = LockDocument.merge(sample, allowed, refresh = true)
      assertTrue(
        LockDocument.merge(sample, refused, refresh = false) == Left(MergeFault.RefreshRequired),
        merged.map(_.retained) == Right(List(updated)),
        merged.map(_.pins) == Right(List(pin(app, "app-2"), pin(tools, "tools-1"))),
      )
    },
    test("aggregation blocks on one reject or frozen drift, and one local solve is ephemeral") {
      val reject = PhaseDecision(LockPhase.Seed, LockDecision.Reject(LockRefusal.Policy(PluginDetail("no"))), changed)
      val frozen = PhaseDecision(LockPhase.Plugins, LockDecision.Frozen, changed)
      val offer  = PhaseDecision(LockPhase.Graph, LockDecision.OfferRepair, changed)
      val update = PhaseDecision(LockPhase.Seed, LockDecision.Update, changed)
      val local  = PhaseDecision(LockPhase.Plugins, LockDecision.LocalSolve, changed)
      val pinned = PhaseDecision(LockPhase.Graph, LockDecision.UsePinned, DeclarationDrift.Same)
      val calm   = PhaseDecision(LockPhase.Seed, LockDecision.Frozen, DeclarationDrift.Same)
      assertTrue(
        RepairScope.aggregate(List(reject, update)) == RepairAggregate.Blocked(LockRefusal.Policy(PluginDetail("no"))),
        RepairScope.aggregate(List(frozen, offer)) == RepairAggregate.Blocked(LockRefusal.FrozenDrift),
        RepairScope.aggregate(List(local, update)) == RepairAggregate.Ephemeral,
        RepairScope.aggregate(List(offer, offer)) == RepairAggregate.Consent,
        RepairScope.aggregate(List(update, update)) == RepairAggregate.AutoUpdate,
        RepairScope.aggregate(List(update, offer)) == RepairAggregate.Consent,
        RepairScope.aggregate(List(update, pinned)) == RepairAggregate.AutoUpdate,
        RepairScope.aggregate(List(pinned, calm)) == RepairAggregate.Pinned,
        RepairScope.aggregate(List(offer, update)) == RepairAggregate.Consent,
        RepairScope.aggregate(List(frozen, local)) == RepairAggregate.Blocked(LockRefusal.FrozenDrift),
      )
    },
    test("an explicit mode is the disposition before discovery") {
      assertTrue(
        RepairScope.builtin(LockMode.Offer, changed) == LockDecision.OfferRepair,
        RepairScope.builtin(LockMode.Offer, DeclarationDrift.Same) == LockDecision.UsePinned,
        RepairScope.builtin(LockMode.Update, changed) == LockDecision.Update,
        RepairScope.builtin(LockMode.Local, DeclarationDrift.Same) == LockDecision.LocalSolve,
        RepairScope.builtin(LockMode.Frozen, changed) == LockDecision.Frozen,
      )
    },
    test("the offer path reaches completed and a commit does not finish early") {
      val offer = List(
        RepairStep.BeginDiscovery,
        RepairStep.Candidate,
        RepairStep.Offer,
        RepairStep.BeginCommit,
        RepairStep.Renamed,
        RepairStep.BeginExecute,
        RepairStep.Finish,
      )
      val local = List(
        RepairStep.BeginDiscovery,
        RepairStep.Candidate,
        RepairStep.UseLocal,
        RepairStep.BeginExecute,
        RepairStep.Finish,
      )
      assertTrue(
        walk(RepairPhase.Captured, offer) == Some(RepairPhase.Completed),
        walk(RepairPhase.Captured, local) == Some(RepairPhase.Completed),
        RepairScope.advance(RepairPhase.Completed, RepairStep.BeginCommit) == None,
        RepairScope.advance(RepairPhase.Captured, RepairStep.Renamed) == None,
        RepairScope.advance(RepairPhase.Committing, RepairStep.Finish) == None,
        RepairScope.advance(RepairPhase.Blocked, RepairStep.BeginExecute) == None,
      )
    },
    test("an ephemeral snapshot does not satisfy a frozen caller") {
      val committed = view(SnapshotSelection.Committed, hash("gen-1"))
      val ephemeral =
        committed.copy(selection = SnapshotSelection.Ephemeral, generation = GenerationId.trusted(hash("gen-2").value))
      assertTrue(
        ResolutionSnapshot.admits(committed, LockMode.Frozen),
        !ResolutionSnapshot.admits(ephemeral, LockMode.Frozen),
        ResolutionSnapshot.admits(ephemeral, LockMode.Local),
      )
    },
    test("a lease keeps the generation it captured") {
      ZIO.scoped:
        temp.flatMap: root =>
          val first  = view(SnapshotSelection.Committed, hash("gen-1"))
          val second = view(SnapshotSelection.Committed, hash("gen-2")).copy(cells = List(pin(tools, "tools-9")))
          for
            owner  <- GenerationOwner.memory(root)
            _      <- owner.publish(first)
            _      <- owner.publish(second)
            leased <- owner.lease(first.generation)
            now    <- owner.current
          yield assertTrue(leased == Some(first), now == Some(second), first.cells != second.cells)
    },
    test("atomic replace failure is not a successful move") {
      val denied = new java.nio.file.AtomicMoveNotSupportedException("source", "target", "unsupported")
      val io     = new java.io.IOException("disk")
      assertTrue(
        AtomicReplace.interpret(Left(denied)) == Left(LockStoreFault.AtomicUnavailable),
        AtomicReplace.interpret(Left(io)) == Left(LockStoreFault.Unwritable),
        AtomicReplace.interpret(Right(())) == Right(()),
      )
    },
  )

  private def walk(start: RepairPhase, steps: List[RepairStep]): Option[RepairPhase] =
    steps.foldLeft[Option[RepairPhase]](Some(start)): (state, step) =>
      state.flatMap(current => RepairScope.advance(current, step))

  private def view(selection: SnapshotSelection, generation: ContentHash): ResolutionSnapshot =
    ResolutionSnapshot(
      ResolutionIntent.Select(Resolution.Lock),
      histos.lock.LockRevisionState.Absent,
      hash("declarations"),
      List(pin(app, "app-1")),
      histos.resolve.SnapshotProvenance(
        histos.plugin.InvocationId.derive("lease"),
        histos.plugin.ClientId("cli"),
        CommandIdentity.Compile(List(ProjectName("app"))),
      ),
      selection,
      GenerationId.trusted(generation.value),
    )

  private def sample: LockDocument =
    LockDocument(List(pin(app, "app-1"), pin(tools, "tools-1")), List(SnapshotLocator(shots, hash("snap-1"))))

  private def pin(coordinate: PluginCoordinate, label: String): VersionedPin =
    VersionedPin(coordinate, LockSubject.Artifact, hash(label), None)

  private def hash(text: String): ContentHash =
    Digests.content(text.getBytes(StandardCharsets.UTF_8))

  private val changed: DeclarationDrift = DeclarationDrift.Changed(hash("declarations"))

  private def temp: ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-lock")).orDie): path =>
      ZIO.attemptBlocking(Workspace.deleteSync(path)).orDie
end LockSnapshotSpec
