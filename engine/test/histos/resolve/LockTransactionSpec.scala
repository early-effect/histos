package histos.resolve

import histos.lock.{
  DeclarationDrift,
  ExecutionKind,
  LockDecision,
  LockMode,
  LockPhase,
  LockRefusal,
  LockReply,
  LockRevisionState,
  LockStrategy,
  LockStrategyError,
  LockSubject,
  LockTransactionError,
}
import histos.plugin.{
  ClientId,
  CommandIdentity,
  ContentHash,
  Interaction,
  Digests,
  InvocationContext,
  InvocationId,
  InvocationPluginEvent,
  LinkStrategy,
  LockRevision,
  OfflineSelection,
  PluginCoordinate,
  PluginDetail,
  PluginEventError,
  PluginEvents,
  ResolutionIntent,
}
import histos.workspace.Workspace
import histos.{ArtifactBase, Organization, Platform, ProjectName, SbtSeries}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Exit, IO, Promise, Ref, Scope, UIO, ZIO}
import zio.test.*

object LockTransactionSpec extends ZIOSpecDefault:
  private val app      = PluginCoordinate.Module(ProjectName("app"))
  private val tools    = PluginCoordinate.Module(ProjectName("tools"))
  private val shots    = LocatorName("shots")
  private val retain   = SnapshotLocator(shots, hash("snap-1"))
  private val toolsPin = pin(tools, "tools-1")
  private val original = LockDocument(List(pin(app, "app-1"), toolsPin), List(retain))
  private val changed  = DeclarationDrift.Changed(hash("declarations"))

  def spec = suite("lock transaction")(
    test("a decline leaves the previous lock bytes") {
      ZIO.scoped:
        open(fixed(LockDecision.OfferRepair)).flatMap: harness =>
          for
            outcome <- harness.coordinator.prepare(request("decline"))
            text    <- lockText(harness.root)
            runs    <- harness.runs.get
            offer   <- offerOf(outcome)
            base    <- present(offer.base)
            _       <- harness.coordinator.decline(LockReply.Decline(offer.proposal))
            after   <- lockText(harness.root)
          yield assertTrue(
            runs == 0,
            text == LockDocument.render(original),
            after == text,
            base.value.nonEmpty,
            after.contains(retainLine),
          )
    },
    test("a solve failure leaves the previous lock bytes") {
      ZIO.scoped:
        open(fixed(LockDecision.Update), fail = Some(PluginDetail("solve failed"))).flatMap: harness =>
          for
            exit <- harness.coordinator.prepare(request("solve")).exit
            text <- lockText(harness.root)
            runs <- harness.runs.get
          yield assertTrue(
            failure(exit) == Some(LockRepairError.Solve(PluginDetail("solve failed"))),
            text == LockDocument.render(original),
            runs == 0,
            text.contains(retainLine),
          )
    },
    test("interruption before rename leaves the previous lock bytes") {
      ZIO.scoped:
        for
          started <- Promise.make[Nothing, Unit]
          hold    <- Promise.make[Nothing, Unit]
          harness <- open(
            fixed(LockDecision.Update),
            probe = new CommitProbe:
              def beforeRename = started.succeed(()) *> hold.await
              def afterRename  = ZIO.unit,
          )
          fiber   <- harness.coordinator.prepare(request("before")).fork
          _       <- started.await
          exit    <- fiber.interrupt
          text    <- lockText(harness.root)
          runs    <- harness.runs.get
          journal <- missing(harness.store.journal)
        yield assertTrue(
          exit.isInterrupted,
          text == LockDocument.render(original),
          text.contains(retainLine),
          runs == 0,
          journal,
        )
    },
    test("interruption after rename recovers the new revision and does not roll back") {
      ZIO.scoped:
        for
          renamed <- Promise.make[Nothing, Unit]
          harness <- open(
            fixed(LockDecision.Update),
            probe = new CommitProbe:
              def beforeRename = ZIO.unit
              def afterRename  = renamed.succeed(()) *> ZIO.never,
          )
          fiber    <- harness.coordinator.prepare(request("after")).fork
          _        <- renamed.await
          exit     <- fiber.interrupt
          recovery <- harness.coordinator.recover
          text     <- lockText(harness.root)
          runs     <- harness.runs.get
          current  <- harness.generations.current
        yield
          val expected = replaced("app-2")
          recovery match
            case LockRecovery.Adopted(revision, snapshot) =>
              assertTrue(
                exit.isInterrupted,
                revision == LockDocument.revision(expected),
                text == LockDocument.render(expected),
                text.contains(retainLine),
                text != LockDocument.render(original),
                runs == 0,
                snapshot.selection == SnapshotSelection.Committed,
                snapshot.lockRevision == LockRevisionState.Present(revision),
                current == Some(snapshot),
              )
            case LockRecovery.Idle | LockRecovery.Retained(_) | LockRecovery.Diverged(_) =>
              assertTrue(false)
    },
    test("a stale proposal fails and does not replace the edited lock") {
      ZIO.scoped:
        open(fixed(LockDecision.OfferRepair)).flatMap: harness =>
          val edited = LockDocument(List(pin(app, "app-other"), toolsPin), List(retain))
          for
            outcome <- harness.coordinator.prepare(request("stale"))
            offer   <- offerOf(outcome)
            _       <- write(harness.store.lock, LockDocument.render(edited))
            exit    <- harness.coordinator.accept(LockReply.Accept(offer.proposal, offer.base)).exit
            text    <- lockText(harness.root)
            runs    <- harness.runs.get
          yield assertTrue(
            failure(exit) == Some(LockRepairError.Transaction(LockTransactionError.StaleProposal(offer.proposal))),
            text == LockDocument.render(edited),
            text.contains(retainLine),
            runs == 0,
          )
    },
    test("interrupting the lead releases the waiting accept") {
      ZIO.scoped:
        for
          started <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          entries <- Ref.make(0)
          harness <- open(
            fixed(LockDecision.OfferRepair),
            command = _ =>
              new SuspendedCommand:
                def run(snapshot: ResolutionSnapshot) =
                  entries.update(_ + 1) *> started.succeed(()) *> release.await,
          )
          outcome <- harness.coordinator.prepare(request("interrupt-lead"))
          offer   <- offerOf(outcome)
          first   <- harness.coordinator.accept(LockReply.Accept(offer.proposal, offer.base)).fork
          _       <- started.await
          second  <- harness.coordinator.accept(LockReply.Accept(offer.proposal, offer.base)).fork
          _       <- first.interrupt
          exit    <- second.await
          third   <- harness.coordinator.accept(LockReply.Accept(offer.proposal, offer.base)).exit
          seen    <- entries.get
        yield assertTrue(seen == 1, exit.isInterrupted, third.isInterrupted)
    },
    test("a duplicate accept does not run the suspended command twice") {
      ZIO.scoped:
        for
          started <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          commits <- Ref.make(0)
          harness <- open(
            fixed(LockDecision.OfferRepair),
            renameLock = (temp, target) => commits.update(_ + 1) *> AtomicReplace.rename(temp, target),
            command = runs =>
              new SuspendedCommand:
                def run(snapshot: ResolutionSnapshot) =
                  started.succeed(()) *> release.await *> runs.update(_ + 1),
          )
          outcome <- harness.coordinator.prepare(request("duplicate"))
          offer   <- offerOf(outcome)
          base    <- present(offer.base)
          before  <- harness.runs.get
          first   <- harness.coordinator.accept(LockReply.Accept(offer.proposal, offer.base)).fork
          _       <- started.await
          second  <- harness.coordinator.accept(LockReply.Accept(offer.proposal, offer.base)).fork
          _       <- release.succeed(())
          done    <- first.join
          again   <- second.join
          runs    <- harness.runs.get
          written <- commits.get
        yield assertTrue(before == 0, runs == 1, written == 1, done == again, done.committed.revision != base)
    },
    test("a concurrent writer loses") {
      ZIO.scoped:
        for
          started <- Promise.make[Nothing, Unit]
          hold    <- Promise.make[Nothing, Unit]
          left  = context("left")
          right = context("right")
          noticed <- Promise.make[Nothing, Unit]
          harness <- open(
            fixed(LockDecision.Update),
            solvedBy = id =>
              if id == left.id then Some(appSolved("app-a"))
              else if id == right.id then Some(appSolved("app-b"))
              else None,
            noticed = id => if id == right.id then noticed.succeed(()).unit else ZIO.unit,
            probe = new CommitProbe:
              def beforeRename = started.succeed(()) *> hold.await
              def afterRename  = ZIO.unit,
          )
          fiberA <- harness.coordinator.prepare(prepare(left)).fork
          _      <- started.await
          fiberB <- harness.coordinator.prepare(prepare(right)).fork
          _      <- noticed.await
          _      <- hold.succeed(())
          winner <- fiberA.join
          loser  <- fiberB.await
          text   <- lockText(harness.root)
          runs   <- harness.runs.get
        yield
          val expected = replaced("app-a")
          val stale    = failure(loser) match
            case Some(LockRepairError.Transaction(LockTransactionError.StaleProposal(_))) => true
            case Some(_)                                                                  => false
            case None                                                                     => false
          winner match
            case PrepareOutcome.Committed(committed, _) =>
              assertTrue(
                stale,
                committed.revision == LockDocument.revision(expected),
                text == LockDocument.render(expected),
                text.contains(retainLine),
                runs == 1,
              )
            case PrepareOutcome.Offered(_) | PrepareOutcome.Blocked(_) | PrepareOutcome.Selected(_) =>
              assertTrue(false)
    },
    test("an atomic replace failure is typed and does not fall back to an ordinary move") {
      ZIO.scoped:
        for
          calls   <- Ref.make(0)
          harness <- open(
            fixed(LockDecision.Update),
            renameLock = (_, _) => calls.updateAndGet(_ + 1) *> ZIO.fail(LockStoreFault.AtomicUnavailable),
          )
          exit    <- harness.coordinator.prepare(request("atomic")).exit
          text    <- lockText(harness.root)
          renames <- calls.get
          journal <- missing(harness.store.journal)
          runs    <- harness.runs.get
        yield assertTrue(
          failure(exit) == Some(LockRepairError.Transaction(LockTransactionError.AtomicUnavailable)),
          renames == 1,
          text == LockDocument.render(original),
          text.contains(retainLine),
          journal,
          runs == 0,
        )
    },
    test("frozen drift performs zero solves") {
      ZIO.scoped:
        open(fixed(LockDecision.Frozen)).flatMap: harness =>
          for
            outcome <- harness.coordinator.prepare(request("frozen"))
            solves  <- harness.solves.get
            runs    <- harness.runs.get
            text    <- lockText(harness.root)
          yield assertTrue(
            refusal(outcome) == Some(LockRefusal.FrozenDrift),
            solves == 0,
            runs == 0,
            text == LockDocument.render(original),
          )
    },
    test("an invocation frozen override does not ask the strategy or solve") {
      val strategy = new LockStrategy:
        def decide(context: histos.lock.LockContext): IO[LockStrategyError, LockDecision] =
          ZIO.fail(LockStrategyError.Policy(PluginDetail("strategy was called")))
      ZIO.scoped:
        open(strategy).flatMap: harness =>
          for
            outcome <- harness.coordinator.prepare(request("override", Some(LockMode.Frozen)))
            solves  <- harness.solves.get
            text    <- lockText(harness.root)
          yield assertTrue(
            refusal(outcome) == Some(LockRefusal.FrozenDrift),
            solves == 0,
            text == LockDocument.render(original),
          )
    },
    test("one reject blocks the invocation before a solve") {
      val strategy = phased:
        case LockPhase.Graph   => LockDecision.Reject(LockRefusal.Policy(PluginDetail("policy refuses repair")))
        case LockPhase.Seed    => LockDecision.Update
        case LockPhase.Plugins => LockDecision.Update
      ZIO.scoped:
        open(strategy).flatMap: harness =>
          for
            outcome <- harness.coordinator.prepare(request("reject"))
            solves  <- harness.solves.get
            runs    <- harness.runs.get
            text    <- lockText(harness.root)
          yield assertTrue(
            refusal(outcome) == Some(LockRefusal.Policy(PluginDetail("policy refuses repair"))),
            solves == 0,
            runs == 0,
            text == LockDocument.render(original),
          )
    },
    test("one local solve publishes an ephemeral snapshot and does not write the tracked lock") {
      val strategy = phased:
        case LockPhase.Plugins => LockDecision.LocalSolve
        case LockPhase.Seed    => LockDecision.Update
        case LockPhase.Graph   => LockDecision.Update
      ZIO.scoped:
        open(strategy).flatMap: harness =>
          for
            outcome <- harness.coordinator.prepare(request("local"))
            text    <- lockText(harness.root)
            side    <- readIfPresent(harness.root.resolve(".histos").resolve("resolution"))
            solves  <- harness.solves.get
            runs    <- harness.runs.get
          yield outcome match
            case PrepareOutcome.Selected(snapshot) =>
              assertTrue(
                snapshot.selection == SnapshotSelection.Ephemeral,
                snapshot.lockRevision == LockRevisionState.Present(LockDocument.revision(original)),
                !ResolutionSnapshot.admits(snapshot, LockMode.Frozen),
                text == LockDocument.render(original),
                side.exists(_.contains("selection ephemeral")),
                side.exists(_.contains(snapshot.generation.value)),
                solves == 1,
                runs == 1,
              )
            case PrepareOutcome.Offered(_) | PrepareOutcome.Committed(_, _) | PrepareOutcome.Blocked(_) =>
              assertTrue(false)
    },
    test("offer repair is one consent for the whole diff") {
      ZIO.scoped:
        open(
          fixed(LockDecision.OfferRepair),
          solvedBy = _ => Some(Solved(List(app, tools), List(pin(app, "app-2"), pin(tools, "tools-2")), Nil, Nil)),
        ).flatMap: harness =>
          for
            outcome <- harness.coordinator.prepare(request("whole"))
            offer   <- offerOf(outcome)
            _       <- harness.coordinator.accept(LockReply.Accept(offer.proposal, offer.base))
            text    <- lockText(harness.root)
            runs    <- harness.runs.get
            names = offer.scope.diff.entries.map(_.coordinate)
          yield assertTrue(
            names == List(app, tools),
            offer.scope.diff.entries.length == 2,
            offer.command == CommandIdentity.Compile(List(ProjectName("app"))),
            text == LockDocument.render(LockDocument(List(pin(app, "app-2"), pin(tools, "tools-2")), List(retain))),
            text.contains(retainLine),
            runs == 1,
          )
    },
    test("update commits only when every repaired phase allows it") {
      val mixed = phased:
        case LockPhase.Plugins => LockDecision.OfferRepair
        case LockPhase.Seed    => LockDecision.Update
        case LockPhase.Graph   => LockDecision.Update
      ZIO.scoped:
        open(mixed).flatMap: offered =>
          for
            outcome <- offered.coordinator.prepare(request("mixed"))
            text    <- lockText(offered.root)
            runs    <- offered.runs.get
          yield assertTrue(asOffer(outcome).isDefined, runs == 0, text == LockDocument.render(original))
    },
    test("update writes the candidate when every repaired phase allows it") {
      ZIO.scoped:
        open(fixed(LockDecision.Update)).flatMap: harness =>
          for
            outcome <- harness.coordinator.prepare(request("update"))
            text    <- lockText(harness.root)
            side    <- missing(harness.root.resolve(".histos").resolve("resolution"))
            runs    <- harness.runs.get
            solves  <- harness.solves.get
          yield outcome match
            case PrepareOutcome.Committed(committed, snapshot) =>
              assertTrue(
                text == LockDocument.render(replaced("app-2")),
                text.contains(retainLine),
                committed.revision == LockDocument.revision(replaced("app-2")),
                snapshot.selection == SnapshotSelection.Committed,
                side,
                runs == 1,
                solves == 1,
              )
            case PrepareOutcome.Offered(_) | PrepareOutcome.Blocked(_) | PrepareOutcome.Selected(_) =>
              assertTrue(false)
    },
    test("a necessary solve that replaces a retained locator asks for refresh and does not write") {
      ZIO.scoped:
        open(
          fixed(LockDecision.Update),
          solvedBy = _ => Some(Solved(List(app), List(pin(app, "app-2")), List(shots), Nil)),
        ).flatMap: harness =>
          for
            exit <- harness.coordinator.prepare(request("refresh")).exit
            text <- lockText(harness.root)
          yield assertTrue(
            failure(exit) == Some(LockRepairError.Transaction(LockTransactionError.RefreshRequired)),
            text == LockDocument.render(original),
            text.contains(retainLine),
          )
    },
    test("an agreed pin does not solve or rewrite the lock") {
      ZIO.scoped:
        open(fixed(LockDecision.UsePinned), drift = DeclarationDrift.Same).flatMap: harness =>
          for
            outcome <- harness.coordinator.prepare(request("pinned"))
            solves  <- harness.solves.get
            runs    <- harness.runs.get
            text    <- lockText(harness.root)
          yield outcome match
            case PrepareOutcome.Selected(snapshot) =>
              assertTrue(
                snapshot.selection == SnapshotSelection.Committed,
                solves == 0,
                runs == 1,
                text == LockDocument.render(original),
              )
            case PrepareOutcome.Offered(_) | PrepareOutcome.Committed(_, _) | PrepareOutcome.Blocked(_) =>
              assertTrue(false)
    },
    test("a plain published pin round-trips with its link word") {
      val java   = histos.Dep.java("com.example", "plugin", histos.LibraryRevision.trusted("1.0.0"))
      val binary = histos.Dep.binary("com.example", "seedplug", histos.LibraryRevision.trusted("1.0.0"))
      val full   = histos.Dep.full("com.example", "seedplug", histos.LibraryRevision.trusted("1.2.0"))
      val exact  = published(java, LinkStrategy.ExactBinary, LockSubject.Artifact, "java", None)
      val linked = published(
        binary,
        LinkStrategy.BinaryOrSource,
        LockSubject.Checksum,
        "binary",
        Some(SnapshotLocator(shots, hash("loc"))),
      )
      val sourced = published(full, LinkStrategy.Source, LockSubject.Recipe, "full", None)
      val text    = LockDocument.render(exact)
      assertTrue(
        LockDocument.roundTrip(exact) == Right(exact),
        LockDocument.roundTrip(linked) == Right(linked),
        LockDocument.roundTrip(sourced) == Right(sourced),
        LockDocument.roundTrip(original) == Right(original),
        text == s"pin com.example:plugin:1.0.0 exact-binary artifact ${hash("java").value}\n",
        LockDocument.render(linked).contains("pin com.example::seedplug:1.0.0 binary-or-source checksum"),
        LockDocument.render(sourced).contains("pin com.example:::seedplug:1.2.0 source recipe"),
      )
    },
    test("api and a dependency that is not a plain coordinate do not round-trip") {
      val java     = histos.Dep.java("com.example", "plugin", histos.LibraryRevision.trusted("1.0.0"))
      val api      = published(java, LinkStrategy.Api, LockSubject.Artifact, "api", None)
      val excluded = java.excluding(Organization("org.example"), ArtifactBase("gone"))
      val filtered = java.on(Platform.Jvm)
      val compile  = java.compileOnly
      val series   = histos.Dep.binary(
        "com.example",
        "seedplug",
        histos.LibraryRevision.trusted("1.0.0"),
        sbtSeries = Some(SbtSeries("1.0")),
      )
      assertTrue(
        LockDocument.roundTrip(api) == Left(LockTextError.Bad(PluginDetail("api is not a pin link"))),
        LockDocument.roundTrip(published(excluded, LinkStrategy.ExactBinary, LockSubject.Artifact, "ex", None)).isLeft,
        LockDocument.roundTrip(published(filtered, LinkStrategy.ExactBinary, LockSubject.Artifact, "on", None)).isLeft,
        LockDocument.roundTrip(published(compile, LinkStrategy.ExactBinary, LockSubject.Artifact, "co", None)).isLeft,
        LockDocument.roundTrip(published(series, LinkStrategy.Source, LockSubject.Artifact, "sbt", None)).isLeft,
        LockDocument.roundTrip(original) == Right(original),
      )
    },
    test("consent against an absent lock commits that base") {
      ZIO.scoped:
        open(fixed(LockDecision.OfferRepair), seed = None).flatMap: harness =>
          for
            outcome <- harness.coordinator.prepare(request("absent"))
            offer   <- offerOf(outcome)
            done    <- harness.coordinator.accept(LockReply.Accept(offer.proposal, LockRevisionState.Absent))
            text    <- lockText(harness.root)
            runs    <- harness.runs.get
            gone    <- missing(harness.root.resolve("dependencies.lock.journal"))
          yield assertTrue(
            offer.base == LockRevisionState.Absent,
            done.snapshot.lockRevision == LockRevisionState.Present(done.committed.revision),
            text == LockDocument.render(LockDocument(List(pin(app, "app-2")), Nil)),
            runs == 1,
            gone,
          )
    },
    test("a present reply does not consent an absent lock") {
      ZIO.scoped:
        open(fixed(LockDecision.OfferRepair), seed = None).flatMap: harness =>
          val wrong = LockRevisionState.Present(LockDocument.revision(original))
          for
            outcome <- harness.coordinator.prepare(request("absent-stale"))
            offer   <- offerOf(outcome)
            exit    <- harness.coordinator.accept(LockReply.Accept(offer.proposal, wrong)).exit
            runs    <- harness.runs.get
            absent  <- missing(harness.store.lock)
          yield assertTrue(
            offer.base == LockRevisionState.Absent,
            failure(exit) == Some(LockRepairError.Transaction(LockTransactionError.StaleProposal(offer.proposal))),
            runs == 0,
            absent,
          )
    },
  )

  private def open(
      strategy: LockStrategy,
      solvedBy: InvocationId => Option[Solved] = _ => Some(appSolved("app-2")),
      probe: CommitProbe = CommitProbe.none,
      renameLock: (Path, Path) => IO[LockStoreFault, Unit] = AtomicReplace.rename,
      fail: Option[PluginDetail] = None,
      pause: UIO[Unit] = ZIO.unit,
      noticed: InvocationId => UIO[Unit] = _ => ZIO.unit,
      drift: DeclarationDrift = changed,
      seed: Option[LockDocument] = Some(original),
      command: Ref[Int] => SuspendedCommand = runs =>
        new SuspendedCommand:
          def run(snapshot: ResolutionSnapshot) = runs.update(_ + 1),
  ): ZIO[Scope, Nothing, Harness] =
    for
      root   <- temp
      runs   <- Ref.make(0)
      solves <- Ref.make(0)
      store  <- LockStore.open(root, renameLock, probe)
      _      <- seed match
        case Some(document) => write(store.lock, LockDocument.render(document))
        case None           => ZIO.unit
      generations <- GenerationOwner.memory(root)
      preparer = ScriptPreparer(inspection(drift), solvedBy, solves, pause, fail, noticed)
      coordinator <- LockCoordinator.open(preparer, strategy, store, generations, command(runs))
    yield Harness(root, store, generations, coordinator, runs, solves)

  private def request(token: String, mode: Option[LockMode] = None): PrepareRequest =
    prepare(context(token), mode)

  private def prepare(context: InvocationContext, mode: Option[LockMode] = None): PrepareRequest =
    PrepareRequest(context, ResolutionIntent.Inherit, Quiet, mode, refresh = false)

  private def context(token: String): InvocationContext =
    InvocationContext(
      InvocationId.derive(token),
      ClientId("cli"),
      CommandIdentity.Compile(List(ProjectName("app"))),
      ExecutionKind.Local,
      Interaction.Offers,
      OfflineSelection.Online,
    )

  private def inspection(drift: DeclarationDrift): Inspection =
    Inspection(
      hash("declarations"),
      List(LockPhase.Seed, LockPhase.Plugins, LockPhase.Graph).map(PhaseInspection(_, drift)),
    )

  private def fixed(decision: LockDecision): LockStrategy =
    phased(_ => decision)

  private def phased(choose: LockPhase => LockDecision): LockStrategy =
    new LockStrategy:
      def decide(context: histos.lock.LockContext): IO[LockStrategyError, LockDecision] =
        ZIO.succeed(choose(context.phase))

  private def appSolved(label: String): Solved =
    Solved(List(app), List(pin(app, label)), Nil, Nil)

  private def replaced(label: String): LockDocument =
    LockDocument(List(pin(app, label), toolsPin), List(retain))

  private def pin(coordinate: PluginCoordinate, label: String): VersionedPin =
    VersionedPin(coordinate, LockSubject.Artifact, hash(label), None)

  private def published(
      dependency: histos.Dep,
      link: LinkStrategy,
      subject: LockSubject,
      label: String,
      locator: Option[SnapshotLocator],
  ): LockDocument =
    LockDocument(
      List(VersionedPin(PluginCoordinate.Published(dependency, link), subject, hash(label), locator)),
      Nil,
    )

  private def hash(text: String): ContentHash =
    Digests.content(text.getBytes(StandardCharsets.UTF_8))

  private val retainLine: String = s"retain ${shots.value} ${hash("snap-1").value}"

  private def asOffer(outcome: PrepareOutcome): Option[histos.lock.LockOffer] =
    outcome match
      case PrepareOutcome.Offered(offer)  => Some(offer)
      case PrepareOutcome.Committed(_, _) => None
      case PrepareOutcome.Blocked(_)      => None
      case PrepareOutcome.Selected(_)     => None

  private def refusal(outcome: PrepareOutcome): Option[LockRefusal] =
    outcome match
      case PrepareOutcome.Blocked(blocked) => Some(blocked.refusal)
      case PrepareOutcome.Offered(_)       => None
      case PrepareOutcome.Committed(_, _)  => None
      case PrepareOutcome.Selected(_)      => None

  private def offerOf(outcome: PrepareOutcome): IO[Nothing, histos.lock.LockOffer] =
    asOffer(outcome) match
      case Some(offer) => ZIO.succeed(offer)
      case None        => ZIO.dieMessage("the fixture offered a repair")

  private def write(path: Path, text: String): UIO[Unit] =
    AtomicReplace.write(path, text).foldZIO(fault => ZIO.dieMessage(fault.toString), ZIO.succeed(_))

  private def present(state: LockRevisionState): IO[Nothing, LockRevision] =
    state match
      case LockRevisionState.Present(revision) => ZIO.succeed(revision)
      case LockRevisionState.Absent            => ZIO.dieMessage("the fixture lock is present")

  private def failure[A](exit: Exit[LockRepairError, A]): Option[LockRepairError] =
    exit match
      case Exit.Success(_)     => None
      case Exit.Failure(cause) => cause.failureOption

  private def lockText(root: Path): UIO[String] =
    ZIO.attemptBlocking(Files.readString(root.resolve("dependencies.lock"), StandardCharsets.UTF_8)).orDie

  private def readIfPresent(path: Path): UIO[Option[String]] =
    ZIO
      .attemptBlocking:
        if Files.isRegularFile(path) then Some(Files.readString(path, StandardCharsets.UTF_8)) else None
      .orDie

  private def missing(path: Path): UIO[Boolean] =
    ZIO.attemptBlocking(!Files.exists(path)).orDie

  private def temp: ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-lock")).orDie): path =>
      ZIO.attemptBlocking(Workspace.deleteSync(path)).orDie

  private object Quiet extends PluginEvents.Invocation:
    def emit(event: InvocationPluginEvent): IO[PluginEventError, Unit] = ZIO.unit

  private final class ScriptPreparer(
      seen: Inspection,
      solvedBy: InvocationId => Option[Solved],
      solves: Ref[Int],
      pause: UIO[Unit],
      fail: Option[PluginDetail],
      noticed: InvocationId => UIO[Unit],
  ) extends LockPreparer:
    def inspect(
        request: PrepareRequest,
        revision: LockRevisionState,
        document: LockDocument,
    ): IO[PrepareFault, Inspection] =
      ZIO.succeed(seen)

    def solve(request: PrepareRequest, inspection: Inspection): IO[PrepareFault, Solved] =
      noticed(request.context.id) *> pause *> solves.update(_ + 1) *> fail.fold(pick(request.context.id))(detail =>
        ZIO.fail(PrepareFault.Solve(detail))
      )

    private def pick(id: InvocationId): IO[PrepareFault, Solved] =
      solvedBy(id) match
        case Some(solved) => ZIO.succeed(solved)
        case None         => ZIO.fail(PrepareFault.Solve(PluginDetail("missing candidate")))
  end ScriptPreparer
end LockTransactionSpec

private final case class Harness(
    root: Path,
    store: LockStore,
    generations: GenerationOwner,
    coordinator: LockCoordinator,
    runs: Ref[Int],
    solves: Ref[Int],
)
