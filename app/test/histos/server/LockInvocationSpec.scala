package histos.server

import histos.doctor.Noted
import histos.lock.{
  ExecutionKind,
  LockBlocked,
  LockEvent,
  LockMode,
  LockRefusal,
  LockReply,
  LockRevisionState,
  LockSubject,
  LockTransactionError,
  NecessaryScope,
}
import histos.plugin.{
  ClientId,
  CommandIdentity,
  ContentHash,
  Digests,
  GenerationId,
  GenerationPluginEvent,
  Interaction,
  InvocationContext,
  InvocationId,
  InvocationPluginEvent,
  OfflineSelection,
  PluginCoordinate,
  PluginDetail,
  PluginEventError,
  ProposalId,
  ResolutionIntent,
}
import histos.resolve.{
  LockDocument,
  LocatorName,
  LockStoreFault,
  ResolutionSnapshot,
  ResolveError,
  SnapshotLocator,
  SnapshotProvenance,
  SnapshotSelection,
  VersionedPin,
}
import histos.workspace.Workspace
import histos.report.{Rejection, WireJson}
import histos.testrun.TestMode
import histos.ProjectName

import WireJson.given

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Queue, Ref, Scope, UIO, ZIO}
import zio.json.*
import zio.test.*

object LockInvocationSpec extends ZIOSpecDefault:
  private val utf8 = StandardCharsets.UTF_8

  private final case class Heard(context: InvocationContext, events: List[InvocationPluginEvent], built: Int)

  private def context(kind: ExecutionKind, command: CommandIdentity): InvocationContext =
    InvocationContext(
      InvocationId.derive(s"${kind.toString}:${command.productPrefix}"),
      ClientId("cli"),
      command,
      kind,
      Interaction.Unattended,
      OfflineSelection.Online,
    )

  private val compile = context(ExecutionKind.Local, CommandIdentity.Compile(Nil))
  private val testCmd = context(ExecutionKind.Ci, CommandIdentity.Test(Nil))

  private def proposal(seed: String): ProposalId =
    ProposalId.trusted(Digests.sha256(seed.getBytes(utf8)))

  private def hash(seed: String): ContentHash =
    Digests.content(seed.getBytes(utf8))

  private def scope: NecessaryScope =
    NecessaryScope(PluginDetail.trusted("none"), histos.lock.LockDiff(Nil))

  private def snapshot(selection: SnapshotSelection): ResolutionSnapshot =
    ResolutionSnapshot(
      ResolutionIntent.Inherit,
      LockRevisionState.Absent,
      hash("declarations"),
      Nil,
      SnapshotProvenance(compile.id, compile.client, compile.command),
      selection,
      GenerationId.trusted(Digests.sha256("generation".getBytes(utf8))),
    )

  def spec = suite("invocation desk, watch consent, and snapshot admission")(
    test("two callers share one construction and only their own events") {
      def caller(
          desk: InvocationDesk,
          gate: zio.Promise[Nothing, Unit],
          arrived: Ref[Int],
          who: InvocationContext,
          text: String,
      ) =
        ZIO.scoped {
          desk.open(who).flatMap { lease =>
            for
              seen  <- arrived.updateAndGet(_ + 1)
              _     <- ZIO.when(seen == 2)(gate.succeed(())).unit
              _     <- gate.await
              built <- desk.constructions
              _     <- lease.invocation.emit(InvocationPluginEvent.Message(PluginDetail.trusted(text)))
              heard <- lease.heard
            yield Heard(lease.context, heard, built)
          }
        }
      for
        desk       <- InvocationDesk.make
        gate       <- zio.Promise.make[Nothing, Unit]
        arrived    <- Ref.make(0)
        leftFiber  <- caller(desk, gate, arrived, compile, "left").fork
        rightFiber <- caller(desk, gate, arrived, testCmd, "right").fork
        left       <- leftFiber.join
        right      <- rightFiber.join
        _          <- desk.emitGeneration(
          GenerationPluginEvent.Diagnosis(
            Noted.carry("lock.generation", "generation", "separate channel", "histos compile app")
          )
        )
        gen <- desk.generationEvents
      yield
        val texts = List(left.events, right.events).map(_.collect { case InvocationPluginEvent.Message(detail) =>
          detail.value
        })
        assertTrue(
          left.built == 1,
          right.built == 1,
          left.context.command == compile.command,
          left.context.execution == ExecutionKind.Local,
          right.context.command == testCmd.command,
          right.context.execution == ExecutionKind.Ci,
          texts.contains(List("left")),
          texts.contains(List("right")),
          gen.length == 1,
        )
    },
    test("closing a lease and the desk rejects later events") {
      for
        desk   <- InvocationDesk.make
        closed <- ZIO.scoped {
          desk.open(compile).flatMap { lease =>
            lease.close *> lease.invocation.emit(InvocationPluginEvent.Message(PluginDetail.trusted("gone"))).either
          }
        }
        _      <- desk.close
        denied <- desk
          .emitGeneration(
            GenerationPluginEvent.Diagnosis(Noted.carry("lock.generation", "closed", "closed", "histos compile app"))
          )
          .either
      yield assertTrue(closed == Left(PluginEventError.Closed), denied == Left(PluginEventError.Closed))
    },
    test("watch resumes one accepted iteration and cancels an obsolete proposal") {
      val first  = proposal("one")
      val second = proposal("two")
      val third  = proposal("three")
      for
        consent    <- WatchConsent.make
        opened     <- consent.offer(first, "same", Interaction.Unattended)
        againOffer <- consent.offer(second, "same", Interaction.Unattended)
        resumed    <- consent.accept(LockReply.Accept(first, LockRevisionState.Absent))
        once       <- consent.resumeCount
        again      <- consent.accept(LockReply.Accept(first, LockRevisionState.Absent))
        still      <- consent.resumeCount
        fresh      <- consent.offer(third, "next", Interaction.Unattended)
        stale      <- consent.invalidate
        missed     <- consent.accept(LockReply.Accept(third, LockRevisionState.Absent))
        _          <- consent.offer(proposal("later"), "later", Interaction.Offers)
        held       <- WatchConsent.onChange(consent, Nil)
        pending    <- consent.invalidate
        _          <- consent.offer(proposal("file"), "file", Interaction.Offers)
        _          <- WatchConsent.onChange(consent, List(Path.of("src")))
        cleared    <- consent.invalidate
      yield assertTrue(
        opened == WatchOffer.Fresh(first),
        againOffer == WatchOffer.Suppressed,
        resumed == WatchAnswer.Resumed,
        again == WatchAnswer.Repeated,
        once == 1,
        still == 1,
        fresh == WatchOffer.Fresh(third),
        stale == Some(third),
        missed == WatchAnswer.Stale(histos.lock.LockTransactionError.StaleProposal(third)),
        pending.isDefined,
        cleared.isEmpty,
        held == (),
      )
    },
    test("an explicit lock runs only when that call applies it") {
      val app              = ProjectName("app")
      val compile          = Call.Compile(Nil)
      val testCall         = Call.Test(Nil, Nil, TestMode.Quick, histos.testrun.TestFilter.empty)
      val run              = Call.Run("app", None, RunLaunch.Tool(Map.empty, None), Nil)
      val update           = Call.Resolve(ResolveAction.Update)
      val refresh          = Call.Resolve(ResolveAction.Refresh(List(app)))
      val verify           = Call.Resolve(ResolveAction.Verify)
      val watch            = Call.Watch(compile, WatchPolicy.After)
      val offered          = LockTransactionError.Unapplied(LockMode.Offer).message
      val frozen           = LockTransactionError.Unapplied(LockMode.Frozen).message
      val help             = histos.cli.Help.entries.find(_.name == "resolve").map(_.about).getOrElse("")
      val verifySelectors  = histos.mcp.HistosMcp.resolveCall(ResolveAction.Verify, List("app"))
      val refreshSelectors = histos.mcp.HistosMcp.resolveCall(ResolveAction.Refresh(Nil), List("app"))
      assertTrue(
        LockDisposition.unapplied(update, None).isEmpty,
        LockDisposition.unapplied(update, Some(LockMode.Update)).isEmpty,
        LockDisposition.unapplied(update, Some(LockMode.Offer)).contains(LockMode.Offer),
        LockDisposition.unapplied(update, Some(LockMode.Frozen)).contains(LockMode.Frozen),
        LockDisposition.unapplied(update, Some(LockMode.Local)).contains(LockMode.Local),
        LockDisposition.unapplied(compile, None).isEmpty,
        LockDisposition.unapplied(compile, Some(LockMode.Frozen)).contains(LockMode.Frozen),
        LockDisposition.unapplied(compile, Some(LockMode.Local)).contains(LockMode.Local),
        LockDisposition.unapplied(compile, Some(LockMode.Offer)).contains(LockMode.Offer),
        LockDisposition.unapplied(testCall, Some(LockMode.Frozen)).contains(LockMode.Frozen),
        LockDisposition.unapplied(run, Some(LockMode.Local)).contains(LockMode.Local),
        LockDisposition.unapplied(refresh, Some(LockMode.Offer)).isEmpty,
        LockDisposition.unapplied(refresh, Some(LockMode.Update)).isEmpty,
        LockDisposition.unapplied(refresh, Some(LockMode.Frozen)).contains(LockMode.Frozen),
        LockDisposition.unapplied(refresh, Some(LockMode.Local)).contains(LockMode.Local),
        LockDisposition.unapplied(verify, Some(LockMode.Offer)).contains(LockMode.Offer),
        LockDisposition.unapplied(watch, Some(LockMode.Frozen)).contains(LockMode.Frozen),
        offered == "the offer was not applied",
        frozen.contains("frozen"),
        frozen.contains("was not applied"),
        help.contains("revalidates retained snapshot locators"),
        help.contains("no fetch"),
        !help.contains("does not revalidate"),
        !help.contains("snapshot refresh was not applied"),
        verifySelectors == Left(Rejection.ResolveAction("verify")),
        refreshSelectors == Right(Call.Resolve(ResolveAction.Refresh(List(app)))),
      )
    },
    test("refresh revalidates retained locators") {
      val shots = LocatorName("shots")
      val tools = LocatorName("tools")
      val pin   = VersionedPin(
        PluginCoordinate.Module(ProjectName("app")),
        LockSubject.Artifact,
        hash("app-1"),
        None,
      )
      def document(shotsDigest: ContentHash, toolsDigest: ContentHash) =
        LockDocument(List(pin), List(SnapshotLocator(shots, shotsDigest), SnapshotLocator(tools, toolsDigest)))
      def open(root: Path, seen: Ref[List[LockEvent]]) =
        ConsentDesk.open(
          root,
          ConsentDesk.strategy(ExecutionKind.Local, Some(LockMode.Offer)),
          ConsentDesk.command,
          ConsentDesk.preparer(root),
          event => seen.update(_ :+ event),
        )
      def scene(
          root: Path,
          shotsBytes: String,
          toolsBytes: String,
          shotsDigest: ContentHash,
          toolsDigest: ContentHash,
      ) =
        for
          _ <- writeText(root.resolve("dependencies.lock"), LockDocument.render(document(shotsDigest, toolsDigest)))
          _ <- writeBytes(root.resolve(shots.value), shotsBytes)
          _ <- writeBytes(root.resolve(tools.value), toolsBytes)
        yield ()
      ZIO.scoped:
        for
          offerRoot <- temp("histos-refresh-offer")
          oldShots  = hash("snap-1")
          newShots  = hash("snap-2")
          toolsHash = hash("tools-1")
          _         <- scene(offerRoot, "snap-2", "tools-1", oldShots, toolsHash)
          offered   <- Ref.make(List.empty[LockEvent])
          offerDesk <- open(offerRoot, offered)
          offerText <- Calls.refresh(offerDesk, offerRoot, Nil, Some(LockMode.Offer), None)
          held      <- readText(offerRoot.resolve("dependencies.lock"))
          events    <- offered.get
          accepted  <- events.collectFirst { case LockEvent.Offered(found) => found } match
            case Some(found) =>
              offerDesk
                .answer(LockReply.Accept(found.proposal, found.base))
                .foldZIO(
                  error => ZIO.dieMessage(error.message),
                  _ => readText(offerRoot.resolve("dependencies.lock")),
                )
            case None => ZIO.dieMessage("refresh did not offer")
          updateRoot <- temp("histos-refresh-update")
          _          <- scene(updateRoot, "snap-2", "tools-1", oldShots, toolsHash)
          updateSeen <- Ref.make(List.empty[LockEvent])
          updateDesk <- open(updateRoot, updateSeen)
          updateText <- Calls.refresh(
            updateDesk,
            updateRoot,
            List(ProjectName("shots")),
            Some(LockMode.Update),
            None,
          )
          written     <- readText(updateRoot.resolve("dependencies.lock"))
          toolsBytes  <- readBytes(updateRoot.resolve(tools.value))
          defaultRoot <- temp("histos-refresh-default")
          _           <- scene(defaultRoot, "snap-2", "tools-1", oldShots, toolsHash)
          defaultSeen <- Ref.make(List.empty[LockEvent])
          defaultDesk <- open(defaultRoot, defaultSeen)
          defaultText <- Calls.refresh(defaultDesk, defaultRoot, Nil, None, None)
          defaultLock <- readText(defaultRoot.resolve("dependencies.lock"))
          defaultFeed <- defaultSeen.get
          quietRoot   <- temp("histos-refresh-quiet")
          _           <- scene(quietRoot, "snap-1", "tools-1", oldShots, toolsHash)
          quietSeen   <- Ref.make(List.empty[LockEvent])
          quietDesk   <- open(quietRoot, quietSeen)
          quietText   <- Calls.refresh(quietDesk, quietRoot, Nil, None, None)
          quietLock   <- readText(quietRoot.resolve("dependencies.lock"))
          cellRoot    <- temp("histos-refresh-cell")
          cell = "cell app 3.8.1 jvm\nartifact com.example demo 1.0.0\n"
          _           <- writeText(cellRoot.resolve("dependencies.lock"), cell)
          cellSeen    <- Ref.make(List.empty[LockEvent])
          cellDesk    <- open(cellRoot, cellSeen)
          refused     <- Calls.refresh(cellDesk, cellRoot, Nil, Some(LockMode.Update), None).exit
          cellAfter   <- readText(cellRoot.resolve("dependencies.lock"))
          missingRoot <- temp("histos-refresh-missing")
          _           <- writeText(
            missingRoot.resolve("dependencies.lock"),
            LockDocument.render(document(oldShots, toolsHash)),
          )
          _           <- writeBytes(missingRoot.resolve(shots.value), "snap-1")
          _           <- writeBytes(missingRoot.resolve(tools.value), "tools-1")
          missingSeen <- Ref.make(List.empty[LockEvent])
          missingDesk <- open(missingRoot, missingSeen)
          unknown     <- Calls
            .refresh(missingDesk, missingRoot, List(ProjectName("missing")), Some(LockMode.Update), None)
            .exit
          missingLock <- readText(missingRoot.resolve("dependencies.lock"))
        yield
          val corrupt = refused match
            case zio.Exit.Success(_)     => false
            case zio.Exit.Failure(cause) =>
              cause.failureOption match
                case Some(ResolveError.LockDetail(detail)) =>
                  detail.value == "line 1 is not a lock record" &&
                  detail.value != LockStoreFault.Corrupt(detail).toString
                case Some(_) => false
                case None    => false
          val unknownError = unknown match
            case zio.Exit.Success(_)     => false
            case zio.Exit.Failure(cause) =>
              cause.failureOption.contains(
                ResolveError.Transaction(LockTransactionError.UnknownSelector(ProjectName("missing")))
              )
          val expected = LockDocument.render(document(newShots, toolsHash))
          assertTrue(
            namedOffer(events, offerText),
            !offerText.contains("snapshot refresh was not applied"),
            !offerText.contains("committed"),
            held == LockDocument.render(document(oldShots, toolsHash)),
            accepted == expected,
            updateText == s"snapshot refresh committed\nlocators shots",
            written == expected,
            toolsBytes.toList == "tools-1".getBytes(utf8).toList,
            namedOffer(defaultFeed, defaultText),
            !defaultText.contains("committed"),
            defaultLock == LockDocument.render(document(oldShots, toolsHash)),
            quietText == "no locator changed\nNo lock write.",
            quietLock == LockDocument.render(document(oldShots, toolsHash)),
            corrupt,
            cellAfter == cell,
            unknownError,
            missingLock == LockDocument.render(document(oldShots, toolsHash)),
          )
    },
    test("an ephemeral snapshot does not solve for a frozen caller") {
      val ephemeral = snapshot(SnapshotSelection.Ephemeral)
      val committed = snapshot(SnapshotSelection.Committed)
      for
        solves  <- Ref.make(0)
        blocked <- SnapshotAdmission.use(ephemeral, LockMode.Frozen, scope, solves.update(_ + 1)).either
        frozen  <- solves.get
        local   <- SnapshotAdmission.use(ephemeral, LockMode.Local, scope, solves.update(_ + 1)).either
        after   <- solves.get
        pinned  <- SnapshotAdmission.use(committed, LockMode.Frozen, scope, solves.update(_ + 1)).either
        end     <- solves.get
      yield assertTrue(
        blocked == Left(LockBlocked(LockRefusal.FrozenDrift, scope)),
        frozen == 0,
        local.isRight,
        after == 1,
        pinned.isRight,
        end == 2,
      )
    },
    test("program stdin stays program input and a consent reply stays consent") {
      val stdin = RunInput.Stdin(RunBytes.encode(zio.Chunk.fromArray("y".getBytes(utf8))))
      val reply = LockReply.Accept(proposal("reply"), LockRevisionState.Absent)
      for
        program <- Queue.unbounded[RunInput]
        consent <- Queue.unbounded[LockReply]
        _       <- ClientFrames.deliver(
          ClientFrames.classify(stdin.toJson),
          programOpen = true,
          (found: RunInput) => program.offer(found).unit,
          (found: LockReply) => consent.offer(found).unit,
          ZIO.unit,
        )
        _ <- ClientFrames.deliver(
          ClientFrames.classify(reply.toJson),
          programOpen = true,
          (found: RunInput) => program.offer(found).unit,
          (found: LockReply) => consent.offer(found).unit,
          ZIO.unit,
        )
        programs <- program.takeAll
        replies  <- consent.takeAll
      yield assertTrue(programs.toList == List(stdin), replies.toList == List(reply))
    },
    test("an unattended refresh consents locally and stays frozen in ci") {
      val shots    = LocatorName("shots")
      val recorded = hash("snap-1")
      val pin      = VersionedPin(
        PluginCoordinate.Module(ProjectName("app")),
        LockSubject.Artifact,
        hash("app-1"),
        None,
      )
      val document          = LockDocument(List(pin), List(SnapshotLocator(shots, recorded)))
      val rendered          = LockDocument.render(document)
      def scene(root: Path) =
        writeText(root.resolve("dependencies.lock"), rendered) *>
          writeBytes(root.resolve(shots.value), "snap-2")
      def open(root: Path, kind: ExecutionKind) =
        ConsentDesk.open(
          root,
          ConsentDesk.strategy(kind, None),
          ConsentDesk.command,
          ConsentDesk.preparer(root),
          _ => ZIO.unit,
        )
      ZIO.scoped {
        for
          localRoot <- temp("histos-refresh-consent")
          ciRoot    <- temp("histos-refresh-frozen")
          _         <- scene(localRoot)
          _         <- scene(ciRoot)
          localDesk <- open(localRoot, ExecutionKind.Local)
          ciDesk    <- open(ciRoot, ExecutionKind.Ci)
          local     <- Calls
            .refresh(localDesk, localRoot, Nil, None, Some(context(ExecutionKind.Local, CommandIdentity.Refresh(Nil))))
            .exit
          frozen <- Calls
            .refresh(ciDesk, ciRoot, Nil, None, Some(context(ExecutionKind.Ci, CommandIdentity.Refresh(Nil))))
            .exit
          localLock <- readText(localRoot.resolve("dependencies.lock"))
          ciLock    <- readText(ciRoot.resolve("dependencies.lock"))
        yield
          val consent = local match
            case zio.Exit.Success(_)     => false
            case zio.Exit.Failure(cause) =>
              cause.failureOption match
                case Some(ResolveError.Transaction(LockTransactionError.ConsentRequired(_))) => true
                case Some(_)                                                                 => false
                case None                                                                    => false
          val blocked = frozen match
            case zio.Exit.Success(_)     => false
            case zio.Exit.Failure(cause) =>
              cause.failureOption.contains(
                ResolveError.Transaction(LockTransactionError.Unapplied(LockMode.Frozen))
              )
          assertTrue(consent, blocked, localLock == rendered, ciLock == rendered)
      }
    },
  )

  private def namedOffer(events: List[LockEvent], text: String): Boolean =
    events match
      case LockEvent.Offered(found) :: Nil =>
        text == s"snapshot refresh offered ${found.proposal.value}\nNo lock write."
      case _ => false

  private def writeText(path: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking(Files.writeString(path, text, utf8)).orDie.unit

  private def writeBytes(path: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking(Files.write(path, text.getBytes(utf8))).orDie.unit

  private def readText(path: Path): UIO[String] =
    ZIO.attemptBlocking(Files.readString(path, utf8)).orDie

  private def readBytes(path: Path): UIO[Array[Byte]] =
    ZIO.attemptBlocking(Files.readAllBytes(path)).orDie

  private def temp(prefix: String): ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory(prefix)).orDie): path =>
      ZIO.attemptBlocking(Workspace.deleteSync(path)).orDie
end LockInvocationSpec
