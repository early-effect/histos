package histos.server

import histos.doctor.Noted
import histos.lock.{
  ExecutionKind,
  LockBlocked,
  LockMode,
  LockRefusal,
  LockReply,
  LockRevisionState,
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
  PluginDetail,
  PluginEventError,
  ProposalId,
  ResolutionIntent,
}
import histos.resolve.{ResolutionSnapshot, SnapshotProvenance, SnapshotSelection}
import histos.report.{Rejection, WireJson}
import histos.testrun.TestMode
import histos.ProjectName

import WireJson.given

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{Queue, Ref, ZIO}
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
      val testCall         = Call.Test(Nil, Nil, TestMode.Quick)
      val run              = Call.Run("app", None, RunLaunch.Tool(Map.empty, None), Nil)
      val update           = Call.Resolve(ResolveAction.Update)
      val refresh          = Call.Resolve(ResolveAction.Refresh(List(app)))
      val verify           = Call.Resolve(ResolveAction.Verify)
      val watch            = Call.Watch(compile, WatchPolicy.After)
      val offered          = LockTransactionError.Unapplied(LockMode.Offer).message
      val frozen           = LockTransactionError.Unapplied(LockMode.Frozen).message
      val quiet            = LockDisposition.refreshText(Nil, None)
      val named            = LockDisposition.refreshText(List(app), Some(LockMode.Offer))
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
        quiet.contains("snapshot refresh was not applied"),
        quiet.contains("No lock write."),
        quiet.contains("disposition absent"),
        !quiet.contains("offer"),
        !quiet.contains("refresh mutable"),
        named.contains("disposition offer"),
        named.contains("selectors app"),
        !help.contains("refresh mutable snapshots"),
        verifySelectors == Left(Rejection.ResolveAction("verify")),
        refreshSelectors == Right(Call.Resolve(ResolveAction.Refresh(List(app)))),
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
  )
end LockInvocationSpec
