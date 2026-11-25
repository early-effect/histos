package histos.client

import histos.build.BuildEvent
import histos.cli.{Ask, CliParseError, Command, ConsentAsk, Grammar, InvocationLine}
import histos.cli.Stdio as Streams
import histos.lock.{
  CallIdentity,
  EnvelopeFault,
  InvocationEnvelope,
  LockDiff,
  LockEvent,
  LockMode,
  LockOffer,
  LockReply,
  LockRevisionState,
  NecessaryScope,
  PrecedenceFault,
}
import histos.plugin.{ClientId, CommandIdentity, Digests, PluginDetail, ProposalId}
import histos.server.ResolveAction
import histos.ProjectName
import histos.report.WireJson.given
import histos.suite.HistosSpec

import java.nio.charset.StandardCharsets

import zio.{Ref, ZIO}
import zio.json.*
import zio.test.*

object LockOfferSpec extends HistosSpec:
  private val utf8 = StandardCharsets.UTF_8

  private def parsed(args: String*) =
    Grammar.parseLine(args.toList).either

  private def offer: LockOffer =
    val hash = Digests.content("offer".getBytes(utf8))
    LockOffer(
      ProposalId.trusted(Digests.sha256("proposal".getBytes(utf8))),
      ClientId("cli"),
      LockRevisionState.Absent,
      hash,
      hash,
      NecessaryScope(PluginDetail.trusted("app"), LockDiff(Nil)),
      CommandIdentity.Compile(List(ProjectName("app"))),
    )

  def spec = suite("lock offer and lock grammar")(
    test("resolve --update with frozen or local is rejected") {
      for
        frozen <- parsed("resolve", "--update", "--lock=frozen")
        local  <- parsed("resolve", "--update", "--lock=local")
      yield assertTrue(
        frozen == Left(CliParseError.Lock(PrecedenceFault.UpdateConflict(LockMode.Frozen))),
        local == Left(CliParseError.Lock(PrecedenceFault.UpdateConflict(LockMode.Local))),
      )
    },
    test("resolve --update with offer is a full proposal and update is redundant but valid") {
      for
        offered <- parsed("resolve", "--update", "--lock=offer")
        updated <- parsed("resolve", "--update", "--lock=update")
      yield assertTrue(
        offered == Right(InvocationLine(Command.Resolve(ResolveAction.Update), Some(LockMode.Offer))),
        updated == Right(InvocationLine(Command.Resolve(ResolveAction.Update), Some(LockMode.Update))),
      )
    },
    test("resolve and resolve --check reject --lock") {
      for
        plain <- parsed("resolve", "--lock=offer")
        check <- parsed("resolve", "--check", "--lock=frozen")
      yield assertTrue(
        plain == Left(CliParseError.Lock(PrecedenceFault.Validation(ResolveAction.Verify))),
        check == Left(CliParseError.Lock(PrecedenceFault.Validation(ResolveAction.Check))),
      )
    },
    test("refresh rejects frozen, local, and a combination with update or check") {
      for
        frozen   <- parsed("resolve", "--refresh", "--lock=frozen")
        local    <- parsed("resolve", "--refresh", "app", "--lock=local")
        together <- parsed("resolve", "--refresh", "--update")
        checked  <- parsed("resolve", "--check", "--refresh")
      yield assertTrue(
        frozen == Left(CliParseError.Lock(PrecedenceFault.RefreshConflict(LockMode.Frozen))),
        local == Left(CliParseError.Lock(PrecedenceFault.RefreshConflict(LockMode.Local))),
        together == Left(CliParseError.Lock(PrecedenceFault.RefreshCombined)),
        checked == Left(CliParseError.Lock(PrecedenceFault.RefreshCombined)),
      )
    },
    test("refresh with offer or update is accepted and names its selectors") {
      for
        offered <- parsed("resolve", "--refresh", "--lock=offer")
        named   <- parsed("resolve", "--refresh", "app", "wire", "--lock=update")
      yield assertTrue(
        offered == Right(InvocationLine(Command.Resolve(ResolveAction.Refresh(Nil)), Some(LockMode.Offer))),
        named == Right(
          InvocationLine(
            Command.Resolve(ResolveAction.Refresh(List(ProjectName("app"), ProjectName("wire")))),
            Some(LockMode.Update),
          )
        ),
      )
    },
    test("a preparing command takes --lock and a plain command does not") {
      for
        compile <- parsed("compile", "app", "--lock=frozen")
        version <- parsed("version", "--lock=offer")
      yield assertTrue(
        compile == Right(InvocationLine(Command.Compile(List(ProjectName("app"))), Some(LockMode.Frozen))),
        version == Left(CliParseError.Lock(PrecedenceFault.NotPreparation)),
      )
    },
    test("a lock flag after a run's -- stays a program argument") {
      parsed("run", "app", "--", "--lock=update").map:
        case Right(InvocationLine(Command.Run(id, None, args, None, _), None)) =>
          assertTrue(id.value == "app", args == List("--lock=update"))
        case other =>
          assertTrue(other == Right(InvocationLine(Command.Version, None)))
    },
    test("a terminal can accept and a redirected CLI does not read") {
      val line = Streams(histos.cli.Attached.Terminal, histos.cli.Attached.Terminal)
      for
        reads  <- Ref.make(0)
        answer <- ConsentAsk.ask(
          line,
          offer,
          histos.server.Call.Compile(List(ProjectName("app"))),
          reads.update(_ + 1).as("y"),
        )
        count        <- reads.get
        blockedReads <- Ref.make(0)
        blocked      <- ConsentAsk.ask(
          Streams.redirected,
          offer,
          histos.server.Call.Compile(List(ProjectName("app"))),
          blockedReads.update(_ + 1).as("y"),
        )
        untouched <- blockedReads.get
      yield blocked match
        case Ask.Blocked(required) =>
          val text = required.summary.mkString("\n") + required.update + required.local
          assertTrue(
            answer == Ask.Answered(LockReply.Accept(offer.proposal, offer.base)),
            count == 1,
            untouched == 0,
            required.error == histos.lock.LockTransactionError.ConsentRequired(offer.proposal),
            text.contains(offer.proposal.value),
            required.update == "histos compile app --lock=update",
            required.local == "histos compile app --lock=local",
            !text.contains("[y/N]"),
            !text.contains("Accept this proposal?"),
          )
        case Ask.Answered(_) => assertTrue(false)
    },
    test("a bare call is skew and an envelope round-trips") {
      val call = histos.server.Call.Compile(List(ProjectName("app")))
      for posted <- CallerFacts.detached(call)
      yield
        val encoded = posted.toJson
        assertTrue(
          InvocationEnvelope.decode(call.toJson) == Left(EnvelopeFault.Skew),
          encoded.fromJson[InvocationEnvelope] == Right(posted),
          posted.lock.isEmpty,
          posted.context.execution == histos.lock.ExecutionKind.Local,
          posted.context.interaction == histos.plugin.Interaction.Unattended,
        )
    },
    test("a lock event prints the summary and does not prompt") {
      val lines = histos.cli.Show(java.nio.file.Path.of("."), BuildEvent.Lock(LockEvent.Offered(offer)))
      val text  = lines.map(_.text).mkString("\n")
      assertTrue(
        text.contains(offer.proposal.value),
        text.contains("base lock: absent"),
        !text.contains("[y/N]"),
        !text.contains("Accept this proposal?"),
      ) && assertTrue(
        CallIdentity
          .rerun(histos.server.Call.Compile(List(ProjectName("app"))), LockMode.Local)
          .contains("--lock=local")
      )
    },
  )
end LockOfferSpec
