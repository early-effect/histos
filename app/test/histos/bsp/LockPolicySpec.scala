package histos.bsp

import histos.cli.ConsentDecision
import histos.lock.{LockDiff, LockOffer, LockRevisionState, NecessaryScope}
import histos.mcp.McpConsent
import histos.plugin.{ClientId, CommandIdentity, Digests, PluginDetail, ProposalId}
import histos.server.Call
import histos.ProjectName
import histos.suite.HistosSpec

import java.nio.charset.StandardCharsets

import zio.test.*

object LockPolicySpec extends HistosSpec:
  private val utf8 = StandardCharsets.UTF_8

  private val call = Call.Compile(List(ProjectName("app")))

  private val offer: LockOffer =
    val hash = Digests.content("policy".getBytes(utf8))
    LockOffer(
      ProposalId.trusted(Digests.sha256("policy".getBytes(utf8))),
      ClientId("bsp"),
      LockRevisionState.Absent,
      hash,
      hash,
      NecessaryScope(PluginDetail.trusted("app"), LockDiff(Nil)),
      CommandIdentity.Compile(List(ProjectName("app"))),
    )

  private def text(decision: ConsentDecision): String =
    decision match
      case ConsentDecision.Diagnosis(noted) =>
        noted.summary + noted.explain + noted.fixes.map(_.toString).mkString("\n")
      case ConsentDecision.Prompt                              => "prompt"
      case ConsentDecision.Structured                          => "structured"
      case ConsentDecision.Required(_, summary, update, local) =>
        summary.mkString("\n") + update + local

  def spec = suite("mcp and bsp consent")(
    test("unnegotiated mcp and bsp return a diagnosis and the resolve hint stays") {
      val mcp = text(McpConsent.unnegotiated(offer, call))
      val bsp = text(BspConsent.unnegotiated(offer, call))
      assertTrue(
        mcp.contains("has not negotiated consent"),
        bsp.contains("has not negotiated consent"),
        mcp.contains("--lock=update"),
        bsp.contains("--lock=local"),
        !mcp.contains("[y/N]"),
        !bsp.contains("[y/N]"),
        !mcp.contains("Accept this proposal?"),
        !bsp.contains("Accept this proposal?"),
        BspConsent.updateHint == "histos resolve --update",
        bsp.contains(offer.proposal.value),
      )
    },
    test("negotiated mcp and bsp do not prompt") {
      assertTrue(
        histos.cli.ConsentRoute.decide(histos.cli.ClientSurface.Mcp(true), offer, call) == ConsentDecision.Structured,
        histos.cli.ConsentRoute.decide(histos.cli.ClientSurface.Bsp(true), offer, call) == ConsentDecision.Structured,
        histos.cli.ConsentRoute.decide(histos.cli.ClientSurface.Cli(true), offer, call) == ConsentDecision.Prompt,
      )
    },
  )
end LockPolicySpec
