package histos.supply

import histos.doctor.{Fix, FixApply, Outcome, ShellLine}
import histos.version.CommitSha
import histos.HttpUrl

import java.nio.file.Files
import java.time.{LocalDate, ZoneOffset}

import zio.{Ref, UIO, ZIO}
import zio.test.*

object ReleaseIndexSpec extends ZIOSpecDefault:
  private val today = Date.trusted("2026-11-05")
  private val old   = millis("2026-10-01")
  private val fresh = millis("2026-11-04")
  private val shaA  = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  private val shaB  = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

  private val lock =
    """direct dev.zio zio 1.2.0 binary
      |artifact dev.zio zio_3 1.2.0 abc central
      |""".stripMargin

  def spec = suite("ReleaseIndexSpec")(
    test("a release inside the cooldown is not applied"):
      plan(maven("1.2.1", fresh), Map.empty).map: proposal =>
        assertTrue(!proposal.text.contains("apply Libraries"), proposal.text.contains("deps plan"))
    ,
    test("a pre-release is skipped"):
      plan(maven("1.3.0-RC1", old), Map.empty).map: proposal =>
        assertTrue(!proposal.text.contains("1.3.0-RC1"))
    ,
    test("a major is listed and not applied"):
      plan(maven("2.0.0", old), Map.empty).map: proposal =>
        assertTrue(
          proposal.text.contains("listed Libraries dev.zio:zio 1.2.0 -> 2.0.0"),
          !proposal.text.contains("apply Libraries"),
        )
    ,
    test("a 0.x minor is a major when that is the policy"):
      val zero =
        """direct dev.zio zio 0.1.0 binary
          |artifact dev.zio zio_3 0.1.0 abc central
          |""".stripMargin
      val policy = SupplyChain(updates = UpdatePolicy(versions = VersionPolicy(zeroX = ZeroX.MinorIsMajor)))
      scripted(url => Reply(200, if url.contains("search.maven.org") then maven("0.2.0", old) else """{"id":1}"""))
        .flatMap: (net, _) =>
          Plan
            .propose(zero, Map.empty, policy, today, false, net, Map.empty)
            .map: proposal =>
              assertTrue(proposal.text.contains("listed Libraries dev.zio:zio 0.1.0 -> 0.2.0"))
    ,
    test("an action tag resolves to the commit sha"):
      val catalog = Map("actions/checkout" -> (shaA, "v1.2.0"))
      val tags    = s"""[{"name":"v1.2.1","commit":{"sha":"$shaB"}}]"""
      val commit  = """{"commit":{"committer":{"date":"2026-10-01T00:00:00Z"}}}"""
      scripted: url =>
        if url.contains("/tags") then Reply(200, tags)
        else if url.contains("/commits/") then Reply(200, commit)
        else Reply(200, """{"id":9}""")
      .flatMap: (net, _) =>
        Plan
          .propose("", catalog, SupplyChain(), today, false, net, Map.empty)
          .map: proposal =>
            assertTrue(proposal.text.contains(s"apply Actions actions/checkout v1.2.0 -> v1.2.1 $shaB"))
    ,
    test("a moved action repository fails the plan"):
      val catalog = Map("actions/checkout" -> (shaA, "v1.2.0"))
      scripted: url =>
        if url.contains("/tags") then Reply(200, s"""[{"name":"v1.2.1","commit":{"sha":"$shaB"}}]""")
        else if url.contains("/commits/") then
          Reply(200, """{"commit":{"committer":{"date":"2026-10-01T00:00:00Z"}}}""")
        else Reply(200, """{"id":2}""")
      .flatMap: (net, _) =>
        Plan
          .propose("", catalog, SupplyChain(), today, false, net, Map("actions/checkout" -> RepoId.trusted("1")))
          .either
          .map: result =>
            assertTrue(result.left.exists(_.isInstanceOf[SupplyFault.ActionRepoMoved]))
    ,
    test("update --yes rewrites a dependency literal"):
      ZIO
        .attempt(Files.createTempDirectory("histos-plan"))
        .flatMap: root =>
          Files.writeString(root.resolve("histos.scala"), """dep"dev.zio::zio:1.2.0""" + "\n")
          Files.writeString(root.resolve("dependencies.lock"), lock)
          scripted(url => Reply(200, if url.contains("search.maven.org") then maven("1.2.1", old) else """{"id":1}"""))
            .flatMap: (net, _) =>
              DepsSurface
                .update(root, SupplyChain(), net, today, true, false)
                .map: result =>
                  val written = Files.readString(root.resolve("histos.scala"))
                  assertTrue(result.code == 0, written.contains("dev.zio::zio:1.2.1")),
  )

  private def plan(page: String, known: Map[String, RepoId]) =
    scripted(url => Reply(200, if url.contains("search.maven.org") then page else """{"id":1}""")).flatMap: (net, _) =>
      Plan.propose(lock, Map.empty, SupplyChain(), today, false, net, known)

  private def maven(version: String, stamp: Long): String =
    s"""{"response":{"docs":[{"v":"$version","timestamp":$stamp}]}}"""

  private def millis(day: String): Long =
    LocalDate.parse(day).atStartOfDay(ZoneOffset.UTC).toInstant.toEpochMilli

  private def scripted(reply: String => Reply): UIO[(Net, Ref[List[Exchange]])] =
    Ref
      .make(List.empty[Exchange])
      .map: sent =>
        val net = new Net:
          def send(method: String, url: String, body: String, headers: List[(String, String)]) =
            val call = Exchange(method, url, body)
            sent.update(_ :+ call).as(reply(url))
        (net, sent)
end ReleaseIndexSpec

object GitHubFlowSpec extends ZIOSpecDefault:
  private val repo   = RepoName.trusted("early-effect/histos")
  private val branch = BranchName.trusted("histos/deps")
  private val main   = CommitSha.trusted("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
  private val first  = CommitSha.trusted("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
  private val second = CommitSha.trusted("cccccccccccccccccccccccccccccccccccccccc")
  private val token  = "token"

  def spec = suite("GitHubFlowSpec")(
    test("the second commit expects the first commit oid"):
      recording.flatMap: (net, sent) =>
        val calls = List(
          GhCall.UpdateRef(branch, main, true),
          GhCall.CommitOnBranch(branch, main, "deps: Libraries", List("histos.scala" -> "one")),
          GhCall.CommitOnBranch(branch, main, "deps: Actions", List("pins" -> "two")),
        )
        GhRun
          .execute(net, repo, token, calls, false)
          .flatMap: _ =>
            sent.get.map: callsMade =>
              val bodies = callsMade.map(_.body)
              assertTrue(
                bodies.exists(_.contains(s""""expectedHeadOid":"${main.value}"""")),
                bodies.exists(_.contains(s""""expectedHeadOid":"${first.value}"""")),
              )
    ,
    test("a dry run sends nothing"):
      recording.flatMap: (net, sent) =>
        GhRun
          .execute(net, repo, token, List(GhCall.UpdateRef(branch, main, true)), true)
          .flatMap: text =>
            sent.get.map(calls => assertTrue(calls.isEmpty, text.contains("dry-run")))
    ,
    test("a rejected expected head is HeadMoved"):
      val net = new Net:
        def send(method: String, url: String, body: String, headers: List[(String, String)]) =
          ZIO.succeed(Reply(422, """{"errors":[{"message":"expectedHeadOid"}]}"""))
      GhRun
        .execute(net, repo, token, List(GhCall.CommitOnBranch(branch, main, "deps: Libraries", Nil)), false)
        .either
        .map(result => assertTrue(result == Left(SupplyFault.HeadMoved(branch, main))))
    ,
    test("alert opens, updates, and closes advisory issues"):
      recording.flatMap: (net, sent) =>
        val policy = SupplyChain(security = SecurityFixes.AlertOnly)
        val found  = List(finding("GHSA-aaaa-bbbb-cccc"), finding("GHSA-dddd-eeee-ffff"))
        GhRun
          .alert(net, repo, token, policy, found, false)
          .flatMap: text =>
            sent.get.map: calls =>
              assertTrue(
                text.contains("open GHSA-dddd-eeee-ffff"),
                text.contains("update GHSA-aaaa-bbbb-cccc"),
                text.contains("close GHSA-1111-2222-3333"),
                calls.exists(_.url.contains("labels=histos-advisory")),
                calls.exists(_.url.endsWith("/issues/4")),
                calls.exists(_.url.endsWith("/issues/5")),
              )
    ,
    test("a fixable advisory opens the security pull request"):
      recording.flatMap: (net, sent) =>
        val policy = SupplyChain()
        GhRun
          .alert(net, repo, token, policy, List(finding("GHSA-aaaa-bbbb-cccc", List("1.2.3"))), false)
          .flatMap: text =>
            sent.get.map: calls =>
              assertTrue(
                text.contains("security opened"),
                calls.exists(call => call.method == "POST" && call.body.contains("histos/security")),
              )
    ,
    test("snapshot posts the lock manifest"):
      recording.flatMap: (net, sent) =>
        val body = SnapshotSubmit.json(
          SnapshotSubmit.fromLock("artifact dev.zio zio_3 1.2.0 abc central"),
          main.value,
          "refs/heads/main",
        )
        GhRun
          .snapshot(net, repo, token, body, false)
          .flatMap: _ =>
            sent.get.map: calls =>
              assertTrue(
                calls.exists(call => call.method == "POST" && call.url.contains("/dependency-graph/snapshots")),
                calls.exists(_.body.contains("pkg:maven/dev.zio/zio_3@1.2.0")),
              ),
  )

  private def finding(id: String, fixed: List[String] = Nil) =
    Finding(AdvisoryId.trusted(id), Severity.High, None, Nil, fixed, id, "2026-01-01")

  private def recording: UIO[(Net, Ref[List[Exchange]])] =
    Ref
      .make(List.empty[Exchange])
      .map: sent =>
        val net = new Net:
          def send(method: String, url: String, body: String, headers: List[(String, String)]) =
            val call  = Exchange(method, url, body)
            val reply =
              if url.contains("/graphql") && body.contains(main.value) then
                Reply(200, s"""{"data":{"createCommitOnBranch":{"commit":{"oid":"${first.value}"}}}}""")
              else if url.contains("/graphql") then
                Reply(200, s"""{"data":{"createCommitOnBranch":{"commit":{"oid":"${second.value}"}}}}""")
              else if url.contains("labels=") then
                Reply(
                  200,
                  """[{"number":4,"title":"GHSA-aaaa-bbbb-cccc"},{"number":5,"title":"GHSA-1111-2222-3333"}]""",
                )
              else if method == "GET" then Reply(200, "[]")
              else Reply(201, """{"number":8}""")
            sent.update(_ :+ call).as(reply)
        (net, sent)
end GitHubFlowSpec

object FixAndProbeSpec extends ZIOSpecDefault:
  private val repo = RepoName.trusted("early-effect/histos")
  private val main = BranchName.trusted("main")

  def spec = suite("FixAndProbeSpec")(
    test("without a terminal, GitHub settings are printed and not called"):
      runFix(tty = false, yes = false, answer = FixApply.Answer.Yes).map: (notes, settings, commands) =>
        assertTrue(
          notes.exists(_.contains("not applied (no terminal)")),
          notes.exists(_.contains("not applied (secret)")),
          settings.isEmpty,
          commands == List("./histos workflow --update"),
        )
    ,
    test("--yes does not apply a GitHub setting"):
      runFix(tty = true, yes = true, answer = FixApply.Answer.Yes).map: (notes, settings, _) =>
        assertTrue(settings.isEmpty, notes.exists(_.contains("does not confirm")))
    ,
    test("a terminal yes applies the GitHub setting"):
      runFix(tty = true, yes = false, answer = FixApply.Answer.Yes).map: (notes, settings, _) =>
        assertTrue(
          settings == List("https://github.com/early-effect/histos/settings/actions"),
          notes.exists(_.startsWith("applied:")),
        )
    ,
    test("a 200 permission view fills the checks that were read"):
      probe(200).flatMap: hit =>
        val ready = hit match
          case histos.doctor.GitHubProbe.Hit.Ready(read) => SupplyChecks.run(read.view, SupplyChain(), read.covered)
          case _                                         => Nil
        assertTrue(
          ready.exists(_.isInstanceOf[Outcome.Ok]),
          ready.exists:
            case Outcome.Ok(id, _) => id.value == "supply.app-installed"
            case _                 => false
          ,
          ready.exists:
            case Outcome.Ok(id, note) => id.value == "supply.required-check" && note.contains("supply-chain")
            case _                    => false,
        )
    ,
    test("a 403 is a refusal and does not invent a missing required check"):
      probe(403).flatMap: hit =>
        val ready = hit match
          case histos.doctor.GitHubProbe.Hit.Ready(read) => SupplyChecks.run(read.view, SupplyChain(), read.covered)
          case _                                         => Nil
        assertTrue(
          ready.exists:
            case Outcome.Found(diagnostic) => diagnostic.check.value == "supply.github-refused"
            case _                         => false
          ,
          !ready.exists:
            case Outcome.Found(diagnostic) => diagnostic.check.value == "supply.required-check"
            case _                         => false,
        ),
  )

  private def runFix(tty: Boolean, yes: Boolean, answer: FixApply.Answer) =
    for
      settings <- Ref.make(List.empty[String])
      commands <- Ref.make(List.empty[String])
      root     <- ZIO.attempt(Files.createTempDirectory("histos-fix"))
      notes    <- DoctorSurface.applyReported(
        root,
        List(
          Fix.Command(ShellLine.trusted("./histos workflow --update")),
          Fix.Command(ShellLine.trusted("gh secret set HISTOS_APP_ID --repo early-effect/histos")),
          Fix.Settings(HttpUrl.trusted("https://github.com/early-effect/histos/settings/actions"), "actions"),
          Fix.Settings(
            HttpUrl.trusted("https://github.com/early-effect/histos/settings/secrets/actions/new"),
            "secrets",
          ),
        ),
        DoctorSurface.Request(None, false, None, true, yes),
        tty,
        _ => ZIO.succeed(answer),
        line => commands.update(_ :+ line),
        url => settings.update(_ :+ url),
      )
      called   <- settings.get
      executed <- commands.get
    yield (notes, called, executed)

  private def probe(status: Int) =
    val ok =
      if status == 200 then
        (
          url: String
        ) =>
          if url.endsWith("/actions/permissions/workflow") then
            Reply(200, """{"default_workflow_permissions":"read","can_approve_pull_request_reviews":false}""")
          else if url.endsWith("/installation") then
            Reply(200, """{"permissions":{"contents":"write","pull_requests":"write","workflows":"write"}}""")
          else if url.endsWith("/actions/secrets") then
            Reply(200, """{"secrets":[{"name":"HISTOS_APP_ID"},{"name":"HISTOS_APP_PRIVATE_KEY"}]}""")
          else if url.contains("/protection") then
            Reply(
              200,
              """{"required_status_checks":{"contexts":["supply-chain"]},"required_signatures":{"enabled":true}}""",
            )
          else
            Reply(
              200,
              """{"id":9,"default_branch":"main","allow_squash_merge":true,"allow_merge_commit":false,"allow_rebase_merge":false}""",
            )
      else (_: String) => Reply(status, "no")
    val net = new Net:
      def send(method: String, url: String, body: String, headers: List[(String, String)]) =
        ZIO.succeed(ok(url))
    histos.doctor.GitHubProbe.read(net, repo, main, Some("token"))
end FixAndProbeSpec
