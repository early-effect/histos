package histos

import histos.central.ReleaseFeed
import histos.doctor.{FixApply, GitHubCredential}
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.plugin.SecretName
import histos.supply.{DoctorSurface, GitAnswers, Net, PrAuthor, Reply, SupplyChain, SupplyFault, UpdatePolicy}
import histos.toolchain.Architecture
import histos.workspace.Workspace

import slayer.stubbed

import zio.*
import zio.test.*

/** The supply survey over one fake GitHub, run as a laptop and as a GitHub Actions job. No socket is opened. */
object DoctorVenueLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val user   = GitHubCredential.Token("user-token", "github.com")

  private val laptop  = Map("GITHUB_REPOSITORY" -> "early-effect/histos")
  private val actions = laptop + ("GITHUB_ACTIONS" -> "true")

  private val minted = actions ++ Map(
    "HISTOS_STEP_APP"                              -> "success",
    "HISTOS_SECRET_PRESENT_HISTOS_APP_ID"          -> "true",
    "HISTOS_SECRET_PRESENT_HISTOS_APP_PRIVATE_KEY" -> "true",
  )

  private val granted =
    Reply(
      200,
      """{"total_count":1,"installations":[{"app_slug":"histos-deps","repository_selection":"all","permissions":{"contents":"write","pull_requests":"write","workflows":"write"}}]}""",
    )

  private val refused = Reply(403, "Resource not accessible by personal access token")

  private val others =
    Reply(
      200,
      """{"total_count":1,"installations":[{"app_slug":"cursor","repository_selection":"all","permissions":{"contents":"write"}}]}""",
    )

  def spec = suite("doctor venue laws")(
    test("an organization owner on a laptop sees the App, and only the unrun mint step is skipped"):
      survey(laptop, granted).map: (exit, text) =>
        assertTrue(
          exit == 0,
          errors(text).isEmpty,
          ok(text).contains("supply.app-installed"),
          ok(text).contains("supply.app-permissions"),
          skipped(text) == List("supply.app-token"),
          text.contains("(needs JobEnv)"),
        )
    ,
    test("a laptop token that cannot list installations skips the App checks with the fix, and exits clean"):
      survey(laptop, refused).map: (exit, text) =>
        assertTrue(
          exit == 0,
          errors(text).isEmpty,
          skipped(text).toSet == Set("supply.app-installed", "supply.app-permissions", "supply.app-token"),
          text.contains("supply.app-installed  (needs GitHubAdmin)"),
          text.contains("admin:read:org"),
          text.contains("generated workflow job"),
        )
    ,
    test("the same refusal inside an Actions job fails the App checks and the exit"):
      survey(actions, refused).map: (exit, text) =>
        assertTrue(
          exit == 1,
          errors(text).toSet == Set("supply.app-installed", "supply.app-permissions", "supply.app-token"),
          skipped(text).isEmpty,
        )
    ,
    test("a complete list without the App is a real absence, so it fails on a laptop too"):
      survey(laptop, others).map: (exit, text) =>
        assertTrue(
          exit == 1,
          errors(text).toSet == Set("supply.app-installed", "supply.app-permissions"),
          skipped(text) == List("supply.app-token"),
        )
    ,
    test("a minted App token answers the App checks in Actions without reading the list"):
      survey(minted, refused).map: (exit, text) =>
        assertTrue(
          exit == 0,
          errors(text).isEmpty,
          skipped(text).isEmpty,
          ok(text).contains("supply.app-installed"),
          ok(text).contains("supply.app-permissions"),
          ok(text).contains("supply.app-token"),
        ),
  ).provideSomeLayer[TestEnvironment](stubbed[ReleaseFeed] ++ GitAnswers().layer) @@
    TestAspect.withLiveClock // probes run under a timeout

  private def survey(env: Map[String, String], installations: Reply) =
    ZIO.scoped:
      for
        repo  <- Workspace.work("histos-doctor-venue")
        built <- ZIO.fromEither(app)
        machine = Machine(
          Map.empty,
          "/home/demo",
          histos.jdk.Os.Mac,
          Architecture.Aarch64,
          repo,
          repo,
          None,
          NativeOrigin.Central,
        )
        result <- DoctorSurface.runWith(
          repo,
          built,
          DoctorSurface.Request(Some("supply-chain"), false, None, false, false, Nil, true),
          machine,
          FakeGitHub(installations),
          env,
          Some(user),
          false,
          fix => ZIO.dieMessage(s"doctor asked about $fix").as(FixApply.Answer.No),
          line => ZIO.dieMessage(s"doctor ran $line"),
          url => ZIO.dieMessage(s"doctor wrote $url"),
        )
      yield result

  private def app =
    Projects
      .organization(organization"example.com")
      .supplyChain(
        SupplyChain(updates =
          UpdatePolicy(author = PrAuthor.App(SecretName("HISTOS_APP_ID"), SecretName("HISTOS_APP_PRIVATE_KEY")))
        )
      )(module("core", Jvm)(scala3))

  private def errors(text: String): List[String] =
    ids(text, "  x error")

  private def ok(text: String): List[String] =
    ids(text, "  ok")

  private def skipped(text: String): List[String] =
    ids(text, "  -  skipped")

  /** The check id that follows `mark` on each line that starts with it. */
  private def ids(text: String, mark: String): List[String] =
    text.linesIterator.toList.collect:
      case line if line.startsWith(mark) =>
        line.drop(mark.length).trim.takeWhile(!_.isWhitespace)
end DoctorVenueLawSpec

/** Answers like an organization-owned repository. Only the installation list varies. */
private final class FakeGitHub(installations: Reply) extends Net:
  private val repository =
    """{"default_branch":"main","owner":{"login":"early-effect","type":"Organization"},"allow_squash_merge":true,"allow_merge_commit":false,"allow_rebase_merge":false}"""

  private val rule =
    """[{"type":"required_status_checks","ruleset_id":7,"ruleset_source_type":"Repository","ruleset_source":"early-effect/histos","parameters":{"required_status_checks":[{"context":"supply-chain"}]}}]"""

  def send(
      method: String,
      url: String,
      body: String,
      headers: List[(String, String)],
  ): zio.IO[SupplyFault, Reply] =
    ZIO.succeed(answer(url))

  private def answer(url: String): Reply =
    if url.endsWith("/actions/permissions/workflow") then
      Reply(200, """{"default_workflow_permissions":"read","can_approve_pull_request_reviews":false}""")
    else if url.contains("/organization-secrets") then Reply(200, """{"secrets":[]}""")
    else if url.endsWith("/actions/secrets") then
      Reply(200, """{"secrets":[{"name":"HISTOS_APP_ID"},{"name":"HISTOS_APP_PRIVATE_KEY"}]}""")
    else if url.contains("/orgs/") && url.contains("/installations") then installations
    else if url.contains("/rules/branches/") then Reply(200, rule)
    else if url.endsWith("/protection") then Reply(404, "")
    else if url == "https://api.github.com/repos/early-effect/histos" then Reply(200, repository)
    else Reply(404, "")
end FakeGitHub
