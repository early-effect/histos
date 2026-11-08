package histos.doctor

import histos.plugin.SecretName
import histos.supply.{
  Access,
  ApiCall,
  AppPermission,
  BranchName,
  Net,
  PrAuthor,
  Reply,
  RepoName,
  Sight,
  SourceName,
  StepOutcome,
  SupplyChain,
  SupplyChecks,
  SupplyFault,
  OutageStrategy,
  UpdatePolicy,
  Venue,
}

import slayer.{any, stub, stubbed}

import zio.*
import zio.test.*

object GitHubProbeLawSpec extends ZIOSpecDefault:
  private val appPolicy =
    SupplyChain(updates =
      UpdatePolicy(author = PrAuthor.App(SecretName("HISTOS_APP_ID"), SecretName("HISTOS_APP_PRIVATE_KEY")))
    )
  private val repo       = RepoName.trusted("early-effect/histos")
  private val branch     = BranchName.trusted("main")
  private val api        = "https://api.github.com"
  private val secret     = SecretName("HISTOS_APP_ID").value
  private val repository =
    """{"default_branch":"main","owner":{"login":"early-effect","type":"Organization"},"allow_squash_merge":true,"allow_merge_commit":false,"allow_rebase_merge":false}"""

  def spec = suite("github probe laws")(
    test("no token and an empty job is absent, not a ready view"):
      read(JobEnv.empty, None, Nil, _ => ZIO.succeed(Reply(200, repository))).map: hit =>
        assertTrue(hit == GitHubProbe.Hit.Absent)
    ,
    test("a successful step marks the app known, and an unread secret list stays unseen"):
      val job = JobEnv(Map.empty, Some(StepOutcome.Success), Venue.Actions)
      read(job, None, Nil, _ => ZIO.dieMessage("sent").as(Reply(200, ""))).map:
        case GitHubProbe.Hit.Ready(found) =>
          assertTrue(
            found.view.appInstalled,
            found.view.appSight == Sight.Known,
            found.view.secretsSight == Sight.Unseen,
            found.view.tokenStep.contains(StepOutcome.Success),
            found.view.venue == Venue.Actions,
          )
        case _ => assertTrue(false)
    ,
    test("a job that already names a secret is a known set, present or absent"):
      val present = JobEnv(Map(secret -> true), None, Venue.Actions)
      val missing = JobEnv(Map(secret -> false), None, Venue.Actions)
      for
        on  <- read(present, None, List(secret), _ => ZIO.dieMessage("sent").as(Reply(200, "")))
        off <- read(missing, None, List(secret), _ => ZIO.dieMessage("sent").as(Reply(200, "")))
      yield assertTrue(
        ready(on).exists(view => view.secretsSight == Sight.Known && view.secrets.contains(secret)),
        ready(off).exists(view => view.secretsSight == Sight.Known && !view.secrets.contains(secret)),
      )
    ,
    test("a workflow document that decodes is known, and one that does not is unreadable"):
      for
        known <- probe(workflow = Reply(200, workflowKnown))
        bad   <- probe(workflow = Reply(200, "nope"))
      yield assertTrue(
        ready(known).exists: view =>
          view.flowSight == Sight.Known && !view.tokenWrite && !view.allowPrs && view.refusals.isEmpty,
        covered(known).exists(_.contains("supply.allow-prs")),
        ready(bad).exists(view => view.flowSight == Sight.Unreadable && view.refusals.isEmpty),
        covered(bad).exists(_.contains("supply.allow-prs")),
      )
    ,
    test("a workflow 401 or 403 is unseen without a refusal, and a 404 or 422 is a refusal that drops the setting"):
      val unseen  = List(401, 403)
      val refused = List(404, 422)
      for
        hidden <- ZIO.foreach(unseen)(status => probe(workflow = Reply(status, "no")))
        denied <- ZIO.foreach(refused)(status => probe(workflow = Reply(status, "no")))
      yield assertTrue(
        hidden
          .zip(unseen)
          .forall: (hit, status) =>
            ready(hit).exists(view => view.flowSight == Sight.Unseen && view.refusals.isEmpty) &&
              covered(hit).exists(_.contains("supply.allow-prs")) && status > 0,
        denied
          .zip(refused)
          .forall: (hit, status) =>
            ready(hit).exists: view =>
              view.flowSight == Sight.Unseen &&
                view.refusals.exists((call, code, _) => call == ApiCall.Workflows && code == status) &&
                !covered(hit).exists(_.contains("supply.allow-prs")),
      )
    ,
    test("a repository 5xx or a failed send is unreachable"):
      for
        down <- probe(repoStatus = 500)
        lost <- read(
          JobEnv.empty,
          Some("token"),
          Nil,
          _ => ZIO.fail(SupplyFault.ServiceDown(SourceName.trusted("net"), OutageStrategy.FailClosed)),
        )
      yield assertTrue(down == GitHubProbe.Hit.Unreachable, lost == GitHubProbe.Hit.Unreachable)
    ,
    test("secrets decode to known, a bad body is unreadable, and 401 is unseen"):
      for
        known  <- probe(secrets = Reply(200, s"""{"secrets":[{"name":"$secret"}]}"""))
        bad    <- probe(secrets = Reply(200, "nope"))
        denied <- probe(secrets = Reply(401, "no"))
      yield assertTrue(
        ready(known).exists(view => view.secretsSight == Sight.Known && view.secrets.contains(secret)),
        ready(bad).exists(_.secretsSight == Sight.Unreadable),
        ready(denied).exists(_.secretsSight == Sight.Unseen),
      )
    ,
    test("a user token reads the org installation list: histos-deps on all repositories is installed"):
      probe(orgs = Reply(200, orgList(s"$cursor,$histosAll", 2))).map: hit =>
        assertTrue(
          ready(hit).exists: view =>
            view.appInstalled && view.appSight == Sight.Known &&
              view.permissions.get(AppPermission.Contents).contains(Access.Write) &&
              view.permissions.get(AppPermission.PullRequests).contains(Access.Write) &&
              view.permissions.get(AppPermission.Workflows).contains(Access.Write) &&
              view.tokenStep.isEmpty &&
              view.venue == Venue.Laptop
        )
    ,
    test("All proves the App, Selected and a truncated page prove nothing, and a complete page without it is absence"):
      val selected = s"""{"app_slug":"histos-deps","repository_selection":"selected","permissions":$grants}"""
      for
        all       <- probe(orgs = Reply(200, orgList(histosAll, 1)))
        picked    <- probe(orgs = Reply(200, orgList(selected, 1)))
        absent    <- probe(orgs = Reply(200, orgList(cursor, 1)))
        truncated <- probe(orgs = Reply(200, orgList(cursor, 150)))
      yield assertTrue(
        ready(all).exists(view => view.appInstalled && view.appSight == Sight.Known),
        ready(picked).exists(view => !view.appInstalled && view.appSight == Sight.Unseen),
        ready(absent).exists(view => !view.appInstalled && view.appSight == Sight.Known),
        ready(truncated).exists(view => !view.appInstalled && view.appSight == Sight.Unseen),
      )
    ,
    test("a refused org list is unseen, a body that does not decode is unreadable, and a 5xx is unreachable"):
      val refused = List(401, 403, 404)
      val strange = s"""{"app_slug":"histos-deps","repository_selection":"weird","permissions":$grants}"""
      for
        denied  <- ZIO.foreach(refused)(status => probe(orgs = Reply(status, "no")))
        bad     <- probe(orgs = Reply(200, "nope"))
        unknown <- probe(orgs = Reply(200, orgList(strange, 1)))
        down    <- probe(orgs = Reply(502, ""))
      yield assertTrue(
        denied.forall(hit => ready(hit).exists(view => !view.appInstalled && view.appSight == Sight.Unseen)),
        ready(bad).exists(view => !view.appInstalled && view.appSight == Sight.Unreadable),
        ready(unknown).exists(view => !view.appInstalled && view.appSight == Sight.Unreadable),
        down == GitHubProbe.Hit.Unreachable,
      )
    ,
    test("a repository owned by a user has no installation list, so the App stays unseen"):
      val owned = """{"default_branch":"main","owner":{"login":"russ","type":"User"}}"""
      probe(repoBody = owned, orgs = Reply(200, orgList(histosAll, 1))).map: hit =>
        assertTrue(ready(hit).exists(view => !view.appInstalled && view.appSight == Sight.Unseen))
    ,
    test("a run with no job step reports the app token as unproven, never as a pass"):
      probe().map: hit =>
        val outcomes = hit match
          case GitHubProbe.Hit.Ready(found) => SupplyChecks.run(found.view, appPolicy, found.covered)
          case _                            => Nil
        val skipped = outcomes.exists:
          case Outcome.Skipped(check, Probe.JobEnv, _) => check.value == "supply.app-token"
          case _                                       => false
        val passed = outcomes.exists:
          case Outcome.Ok(check, _) => check.value == "supply.app-token"
          case _                    => false
        assertTrue(skipped, !passed)
    ,
    test("rules that decode are known, a bad body is unreadable, and 401 is unseen"):
      for
        known  <- probe(rules = Reply(200, "[]"))
        bad    <- probe(rules = Reply(200, "nope"))
        denied <- probe(rules = Reply(401, "no"))
      yield assertTrue(
        ready(known).exists(_.rulesSight == Sight.Known),
        ready(bad).exists(_.rulesSight == Sight.Unreadable),
        ready(denied).exists(_.rulesSight == Sight.Unseen),
      ),
  )

  private val workflowKnown =
    """{"default_workflow_permissions":"read","can_approve_pull_request_reviews":false}"""

  private val grants = """{"contents":"write","metadata":"read","pull_requests":"write","workflows":"write"}"""

  private def orgList(rows: String, total: Int): String =
    s"""{"total_count":$total,"installations":[$rows]}"""

  private val cursor = """{"app_slug":"cursor","repository_selection":"all","permissions":{"contents":"write"}}"""

  private val histosAll =
    s"""{"app_slug":"histos-deps","repository_selection":"all","permissions":$grants}"""

  /** What the fake GitHub answers. A field the test does not set gets an ordinary reply. */
  private final case class Answers(
      workflow: Reply,
      secrets: Reply,
      orgs: Reply,
      rules: Reply,
      repoStatus: Int,
      repoBody: String,
  )

  private def probe(
      workflow: Reply = Reply(200, workflowKnown),
      secrets: Reply = Reply(200, """{"secrets":[]}"""),
      orgs: Reply = Reply(200, "{}"),
      rules: Reply = Reply(200, "[]"),
      repoStatus: Int = 200,
      repoBody: String = repository,
  ): UIO[GitHubProbe.Hit] =
    val answers = Answers(workflow, secrets, orgs, rules, repoStatus, repoBody)
    read(JobEnv.empty, Some("token"), Nil, url => answer(url, answers))

  private def answer(url: String, answers: Answers): UIO[Reply] =
    val reply =
      if url.endsWith("/actions/permissions/workflow") then answers.workflow
      else if url.contains("/organization-secrets") then Reply(200, """{"secrets":[]}""")
      else if url.endsWith("/actions/secrets") then answers.secrets
      else if url.contains("/orgs/") && url.contains("/installations") then answers.orgs
      else if url.contains("/rules/branches/") then answers.rules
      else if url.endsWith("/protection") then Reply(404, "")
      else if url == s"$api/repos/${repo.value}" then
        if answers.repoStatus >= 500 then Reply(answers.repoStatus, "") else Reply(200, answers.repoBody)
      else Reply(200, "{}")
    ZIO.succeed(reply)

  private def read(
      job: JobEnv,
      token: Option[String],
      needed: List[String],
      send: String => IO[SupplyFault, Reply],
  ): UIO[GitHubProbe.Hit] =
    val program =
      for
        _ <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
          (_: String, url: String, _: String, _: List[(String, String)]) => send(url)
        )
        net <- ZIO.service[Net]
        hit <- GitHubProbe.read(net, repo, branch, token, api, job, needed)
      yield hit
    program.provide(stubbed[Net])

  private def ready(hit: GitHubProbe.Hit): Option[histos.supply.GitHubView] =
    hit match
      case GitHubProbe.Hit.Ready(found) => Some(found.view)
      case _                            => None

  private def covered(hit: GitHubProbe.Hit): Option[Set[String]] =
    hit match
      case GitHubProbe.Hit.Ready(found) => Some(found.covered)
      case _                            => None
end GitHubProbeLawSpec
