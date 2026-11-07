package histos.doctor

import histos.plugin.SecretName
import histos.supply.{
  Access,
  ApiCall,
  AppPermission,
  BranchName,
  Net,
  Reply,
  RepoName,
  Sight,
  SourceName,
  StepOutcome,
  SupplyFault,
  OutageStrategy,
}

import slayer.{any, stub, stubbed}

import zio.*
import zio.test.*

object GitHubProbeLawSpec extends ZIOSpecDefault:
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
      val job = JobEnv(Map.empty, Some(StepOutcome.Success), true, None)
      read(job, None, Nil, _ => ZIO.dieMessage("sent").as(Reply(200, ""))).map:
        case GitHubProbe.Hit.Ready(found) =>
          assertTrue(
            found.view.appInstalled,
            found.view.appSight == Sight.Known,
            found.view.secretsSight == Sight.Unseen,
          )
        case _ => assertTrue(false)
    ,
    test("a job that already names a secret is a known set, present or absent"):
      val present = JobEnv(Map(secret -> true), None, true, None)
      val missing = JobEnv(Map(secret -> false), None, true, None)
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
    test("a missing installation is known and not installed, a bad body is unreadable, and a write grant is installed"):
      for
        missing <- probe(install = Reply(404, ""))
        bad     <- probe(install = Reply(200, "nope"))
        on      <- probe(install = Reply(200, installKnown))
      yield assertTrue(
        ready(missing).exists(view => !view.appInstalled && view.appSight == Sight.Known),
        ready(bad).exists(view => !view.appInstalled && view.appSight == Sight.Unreadable),
        ready(on).exists: view =>
          view.appInstalled && view.appSight == Sight.Known &&
            view.permissions.get(AppPermission.Contents).contains(Access.Write) &&
            view.permissions.get(AppPermission.PullRequests).contains(Access.Write) &&
            view.permissions.get(AppPermission.Workflows).contains(Access.Write),
      )
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

  private val installKnown =
    """{"permissions":{"contents":"write","pull_requests":"write","workflows":"write"}}"""

  private def probe(
      workflow: Reply = Reply(200, workflowKnown),
      secrets: Reply = Reply(200, """{"secrets":[]}"""),
      install: Reply = Reply(404, ""),
      rules: Reply = Reply(200, "[]"),
      repoStatus: Int = 200,
  ): UIO[GitHubProbe.Hit] =
    read(JobEnv.empty, Some("token"), Nil, url => answer(url, workflow, secrets, install, rules, repoStatus))

  private def answer(
      url: String,
      workflow: Reply,
      secrets: Reply,
      install: Reply,
      rules: Reply,
      repoStatus: Int,
  ): UIO[Reply] =
    val reply =
      if url.endsWith("/actions/permissions/workflow") then workflow
      else if url.contains("/organization-secrets") then Reply(200, """{"secrets":[]}""")
      else if url.endsWith("/actions/secrets") then secrets
      else if url.endsWith("/installation") then install
      else if url.contains("/rules/branches/") then rules
      else if url.endsWith("/protection") then Reply(404, "")
      else if url == s"$api/repos/${repo.value}" then
        if repoStatus >= 500 then Reply(repoStatus, "") else Reply(200, repository)
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
