package histos.supply

import histos.doctor.Outcome
import histos.plugin.{JobName, RepoPath, SecretName}

import zio.test.*

object SupplyChecksLawSpec extends ZIOSpecDefault:
  private val id     = SecretName("HISTOS_APP_ID")
  private val key    = SecretName("HISTOS_APP_PRIVATE_KEY")
  private val app    = PrAuthor.App(id, key)
  private val branch = BranchName.trusted("main")
  private val other  = BranchName.trusted("develop")
  private val job    = JobName("supply-chain")

  def spec = suite("supply check laws")(
    test("an unread secret is not a pass, and a known missing secret is a finding"):
      val sights = Sight.values.toList
      assertTrue:
        sights.forall: sight =>
          val outcome = one(run(view(secretsSight = sight, secrets = Set.empty), app), "supply.secret-missing")
          sight match
            case Sight.Known                     => found(outcome) && !passed(outcome)
            case Sight.Unseen | Sight.Unreadable => found(outcome) && !passed(outcome)
        && passed(one(run(view(secrets = Set(id.value, key.value)), app), "supply.secret-missing"))
    ,
    test("an unread app permission is not a pass, and a missing write is a finding"):
      assertTrue(
        Sight.values.forall: sight =>
          val outcome = one(run(view(appSight = sight, permissions = Map.empty), app), "supply.app-permissions")
          sight match
            case Sight.Known => found(outcome)
            case _           => found(outcome) && !passed(outcome)
        ,
        passed(
          one(
            run(
              view(permissions =
                Map(
                  AppPermission.Contents     -> Access.Write,
                  AppPermission.PullRequests -> Access.Write,
                  AppPermission.Workflows    -> Access.Write,
                )
              ),
              app,
            ),
            "supply.app-permissions",
          )
        ),
      )
    ,
    test("a token step passes only when it succeeded"):
      assertTrue:
        StepOutcome.values.forall: step =>
          val outcome = one(run(view(tokenStep = step), app), "supply.app-token")
          if step == StepOutcome.Success then passed(outcome) else found(outcome) && !passed(outcome)
    ,
    test("a required check passes only when the job was read"):
      assertTrue(
        passed(one(run(view(required = List(job)), app), "supply.required-check")),
        found(one(run(view(required = Nil), app), "supply.required-check")),
        Sight.values
          .filter(_ != Sight.Known)
          .forall: sight =>
            val outcome = one(run(view(rulesSight = sight), app), "supply.required-check")
            found(outcome) && !passed(outcome),
      )
    ,
    test("signing passes for an api signature or a squash-only ssh push, and a plain push is a finding"):
      val signed = view(signedCommits = true)
      val open   = view(signedCommits = false)
      val ssh    = CommitSigning.GitPushSshSigned(Identity.FirstDeveloper, SecretName("HISTOS_TOKEN"))
      assertTrue(
        passed(one(run(open, app), "supply.signing")),
        passed(one(run(signed, app, CommitSigning.ApiSigned), "supply.signing")),
        found(one(run(signed, app, CommitSigning.GitPush(Identity.FirstDeveloper)), "supply.signing")),
        passed(one(run(signed, app, ssh), "supply.signing")),
        found(
          one(run(signed.copy(mergeMethods = Set(MergeMethod.Squash, MergeMethod.Merge)), app, ssh), "supply.signing")
        ),
      )
    ,
    test("maven or github-actions overlap is a finding, and npm alone is not"):
      assertTrue(
        found(one(run(view(dependabot = List(Ecosystem.Maven)), app), "supply.dependabot-overlap")),
        found(one(run(view(dependabot = List(Ecosystem.GithubActions)), app), "supply.dependabot-overlap")),
        passed(one(run(view(dependabot = List(Ecosystem.Npm)), app), "supply.dependabot-overlap")),
        found(one(run(view(securityPrs = List(Ecosystem.Maven)), app), "supply.dependabot-security")),
        passed(one(run(view(securityPrs = List(Ecosystem.Npm)), app), "supply.dependabot-security")),
      )
    ,
    test("the graph and the alerts fail only when a snapshot is on and the switch is off"):
      assertTrue(
        passed(one(run(view(snapshot = false, graphOn = false), app), "supply.dependency-graph")),
        passed(one(run(view(snapshot = false, alertsOn = false), app), "supply.dependabot-alerts")),
        passed(one(run(view(snapshot = true, graphOn = true), app), "supply.dependency-graph")),
        found(one(run(view(snapshot = true, graphOn = false), app), "supply.dependency-graph")),
        found(one(run(view(snapshot = true, alertsOn = false), app), "supply.dependabot-alerts")),
      )
    ,
    test("a branch mismatch, a write token, a missing pin, and a disabled schedule are findings"):
      assertTrue(
        passed(one(run(view(), app), "supply.default-branch")),
        found(one(run(view(expected = other), app), "supply.default-branch")),
        passed(one(run(view(tokenWrite = false), app), "supply.default-token")),
        found(one(run(view(tokenWrite = true), app), "supply.default-token")),
        Sight.values
          .filter(_ != Sight.Known)
          .forall: sight =>
            found(one(run(view(flowSight = sight), app), "supply.default-token")) &&
              !passed(one(run(view(flowSight = sight), app), "supply.default-token")),
        passed(one(run(view(shaPins = true), app), "supply.sha-pin-policy")),
        found(one(run(view(shaPins = false), app), "supply.sha-pin-policy")),
        passed(one(run(view(disabled = None), app), "supply.schedule-disabled")),
        found(
          one(run(view(disabled = Some(RepoPath.trusted(".github/workflows/ci.yml"))), app), "supply.schedule-disabled")
        ),
      )
    ,
    test("a refusal status is a finding and a 200 is not"):
      val refused = run(view(refusals = List((ApiCall.Workflows, 401, "no"))), app)
      val read    = run(view(refusals = List((ApiCall.Workflows, 200, "ok"))), app)
      assertTrue(
        refused.exists:
          case Outcome.Found(SupplyDiagnostic.GitHubRefused(ApiCall.Workflows, 401, _)) => true
          case _                                                                        => false
        ,
        !read.exists:
          case Outcome.Found(SupplyDiagnostic.GitHubRefused(_, _, _)) => true
          case _                                                      => false,
      )
    ,
    test("an unread app install is not a pass"):
      assertTrue:
        Sight.values.forall: sight =>
          val outcome =
            one(run(view(appSight = sight, appInstalled = sight == Sight.Known), app), "supply.app-installed")
          sight match
            case Sight.Known => passed(outcome)
            case _           => found(outcome) && !passed(outcome)
    ,
    test("a blind answer for an author with a credential is four findings and never a pass"):
      assertTrue:
        Blind.values.forall: why =>
          val outcomes = SupplyChecks.blind(Some(RepoName.trusted("early-effect/histos")), policy(app), why)
          outcomes.length == 4 && outcomes.forall(found) && !outcomes.exists(passed),
  )

  private def policy(author: PrAuthor, signing: CommitSigning = CommitSigning.ApiSigned): SupplyChain =
    SupplyChain(updates = UpdatePolicy(author = author, signing = signing))

  private def run(seen: GitHubView, author: PrAuthor, signing: CommitSigning = CommitSigning.ApiSigned): List[Outcome] =
    SupplyChecks.run(seen, policy(author, signing))

  private def view(
      secretsSight: Sight = Sight.Known,
      secrets: Set[String] = Set("HISTOS_APP_ID", "HISTOS_APP_PRIVATE_KEY"),
      appSight: Sight = Sight.Known,
      appInstalled: Boolean = true,
      permissions: Map[AppPermission, Access] = Map(
        AppPermission.Contents     -> Access.Write,
        AppPermission.PullRequests -> Access.Write,
        AppPermission.Workflows    -> Access.Write,
      ),
      tokenStep: StepOutcome = StepOutcome.Success,
      rulesSight: Sight = Sight.Known,
      required: List[JobName] = List(JobName("supply-chain")),
      signedCommits: Boolean = true,
      mergeMethods: Set[MergeMethod] = Set(MergeMethod.Squash),
      dependabot: List[Ecosystem] = Nil,
      securityPrs: List[Ecosystem] = Nil,
      graphOn: Boolean = true,
      alertsOn: Boolean = true,
      snapshot: Boolean = false,
      tokenWrite: Boolean = false,
      flowSight: Sight = Sight.Known,
      shaPins: Boolean = true,
      disabled: Option[RepoPath] = None,
      refusals: List[(ApiCall, Int, String)] = Nil,
      expected: BranchName = BranchName.trusted("main"),
  ): GitHubView =
    GitHubView(
      repo = RepoName.trusted("early-effect/histos"),
      token = TokenKind.Github,
      appInstalled = appInstalled,
      permissions = permissions,
      secrets = secrets,
      required = required,
      signedCommits = signedCommits,
      mergeMethods = mergeMethods,
      rule = RuleSource.Absent,
      dependabot = dependabot,
      securityPrs = securityPrs,
      graphOn = graphOn,
      alertsOn = alertsOn,
      defaultBranch = branch,
      expectedBranch = expected,
      tokenWrite = tokenWrite,
      allowPrs = false,
      shaPins = shaPins,
      disabled = disabled,
      refusals = refusals,
      snapshot = snapshot,
      owner = RepoOwner.Organization("early-effect"),
      secretsSight = secretsSight,
      appSight = appSight,
      rulesSight = rulesSight,
      flowSight = flowSight,
      tokenStep = tokenStep,
    )

  private def one(outcomes: List[Outcome], id: String): Option[Outcome] =
    outcomes.find:
      case Outcome.Ok(check, _)         => check.value == id
      case Outcome.Found(found)         => found.check.value == id
      case Outcome.Skipped(check, _, _) => check.value == id

  private def passed(outcome: Option[Outcome]): Boolean =
    outcome match
      case Some(Outcome.Ok(_, _)) => true
      case _                      => false

  private def passed(outcome: Outcome): Boolean =
    outcome match
      case Outcome.Ok(_, _) => true
      case _                => false

  private def found(outcome: Option[Outcome]): Boolean =
    outcome match
      case Some(Outcome.Found(_)) => true
      case _                      => false

  private def found(outcome: Outcome): Boolean =
    outcome match
      case Outcome.Found(_) => true
      case _                => false
end SupplyChecksLawSpec
