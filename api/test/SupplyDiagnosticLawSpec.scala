package histos.supply

import histos.doctor.{Fix, Level}
import histos.plugin.{JobName, RepoPath, SecretName}

import zio.test.*

object SupplyDiagnosticLawSpec extends ZIOSpecDefault:
  private val secret = SecretName("HISTOS_TOKEN")
  private val job    = JobName("supply-chain")
  private val repo   = RepoName.trusted("early-effect/histos")
  private val branch = BranchName.trusted("main")
  private val org    = RepoOwner.Organization("early-effect")
  private val user   = RepoOwner.User("russ")

  def spec = suite("supply diagnostic laws")(
    test("each finding names its check, its level, and the fix that branch opens"):
      val missing    = SupplyDiagnostic.SecretMissing(secret, job, repo, org)
      val owned      = SupplyDiagnostic.SecretMissing(secret, job, repo, user)
      val blind      = SupplyDiagnostic.SecretBlind(secret, Some(repo), Blind.NoCredential)
      val install    = SupplyDiagnostic.AppNotInstalled(repo)
      val token      = SupplyDiagnostic.AppTokenFailed(StepOutcome.Failure)
      val rule       = SupplyDiagnostic.RequiredCheckMissing(branch, job, Nil, RuleSource.Absent, repo)
      val overlap    = SupplyDiagnostic.DependabotOverlap(::(Ecosystem.Maven, Nil), RepoPath("dependabot.yml"))
      val alerts     = SupplyDiagnostic.DependabotAlertsOff(repo)
      val graph      = SupplyDiagnostic.DependencyGraphOff(repo)
      val branchGap  = SupplyDiagnostic.DefaultBranchMismatch(branch, BranchName.trusted("trunk"), repo)
      val tokenWrite = SupplyDiagnostic.DefaultTokenWrite(repo)
      val actions    = SupplyDiagnostic.AllowActionsToCreatePrs(repo, PrAuthor.Actions)
      val cannot     = SupplyDiagnostic.ActionsCannotOpenPullRequests(repo)
      val files      = SupplyDiagnostic.WorkflowFiles
      val pins       = SupplyDiagnostic.ShaPinPolicyOff(repo)
      val schedule   = SupplyDiagnostic.ScheduleDisabled(RepoPath(".github/workflows/ci.yml"), repo)
      val refused    = SupplyDiagnostic.GitHubRefused(ApiCall.Pulls, 403, "no")
      assertTrue(
        identity(missing) == ("supply.secret-missing", Level.Error),
        settings(missing) == List(
          "https://github.com/organizations/early-effect/settings/secrets/actions",
          s"https://github.com/${repo.value}/settings/secrets/actions/new",
        ),
        settings(owned) == List(s"https://github.com/${repo.value}/settings/secrets/actions/new"),
        identity(blind) == ("supply.secret-missing", Level.Error),
        steps(blind).nonEmpty,
        identity(install) == ("supply.app-installed", Level.Error),
        settings(install) == List(
          s"https://github.com/apps/histos-deps/installations/new/permissions?target_id=${repo.value}"
        ),
        identity(SupplyDiagnostic.AppBlind(None, Blind.Refused)) == ("supply.app-installed", Level.Error),
        identity(SupplyDiagnostic.AppPermissionMissing(AppPermission.Contents, Access.Write, "why")) ==
          ("supply.app-permissions", Level.Error),
        settings(SupplyDiagnostic.AppPermissionMissing(AppPermission.Workflows, Access.Read, "why")) == List(
          "https://github.com/settings/apps/histos-deps/permissions"
        ),
        identity(SupplyDiagnostic.AppPermissionsBlind(None, Blind.Unreadable)) == (
          "supply.app-permissions",
          Level.Error,
        ),
        identity(token) == ("supply.app-token", Level.Error),
        steps(token) == List("Open the histos-deps run and read the app step."),
        identity(SupplyDiagnostic.AppTokenUnreported) == ("supply.app-token", Level.Error),
        steps(SupplyDiagnostic.AppTokenUnreported).exists(_.contains("workflow --update")),
        commands(SupplyDiagnostic.AppTokenUnreported).isEmpty,
        AppSlug.histosDeps.value == "histos-deps",
        identity(rule) == ("supply.required-check", Level.Error),
        identity(SupplyDiagnostic.RuleBlind(None, Blind.Unreachable)) == ("supply.required-check", Level.Error),
        identity(overlap) == ("supply.dependabot-overlap", Level.Warning),
        commands(overlap) == List("./histos setup supply-chain --fix"),
        identity(SupplyDiagnostic.DependabotSecurityPrsOn(::(Ecosystem.Npm, Nil))) ==
          ("supply.dependabot-security", Level.Warning),
        settings(SupplyDiagnostic.DependabotSecurityPrsOn(::(Ecosystem.Npm, Nil))) == List(
          "https://github.com/settings/security_analysis"
        ),
        identity(graph) == ("supply.dependency-graph", Level.Error),
        commands(graph) == List(s"gh api -X PUT repos/${repo.value}/vulnerability-alerts"),
        settings(graph) == List(s"https://github.com/${repo.value}/settings/security_analysis"),
        identity(alerts) == ("supply.dependabot-alerts", Level.Error),
        commands(alerts) == List(s"gh api -X PUT repos/${repo.value}/vulnerability-alerts"),
        settings(alerts).isEmpty,
        identity(branchGap) == ("supply.default-branch", Level.Error),
        settings(branchGap) == List(s"https://github.com/${repo.value}/settings"),
        identity(tokenWrite) == ("supply.default-token", Level.Warning),
        identity(SupplyDiagnostic.DefaultTokenBlind(repo, Blind.NoCredential)) == (
          "supply.default-token",
          Level.Warning,
        ),
        identity(actions) == ("supply.allow-prs", Level.Info),
        identity(cannot) == ("supply.allow-prs", Level.Error),
        settings(cannot) == List(s"https://github.com/${repo.value}/settings/actions"),
        identity(files) == ("supply.workflow-files", Level.Warning),
        commands(files) == List("./histos workflow --update"),
        identity(SupplyDiagnostic.AllowPullRequestsBlind(repo, Blind.Refused)) == ("supply.allow-prs", Level.Warning),
        identity(pins) == ("supply.sha-pin-policy", Level.Info),
        identity(schedule) == ("supply.schedule-disabled", Level.Error),
        commands(schedule) == List("gh workflow enable .github/workflows/ci.yml"),
        settings(schedule) == List(s"https://github.com/${repo.value}/actions"),
        identity(refused) == ("supply.github-refused", Level.Error),
        steps(refused) == List("Read the 403 response for POST /pulls."),
      )
    ,
    test("a required-check fix opens the rule that was read"):
      val classic    = RuleSource.BranchProtection(branch)
      val repoRule   = RuleSource.RepositoryRuleset(9L, "local")
      val orgRule    = RuleSource.OrganizationRuleset("early-effect", 4L)
      val absent     = RuleSource.Absent
      val classicFix = SupplyDiagnostic.RequiredCheckMissing(branch, job, List(job), classic, repo)
      val repoFix    = SupplyDiagnostic.RequiredCheckMissing(branch, job, Nil, repoRule, repo)
      val orgFix     = SupplyDiagnostic.RequiredCheckMissing(branch, job, Nil, orgRule, repo)
      val absentFix  = SupplyDiagnostic.RequiredCheckMissing(branch, job, Nil, absent, repo)
      assertTrue(
        settings(classicFix) == List(s"https://github.com/${repo.value}/settings/branches"),
        settings(repoFix) == List(s"https://github.com/${repo.value}/rules/9"),
        settings(orgFix) == List("https://github.com/organizations/early-effect/settings/rules/4"),
        settings(absentFix) == List(s"https://github.com/${repo.value}/settings/rules"),
        classicFix.summary.contains(classic.wire),
        absentFix.summary.contains("No rule"),
      )
    ,
    test("signed commits name the signing arm, and allow-prs explains the author"):
      val signing = List(
        CommitSigning.ApiSigned                                         -> "CommitSigning.ApiSigned",
        CommitSigning.GitPush(Identity.FirstDeveloper)                  -> "CommitSigning.GitPush",
        CommitSigning.GitPushSshSigned(Identity.FirstDeveloper, secret) -> "CommitSigning.GitPushSshSigned",
      )
      val authors = List(
        PrAuthor.Actions                    -> "Actions bot",
        PrAuthor.App(secret, secret)        -> "An App",
        PrAuthor.OwnerOpens(secret, secret) -> "An App",
        PrAuthor.Pat(secret)                -> "A PAT",
      )
      val methods = Set(MergeMethod.Squash, MergeMethod.Rebase)
      assertTrue(
        signing.forall: (arm, token) =>
          val found = SupplyDiagnostic.SignedCommitsConflict(RuleSource.Absent, arm, methods)
          val ssh   = found.summary.contains("SshSigned")
          found.level == Level.Error &&
          found.check.value == "supply.signing" &&
          found.summary.contains(token) &&
          (token != "CommitSigning.GitPush" || !ssh) &&
          commands(found) == List("switch to CommitSigning.ApiSigned, or allow squash only")
        ,
        authors.forall: (author, token) =>
          val found = SupplyDiagnostic.AllowActionsToCreatePrs(repo, author)
          found.level == Level.Info && found.explain.contains(token),
      )
    ,
    test("a blind finding tells the reader what fits that blindness, and offers no command to run"):
      def blinds(why: Blind): List[SupplyDiagnostic] =
        List(
          SupplyDiagnostic.SecretBlind(secret, Some(repo), why),
          SupplyDiagnostic.AppBlind(Some(repo), why),
          SupplyDiagnostic.AppPermissionsBlind(Some(repo), why),
          SupplyDiagnostic.RuleBlind(Some(repo), why),
        )
      def lead(diagnostic: SupplyDiagnostic): String = steps(diagnostic).headOption.getOrElse("")
      val apps                                       = List(
        SupplyDiagnostic.AppBlind(Some(repo), Blind.Refused),
        SupplyDiagnostic.AppPermissionsBlind(Some(repo), Blind.Refused),
      )
      assertTrue(
        Blind.values.forall(why => blinds(why).forall(commands(_).isEmpty)),
        blinds(Blind.NoCredential).forall(lead(_).contains("GH_TOKEN")),
        List(Blind.Refused, Blind.Unreadable, Blind.Unreachable).forall: why =>
          blinds(why).forall(diagnostic => lead(diagnostic).nonEmpty && !lead(diagnostic).contains("GH_TOKEN")),
        blinds(Blind.Unreachable).forall(lead(_).contains("api.github.com")),
        blinds(Blind.Unreadable).forall(lead(_).contains("early-effect/histos")),
        apps.forall(lead(_).contains("generated workflow job")),
        apps.forall(allSteps(_).exists(_.contains("admin:read:org"))),
        apps.forall(allSteps(_).exists(_.contains("selected repositories"))),
        allSteps(SupplyDiagnostic.AppTokenUnreported).length == 2,
      )
    ,
    test("wire values and a step outcome are the protocol, and a blind lead is that case"):
      val outcomes = List(
        "success"   -> Some(StepOutcome.Success),
        "failure"   -> Some(StepOutcome.Failure),
        "cancelled" -> Some(StepOutcome.Failure),
        "skipped"   -> Some(StepOutcome.Skipped),
        "timed"     -> None,
      )
      val leads = List(
        Blind.NoCredential -> "No credential can answer",
        Blind.Refused      -> "This token cannot see",
        Blind.Unreadable   -> "GitHub returned a document we do not understand for",
        Blind.Unreachable  -> "GitHub could not be reached for",
      )
      assertTrue(
        AppPermission.values.map(_.wire).toSet == Set("contents", "pull-requests", "workflows"),
        Access.values.map(_.wire).toSet == Set("read", "write"),
        StepOutcome.Success.wire == "success",
        StepOutcome.Failure.wire == "failure",
        StepOutcome.Skipped.wire == "skipped",
        outcomes.forall((raw, parsed) => StepOutcome.parse(raw) == parsed),
        MergeMethod.values.map(_.wire).toSet == Set("merge", "squash", "rebase"),
        Ecosystem.values.map(_.wire).toSet == Set("github-actions", "maven", "npm", "docker"),
        ApiCall.Pulls.wire == "POST /pulls",
        ApiCall.VulnerabilityAlerts.wire == "PUT /vulnerability-alerts",
        ApiCall.Contents.wire == "contents",
        ApiCall.Workflows.wire == "workflows",
        ApiCall.Rules.wire == "rulesets",
        RuleSource.Absent.wire == "no branch rule",
        RuleSource.BranchProtection(branch).wire.contains(branch.value),
        leads.forall((blind, lead) => blind.lead == lead),
        Blind.values.forall: why =>
          val swapped = s"${why.lead} on ${repo.value}."
          val token   = SupplyDiagnostic.DefaultTokenBlind(repo, why).summary
          val allow   = SupplyDiagnostic.AllowPullRequestsBlind(repo, why).summary
          token.contains("workflow permissions") && token.contains(repo.value) && token != swapped && allow == token,
      ),
  )

  private def identity(diagnostic: SupplyDiagnostic): (String, Level) =
    (diagnostic.check.value, diagnostic.level)

  private def settings(diagnostic: SupplyDiagnostic): List[String] =
    diagnostic.fixes.toList.collect { case Fix.Settings(url, _) => url.value }

  private def commands(diagnostic: SupplyDiagnostic): List[String] =
    diagnostic.fixes.toList.collect { case Fix.Command(line) => line.value }

  private def steps(diagnostic: SupplyDiagnostic): List[String] =
    diagnostic.fixes.toList.collect:
      case Fix.Steps(head :: _) => head

  private def allSteps(diagnostic: SupplyDiagnostic): List[String] =
    diagnostic.fixes.toList.flatMap:
      case Fix.Steps(items) => items.toList
      case _                => Nil

end SupplyDiagnosticLawSpec
