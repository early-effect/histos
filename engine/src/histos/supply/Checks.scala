package histos.supply

import histos.doctor.{CheckId, Outcome}
import histos.plugin.{JobName, RepoPath}

/** A recorded GitHub view. Tests build one and never open a socket. */
final case class GitHubView(
    repo: RepoName,
    token: TokenKind,
    appInstalled: Boolean,
    permissions: Map[AppPermission, Access],
    secrets: Set[String],
    required: List[JobName],
    signedCommits: Boolean,
    mergeMethods: Set[MergeMethod],
    rule: RuleSource,
    dependabot: List[Ecosystem],
    securityPrs: List[Ecosystem],
    graphOn: Boolean,
    alertsOn: Boolean,
    defaultBranch: BranchName,
    expectedBranch: BranchName,
    tokenWrite: Boolean,
    allowPrs: Boolean,
    shaPins: Boolean,
    disabled: Option[RepoPath],
    refusals: List[(ApiCall, Int, String)],
    snapshot: Boolean,
    owner: RepoOwner,
    secretsSight: Sight,
    appSight: Sight,
    rulesSight: Sight,
    tokenStep: StepOutcome = StepOutcome.Success,
)

object SupplyChecks:
  val owned: Set[Ecosystem] = Set(Ecosystem.GithubActions, Ecosystem.Maven)

  /** Every id `run` can emit, apart from a refusal. A probe passes the ids it actually read. */
  val all: Set[String] = Set(
    "supply.app-installed",
    "supply.secret-missing",
    "supply.app-permissions",
    "supply.app-token",
    "supply.required-check",
    "supply.signing",
    "supply.dependabot-overlap",
    "supply.dependabot-security",
    "supply.dependency-graph",
    "supply.dependabot-alerts",
    "supply.default-branch",
    "supply.default-token",
    "supply.allow-prs",
    "supply.sha-pin-policy",
    "supply.schedule-disabled",
  )

  def run(view: GitHubView, policy: SupplyChain, covered: Set[String] = all): List[Outcome] =
    val refused = view.refusals.flatMap: (call, status, body) =>
      histos.doctor.RefusalMap.refused(call, status, body).map(Outcome.Found(_))
    val checks = List(
      installed(view),
      secret(view, policy),
      permission(view),
      token(view),
      required(view, policy),
      signing(view, policy),
      overlap(view),
      security(view),
      graph(view),
      alerts(view),
      branch(view),
      writes(view),
      allow(view),
      pins(view),
      schedule(view),
    ).filter(outcome => covered.contains(idOf(outcome)))
    refused ++ checks

  /** The three supply answers, plus App permissions, when nothing was read. A skip is not a pass. */
  def blind(repo: Option[RepoName], policy: SupplyChain, why: Blind): List[Outcome] =
    val secret = policy.updates.author.secretNames match
      case head :: _ => found(SupplyDiagnostic.SecretBlind(head, repo, why))
      case Nil       => ok("supply.secret-missing", "no secret is required")
    List(
      secret,
      found(SupplyDiagnostic.AppBlind(repo, why)),
      found(SupplyDiagnostic.AppPermissionsBlind(repo, why)),
      found(SupplyDiagnostic.RuleBlind(repo, why)),
    )

  private def idOf(outcome: Outcome): String =
    outcome match
      case Outcome.Ok(id, _)         => id.value
      case Outcome.Found(found)      => found.check.value
      case Outcome.Skipped(id, _, _) => id.value

  private def installed(view: GitHubView): Outcome =
    view.appSight match
      case Sight.Known =>
        if view.appInstalled then ok("supply.app-installed", s"installed on ${view.repo.value}")
        else found(SupplyDiagnostic.AppNotInstalled(view.repo))
      case Sight.Unseen     => found(SupplyDiagnostic.AppBlind(Some(view.repo), Blind.Refused))
      case Sight.Unreadable => found(SupplyDiagnostic.AppBlind(Some(view.repo), Blind.Unreadable))

  private def secret(view: GitHubView, policy: SupplyChain): Outcome =
    view.secretsSight match
      case Sight.Unseen =>
        blindSecret(view, policy, Blind.Refused)
      case Sight.Unreadable =>
        blindSecret(view, policy, Blind.Unreadable)
      case Sight.Known =>
        policy.updates.author.secretNames.find(name => !view.secrets.contains(name.value)) match
          case None       => ok("supply.secret-missing", "secrets are set")
          case Some(name) =>
            found(SupplyDiagnostic.SecretMissing(name, policy.checkName, view.repo, view.owner))

  private def blindSecret(view: GitHubView, policy: SupplyChain, why: Blind): Outcome =
    policy.updates.author.secretNames match
      case head :: _ => found(SupplyDiagnostic.SecretBlind(head, Some(view.repo), why))
      case Nil       => ok("supply.secret-missing", "no secret is required")

  private def permission(view: GitHubView): Outcome =
    view.appSight match
      case Sight.Unseen     => found(SupplyDiagnostic.AppPermissionsBlind(Some(view.repo), Blind.Refused))
      case Sight.Unreadable => found(SupplyDiagnostic.AppPermissionsBlind(Some(view.repo), Blind.Unreadable))
      case Sight.Known      =>
        val needed = List(AppPermission.Contents, AppPermission.PullRequests, AppPermission.Workflows)
        needed.find(item => view.permissions.get(item) != Some(Access.Write)) match
          case None       => ok("supply.app-permissions", "contents, pull-requests, and workflows are write")
          case Some(item) =>
            found(SupplyDiagnostic.AppPermissionMissing(item, Access.Write, s"${item.wire} must be write"))

  private def token(view: GitHubView): Outcome =
    view.tokenStep match
      case StepOutcome.Success => ok("supply.app-token", "the token step can mint")
      case other               => found(SupplyDiagnostic.AppTokenFailed(other))

  private def required(view: GitHubView, policy: SupplyChain): Outcome =
    view.rulesSight match
      case Sight.Unseen     => found(SupplyDiagnostic.RuleBlind(Some(view.repo), Blind.Refused))
      case Sight.Unreadable => found(SupplyDiagnostic.RuleBlind(Some(view.repo), Blind.Unreadable))
      case Sight.Known      =>
        if view.required.exists(_.value == policy.checkName.value) then
          ok("supply.required-check", policy.checkName.value)
        else
          found(
            SupplyDiagnostic.RequiredCheckMissing(
              view.defaultBranch,
              policy.checkName,
              view.required,
              view.rule,
              view.repo,
            )
          )

  private def signing(view: GitHubView, policy: SupplyChain): Outcome =
    if !view.signedCommits then ok("supply.signing", "signed commits are not required")
    else
      policy.updates.signing match
        case CommitSigning.ApiSigned => ok("supply.signing", "CommitSigning.ApiSigned satisfies require signed commits")
        case CommitSigning.GitPush(_) =>
          found(SupplyDiagnostic.SignedCommitsConflict(view.rule, policy.updates.signing, view.mergeMethods))
        case CommitSigning.GitPushSshSigned(_, _) if view.mergeMethods == Set(MergeMethod.Squash) =>
          ok("supply.signing", "squash-only accepts an ssh-signed push")
        case CommitSigning.GitPushSshSigned(_, _) =>
          found(SupplyDiagnostic.SignedCommitsConflict(view.rule, policy.updates.signing, view.mergeMethods))

  private def overlap(view: GitHubView): Outcome =
    val hit = view.dependabot.filter(owned.contains)
    hit match
      case head :: tail =>
        found(SupplyDiagnostic.DependabotOverlap(::(head, tail), RepoPath.trusted(".github/dependabot.yml")))
      case Nil => ok("supply.dependabot-overlap", "no overlapping ecosystem")

  private def security(view: GitHubView): Outcome =
    val hit = view.securityPrs.filter(owned.contains)
    hit match
      case head :: tail => found(SupplyDiagnostic.DependabotSecurityPrsOn(::(head, tail)))
      case Nil          => ok("supply.dependabot-security", "no overlapping security pull requests")

  private def graph(view: GitHubView): Outcome =
    if !view.snapshot || view.graphOn then ok("supply.dependency-graph", "dependency graph is on")
    else found(SupplyDiagnostic.DependencyGraphOff(view.repo))

  private def alerts(view: GitHubView): Outcome =
    if !view.snapshot || view.alertsOn then ok("supply.dependabot-alerts", "Dependabot alerts are on")
    else found(SupplyDiagnostic.DependabotAlertsOff(view.repo))

  private def branch(view: GitHubView): Outcome =
    if view.defaultBranch == view.expectedBranch then ok("supply.default-branch", view.defaultBranch.value)
    else found(SupplyDiagnostic.DefaultBranchMismatch(view.expectedBranch, view.defaultBranch, view.repo))

  private def writes(view: GitHubView): Outcome =
    if view.tokenWrite then found(SupplyDiagnostic.DefaultTokenWrite(view.repo))
    else ok("supply.default-token", "the default token is read")

  private def allow(view: GitHubView): Outcome =
    if view.allowPrs then found(SupplyDiagnostic.AllowActionsToCreatePrs(view.repo, view.token))
    else ok("supply.allow-prs", "Actions cannot create pull requests")

  private def pins(view: GitHubView): Outcome =
    if view.shaPins then ok("supply.sha-pin-policy", "full sha pins are required")
    else found(SupplyDiagnostic.ShaPinPolicyOff(view.repo))

  private def schedule(view: GitHubView): Outcome =
    view.disabled match
      case Some(file) => found(SupplyDiagnostic.ScheduleDisabled(file, view.repo))
      case None       => ok("supply.schedule-disabled", "schedules are enabled")

  private def ok(id: String, note: String): Outcome =
    Outcome.Ok(CheckId.trusted(id), note)

  private def found(diagnostic: SupplyDiagnostic): Outcome =
    Outcome.Found(diagnostic)
end SupplyChecks

object SetupPlan:
  def insert(source: String): String =
    if source.contains(".supplyChain(") then source
    else
      val line = "      .supplyChain(SupplyChain())\n"
      val at   = source.indexOf(".docCheck(")
      if at < 0 then source + "\n" + line else source.patch(at, line, 0)

  def idempotent(source: String): Boolean =
    insert(source) == insert(insert(source))
end SetupPlan

object ZipxImport:
  final case class Row(kind: String, text: String, mapped: Boolean)

  final case class Report(rows: List[Row], gate: Option[String], secrets: List[String], todos: List[String]):
    def mappedCount(kind: String): Int = rows.count(row => row.kind == kind && row.mapped)
    def unmapped: List[Row]            = rows.filterNot(_.mapped)

  def read(versions: String, workflows: String, dependabot: String): Report =
    val lines = versions.split('\n').toList.map(_.trim).filter(_.nonEmpty)
    val rows  = lines.flatMap(classify)
    val gate  =
      if workflows.contains("Introduced") then Some("Introduced")
      else if workflows.contains("All") then Some("All")
      else None
    val secrets = List("ZIPX_APP_ID", "ZIPX_APP_PRIVATE_KEY").filter(workflows.contains)
    val todos   = rows.filterNot(_.mapped).map(row => s"// TODO ${row.kind} ${row.text}")
    val eco     = List("npm", "docker", "github-actions", "maven").filter(dependabot.contains)
    Report(rows, gate, secrets ++ eco.map(name => s"dependabot:$name"), todos)

  private def classify(line: String): Option[Row] =
    def take(kind: String): Option[Row] =
      val start = line.indexOf(kind + "(")
      if start < 0 then None
      else
        val body   = line.drop(start + kind.length + 1).takeWhile(_ != ')')
        val mapped = kind match
          case "Pin" => false
          case _     => true
        Some(Row(kind, body, mapped))
    take("Lib").orElse(take("Plugin")).orElse(take("Action")).orElse(take("Pin"))
end ZipxImport
