package histos.supply

import histos.doctor.{CheckId, Diagnostic, Fix, GitHubPut, HistosCommand, Level, Location, ShellLine}
import histos.plugin.{JobName, RepoPath, SecretName}
import histos.HttpUrl

private def fixes(head: Fix, tail: Fix*): ::[Fix] = ::(head, tail.toList)

private def lines(head: String, tail: String*): ::[String] = ::(head, tail.toList)

/** Supply-chain findings. The first fix is the one to apply. */
enum SupplyDiagnostic(
    val check: CheckId,
    val level: Level,
    val title: String,
    val summary: String,
    val explain: String,
    val fixes: ::[Fix],
) extends Diagnostic:
  case SecretMissing(name: SecretName, usedBy: JobName, repo: RepoName, owner: RepoOwner)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.secret-missing"),
        Level.Error,
        s"${name.value} is not set",
        s"${name.value} is not set as a repository or organization secret.",
        s"${usedBy.value} uses it to mint the App token. A repository secret counts. An organization secret shared with the repository counts.",
        secretFixes(name, repo, owner),
      )

  case SecretBlind(name: SecretName, repo: Option[RepoName], why: Blind)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.secret-missing"),
        Level.Error,
        s"${name.value} was not read",
        s"${why.lead} whether ${name.value} is set.",
        "A secret the workflow can read counts, including an organization secret shared with the repository.",
        blindFixes(
          why,
          lines(
            "This token cannot list Actions secrets. Run doctor in the generated workflow job, which reads secret presence from the workflow, or use a token with the repo scope and collaborator access to this repository."
          ),
        ),
      )

  case AppNotInstalled(repo: RepoName)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.app-installed"),
        Level.Error,
        "the histos-deps App is not installed",
        s"the histos-deps App is not installed on ${repo.value}.",
        "Without the installation, histos-deps cannot mint a token.",
        fixes(
          Fix.Settings(
            HttpUrl.trusted(
              s"https://github.com/apps/${AppSlug.histosDeps.value}/installations/new/permissions?target_id=${repo.value}"
            ),
            "install the App on this repository",
            None,
          )
        ),
      )

  case AppBlind(repo: Option[RepoName], why: Blind)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.app-installed"),
        Level.Error,
        "the App installation was not read",
        s"${why.lead} whether the histos-deps App is installed${onRepo(repo)}.",
        "An organization owner's user token reads GET /orgs/{org}/installations. The generated job reads HISTOS_STEP_APP from the mint step. An App installed on selected repositories only cannot be listed by a user token.",
        blindFixes(why, appRefused),
      )

  case AppPermissionMissing(permission: AppPermission, needed: Access, why: String)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.app-permissions"),
        Level.Error,
        s"the App lacks ${permission.wire}: ${needed.wire}",
        s"App histos-deps lacks ${permission.wire}: ${needed.wire}.",
        why,
        fixes(
          Fix.Settings(
            HttpUrl.trusted(s"https://github.com/settings/apps/${AppSlug.histosDeps.value}/permissions"),
            "set the permission, then accept it on the installation",
            None,
          )
        ),
      )

  case AppPermissionsBlind(repo: Option[RepoName], why: Blind)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.app-permissions"),
        Level.Error,
        "the App permissions were not read",
        s"${why.lead} whether the App has contents, pull requests, and workflows write${onRepo(repo)}.",
        "Those writes are what the weekly job requests. A token that cannot see the installation does not drop this check.",
        blindFixes(why, appRefused),
      )

  case AppTokenFailed(stepOutcome: StepOutcome)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.app-token"),
        Level.Error,
        "the App token step failed",
        s"the App token step finished ${stepOutcome.wire}.",
        "The secrets are present, and minting the token still failed.",
        fixes(
          Fix.Steps(
            lines(
              "Open the histos-deps run and read the app step.",
              "Confirm the App is installed on this repository and the private key matches HISTOS_APP_ID.",
            )
          )
        ),
      )

  case AppTokenUnreported
      extends SupplyDiagnostic(
        CheckId.trusted("supply.app-token"),
        Level.Error,
        "the App token step reported nothing",
        "this run did not pass HISTOS_STEP_APP, so the mint cannot be shown.",
        "The generated workflow and supply-chain jobs pass HISTOS_STEP_APP from the App token step. A job that runs doctor without it cannot show that the token minted.",
        fixes(
          Fix.Steps(
            lines(
              "If a generated job runs doctor without HISTOS_STEP_APP, run ./histos workflow --update and commit the result.",
              "A laptop run cannot answer this check: only the generated workflow and supply-chain jobs set HISTOS_STEP_APP, from the mint step.",
            )
          )
        ),
      )

  case RequiredCheckMissing(
      branch: BranchName,
      expected: JobName,
      found: List[JobName],
      rule: RuleSource,
      repo: RepoName,
  ) extends SupplyDiagnostic(
        CheckId.trusted("supply.required-check"),
        Level.Error,
        s"${expected.value} is not a required check",
        requiredSummary(branch, expected, found, rule),
        "A pull request with an unpinned action or a High advisory can still merge.",
        fixes(ruleFix(rule, repo, expected)),
      )

  case RuleBlind(repo: Option[RepoName], why: Blind)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.required-check"),
        Level.Error,
        "the required check was not read",
        s"${why.lead} the rule GitHub enforces on the default branch${onRepo(repo)}.",
        "Classic branch protection, a repository ruleset, or an organization ruleset can require the check. A 404 from classic protection means classic protection is off. It does not mean the required contexts are empty.",
        blindFixes(
          why,
          lines(
            "This token cannot read the default branch rules. Use a token with the repo scope on a repository you can read, or run doctor in the generated workflow job."
          ),
        ),
      )

  case SignedCommitsConflict(rule: RuleSource, signing: CommitSigning, mergeMethods: Set[MergeMethod])
      extends SupplyDiagnostic(
        CheckId.trusted("supply.signing"),
        Level.Error,
        "signed commits conflict with CommitSigning",
        s"${rule.wire} requires signed commits, and ${signingName(signing)} does not satisfy ${mergeMethods.map(_.wire).mkString(", ")}.",
        "Switch to CommitSigning.ApiSigned, or allow squash only.",
        fixes(Fix.Steps(lines("Switch to CommitSigning.ApiSigned, or allow squash only."))),
      )

  case DependabotOverlap(ecosystems: ::[Ecosystem], file: RepoPath)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.dependabot-overlap"),
        Level.Warning,
        "Dependabot updates an ecosystem histos also bumps",
        s"${file.value} updates ${ecosystems.map(_.wire).mkString(", ")}. histos also bumps these.",
        "You would get two pull requests for the same bump, and Dependabot's would fail workflow --check.",
        fixes(Fix.Histos(HistosCommand.of("setup", "supply-chain", "--fix"))),
      )

  case DependabotUnread(file: RepoPath)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.dependabot-overlap"),
        Level.Warning,
        s"${file.value} was not read",
        s"${file.value} is not readable text, so its overlap with histos was not checked.",
        "Doctor reads the ecosystems Dependabot updates there, because histos also bumps GitHub Actions and Maven.",
        fixes(Fix.Steps(lines(s"Make ${file.value} a readable UTF-8 file, then run histos doctor again."))),
      )

  case DependabotSecurityPrsOn(ecosystems: ::[Ecosystem])
      extends SupplyDiagnostic(
        CheckId.trusted("supply.dependabot-security"),
        Level.Warning,
        "Dependabot security updates overlap histos",
        s"Dependabot opens security pull requests for ${ecosystems.map(_.wire).mkString(", ")}.",
        "histos opens the histos/security pull request for those ecosystems.",
        fixes(
          Fix.Settings(
            HttpUrl.trusted("https://github.com/settings/security_analysis"),
            "security_analysis",
            None,
          )
        ),
      )

  case DependencyGraphOff(repo: RepoName)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.dependency-graph"),
        Level.Error,
        "the dependency graph is off",
        "Snapshot.OnPushToDefault needs the dependency graph.",
        "The snapshot submission is rejected while the graph is off.",
        fixes(
          Fix.Command(ShellLine.trusted(s"gh api -X PUT repos/${repo.value}/vulnerability-alerts")),
          Fix.Settings(
            HttpUrl.trusted(s"https://github.com/${repo.value}/settings/security_analysis"),
            "security_analysis",
            Some(GitHubPut(HttpUrl.trusted(s"https://api.github.com/repos/${repo.value}/vulnerability-alerts"), "")),
          ),
        ),
      )

  case DependabotAlertsOff(repo: RepoName)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.dependabot-alerts"),
        Level.Error,
        "Dependabot alerts are off",
        "Dependabot alerts are off.",
        "The dependency graph is only half of what Snapshot needs.",
        fixes(Fix.Command(ShellLine.trusted(s"gh api -X PUT repos/${repo.value}/vulnerability-alerts"))),
      )

  case DefaultBranchMismatch(expected: BranchName, actual: BranchName, repo: RepoName)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.default-branch"),
        Level.Error,
        "the default branch does not match",
        s"generated triggers follow ${expected.value}, and the repository default branch is ${actual.value}.",
        "Rename the branch, or change the generated trigger.",
        fixes(
          Fix.Settings(
            HttpUrl.trusted(s"https://github.com/${repo.value}/settings"),
            "default branch",
            None,
          )
        ),
      )

  case DefaultTokenWrite(repo: RepoName)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.default-token"),
        Level.Warning,
        "the default GITHUB_TOKEN can write",
        "the repository default workflow token is read and write.",
        "Jobs that do not need write should not inherit it.",
        fixes(
          Fix.Command(
            ShellLine.trusted(
              s"gh api -X PUT repos/${repo.value}/actions/permissions/workflow -f default_workflow_permissions=read"
            )
          )
        ),
      )

  case DefaultTokenBlind(repo: RepoName, why: Blind)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.default-token"),
        Level.Warning,
        "the default token was not read",
        workflowSight(why, repo),
        "GET /repos/{owner}/{repo}/actions/permissions/workflow needs repository Administration read. The supply App token and the Actions token do not have it.",
        workflowSightFixes,
      )

  case AllowActionsToCreatePrs(repo: RepoName, author: PrAuthor)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.allow-prs"),
        allowLevel(author),
        "Actions may create pull requests",
        "Allow GitHub Actions to create and approve pull requests.",
        allowExplain(author),
        fixes(
          Fix.Settings(
            HttpUrl.trusted(s"https://github.com/${repo.value}/settings/actions"),
            "Workflow permissions",
            None,
          )
        ),
      )

  case ActionsCannotOpenPullRequests(repo: RepoName)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.allow-prs"),
        Level.Error,
        "Actions cannot open pull requests",
        "Allow GitHub Actions to create and approve pull requests is off.",
        "The weekly pull request is opened by the Actions bot. Turn the setting on, or name an App, an owner, or a PAT as the author.",
        fixes(
          Fix.Settings(
            HttpUrl.trusted(s"https://github.com/${repo.value}/settings/actions"),
            "Workflow permissions",
            None,
          )
        ),
      )

  case WorkflowFiles
      extends SupplyDiagnostic(
        CheckId.trusted("supply.workflow-files"),
        Level.Warning,
        "a person commits the workflow files",
        "The Actions bot does not commit .github/workflows.",
        "Run ./histos workflow --update and commit the result.",
        fixes(Fix.Histos(HistosCommand.of("workflow", "--update"))),
      )

  case AllowPullRequestsBlind(repo: RepoName, why: Blind)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.allow-prs"),
        Level.Warning,
        "pull request approval by Actions was not read",
        workflowSight(why, repo),
        "The same document says whether Actions may create and approve pull requests.",
        workflowSightFixes,
      )

  case ShaPinPolicyOff(repo: RepoName)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.sha-pin-policy"),
        Level.Info,
        "full-sha pin policy is off",
        "the repository does not require full sha action pins.",
        "histos still pins every generated uses. The GitHub policy is the backstop for hand-written files.",
        fixes(
          Fix.Settings(
            HttpUrl.trusted(s"https://github.com/${repo.value}/settings/actions"),
            "settings/actions",
            None,
          )
        ),
      )

  case ScheduleDisabled(workflow: RepoPath, repo: RepoName)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.schedule-disabled"),
        Level.Error,
        s"${workflow.value} is disabled",
        s"GitHub disabled the schedule on ${workflow.value}.",
        "A disabled schedule does not open the weekly pull request.",
        fixes(
          Fix.Command(ShellLine.trusted(s"gh workflow enable ${workflow.value}")),
          Fix.Settings(
            HttpUrl.trusted(s"https://github.com/${repo.value}/actions"),
            "enable the workflow",
            None,
          ),
        ),
      )

  case GitHubRefused(call: ApiCall, status: Int, detail: String)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.github-refused"),
        Level.Error,
        s"GitHub refused ${call.wire} with $status",
        detail,
        "The token cannot perform that call.",
        fixes(
          Fix.Steps(lines(s"Read the $status response for ${call.wire}.", "Grant the permission the response names."))
        ),
      )
end SupplyDiagnostic

enum AppPermission:
  case Contents, PullRequests, Workflows

  def wire: String = this match
    case Contents     => "contents"
    case PullRequests => "pull-requests"
    case Workflows    => "workflows"
end AppPermission

enum Access:
  case Read, Write

  def wire: String = this match
    case Read  => "read"
    case Write => "write"
end Access

enum StepOutcome:
  case Success, Failure, Skipped

  def wire: String = this match
    case Success => "success"
    case Failure => "failure"
    case Skipped => "skipped"
end StepOutcome

object StepOutcome:
  def parse(raw: String): Option[StepOutcome] =
    raw match
      case "success"               => Some(StepOutcome.Success)
      case "failure" | "cancelled" => Some(StepOutcome.Failure)
      case "skipped"               => Some(StepOutcome.Skipped)
      case _                       => None

/** Whether a probe read the fact, could not see it, or could not decode it. */
enum Sight:
  case Known, Unseen, Unreadable

/** Why a supply check failed closed instead of reporting a value. */
enum Blind:
  case NoCredential, Refused, Unreadable, Unreachable

  def lead: String = this match
    case NoCredential => "No credential can answer"
    case Refused      => "This token cannot see"
    case Unreadable   => "GitHub returned a document we do not understand for"
    case Unreachable  => "GitHub could not be reached for"
end Blind

/** Who owns the repository. The secret fix opens that owner's settings. */
enum RepoOwner:
  case Organization(login: String)
  case User(login: String)

/** The rule GitHub enforces on the default branch, when one was read. */
enum RuleSource:
  case BranchProtection(branch: BranchName)
  case RepositoryRuleset(id: Long, source: String)
  case OrganizationRuleset(org: String, id: Long)
  case Absent

  def wire: String = this match
    case BranchProtection(branch)      => s"branch protection on ${branch.value}"
    case RepositoryRuleset(id, source) => s"repository ruleset $id ($source)"
    case OrganizationRuleset(org, id)  => s"organization ruleset $id on $org"
    case Absent                        => "no branch rule"
end RuleSource

enum MergeMethod:
  case Merge, Squash, Rebase

  def wire: String = this match
    case Merge  => "merge"
    case Squash => "squash"
    case Rebase => "rebase"
end MergeMethod

enum Ecosystem:
  case GithubActions, Maven, Npm, Docker

  def wire: String = this match
    case GithubActions => "github-actions"
    case Maven         => "maven"
    case Npm           => "npm"
    case Docker        => "docker"
end Ecosystem

enum ApiCall:
  case Pulls, Contents, Workflows, VulnerabilityAlerts, Rules

  def wire: String = this match
    case Pulls               => "POST /pulls"
    case Contents            => "contents"
    case Workflows           => "workflows"
    case VulnerabilityAlerts => "PUT /vulnerability-alerts"
    case Rules               => "rulesets"
end ApiCall

private def onRepo(repo: Option[RepoName]): String =
  repo match
    case Some(name) => s" on ${name.value}"
    case None       => ""

private def credentialFixes: ::[Fix] =
  fixes(
    Fix.Steps(
      lines(
        "Set GH_TOKEN or GITHUB_TOKEN. For a host other than github.com or *.ghe.com, set GH_ENTERPRISE_TOKEN or GITHUB_ENTERPRISE_TOKEN.",
        "When the GitHub CLI is installed, histos reads gh auth token after those variables. gh auth login stores one.",
      )
    )
  )

/** The repair that fits why a check was blind. A skipped check prints every step, so the order is the order to try. */
private def blindFixes(why: Blind, refused: ::[String]): ::[Fix] =
  why match
    case Blind.NoCredential => credentialFixes
    case Blind.Refused      => fixes(Fix.Steps(refused))
    case Blind.Unreadable   =>
      fixes(
        Fix.Steps(
          lines(
            "GitHub returned a document histos does not decode. File an issue at https://github.com/early-effect/histos with this check id."
          )
        )
      )
    case Blind.Unreachable =>
      fixes(Fix.Steps(lines("Reach api.github.com, then run histos doctor again.")))

private def appRefused: ::[String] =
  lines(
    "This token cannot read the App installation. Run doctor in the generated workflow job, where the mint step answers it.",
    "For an organization-owned repository, a token with organization administration read also answers it: gh auth refresh -h github.com -s admin:read:org.",
    "An App installed on selected repositories only cannot be listed by a user token.",
  )

private def secretFixes(name: SecretName, repo: RepoName, owner: RepoOwner): ::[Fix] =
  owner match
    case RepoOwner.Organization(login) =>
      fixes(
        Fix.Settings(
          HttpUrl.trusted(s"https://github.com/organizations/$login/settings/secrets/actions"),
          s"share ${name.value} with this repository",
          None,
        ),
        Fix.Settings(
          HttpUrl.trusted(s"https://github.com/${repo.value}/settings/secrets/actions/new"),
          "a repository secret also counts",
          None,
        ),
      )
    case RepoOwner.User(_) =>
      fixes(
        Fix.Settings(
          HttpUrl.trusted(s"https://github.com/${repo.value}/settings/secrets/actions/new"),
          s"set ${name.value}",
          None,
        )
      )

private def requiredSummary(branch: BranchName, expected: JobName, found: List[JobName], rule: RuleSource): String =
  val seen = found.map(_.value).mkString(", ")
  rule match
    case RuleSource.Absent =>
      s"No rule on ${branch.value} requires ${expected.value}. Seen checks: [$seen]."
    case other =>
      s"${other.wire} requires [$seen]. It does not require ${expected.value}."

private def ruleFix(rule: RuleSource, repo: RepoName, expected: JobName): Fix =
  val (url, click) = rule match
    case RuleSource.BranchProtection(_) =>
      (s"https://github.com/${repo.value}/settings/branches", s"add status check \"${expected.value}\"")
    case RuleSource.RepositoryRuleset(id, _) =>
      (s"https://github.com/${repo.value}/rules/$id", s"add status check \"${expected.value}\"")
    case RuleSource.OrganizationRuleset(org, id) =>
      (s"https://github.com/organizations/$org/settings/rules/$id", s"add status check \"${expected.value}\"")
    case RuleSource.Absent =>
      (s"https://github.com/${repo.value}/settings/rules", s"add a rule that requires \"${expected.value}\"")
  Fix.Settings(HttpUrl.trusted(url), click, None)

private def workflowSight(why: Blind, repo: RepoName): String =
  val lead = why match
    case Blind.Refused      => "This token cannot see workflow permissions (Administration read)"
    case Blind.Unreadable   => "GitHub returned a document we do not understand for workflow permissions"
    case Blind.NoCredential => "No credential can answer workflow permissions"
    case Blind.Unreachable  => "GitHub could not be reached for workflow permissions"
  s"$lead on ${repo.value}."

private def workflowSightFixes: ::[Fix] =
  fixes(
    Fix.Steps(
      lines(
        "Read workflow permissions with a credential that has repository Administration read.",
        "The Actions token and the supply App token do not. A user token does.",
      )
    )
  )

private def allowLevel(author: PrAuthor): Level =
  author match
    case PrAuthor.Actions | PrAuthor.App(_, _) | PrAuthor.OwnerOpens(_, _) | PrAuthor.Pat(_) => Level.Info

private def allowExplain(author: PrAuthor): String =
  author match
    case PrAuthor.Actions =>
      "The Actions bot opens the pull request, so this setting is what lets it."
    case PrAuthor.App(_, _) | PrAuthor.OwnerOpens(_, _) =>
      "An App opens the pull request, so this setting is not required."
    case PrAuthor.Pat(_) =>
      "A PAT opens the pull request, so this setting is not required."

private def signingName(signing: CommitSigning): String = signing match
  case CommitSigning.ApiSigned              => "CommitSigning.ApiSigned"
  case CommitSigning.GitPush(_)             => "CommitSigning.GitPush"
  case CommitSigning.GitPushSshSigned(_, _) => "CommitSigning.GitPushSshSigned"
