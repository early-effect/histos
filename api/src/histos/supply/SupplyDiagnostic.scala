package histos.supply

import histos.doctor.{CheckId, Diagnostic, Fix, Level, Location, ShellLine}
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
  case SecretMissing(name: SecretName, usedBy: JobName, repo: RepoName)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.secret-missing"),
        Level.Error,
        s"${name.value} is not set",
        s"${name.value} is not set as a repository or organization secret.",
        s"${usedBy.value} uses it to mint the App token; without it the weekly run stops at preflight.",
        fixes(
          Fix.Command(ShellLine.trusted(s"gh secret set ${name.value} --repo ${repo.value}")),
          Fix.Settings(
            HttpUrl.trusted(s"https://github.com/${repo.value}/settings/secrets/actions/new"),
            "settings/secrets/actions/new",
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
              s"https://github.com/apps/histos-deps/installations/new/permissions?target_id=${repo.value}"
            ),
            "install the App on this repository",
          )
        ),
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
            HttpUrl.trusted("https://github.com/settings/apps/histos-deps/permissions"),
            "set the permission, then accept it on the installation",
          )
        ),
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

  case RequiredCheckMissing(branch: BranchName, expected: JobName, found: List[JobName])
      extends SupplyDiagnostic(
        CheckId.trusted("supply.required-check"),
        Level.Error,
        s"${expected.value} is not a required check",
        s"Branch protection on ${branch.value} requires [${found.map(_.value).mkString(", ")}]. It does not require ${expected.value}.",
        "A pull request with an unpinned action or a High advisory can still merge.",
        fixes(
          Fix.Settings(
            HttpUrl.trusted("https://github.com/settings/rules"),
            s"add status check \"${expected.value}\"",
          )
        ),
      )

  case SignedCommitsConflict(rule: RuleSource, signing: CommitSigning, mergeMethods: Set[MergeMethod])
      extends SupplyDiagnostic(
        CheckId.trusted("supply.signing"),
        Level.Error,
        "signed commits conflict with CommitSigning",
        s"${rule.wire} requires signed commits, and ${signingName(signing)} does not satisfy ${mergeMethods.map(_.wire).mkString(", ")}.",
        "Switch to CommitSigning.ApiSigned, or allow squash only.",
        fixes(Fix.Command(ShellLine.trusted("switch to CommitSigning.ApiSigned, or allow squash only"))),
      )

  case DependabotOverlap(ecosystems: ::[Ecosystem], file: RepoPath)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.dependabot-overlap"),
        Level.Warning,
        "Dependabot updates an ecosystem histos also bumps",
        s"${file.value} updates ${ecosystems.map(_.wire).mkString(", ")}. histos also bumps these.",
        "You would get two pull requests for the same bump, and Dependabot's would fail workflow --check.",
        fixes(Fix.Command(ShellLine.trusted("./histos setup supply-chain --fix"))),
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

  case AllowActionsToCreatePrs(repo: RepoName, token: TokenKind)
      extends SupplyDiagnostic(
        CheckId.trusted("supply.allow-prs"),
        if token == TokenKind.Github then Level.Error else Level.Info,
        "Actions may create pull requests",
        "Allow GitHub Actions to create and approve pull requests.",
        if token == TokenKind.Github then "A GITHUB_TOKEN author needs this setting. An App token does not."
        else "An App or a PAT opens the pull request, so this setting is not required.",
        fixes(
          Fix.Settings(
            HttpUrl.trusted(s"https://github.com/${repo.value}/settings/actions"),
            "Workflow permissions",
          )
        ),
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

enum RuleSource:
  case BranchProtection, Ruleset

  def wire: String = this match
    case BranchProtection => "branch protection"
    case Ruleset          => "a ruleset"
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

private def signingName(signing: CommitSigning): String = signing match
  case CommitSigning.ApiSigned              => "CommitSigning.ApiSigned"
  case CommitSigning.GitPush(_)             => "CommitSigning.GitPush"
  case CommitSigning.GitPushSshSigned(_, _) => "CommitSigning.GitPushSshSigned"
