package histos.doctor

import zio.json.*

/** GitHub documents the probe reads.
  *
  * The Scala name is the field. `@jsonAliases` accepts the wire name beside it. An `Option` field needs no default: a
  * missing key decodes as `None`. A default parameter is what would require `-Yretain-trees`.
  */
object GitHubWire:
  final case class Owner(
      login: String,
      @jsonAliases("type") kind: String,
  ) derives JsonDecoder

  final case class Repository(
      @jsonAliases("default_branch") defaultBranch: String,
      owner: Option[Owner],
      @jsonAliases("allow_merge_commit") allowMergeCommit: Option[Boolean],
      @jsonAliases("allow_squash_merge") allowSquashMerge: Option[Boolean],
      @jsonAliases("allow_rebase_merge") allowRebaseMerge: Option[Boolean],
  ) derives JsonDecoder

  final case class Permissions(
      contents: Option[String],
      @jsonAliases("pull_requests") pullRequests: Option[String],
      workflows: Option[String],
  ) derives JsonDecoder

  /** How much of an account an App installation covers. An unknown value fails the decode. */
  enum RepositorySelection derives JsonDecoder:
    @jsonHint("all") case All
    @jsonHint("selected") case Selected

  /** One row of `GET /orgs/{org}/installations`. */
  final case class AppInstallation(
      @jsonAliases("app_slug") appSlug: String,
      @jsonAliases("repository_selection") repositorySelection: RepositorySelection,
      permissions: Option[Permissions],
  ) derives JsonDecoder

  /** `totalCount` is the full number of installations. `installations` is the page that came back. */
  final case class AppInstallations(
      @jsonAliases("total_count") totalCount: Int,
      installations: List[AppInstallation],
  ) derives JsonDecoder

  final case class SecretName(name: String) derives JsonDecoder

  final case class SecretList(
      secrets: List[SecretName]
  ) derives JsonDecoder

  final case class StatusCheck(context: String) derives JsonDecoder

  final case class RuleParameters(
      @jsonAliases("required_status_checks") requiredStatusChecks: Option[List[StatusCheck]]
  ) derives JsonDecoder

  final case class BranchRule(
      @jsonAliases("type") kind: String,
      @jsonAliases("ruleset_id") rulesetId: Option[Long],
      @jsonAliases("ruleset_source_type") sourceType: Option[String],
      @jsonAliases("ruleset_source") source: Option[String],
      parameters: Option[RuleParameters],
  ) derives JsonDecoder

  final case class StatusContexts(
      contexts: Option[List[String]]
  ) derives JsonDecoder

  final case class Signatures(
      enabled: Option[Boolean]
  ) derives JsonDecoder

  final case class Protection(
      @jsonAliases("required_status_checks") requiredStatusChecks: Option[StatusContexts],
      @jsonAliases("required_signatures") requiredSignatures: Option[Signatures],
  ) derives JsonDecoder

  final case class WorkflowPermissions(
      @jsonAliases("default_workflow_permissions") defaultWorkflowPermissions: Option[String],
      @jsonAliases("can_approve_pull_request_reviews") canApprove: Option[Boolean],
  ) derives JsonDecoder

  def decode[A](body: String)(using JsonDecoder[A]): Option[A] =
    body.fromJson[A] match
      case Right(value) => Some(value)
      case Left(_)      => None
end GitHubWire
