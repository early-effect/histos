package histos.doctor

import histos.plugin.JobName
import histos.supply.{
  Access,
  ApiCall,
  AppPermission,
  BranchName,
  Ecosystem,
  GitHubView,
  MergeMethod,
  Net,
  Reply,
  RepoName,
  RuleSource,
  Scan,
  SupplyChecks,
  SupplyFault,
  TokenKind,
}

import zio.{UIO, ZIO}

/** The permission view a token can actually read. A missing token skips; a refusal is a diagnostic. */
object GitHubProbe:
  final case class Read(view: GitHubView, covered: Set[String])

  enum Hit:
    case Absent
    case Unreachable
    case Ready(read: Read)

  def read(net: Net, repo: RepoName, expected: BranchName, token: Option[String]): UIO[Hit] =
    token.filter(_.nonEmpty) match
      case None        => ZIO.succeed(Hit.Absent)
      case Some(value) => load(net, repo, expected, value).catchAll(_ => ZIO.succeed(Hit.Unreachable))

  def ecosystems(dependabot: String): List[Ecosystem] =
    List(
      "github-actions" -> Ecosystem.GithubActions,
      "maven"          -> Ecosystem.Maven,
      "npm"            -> Ecosystem.Npm,
      "docker"         -> Ecosystem.Docker,
    ).collect { case (name, eco) if dependabot.contains(name) => eco }

  private final case class Piece(body: Option[String], covered: Set[String], refusals: List[(ApiCall, Int, String)])

  private def load(net: Net, repo: RepoName, expected: BranchName, token: String): ZIO[Any, SupplyFault, Hit] =
    val root = s"https://api.github.com/repos/${repo.value}"
    for
      repository <- get(net, root, token)
      workflow   <- get(net, s"$root/actions/permissions/workflow", token)
      install    <- get(net, s"$root/installation", token)
      secrets    <- get(net, s"$root/actions/secrets", token)
      protection <- get(net, s"$root/branches/${expected.value}/protection", token)
    yield
      if repository.status >= 500 then Hit.Unreachable
      else
        val repoPiece   = refusedOr(repository, ApiCall.Contents, Set("supply.default-branch"))
        val flowPiece   = refusedOr(workflow, ApiCall.Workflows, Set("supply.default-token", "supply.allow-prs"))
        val appPiece    = installation(install)
        val secretPiece = refusedOr(secrets, ApiCall.Contents, Set("supply.secret-missing"))
        val rulePiece   = protectionOf(protection)
        val covered     = repoPiece.covered ++ flowPiece.covered ++ appPiece.covered ++ secretPiece.covered ++
          rulePiece.covered + "supply.app-token" + "supply.dependabot-overlap"
        val refusals = repoPiece.refusals ++ flowPiece.refusals ++ appPiece.refusals ++ secretPiece.refusals ++
          rulePiece.refusals
        val defaultBranch = repoPiece.body
          .flatMap(body => Scan.strings(body, "default_branch").headOption)
          .flatMap(name => BranchName.from(name).toOption)
          .getOrElse(expected)
        val installed   = appPiece.body.isDefined
        val permissions = appPiece.body.fold(Map.empty[AppPermission, Access])(readPermissions)
        val secretNames = secretPiece.body.fold(Set.empty[String])(body => Scan.strings(body, "name").toSet)
        val required    = rulePiece.body.fold(List.empty[JobName]): body =>
          Scan.array(body, "contexts").flatMap(name => JobName.from(name).toOption)
        val signed     = rulePiece.body.flatMap(body => Scan.bool(body, "enabled")).getOrElse(false)
        val merges     = repoPiece.body.fold(Set.empty[MergeMethod])(readMerges)
        val tokenWrite = flowPiece.body
          .flatMap(body => Scan.strings(body, "default_workflow_permissions").headOption)
          .contains("write")
        val allowPrs =
          flowPiece.body.flatMap(body => Scan.bool(body, "can_approve_pull_request_reviews")).contains(true)
        val kind =
          if sys.env.get("GITHUB_ACTIONS").contains("true") && sys.env.get("GITHUB_TOKEN").contains(token) then
            TokenKind.Github
          else if installed then TokenKind.App
          else TokenKind.Pat
        val view = GitHubView(
          repo,
          kind,
          installed,
          permissions,
          secretNames,
          required,
          signed,
          merges,
          RuleSource.BranchProtection,
          Nil,
          Nil,
          true,
          true,
          defaultBranch,
          expected,
          tokenWrite,
          allowPrs,
          true,
          None,
          refusals,
          false,
        )
        Hit.Ready(Read(view, covered.intersect(SupplyChecks.all)))

  /** 404 on the installation endpoint means the App is not installed. */
  private def installation(reply: Reply): Piece =
    if reply.status == 404 then Piece(None, Set("supply.app-installed"), Nil)
    else refusedOr(reply, ApiCall.Contents, Set("supply.app-installed", "supply.app-permissions"))

  /** 404 on branch protection means there are no required checks. */
  private def protectionOf(reply: Reply): Piece =
    if reply.status == 404 then Piece(Some("{}"), Set("supply.required-check", "supply.signing"), Nil)
    else refusedOr(reply, ApiCall.Rules, Set("supply.required-check", "supply.signing"))

  private def refusedOr(reply: Reply, call: ApiCall, ids: Set[String]): Piece =
    if reply.status >= 200 && reply.status < 300 then Piece(Some(reply.body), ids, Nil)
    else if reply.status == 401 || reply.status == 403 || reply.status == 404 || reply.status == 422 then
      Piece(None, Set.empty, List((call, reply.status, reply.body)))
    else Piece(None, Set.empty, Nil)

  private def readPermissions(body: String): Map[AppPermission, Access] =
    List(
      AppPermission.Contents     -> "contents",
      AppPermission.PullRequests -> "pull_requests",
      AppPermission.Workflows    -> "workflows",
    ).flatMap: (item, wire) =>
      Scan
        .strings(body, wire)
        .headOption
        .flatMap:
          case "write" => Some(item -> Access.Write)
          case "read"  => Some(item -> Access.Read)
          case _       => None
    .toMap

  private def readMerges(body: String): Set[MergeMethod] =
    List(
      "allow_merge_commit" -> MergeMethod.Merge,
      "allow_squash_merge" -> MergeMethod.Squash,
      "allow_rebase_merge" -> MergeMethod.Rebase,
    ).collect { case (key, method) if Scan.bool(body, key).contains(true) => method }.toSet

  private def get(net: Net, url: String, token: String) =
    net.send(
      "GET",
      url,
      "",
      List("Authorization" -> s"Bearer $token", "Accept" -> "application/vnd.github+json"),
    )
end GitHubProbe
