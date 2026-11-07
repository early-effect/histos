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
  RepoOwner,
  RuleSource,
  Sight,
  SupplyFault,
  StepOutcome,
  TokenKind,
}

import zio.{UIO, ZIO}

/** What a token can read. A missing credential is fail-closed on the supply answers, not a skip. */
object GitHubProbe:
  final case class Read(view: GitHubView, covered: Set[String])

  enum Hit:
    case Absent
    case Unreachable
    case Ready(read: Read)

  def read(
      net: Net,
      repo: RepoName,
      expected: BranchName,
      token: Option[String],
      api: String,
      job: JobEnv,
      needed: List[String],
  ): UIO[Hit] =
    token.filter(_.nonEmpty) match
      case None =>
        if job.present.isEmpty && job.step.isEmpty then ZIO.succeed(Hit.Absent)
        else ZIO.succeed(offline(repo, expected, job, needed))
      case Some(value) => load(net, repo, expected, value, api, job, needed).catchAll(_ => ZIO.succeed(Hit.Unreachable))

  def ecosystems(dependabot: String): List[Ecosystem] =
    List(
      "github-actions" -> Ecosystem.GithubActions,
      "maven"          -> Ecosystem.Maven,
      "npm"            -> Ecosystem.Npm,
      "docker"         -> Ecosystem.Docker,
    ).collect { case (name, eco) if dependabot.contains(name) => eco }

  private val writes: Map[AppPermission, Access] = Map(
    AppPermission.Contents     -> Access.Write,
    AppPermission.PullRequests -> Access.Write,
    AppPermission.Workflows    -> Access.Write,
  )

  private def offline(repo: RepoName, expected: BranchName, job: JobEnv, needed: List[String]): Hit =
    val (secrets, secretsSight) = secretsFrom(job, needed, None, None)
    val app                     = appFrom(job, secretsSight, secrets, needed, None)
    val covered                 = cover(app.report, unseenRules, repoRead = false, flow = false)
    Hit.Ready(
      Read(
        view(
          repo,
          expected,
          expected,
          job,
          "",
          secrets,
          secretsSight,
          app,
          Nil,
          false,
          RuleSource.Absent,
          Sight.Unseen,
          ownerOf(repo, None),
          Set.empty,
          false,
          false,
          Nil,
          Sight.Unseen,
        ),
        covered,
      )
    )

  private def load(
      net: Net,
      repo: RepoName,
      expected: BranchName,
      token: String,
      api: String,
      job: JobEnv,
      needed: List[String],
  ): ZIO[Any, SupplyFault, Hit] =
    val root      = s"$api/repos/${repo.value}"
    val envCovers = needed.nonEmpty && needed.forall(job.present.contains)
    for
      repository <- get(net, root, token)
      workflow   <- get(net, s"$root/actions/permissions/workflow", token)
      install    <-
        if job.step.contains(StepOutcome.Success) then ZIO.succeed(None)
        else get(net, s"$root/installation", token).map(Some(_))
      repoSecrets <-
        if envCovers then ZIO.succeed(None)
        else get(net, s"$root/actions/secrets", token).map(Some(_))
      orgSecrets <-
        if envCovers then ZIO.succeed(None)
        else get(net, s"$root/actions/organization-secrets", token).map(Some(_))
      parsedRepo = GitHubWire.decode[GitHubWire.Repository](repository.body)
      branch     = parsedRepo.flatMap(doc => BranchName.from(doc.defaultBranch).toOption)
      protection <- branch match
        case Some(name) => get(net, s"$root/branches/${name.value}/protection", token).map(Some(_))
        case None       => ZIO.succeed(None)
      rules <- branch match
        case Some(name) => get(net, s"$root/rules/branches/${name.value}", token).map(Some(_))
        case None       => ZIO.succeed(None)
    yield
      val fetched = List(Some(repository), Some(workflow), install, repoSecrets, orgSecrets, protection, rules).flatten
      if fetched.exists(_.status >= 500) || repository.status >= 500 then Hit.Unreachable
      else
        val (secrets, secretsSight) = secretsFrom(job, needed, repoSecrets, orgSecrets)
        val app                     = appFrom(job, secretsSight, secrets, needed, install)
        val rulesRead               = rulesOf(branch, protection, rules, repo)
        val flow                    = workflowOf(workflow)
        val refusals                = repoRefusal(repository) ++ flow.refusals
        val defaultBranch           = branch.getOrElse(expected)
        val covered                 = cover(app.report, rulesRead, parsedRepo.isDefined, flow.report)
        Hit.Ready(
          Read(
            view(
              repo,
              defaultBranch,
              expected,
              job,
              token,
              secrets,
              secretsSight,
              app,
              rulesRead.required,
              rulesRead.signed,
              rulesRead.rule,
              rulesRead.sight,
              ownerOf(repo, parsedRepo.flatMap(_.owner)),
              parsedRepo.fold(Set.empty[MergeMethod])(merges),
              flow.write,
              flow.allow,
              refusals,
              flow.sight,
            ),
            covered,
          )
        )

  private final case class AppRead(
      installed: Boolean,
      permissions: Map[AppPermission, Access],
      sight: Sight,
      report: Boolean,
  )

  private final case class RulesRead(
      required: List[JobName],
      signed: Boolean,
      rule: RuleSource,
      sight: Sight,
      report: Boolean,
  )

  private final case class FlowRead(
      write: Boolean,
      allow: Boolean,
      sight: Sight,
      report: Boolean,
      refusals: List[(ApiCall, Int, String)],
  )

  private def secretsFrom(
      job: JobEnv,
      needed: List[String],
      repoSecrets: Option[Reply],
      orgSecrets: Option[Reply],
  ): (Set[String], Sight) =
    if needed.nonEmpty && needed.forall(job.present.contains) then (named(job, needed), Sight.Known)
    else
      val api = (repoSecrets, orgSecrets) match
        case (None, None)                      => Left(Sight.Unseen)
        case (Some(repoReply), Some(orgReply)) =>
          (namesOf(repoReply, emptyOn404 = false), namesOf(orgReply, emptyOn404 = true)) match
            case (Right(repoNames), Right(orgNames)) => Right(repoNames ++ orgNames)
            case (Left(sight), _)                    => Left(sight)
            case (_, Left(sight))                    => Left(sight)
        case _ => Left(Sight.Unseen)
      api match
        case Right(names) => (overlay(names, job), Sight.Known)
        case Left(sight)  => (Set.empty, sight)

  /** A 404 on organization secrets means this repository has none. A 404 on repository secrets does not. */
  private def namesOf(reply: Reply, emptyOn404: Boolean): Either[Sight, Set[String]] =
    if reply.status >= 200 && reply.status < 300 then
      GitHubWire.decode[GitHubWire.SecretList](reply.body) match
        case Some(doc) => Right(doc.secrets.map(_.name).toSet)
        case None      => Left(Sight.Unreadable)
    else if emptyOn404 && reply.status == 404 then Right(Set.empty)
    else Left(Sight.Unseen)

  private def named(job: JobEnv, needed: List[String]): Set[String] =
    needed.filter(name => job.present.get(name).contains(true)).toSet

  private def overlay(names: Set[String], job: JobEnv): Set[String] =
    job.present.foldLeft(names): (acc, item) =>
      val (name, on) = item
      if on then acc + name else acc - name

  private def appFrom(
      job: JobEnv,
      secretsSight: Sight,
      secrets: Set[String],
      needed: List[String],
      install: Option[Reply],
  ): AppRead =
    val missing = secretsSight == Sight.Known && needed.exists(name => !secrets.contains(name))
    job.step match
      case Some(StepOutcome.Success) => AppRead(true, writes, Sight.Known, true)
      case _                         =>
        install match
          case Some(reply) if reply.status == 404 =>
            AppRead(false, Map.empty, Sight.Known, true)
          case Some(reply) if reply.status >= 200 && reply.status < 300 =>
            GitHubWire.decode[GitHubWire.Installation](reply.body) match
              case Some(doc) =>
                AppRead(true, doc.permissions.fold(Map.empty)(permissionMap), Sight.Known, true)
              case None => AppRead(false, Map.empty, Sight.Unreadable, true)
          case _ if missing =>
            AppRead(false, Map.empty, Sight.Unseen, false)
          case _ =>
            AppRead(false, Map.empty, Sight.Unseen, true)

  private def rulesOf(
      branch: Option[BranchName],
      protection: Option[Reply],
      rules: Option[Reply],
      repo: RepoName,
  ): RulesRead =
    branch match
      case None       => RulesRead(Nil, false, RuleSource.Absent, Sight.Unseen, true)
      case Some(name) =>
        val classic = protection.fold(Classic.Blind(Sight.Unseen))(classicOf)
        val listed  = rules.fold(RulesBody.Blind(Sight.Unseen))(rulesBody)
        (classic, listed) match
          case (_, RulesBody.Open(docs)) =>
            val classicDoc = classic match
              case Classic.Open(doc) => Some(doc)
              case _                 => None
            val read = assemble(docs, classicDoc, name, repo)
            RulesRead(read.required, read.signed, read.rule, Sight.Known, true)
          case (_, RulesBody.Blind(Sight.Unreadable)) =>
            RulesRead(Nil, false, RuleSource.Absent, Sight.Unreadable, true)
          case _ =>
            RulesRead(Nil, false, RuleSource.Absent, Sight.Unseen, true)

  private enum Classic:
    case Off
    case Open(doc: GitHubWire.Protection)
    case Blind(sight: Sight)

  private enum RulesBody:
    case Open(rules: List[GitHubWire.BranchRule])
    case Blind(sight: Sight)

  private def classicOf(reply: Reply): Classic =
    if reply.status == 404 then Classic.Off
    else if reply.status >= 200 && reply.status < 300 then
      GitHubWire.decode[GitHubWire.Protection](reply.body) match
        case Some(doc) => Classic.Open(doc)
        case None      => Classic.Blind(Sight.Unreadable)
    else Classic.Blind(Sight.Unseen)

  private def rulesBody(reply: Reply): RulesBody =
    if reply.status >= 200 && reply.status < 300 then
      GitHubWire.decode[List[GitHubWire.BranchRule]](reply.body) match
        case Some(docs) => RulesBody.Open(docs)
        case None       => RulesBody.Blind(Sight.Unreadable)
    else RulesBody.Blind(Sight.Unseen)

  private def assemble(
      rules: List[GitHubWire.BranchRule],
      classic: Option[GitHubWire.Protection],
      branch: BranchName,
      repo: RepoName,
  ): RulesRead =
    val contexts = (ruleContexts(rules) ++ classic.toList.flatMap(classicContexts)).distinct
    val required = contexts.flatMap(name => JobName.from(name).toOption)
    val signed   = rules.exists(_.kind == "required_signatures") || classic.exists(classicSigned)
    val rule     = ruleSource(rules, classic.isDefined, branch, repo)
    RulesRead(required, signed, rule, Sight.Known, true)

  private def ruleContexts(rules: List[GitHubWire.BranchRule]): List[String] =
    rules
      .filter(_.kind == "required_status_checks")
      .flatMap: rule =>
        rule.parameters match
          case Some(params) =>
            params.requiredStatusChecks match
              case Some(checks) => checks.map(_.context)
              case None         => Nil
          case None => Nil

  private def classicContexts(doc: GitHubWire.Protection): List[String] =
    doc.requiredStatusChecks match
      case Some(checks) =>
        checks.contexts match
          case Some(values) => values
          case None         => Nil
      case None => Nil

  private def classicSigned(doc: GitHubWire.Protection): Boolean =
    doc.requiredSignatures.flatMap(_.enabled).contains(true)

  private def ruleSource(
      rules: List[GitHubWire.BranchRule],
      classicOpen: Boolean,
      branch: BranchName,
      repo: RepoName,
  ): RuleSource =
    val status = rules.filter(_.kind == "required_status_checks")
    identified(status, "Repository", repo)
      .orElse(identified(status, "Organization", repo))
      .orElse(if classicOpen then Some(RuleSource.BranchProtection(branch)) else None)
      .getOrElse(RuleSource.Absent)

  private def identified(rules: List[GitHubWire.BranchRule], kind: String, repo: RepoName): Option[RuleSource] =
    rules.flatMap { rule =>
      (rule.sourceType, rule.rulesetId, rule.source) match
        case (Some(source), Some(id), _) if source == kind && kind == "Repository" =>
          Some(RuleSource.RepositoryRuleset(id, rule.source.getOrElse(repo.value)))
        case (Some(source), Some(id), Some(org)) if source == kind && kind == "Organization" =>
          Some(RuleSource.OrganizationRuleset(org, id))
        case (Some(source), Some(id), None) if source == kind && kind == "Organization" =>
          Some(RuleSource.OrganizationRuleset(repo.value.takeWhile(_ != '/'), id))
        case _ => None
    }.headOption

  /** Administration read is what this document needs. A 401 or 403 is unseen, not a supply refusal. */
  private def workflowOf(reply: Reply): FlowRead =
    if reply.status >= 200 && reply.status < 300 then
      GitHubWire.decode[GitHubWire.WorkflowPermissions](reply.body) match
        case Some(doc) =>
          FlowRead(
            doc.defaultWorkflowPermissions.contains("write"),
            doc.canApprove.contains(true),
            Sight.Known,
            true,
            Nil,
          )
        case None => FlowRead(false, false, Sight.Unreadable, true, Nil)
    else if reply.status == 401 || reply.status == 403 then FlowRead(false, false, Sight.Unseen, true, Nil)
    else FlowRead(false, false, Sight.Unseen, false, refusal(reply, ApiCall.Workflows))

  private def repoRefusal(reply: Reply): List[(ApiCall, Int, String)] =
    if reply.status >= 200 && reply.status < 300 then Nil
    else refusal(reply, ApiCall.Contents)

  private def refusal(reply: Reply, call: ApiCall): List[(ApiCall, Int, String)] =
    if reply.status == 401 || reply.status == 403 || reply.status == 404 || reply.status == 422 then
      List((call, reply.status, reply.body))
    else Nil

  /** Signing is covered only when the rule was read. An unseen rule is still an error on the required check. */
  private val unseenRules: RulesRead = RulesRead(Nil, false, RuleSource.Absent, Sight.Unseen, true)

  private def cover(
      app: Boolean,
      rules: RulesRead,
      repoRead: Boolean,
      flow: Boolean,
  ): Set[String] =
    val base    = Set("supply.secret-missing", "supply.dependabot-overlap")
    val withApp =
      if app then base + "supply.app-installed" + "supply.app-permissions" + "supply.app-token" else base
    val withRules =
      if rules.sight == Sight.Known then withApp + "supply.required-check" + "supply.signing"
      else if rules.report then withApp + "supply.required-check"
      else withApp
    val withRepo = if repoRead then withRules + "supply.default-branch" else withRules
    if flow then withRepo + "supply.default-token" + "supply.allow-prs" else withRepo

  private def view(
      repo: RepoName,
      defaultBranch: BranchName,
      expected: BranchName,
      job: JobEnv,
      token: String,
      secrets: Set[String],
      secretsSight: Sight,
      app: AppRead,
      required: List[JobName],
      signed: Boolean,
      rule: RuleSource,
      rulesSight: Sight,
      owner: RepoOwner,
      merges: Set[MergeMethod],
      tokenWrite: Boolean,
      allowPrs: Boolean,
      refusals: List[(ApiCall, Int, String)],
      flowSight: Sight,
  ): GitHubView =
    val kind =
      if job.actions && job.githubToken.contains(token) then TokenKind.Github
      else if app.installed && app.sight == Sight.Known then TokenKind.App
      else TokenKind.Pat
    GitHubView(
      repo,
      kind,
      app.installed,
      app.permissions,
      secrets,
      required,
      signed,
      merges,
      rule,
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
      owner,
      secretsSight,
      app.sight,
      rulesSight,
      flowSight,
      job.step.getOrElse(StepOutcome.Success),
    )

  private def ownerOf(repo: RepoName, owner: Option[GitHubWire.Owner]): RepoOwner =
    owner match
      case Some(wire) if wire.kind == "Organization" => RepoOwner.Organization(wire.login)
      case Some(wire)                                => RepoOwner.User(wire.login)
      case None                                      =>
        repo.value.split("/", -1).toList match
          case login :: _ if login.nonEmpty => RepoOwner.User(login)
          case _                            => RepoOwner.User("unknown")

  private def merges(doc: GitHubWire.Repository): Set[MergeMethod] =
    List(
      doc.allowMergeCommit -> MergeMethod.Merge,
      doc.allowSquashMerge -> MergeMethod.Squash,
      doc.allowRebaseMerge -> MergeMethod.Rebase,
    ).collect { case (Some(true), method) => method }.toSet

  private def permissionMap(doc: GitHubWire.Permissions): Map[AppPermission, Access] =
    List(
      AppPermission.Contents     -> doc.contents,
      AppPermission.PullRequests -> doc.pullRequests,
      AppPermission.Workflows    -> doc.workflows,
    ).flatMap((item, raw) => granted(raw).map(item -> _)).toMap

  private def granted(raw: Option[String]): Option[Access] =
    raw match
      case Some("write") | Some("admin") => Some(Access.Write)
      case Some("read")                  => Some(Access.Read)
      case _                             => None

  private def get(net: Net, url: String, token: String) =
    net.send(
      "GET",
      url,
      "",
      List("Authorization" -> s"Bearer $token", "Accept" -> "application/vnd.github+json"),
    )
end GitHubProbe
