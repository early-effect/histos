package histos.workflow

import histos.plugin.{
  CiHeader,
  CiSettings,
  Cond,
  DispatchInput,
  EnvName,
  EnvValue,
  EventKind,
  GithubField,
  Grant,
  GraphSecrets,
  InputKind,
  JobCheck,
  JobName,
  JobPermissions,
  JobRender,
  NeedsPolicy,
  On,
  PinnedAction,
  PushBranch,
  Placed,
  PluginId,
  PluginTaskId,
  PullType,
  Runner,
  SiteArrange,
  SiteShape,
  Target,
  TaskArrange,
  TaskName,
  WorkflowFile,
}
import histos.{BuildError, PluginGraphFault, ProjectGraph}

/** Prints [[JobRender]]. The engine does not match a [[histos.plugin.CiJob]] class. */
object JobYaml:
  final case class Rendered(ci: List[String], files: List[RenderedWorkflow], header: CiHeader)

  def uses(use: ActionUse): String =
    use match
      case ActionUse.Pinned(name, pin) => ActionPins.line(name, pin)
      case ActionUse.Local(path)       => path

  /** The loader adds the wiring's plugins after the build checked its jobs, so a graph fault can first show here. It
    * stays a [[BuildError]].
    */
  def render(graph: ProjectGraph, defaultBranch: String): Either[CiError | BuildError, Rendered] =
    JobCheck
      .arrange(graph)
      .left
      .map:
        case BuildError.Ci(fault)                                                  => CiError.Fault(fault)
        case BuildError.PluginGraph(PluginGraphFault.DuplicateSite(first, second)) =>
          CiError.DuplicateSite(first, second)
        case graphFault => graphFault
      .map: placed =>
        val ci = placed.collect:
          case item @ Placed(WorkflowFile.Ci, _, _) => jobBlock(graph, item)
        val pages = placed.collect:
          case Placed(WorkflowFile.Pages, plugin, JobRender.Site(job, SiteShape.Dispatch)) =>
            RenderedWorkflow(Workflow.pagesName, dispatch(graph, plugin, job))
        val owned = ownedFiles(graph, placed, defaultBranch)
        Rendered(ci.flatten, pages ++ owned, JobCheck.header(placed))

  private def jobBlock(graph: ProjectGraph, item: Placed): List[String] =
    val body = item.render match
      case JobRender.Task(job)        => List(task(graph, item.plugin, job, None))
      case JobRender.Fan(job, rows)   => List(fan(graph, item.plugin, job, rows))
      case JobRender.Site(job, shape) => site(graph, item.plugin, job, shape)
      case JobRender.Schedule(_, _)   => Nil
    body.map(_.mkString("\n"))

  private def ownedFiles(graph: ProjectGraph, placed: List[Placed], defaultBranch: String): List[RenderedWorkflow] =
    val named = placed.collect:
      case item @ Placed(WorkflowFile.Named(name), _, _) => (name, item)
    named
      .foldLeft(List.empty[(String, List[Placed])]):
        case (acc, (name, item)) =>
          acc.find(_._1 == name) match
            case None            => acc :+ (name, List(item))
            case Some((_, jobs)) =>
              acc.map: pair =>
                if pair._1 == name then (name, jobs :+ item) else pair
      .map: (name, jobs) =>
        RenderedWorkflow(name, document(graph, name, jobs, defaultBranch))

  private def document(graph: ProjectGraph, name: String, jobs: List[Placed], defaultBranch: String): String =
    val cron   = jobs.collectFirst { case Placed(_, _, JobRender.Schedule(expr, _)) => expr }
    val events = jobs.flatMap:
      case Placed(_, _, render) => eventsOf(render)
    val blocks = jobs.flatMap: item =>
      item.render match
        case JobRender.Schedule(_, job) => List(task(graph, item.plugin, job, None).mkString("\n"))
        case JobRender.Task(job)        => List(task(graph, item.plugin, job, None).mkString("\n"))
        case JobRender.Fan(job, rows)   => List(fan(graph, item.plugin, job, rows).mkString("\n"))
        case JobRender.Site(job, shape) => site(graph, item.plugin, job, shape).map(_.mkString("\n"))
    val lines = List(s"name: ${name.stripSuffix(".yml")}", "on:") ++ onLines(events, cron, defaultBranch) ++ List(
      "permissions:",
      "  contents: read",
      "jobs:",
    ) ++ blocks
    lines.mkString("\n") + "\n"

  private def eventsOf(render: JobRender): List[On] =
    render match
      case JobRender.Task(job)        => job.events
      case JobRender.Schedule(_, job) => job.events
      case JobRender.Fan(job, _)      => job.events
      case JobRender.Site(job, _)     => job.events

  private def runOf(plugin: PluginId, task: TaskName): String =
    s"./histos run ${PluginTaskId(plugin, task).render}"

  private def site(
      graph: ProjectGraph,
      plugin: PluginId,
      job: SiteArrange,
      shape: SiteShape,
  ): List[List[String]] =
    shape match
      case SiteShape.Dispatch     => Nil
      case SiteShape.Build        => List(taskLike(graph, plugin, job, upload = false))
      case SiteShape.DeployOnPush =>
        List(taskLike(graph, plugin, job, upload = true), pages(graph, job.name))

  private def task(
      graph: ProjectGraph,
      plugin: PluginId,
      job: TaskArrange,
      rows: Option[List[Target]],
  ): List[String] =
    rows match
      case Some(targets) => fan(graph, plugin, job, targets)
      case None          =>
        stepJob(
          graph,
          plugin,
          job.name,
          job.task,
          job.permissions,
          job.needs,
          job.cond,
          job.needsPolicy,
          job.runner,
          job.env,
          job.environment,
          job.pre,
          job.graphSecrets,
          job.concurrency,
          upload = None,
          matrix = Nil,
        )

  private def fan(
      graph: ProjectGraph,
      plugin: PluginId,
      job: TaskArrange,
      rows: List[Target],
  ): List[String] =
    stepJob(
      graph,
      plugin,
      job.name,
      job.task,
      job.permissions,
      job.needs,
      job.cond,
      job.needsPolicy,
      job.runner,
      job.env,
      None,
      job.pre,
      job.graphSecrets,
      job.concurrency,
      upload = None,
      matrix = rows,
    )

  private def taskLike(
      graph: ProjectGraph,
      plugin: PluginId,
      job: SiteArrange,
      upload: Boolean,
  ): List[String] =
    val grants =
      if upload then job.permissions.copy(grants = job.permissions.grants + Grant.ActionsWrite)
      else job.permissions
    stepJob(
      graph,
      plugin,
      job.name,
      job.task,
      grants,
      job.needs,
      job.cond,
      job.needsPolicy,
      job.runner,
      job.env,
      job.environment,
      job.pre,
      job.graphSecrets,
      job.concurrency,
      upload = if upload then Some(job.site.dir.value) else None,
      matrix = Nil,
    )

  private def stepJob(
      graph: ProjectGraph,
      plugin: PluginId,
      name: JobName,
      task: TaskName,
      permissions: JobPermissions,
      needs: List[JobName],
      cond: Cond,
      policy: NeedsPolicy,
      runner: Runner,
      env: List[(EnvName, EnvValue)],
      environment: Option[histos.plugin.EnvironmentName],
      pre: List[PinnedAction],
      secrets: GraphSecrets,
      concurrency: Option[histos.plugin.Concurrency],
      upload: Option[String],
      matrix: List[Target],
  ): List[String] =
    val thirdParty = pre.nonEmpty
    val keys       = matrix.flatMap(_.env.map(_._1)).distinct
    val own        = jobEnv(graph, env, secrets, thirdParty).filterNot { case (key, _) => keys.contains(key) }
    val matrixEnv  = keys.map(key => s"      ${key.value}: $${{ matrix.${key.value} }}")
    List(s"  ${name.value}:") ++
      needsLine(needs) ++
      when(cond, needs, policy) ++
      List(s"    runs-on: ${runsOn(runner, graph.ci)}") ++
      Workflow.jobLimit(graph) ++
      matrixBlock(matrix) ++
      environmentLine(environment, matrix) ++
      concurrencyBlock(concurrency) ++
      List("    permissions:") ++
      permissionLines(permissions) ++
      envLines(own, matrixEnv) ++
      List("    steps:", s"      - uses: $Checkout", s"      - uses: $Setup") ++
      SupplyYaml.preflight(graph, name.value) ++
      pre.flatMap(action => actionStep(graph.ci, action)) ++
      List(s"      - run: ${quote(runOf(plugin, task))}") ++
      upload.toList.flatMap(uploadStep)

  private def pages(graph: ProjectGraph, name: JobName): List[String] =
    List(
      "  pages:",
      s"    needs: ${name.value}",
      s"    if: ${quote(OnDefaultPush)}",
      "    runs-on: ubuntu-26.04",
    ) ++ Workflow.jobLimit(graph) ++ List(
      "    environment:",
      "      name: github-pages",
      "      url: ${{ steps.deployment.outputs.page_url }}",
      "    concurrency:",
      "      group: pages",
      "      cancel-in-progress: false",
      "    permissions:",
      "      actions: read",
      "      contents: read",
      "      id-token: write",
      "      pages: write",
      "    steps:",
      "      - id: deployment",
      s"        uses: ${ActionPins.deployPages.line}",
    )

  private def dispatch(graph: ProjectGraph, plugin: PluginId, job: SiteArrange): String =
    val run   = quote(runOf(plugin, job.task))
    val lines = List(
      "name: pages",
      "on:",
      "  workflow_dispatch:",
      "permissions:",
      "  contents: read",
      "concurrency:",
      "  group: pages",
      "  cancel-in-progress: false",
      "jobs:",
      "  pages:",
      "    runs-on: ubuntu-26.04",
    ) ++ Workflow.jobLimit(graph) ++ List(
      "    environment:",
      "      name: github-pages",
      "      url: ${{ steps.deployment.outputs.page_url }}",
      "    permissions:",
      "      actions: write",
      "      contents: read",
      "      id-token: write",
      "      pages: write",
      "    steps:",
      s"      - uses: $Checkout",
      s"      - uses: $Setup",
    ) ++ SupplyYaml.preflight(graph, "pages") ++ List(
      s"      - run: $run",
      s"      - uses: ${ActionPins.uploadPages.line}",
      "        with:",
      s"          path: ${quote(job.site.dir.value)}",
      "      - id: deployment",
      s"        uses: ${ActionPins.deployPages.line}",
    )
    lines.mkString("\n") + "\n"

  private def jobEnv(
      graph: ProjectGraph,
      env: List[(EnvName, EnvValue)],
      secrets: GraphSecrets,
      thirdParty: Boolean,
  ): List[(EnvName, EnvValue)] =
    val shared = graph.ci.env.filter {
      case (_, EnvValue.Secret(_)) =>
        secrets match
          case GraphSecrets.Include          => true
          case GraphSecrets.Omit             => false
          case GraphSecrets.WhenNoThirdParty => !thirdParty
      case _ => false
    }
    val kept = shared.filterNot { case (name, _) => env.exists(_._1 == name) }
    kept ++ env

  private def needsLine(needs: List[JobName]): List[String] =
    needs match
      case Nil         => Nil
      case name :: Nil => List(s"    needs: ${name.value}")
      case many        => List(s"    needs: [${many.map(_.value).mkString(", ")}]")

  private def when(cond: Cond, needs: List[JobName], policy: NeedsPolicy): List[String] =
    val upstream =
      if needs.nonEmpty && policy == NeedsPolicy.NoneFailed then Some(NoneFailed) else None
    Expr.combine(Expr.render(cond), upstream) match
      case None       => Nil
      case Some(text) => List(s"    if: ${quote(text)}")

  private def runsOn(runner: Runner, settings: CiSettings): String =
    runner match
      case Runner.Default     => RunnerImage
      case Runner.Named(name) => settings.runners.getOrElse(name, RunnerImage)

  private def matrixBlock(rows: List[Target]): List[String] =
    rows match
      case Nil => Nil
      case _   =>
        val keys = rows.flatMap(_.env.map(_._1)).distinct
        List("    strategy:", "      fail-fast: false", "      matrix:", "        include:") ++
          rows.flatMap: row =>
            List(s"          - name: ${quote(row.name.value)}") ++
              row.environment.toList.map(env => s"            environment: ${quote(env.value)}") ++
              keys.map: key =>
                val value = row.env.collectFirst { case (found, EnvValue.Plain(text)) if found == key => text }
                s"            ${key.value}: ${quote(value.getOrElse(""))}"

  private def environmentLine(
      environment: Option[histos.plugin.EnvironmentName],
      matrix: List[Target],
  ): List[String] =
    if matrix.exists(_.environment.isDefined) then List("    environment: ${{ matrix.environment }}")
    else environment.toList.map(name => s"    environment: ${name.value}")

  private def concurrencyBlock(concurrency: Option[histos.plugin.Concurrency]): List[String] =
    concurrency.toList.flatMap: value =>
      val cancel = value.cancel match
        case histos.plugin.Cancel.InProgress => "true"
        case histos.plugin.Cancel.Finish     => "false"
      List("    concurrency:", s"      group: ${value.group.value}", s"      cancel-in-progress: $cancel")

  private def permissionLines(permissions: JobPermissions): List[String] =
    val grants = permissions.grants
    Scope.order.flatMap: scope =>
      val access =
        if grants.contains(scope.write) then Some("write")
        else if grants.contains(scope.read) then Some("read")
        else if scope.floor && !grants.contains(scope.write) then Some("read")
        else None
      access.map(level => s"      ${scope.key}: $level")

  private def envLines(own: List[(EnvName, EnvValue)], matrix: List[String]): List[String] =
    val lines = own.map { case (name, value) => s"      ${name.value}: ${envText(value)}" } ++ matrix
    if lines.isEmpty then Nil else "    env:" :: lines

  private def actionStep(settings: CiSettings, action: PinnedAction): List[String] =
    val pin  = settings.actions(action.name)
    val uses = s"      - uses: ${action.name.value}@${pin.sha.value} # ${pin.tag}"
    action.inputs match
      case Nil    => List(uses)
      case inputs =>
        uses :: "        with:" :: inputs.map { case (key, value) => s"          ${key.value}: ${envText(value)}" }

  private def uploadStep(path: String): List[String] =
    List(
      s"      - if: ${quote(OnDefaultPush)}",
      s"        uses: ${ActionPins.uploadPages.line}",
      "        with:",
      s"          path: ${quote(path)}",
    )

  private def envText(value: EnvValue): String =
    value match
      case EnvValue.Plain(text)   => quote(text)
      case EnvValue.Secret(name)  => s"$${{ secrets.${name.value} }}"
      case EnvValue.Github(field) => github(field)
      case EnvValue.Env(name)     => s"$${{ env.${name.value} }}"

  private def github(field: GithubField): String =
    field match
      case GithubField.Sha     => "${{ github.sha }}"
      case GithubField.RefName => "${{ github.ref_name }}"
      case GithubField.Token   => "${{ github.token }}"

  private def onLines(events: List[On], cron: Option[histos.plugin.Cron], defaultBranch: String): List[String] =
    val pull      = events.collect { case On.PullRequest(types, paths) => (types, paths) }
    val push      = events.collect { case On.Push(branches, tags, paths) => (branches, tags, paths) }
    val inputs    = events.collect { case On.Dispatch(found) => found }.flatten
    val merge     = events.contains(On.MergeGroup)
    val release   = events.contains(On.Release)
    val runs      = events.collect { case On.WorkflowRun(names) => names }.flatten.distinct
    val types     = pull.flatMap(_._1).distinct
    val paths     = pull.flatMap(_._2).distinct
    val branches  = push.flatMap(_._1).distinct
    val tags      = push.flatMap(_._2).distinct
    val pushPaths = push.flatMap(_._3).distinct
    val dispatch  = events.exists(_.isInstanceOf[On.Dispatch])
    (if pull.nonEmpty then pullLines(types, paths) else Nil) ++
      (if push.nonEmpty then pushLines(branches, tags, pushPaths, defaultBranch) else Nil) ++
      (if dispatch then dispatchLines(inputs) else Nil) ++
      (if merge then List("  merge_group:") else Nil) ++
      (if release then List("  release:") else Nil) ++
      (if runs.nonEmpty then workflowRun(runs) else Nil) ++
      cron.toList.flatMap(expr => List("  schedule:", s"    - cron: ${quote(expr.value)}"))

  private def pullLines(types: List[PullType], paths: List[String]): List[String] =
    val typed =
      if types.isEmpty || types.toSet == On.defaultPull.toSet then Nil
      else "    types:" :: types.map(kind => s"      - ${pull(kind)}")
    val listed = paths.map(path => s"      - ${quote(path)}")
    "  pull_request:" :: typed ++ (if listed.isEmpty then Nil else "    paths:" :: listed)

  private def pushLines(
      branches: List[PushBranch],
      tags: List[String],
      paths: List[String],
      defaultBranch: String,
  ): List[String] =
    val names = branches.map:
      case PushBranch.Default     => defaultBranch
      case PushBranch.Named(name) => name
    val branchLines =
      if names.isEmpty then Nil else "    branches:" :: names.map(branch => s"      - ${quote(branch)}")
    val tagLines  = if tags.isEmpty then Nil else "    tags:" :: tags.map(tag => s"      - ${quote(tag)}")
    val pathLines = if paths.isEmpty then Nil else "    paths:" :: paths.map(path => s"      - ${quote(path)}")
    "  push:" :: branchLines ++ tagLines ++ pathLines

  private def dispatchLines(inputs: List[DispatchInput]): List[String] =
    inputs match
      case Nil => List("  workflow_dispatch:")
      case _   =>
        "  workflow_dispatch:" :: "    inputs:" :: inputs.flatMap: input =>
          val kind = input.kind match
            case InputKind.Text    => "string"
            case InputKind.Boolean => "boolean"
            case InputKind.Choice  => "choice"
          List(
            s"      ${input.name.value}:",
            s"        description: ${input.name.value}",
            "        required: false",
            s"        type: $kind",
          ) ++ input.default.toList.map(value => s"        default: ${quote(value)}")

  private def workflowRun(names: List[String]): List[String] =
    "  workflow_run:" :: "    workflows:" :: names.map(name => s"      - ${quote(name)}") ++ List(
      "    types:",
      "      - completed",
    )

  private def pull(kind: PullType): String =
    kind match
      case PullType.Opened      => "opened"
      case PullType.Synchronize => "synchronize"
      case PullType.Reopened    => "reopened"
      case PullType.Labeled     => "labeled"
      case PullType.Unlabeled   => "unlabeled"

  private val Checkout      = ActionPins.checkout.line
  private val Setup         = "./.github/actions/setup"
  private val RunnerImage   = "ubuntu-26.04"
  private val OnDefaultPush =
    "github.event_name == 'push' && github.ref == format('refs/heads/{0}', github.event.repository.default_branch)"
  private val NoneFailed = "!cancelled() && !contains(needs.*.result, 'failure')"

  private object Scope:
    final case class One(key: String, read: Grant, write: Grant, floor: Boolean)
    val order: List[One] = List(
      One("contents", Grant.ContentsRead, Grant.ContentsWrite, true),
      One("packages", Grant.PackagesRead, Grant.PackagesWrite, false),
      One("pull-requests", Grant.PullRequestsRead, Grant.PullRequestsWrite, false),
      One("actions", Grant.ActionsRead, Grant.ActionsWrite, false),
      One("id-token", Grant.IdTokenWrite, Grant.IdTokenWrite, false),
      One("pages", Grant.PagesRead, Grant.PagesWrite, false),
      One("issues", Grant.IssuesRead, Grant.IssuesWrite, false),
      One("security-events", Grant.SecurityEventsWrite, Grant.SecurityEventsWrite, false),
    )

  private def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private object Expr:
    def combine(cond: String, upstream: Option[String]): Option[String] =
      (cond, upstream) match
        case ("true", None)        => None
        case ("true", Some(extra)) => Some(extra)
        case (text, None)          => Some(text)
        case (text, Some(extra))   => Some(s"${wrap(text)} && ${wrap(extra)}")

    def render(cond: Cond): String =
      cond match
        case Cond.True                 => "true"
        case Cond.Event(kind)          => event(kind)
        case Cond.DefaultBranch        => OnDefaultBranch
        case Cond.DefaultBranchPush    => OnDefaultPush
        case Cond.TagMatches(glob)     => tag(glob)
        case Cond.HasLabel(label)      => s"contains(github.event.pull_request.labels.*.name, '${lit(label)}')"
        case Cond.InputIs(name, value) => s"github.event.inputs.${name.value} == '${lit(value)}'"
        case Cond.Affected(module)     => contains(module.value)
        case Cond.AffectedAll          => contains("all")
        case Cond.NoUpstreamFailed     => NoneFailed
        case Cond.Not(inner)           => s"!(${render(inner)})"
        case Cond.All(parts)           => join(parts.map(render).filter(_ != "true"), " && ", "true")
        case Cond.AnyOf(parts)         => join(parts.map(render).filter(_ != "false"), " || ", "false")

    private def join(parts: List[String], sep: String, empty: String): String =
      parts match
        case Nil        => empty
        case one :: Nil => one
        case many       => many.map(wrap).mkString(sep)

    private def wrap(text: String): String =
      if text.contains(" && ") || text.contains(" || ") then s"($text)" else text

    private def event(kind: EventKind): String =
      kind match
        case EventKind.PullRequest => "github.event_name == 'pull_request'"
        case EventKind.Push        => "github.event_name == 'push'"
        case EventKind.Dispatch    => "github.event_name == 'workflow_dispatch'"
        case EventKind.Schedule    => "github.event_name == 'schedule'"
        case EventKind.MergeGroup  => "github.event_name == 'merge_group'"
        case EventKind.Release     => "github.event_name == 'release'"
        case EventKind.WorkflowRun => "github.event_name == 'workflow_run'"
        case EventKind.Tag         => "startsWith(github.ref, 'refs/tags/')"

    private def tag(glob: String): String =
      if glob.isEmpty || glob == "*" then "startsWith(github.ref, 'refs/tags/')"
      else if glob.endsWith("*") then s"startsWith(github.ref, 'refs/tags/${lit(glob.dropRight(1))}')"
      else s"github.ref == 'refs/tags/${lit(glob)}'"

    private def contains(name: String): String =
      s"contains(fromJSON(needs.affected.outputs.modules), '${lit(name)}')"

    private def lit(text: String): String =
      text.filter(char => char != '\'' && char != '\\' && char != '\n' && char != '}')

    private val OnDefaultBranch =
      "github.ref == format('refs/heads/{0}', github.event.repository.default_branch)"

    private val OnDefaultPush =
      s"github.event_name == 'push' && $OnDefaultBranch"
  end Expr
end JobYaml
