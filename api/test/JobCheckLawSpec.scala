package histos

import histos.plugin.*
import histos.supply.SupplyChain
import histos.version.Catalog

import zio.test.*

object JobCheckLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val sha    = version.CommitSha.trusted("0123456789abcdef0123456789abcdef01234567")
  private val echo   = PluginId("test.echo")

  def spec = suite("job check laws")(
    test("generated names follow the modules, the catalog, and the client"):
      val core    = module("core", Jvm)(scala3)
      val client  = module("client", Native)(scala3).clang(ClangMajor(21)).main(MainClass("histos.Client"))
      val desktop = module("client", Jvm)(scala3).main(MainClass("histos.Client"))
      val row     = Ship("histos").next(1, 4, 2)(core)
      assertTrue(
        names(Projects()) == Right(Set("workflow", "affected")),
        names(Projects(core)) == Right(Set("workflow", "affected", "test", "cache-seed")),
        names(Projects.docCheck(core)) == Right(Set("workflow", "affected", "test", "cache-seed", "scaladoc")),
        names(Projects.coverage()(core)) == Right(Set("workflow", "affected", "test", "cache-seed", "coverage")),
        names(Projects(ships(row), core)) == Right(Set("workflow", "affected", "test", "cache-seed", "versions")),
        names(Projects(client)) == Right(Set("workflow", "affected", "test", "cache-seed", "launcher")),
        names(Projects(desktop)) == Right(Set("workflow", "affected", "test", "cache-seed")),
      )
    ,
    test("a site is one of the five legal shapes, and a render with no site shape is not a site"):
      assertTrue(
        fault(arrange(List(site("docs", List(SiteShape.Build))))) == "graph",
        fault(arrange(List(site("docs", List(SiteShape.DeployOnPush))))) == "graph",
        fault(arrange(List(site("docs", List(SiteShape.Dispatch))))) == "graph",
        fault(arrange(List(site("docs", List(SiteShape.Build, SiteShape.Dispatch))))) == "graph",
        fault(arrange(List(site("docs", List(SiteShape.DeployOnPush, SiteShape.Dispatch))))) == "graph",
        fault(arrange(List(site("docs", Nil)))) == "graph",
        fault(arrange(List(site("docs", List(SiteShape.Build, SiteShape.Build))))) == "IllegalSite",
        fault(arrange(List(site("docs", List(SiteShape.Build, SiteShape.DeployOnPush))))) == "IllegalSite",
        fault(arrange(List(site("docs", List(SiteShape.Build, SiteShape.DeployOnPush, SiteShape.Dispatch))))) ==
          "IllegalSite",
        fault(arrange(List(site("docs", List(SiteShape.Build)), site("guide", List(SiteShape.Build))))) ==
          "DuplicateSite",
      )
    ,
    test("a need may name a generated job, and not itself, a missing job, or a repeated job"):
      val nightly = TaskJob(JobName("nightly"), TaskName("echo"), needs = List(JobName("test")))
      val doubled = TaskJob(JobName("nightly"), TaskName("echo"), needs = List(JobName("test"), JobName("test")))
      val itself  = TaskJob(JobName("nightly"), TaskName("echo"), needs = List(JobName("nightly")))
      val doc     = TaskJob(JobName("nightly"), TaskName("echo"), needs = List(JobName("scaladoc")))
      val cover   = TaskJob(JobName("nightly"), TaskName("echo"), needs = List(JobName("coverage")))
      assertTrue(
        fault(arrange(List(nightly))) == "UnknownNeed",
        fault(arrange(List(nightly), projects = List(coreModule))) == "graph",
        fault(arrange(List(doubled), projects = List(coreModule))) == "DuplicateNeed",
        fault(arrange(List(itself), projects = List(coreModule))) == "UnknownNeed",
        fault(arrange(List(doc), projects = List(coreModule))) == "UnknownNeed",
        fault(arrange(List(doc), projects = List(coreModule), doc = DocCheck.On)) == "graph",
        fault(arrange(List(cover), projects = List(coreModule))) == "UnknownNeed",
        fault(arrange(List(cover), projects = List(coreModule), coverage = CoverageJob.Ask(CoverageFloors()))) ==
          "graph",
      )
    ,
    test("a runner, an action, a grant, and a secret have to be on the graph"):
      val runner = RunnerName("large")
      val action = ActionName("aws-actions/configure-aws-credentials")
      val pin    = ActionPin(sha, "v4")
      val secret = SecretName("HISTOS_TOKEN")
      val token  = EnvName("TOKEN")
      val named  = TaskJob(JobName("nightly"), TaskName("echo"), runner = Runner.Named(runner))
      val third  = TaskJob(
        JobName("nightly"),
        TaskName("echo"),
        pre = List(PinnedAction(action, Nil, Set(Grant.IdTokenWrite))),
      )
      val granted = third.copy(permissions = JobPermissions(Set(Grant.ContentsRead, Grant.IdTokenWrite)))
      val hidden  = TaskJob(
        JobName("nightly"),
        TaskName("echo"),
        env = List(token -> EnvValue.Secret(secret)),
      )
      val pinned  = CiSettings.empty.copy(actions = Map(action -> pin))
      val runners = CiSettings.empty.copy(runners = Map(runner -> "ubuntu-latest"))
      assertTrue(
        fault(arrange(List(named))) == "UnknownRunner",
        fault(arrange(List(named), ci = runners)) == "graph",
        fault(arrange(List(third))) == "UnpinnedAction",
        fault(arrange(List(third), ci = pinned)) == "GrantMissing",
        fault(arrange(List(granted), ci = pinned)) == "graph",
        fault(arrange(List(hidden))) == "UnknownSecret",
        fault(
          arrange(
            List(TaskJob(JobName("nightly"), TaskName("echo"))),
            ci = CiSettings.empty.copy(env = List(token -> EnvValue.Secret(secret))),
          )
        ) == "UnknownSecret",
        fault(arrange(List(hidden), ci = CiSettings.empty.copy(secrets = Set(secret)))) == "graph",
      )
    ,
    test("ci.yml hears push, and negation does not invert a condition"):
      val tag       = TaskJob(JobName("release"), TaskName("echo"), trigger = Trigger.Tag("v*"))
      val labeled   = TaskJob(JobName("labeled"), TaskName("echo"), trigger = Trigger.Label("ship"))
      val schedule  = rendered(Cond.Event(EventKind.Schedule), List(On.pullRequest))
      val tagless   = rendered(Cond.TagMatches("v*"), List(On.pullRequest))
      val labelOnCi = rendered(Cond.HasLabel("ship"), List(On.pullRequest))
      val notPush   = rendered(Cond.Not(Cond.DefaultBranchPush), List(On.Dispatch(Nil)))
      val notHeard  = rendered(Cond.Not(Cond.DefaultBranchPush), List(On.pushDefault), "push")
      val notCron   = rendered(Cond.Not(Cond.Event(EventKind.Schedule)), List(On.Dispatch(Nil)))
      val affected  = rendered(Cond.Affected(ProjectName("core")), Nil, "marked")
      val steady    = rendered(Cond.NoUpstreamFailed, Nil, "steady")
      val branch    = rendered(Cond.DefaultBranch, Nil, "branch")
      val tagged    = rendered(Cond.Event(EventKind.Tag), List(On.Push(Nil, List("v*"), Nil)), "tagged")
      val any       = rendered(Cond.AnyOf(List(Cond.True, Cond.Event(EventKind.Schedule))), Nil)
      val all       = rendered(Cond.All(List(Cond.True, Cond.Event(EventKind.Schedule))), Nil)
      val input     = InputKey("mode")
      val asked     = rendered(
        Cond.InputIs(input, "no"),
        List(On.Dispatch(List(DispatchInput(input, InputKind.Text, Some("yes"))))),
      )
      assertTrue(
        fault(arrange(List(tag))) == "graph",
        headerOf(arrange(List(tag))).map(_.tags) == Right(List("v*")),
        fault(arrange(List(labeled))) == "graph",
        fileOf(arrange(List(labeled))) == Right(List(WorkflowFile.Named("histos-labeled.yml"))),
        fault(arrange(List(schedule))) == "UnreachableCond",
        fault(arrange(List(tagless))) == "UnreachableCond",
        fault(arrange(List(labelOnCi))) == "UnreachableCond",
        fault(arrange(List(notPush))) == "graph",
        fault(arrange(List(notHeard))) == "graph",
        fault(arrange(List(notCron))) == "UnreachableCond",
        fault(arrange(List(affected), projects = List(coreModule))) == "graph",
        fault(arrange(List(steady))) == "graph",
        fault(arrange(List(branch))) == "graph",
        fault(arrange(List(tagged))) == "graph",
        fault(arrange(List(any))) == "graph",
        fault(arrange(List(all))) == "UnreachableCond",
        fault(arrange(List(asked))) == "graph",
      )
    ,
    test("a dispatch input conflict and a ci concurrency conflict name that file"):
      val mode       = InputKey("mode")
      val text       = DispatchInput(mode, InputKind.Text, Some("a"))
      val other      = DispatchInput(mode, InputKind.Text, Some("b"))
      val bool       = DispatchInput(mode, InputKind.Boolean, Some("true"))
      val same       = DispatchInput(mode, InputKind.Text, Some("a"))
      val left       = rendered(Cond.True, List(On.Dispatch(List(text))), "left")
      val right      = rendered(Cond.True, List(On.Dispatch(List(other))), "right")
      val typed      = rendered(Cond.True, List(On.Dispatch(List(bool))), "right")
      val copy       = rendered(Cond.True, List(On.Dispatch(List(same))), "right")
      val group      = GroupName("ship")
      val otherGroup = GroupName("docs")
      val first  = TaskJob(JobName("left"), TaskName("echo"), concurrency = Some(Concurrency(group, Cancel.InProgress)))
      val second = TaskJob(
        JobName("right"),
        TaskName("echo"),
        concurrency = Some(Concurrency(otherGroup, Cancel.InProgress)),
      )
      val shared = TaskJob(
        JobName("right"),
        TaskName("echo"),
        concurrency = Some(Concurrency(group, Cancel.InProgress)),
      )
      val recancel = TaskJob(
        JobName("right"),
        TaskName("echo"),
        concurrency = Some(Concurrency(group, Cancel.Finish)),
      )
      assertTrue(
        fault(arrange(List(left, right))) == "DispatchInputConflict",
        fault(arrange(List(left, typed))) == "DispatchInputConflict",
        fault(arrange(List(left, copy))) == "graph",
        headerOf(arrange(List(left, copy))).map(_.inputs) == Right(List(text)),
        fault(arrange(List(first, second))) == "ConcurrencyConflict",
        fault(arrange(List(first, recancel))) == "ConcurrencyConflict",
        fault(arrange(List(first, shared))) == "graph",
      )
    ,
    test("a workflow run names a file this graph renders, and the catalog adds the release files"):
      val missing = runs(List("missing.yml"))
      val ci      = runs(List("ci.yml"))
      val release = runs(List("histos-release.yml"))
      val core    = module("core", Jvm)(scala3)
      val row     = Ship("histos").next(1, 4, 2)(core)
      val catalog = Projects(ships(row), core).map(_.catalog)
      assertTrue(
        fault(arrange(List(missing))) == "UnpinnedWorkflowRun",
        fault(arrange(List(ci))) == "graph",
        fault(arrange(List(release))) == "UnpinnedWorkflowRun",
        catalog.flatMap(found => arrange(List(release), catalog = found).map(_ => ())) == Right(()),
        JobCheck.knownWorkflows(ProjectGraph.trusted(Nil), Nil) == Set("ci.yml"),
        catalog.map(found => JobCheck.knownWorkflows(ProjectGraph.trusted(Nil, Splits.aggregate, found), Nil)) ==
          Right(Set("ci.yml", "histos-release.yml", "histos-promote.yml")),
      )
    ,
    test("an affected guard names a module, and a repeated target is refused"):
      val missing = TaskJob(
        JobName("nightly"),
        TaskName("echo"),
        guard = Some(Guard.Affected(::(ProjectName("missing"), Nil), Guard.Unguarded)),
      )
      val present = TaskJob(
        JobName("nightly"),
        TaskName("echo"),
        guard = Some(Guard.Affected(::(ProjectName("core"), Nil), Guard.Unguarded)),
      )
      val target   = Target(TargetName("one"))
      val fan      = Rendered.fan(base("fan"), ::(target, List(target)))
      val distinct = Rendered.fan(base("fan"), ::(target, List(Target(TargetName("two")))))
      assertTrue(
        fault(arrange(List(missing))) == "UnknownModule",
        fault(arrange(List(present), projects = List(coreModule))) == "graph",
        fault(arrange(List(fan))) == "DuplicateTarget",
        fault(arrange(List(distinct))) == "graph",
      )
    ,
    test("placement follows the shape, and a cron or a splitting event leaves ci.yml"):
      val nightly = TaskJob.nightly(JobName("nightly"), TaskName("echo"), Cron("0 6 * * 1"))
      val paths   = TaskJob(JobName("paths"), TaskName("echo"), trigger = Trigger.Paths(::("docs/**", Nil)))
      val pages   = SiteJob(JobName("docs"), TaskName("site"), Site(RepoPath("docs"), SitePublish.Manual))
      val merge   = rendered(Cond.Event(EventKind.MergeGroup), List(On.MergeGroup), "merge")
      assertTrue(
        fileOf(arrange(List(nightly))) == Right(List(WorkflowFile.Named("histos-nightly.yml"))),
        fileOf(arrange(List(paths))) == Right(List(WorkflowFile.Named("histos-paths.yml"))),
        fileOf(arrange(List(pages))) == Right(List(WorkflowFile.Ci, WorkflowFile.Pages)),
        fileOf(arrange(List(merge))) == Right(List(WorkflowFile.Ci)),
        headerOf(arrange(List(merge))).map(_.extras) == Right(Set(CiExtra.MergeGroup)),
        fault(arrange(Nil)) == "graph",
      ),
  )

  private def coreModule: Module =
    Module(ProjectName("core"), Platforms(Jvm, Nil), ScalaVersions(scala3, Nil), Nil, Nil, SourcePos("law", 1, 1))

  private def names(built: Either[BuildError, ProjectGraph]): Either[BuildError, Set[String]] =
    built.map(graph => JobCheck.generated(graph).map(_.value))

  private def arrange(
      jobs: List[CiJob],
      ci: CiSettings = CiSettings.empty,
      projects: List[Project] = Nil,
      catalog: Catalog = Catalog.Empty,
      doc: DocCheck = DocCheck.Off,
      coverage: CoverageJob = CoverageJob.Off,
  ): Either[BuildError, List[Placed]] =
    val contribution = Contribution(Nil, Nil, jobs)
    JobCheck.arrange(
      ProjectGraph.trusted(
        projects,
        Splits.aggregate,
        catalog,
        None,
        Nil,
        List(RegisteredPlugin(echo, contribution)(Fixed(echo, contribution))),
        docCheck = doc,
        ci = ci,
        supply = SupplyChain(),
        coverage = coverage,
      )
    )

  private def fault(built: Either[BuildError, List[Placed]]): String =
    built match
      case Left(BuildError.Ci(found))          => found.productPrefix
      case Left(BuildError.PluginGraph(found)) => found.productPrefix
      case Left(other)                         => other.productPrefix
      case Right(_)                            => "graph"

  private def fileOf(built: Either[BuildError, List[Placed]]): Either[BuildError, List[WorkflowFile]] =
    built.map(_.map(_.file))

  private def headerOf(built: Either[BuildError, List[Placed]]): Either[BuildError, CiHeader] =
    built.map(JobCheck.header)

  private def base(name: String, cond: Cond = Cond.True, events: List[On] = Nil): TaskArrange =
    TaskArrange(job(name), RunLine.Task(TaskName("echo")), JobPermissions.read, Nil, cond, events)

  private def job(raw: String): JobName =
    JobName.from(raw) match
      case Right(parsed) => parsed
      case Left(_)       => JobName("nightly")

  private def rendered(cond: Cond, events: List[On], name: String = "nightly"): CiJob =
    Rendered.task(base(name, cond, events))

  private def site(name: String, shapes: List[SiteShape]): CiJob =
    Rendered.site(name, shapes)

  private def runs(workflows: List[String]): CiJob =
    rendered(Cond.Event(EventKind.WorkflowRun), List(On.WorkflowRun(workflows)), "follow")
end JobCheckLawSpec

private object Rendered:
  def task(arranged: TaskArrange): CiJob =
    new CiJob:
      def name: JobName               = arranged.name
      def step: JobStep               = stepOf(arranged.line)
      def permissions: JobPermissions = arranged.permissions
      def needs: List[JobName]        = arranged.needs
      def render: List[JobRender]     = List(JobRender.Task(arranged))

  def fan(arranged: TaskArrange, targets: ::[Target]): CiJob =
    new CiJob:
      def name: JobName               = arranged.name
      def step: JobStep               = stepOf(arranged.line)
      def permissions: JobPermissions = arranged.permissions
      def needs: List[JobName]        = arranged.needs
      def render: List[JobRender]     = List(JobRender.Fan(arranged, targets))

  def site(name: String, shapes: List[SiteShape]): CiJob =
    val arranged = SiteArrange(
      JobName.from(name) match
        case Right(parsed) => parsed
        case Left(_)       => JobName("docs"),
      TaskName("site"),
      JobPermissions.read,
      Nil,
      Site(RepoPath("docs")),
      Cond.True,
      List(On.Dispatch(Nil)),
    )
    new CiJob:
      def name: JobName               = arranged.name
      def step: JobStep               = JobStep.Task(arranged.task)
      def permissions: JobPermissions = arranged.permissions
      def needs: List[JobName]        = arranged.needs
      def render: List[JobRender]     = shapes.map(shape => JobRender.Site(arranged, shape))

  private def stepOf(line: RunLine): JobStep =
    line match
      case RunLine.Task(name)   => JobStep.Task(name)
      case RunLine.Client(when) => JobStep.Command(HistosCommand.Client, when)
end Rendered
