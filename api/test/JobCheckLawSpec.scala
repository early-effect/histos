package histos

import histos.plugin.*
import histos.supply.SupplyChain
import histos.version.Catalog

import zio.test.*

object JobCheckLawSpec extends ZIOSpecDefault:
  private val scala3  = scala"3.10.0"
  private val sha     = version.CommitSha.trusted("0123456789abcdef0123456789abcdef01234567")
  private val echo    = PluginId("test.echo")
  private val nightly = JobName("nightly")
  private val inCi: Either[BuildError, List[WorkflowFile]] = Right(List(WorkflowFile.Ci))

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
      val docs                       = JobName("docs")
      def shaped(shapes: SiteShape*) = fileOf(arrange(List(site(docs, shapes.toList))))
      val illegal                    = Left(BuildError.Ci(CiFault.IllegalSite(docs)))
      assertTrue(
        shaped(SiteShape.Build) == inCi,
        shaped(SiteShape.DeployOnPush) == inCi,
        shaped(SiteShape.Dispatch) == Right(List(WorkflowFile.Pages)),
        shaped(SiteShape.Build, SiteShape.Dispatch) == Right(List(WorkflowFile.Ci, WorkflowFile.Pages)),
        shaped(SiteShape.DeployOnPush, SiteShape.Dispatch) == Right(List(WorkflowFile.Ci, WorkflowFile.Pages)),
        shaped() == Right(Nil),
        shaped(SiteShape.Build, SiteShape.Build) == illegal,
        shaped(SiteShape.Build, SiteShape.DeployOnPush) == illegal,
        shaped(SiteShape.Build, SiteShape.DeployOnPush, SiteShape.Dispatch) == illegal,
        arrange(List(site(docs, List(SiteShape.Build)), site(JobName("guide"), List(SiteShape.Build)))) ==
          Left(BuildError.PluginGraph(PluginGraphFault.DuplicateSite(docs, JobName("guide")))),
      )
    ,
    test("a need names a generated job, and not itself, an absent or ungenerated job, or a repeat"):
      val core                            = List(coreModule)
      def needing(needs: JobName*): CiJob = TaskJob(nightly, TaskName("echo"), needs = needs.toList)
      def unknown(need: JobName)          = refused(CiFault.UnknownNeed(nightly, need))
      assertTrue(
        arrange(List(needing(JobName("test")))) == unknown(JobName("test")),
        fileOf(arrange(List(needing(JobName("test"))), projects = core)) == inCi,
        arrange(List(needing(JobName("test"), JobName("test"))), projects = core) ==
          refused(CiFault.DuplicateNeed(nightly, JobName("test"))),
        arrange(List(needing(nightly)), projects = core) == unknown(nightly),
        arrange(List(needing(JobName("absent"))), projects = core) == unknown(JobName("absent")),
        arrange(List(needing(JobName("pages"))), projects = core) == unknown(JobName("pages")),
        arrange(List(needing(JobName("scaladoc"))), projects = core) == unknown(JobName("scaladoc")),
        fileOf(arrange(List(needing(JobName("scaladoc"))), projects = core, doc = DocCheck.On)) == inCi,
        arrange(List(needing(JobName("coverage"))), projects = core) == unknown(JobName("coverage")),
        fileOf(
          arrange(List(needing(JobName("coverage"))), projects = core, coverage = CoverageJob.Ask(CoverageFloors()))
        ) == inCi,
      )
    ,
    test("a ci and dispatch trigger stays in ci.yml with both event lists, and either condition runs it"):
      val both = TaskJob(JobName("both"), TaskName("echo"), trigger = Trigger.Ci ++ Trigger.Dispatch)
      val cond = Cond.AnyOf(
        List(
          Cond.AnyOf(List(Cond.Event(EventKind.PullRequest), Cond.DefaultBranchPush)),
          Cond.Event(EventKind.Dispatch),
        )
      )
      val events = List(
        On.PullRequest(List(PullType.Opened, PullType.Synchronize, PullType.Reopened), Nil),
        On.Push(List(PushBranch.Default), Nil, Nil),
        On.Dispatch(Nil),
      )
      assertTrue(
        arrange(List(both)).map(_.map(placed => (placed.file, placed.render))) == Right(
          List(
            (
              WorkflowFile.Ci,
              JobRender.Task(TaskArrange(JobName("both"), TaskName("echo"), JobPermissions.read, Nil, cond, events)),
            )
          )
        )
      )
    ,
    test("a runner, an action, a grant, and a secret have to be on the graph"):
      val runner  = RunnerName("large")
      val action  = ActionName("aws-actions/configure-aws-credentials")
      val pin     = ActionPin(sha, "v4")
      val secret  = SecretName("HISTOS_TOKEN")
      val token   = EnvName("TOKEN")
      val named   = TaskJob(nightly, TaskName("echo"), runner = Runner.Named(runner))
      val third   = TaskJob(nightly, TaskName("echo"), pre = List(PinnedAction(action, Nil, Set(Grant.IdTokenWrite))))
      val granted = third.copy(permissions = JobPermissions(Set(Grant.ContentsRead, Grant.IdTokenWrite)))
      val hidden  = TaskJob(nightly, TaskName("echo"), env = List(token -> EnvValue.Secret(secret)))
      val pinned  = CiSettings.empty.copy(actions = Map(action -> pin))
      val runners = CiSettings.empty.copy(runners = Map(runner -> "ubuntu-latest"))
      val shared  = CiSettings.empty.copy(env = List(token -> EnvValue.Secret(secret)))
      assertTrue(
        arrange(List(named)) == refused(CiFault.UnknownRunner(nightly, runner)),
        fileOf(arrange(List(named), ci = runners)) == inCi,
        arrange(List(third)) == refused(CiFault.UnpinnedAction(nightly, action)),
        arrange(List(third), ci = pinned) == refused(CiFault.GrantMissing(nightly, action)),
        fileOf(arrange(List(granted), ci = pinned)) == inCi,
        arrange(List(hidden)) == refused(CiFault.UnknownSecret(secret)),
        arrange(List(TaskJob(nightly, TaskName("echo"))), ci = shared) == refused(CiFault.UnknownSecret(secret)),
        fileOf(arrange(List(hidden), ci = CiSettings.empty.copy(secrets = Set(secret)))) == inCi,
      )
    ,
    test("ci.yml hears push, and a condition its events never reach is refused"):
      val tag       = TaskJob(JobName("release"), TaskName("echo"), trigger = Trigger.Tag("v*"))
      val labeled   = TaskJob(JobName("labeled"), TaskName("echo"), trigger = Trigger.Label("ship"))
      val schedule  = rendered(Cond.Event(EventKind.Schedule), List(On.pullRequest))
      val tagless   = rendered(Cond.TagMatches("v*"), List(On.pullRequest))
      val labelOnCi = rendered(Cond.HasLabel("ship"), List(On.pullRequest))
      val affected  = rendered(Cond.Affected(ProjectName("core")), Nil, JobName("marked"))
      val steady    = rendered(Cond.NoUpstreamFailed, Nil, JobName("steady"))
      val branch    = rendered(Cond.DefaultBranch, Nil, JobName("branch"))
      val tagged    = rendered(Cond.Event(EventKind.Tag), List(On.Push(Nil, List("v*"), Nil)), JobName("tagged"))
      val any       = rendered(Cond.AnyOf(List(Cond.True, Cond.Event(EventKind.Schedule))), Nil)
      val all       = rendered(Cond.All(List(Cond.True, Cond.Event(EventKind.Schedule))), Nil)
      val input     = InputKey("mode")
      val asked     = rendered(
        Cond.InputIs(input, "no"),
        List(On.Dispatch(List(DispatchInput(input, InputKind.Text, Some("yes"))))),
      )
      assertTrue(
        fileOf(arrange(List(tag))) == inCi,
        headerOf(arrange(List(tag))).map(_.tags) == Right(List("v*")),
        fileOf(arrange(List(labeled))) == Right(List(WorkflowFile.Named("histos-labeled.yml"))),
        arrange(List(schedule)) == unreachable(nightly),
        arrange(List(tagless)) == unreachable(nightly),
        arrange(List(labelOnCi)) == unreachable(nightly),
        fileOf(arrange(List(affected), projects = List(coreModule))) == inCi,
        fileOf(arrange(List(steady))) == inCi,
        fileOf(arrange(List(branch))) == inCi,
        fileOf(arrange(List(tagged))) == inCi,
        fileOf(arrange(List(any))) == inCi,
        arrange(List(all)) == unreachable(nightly),
        fileOf(arrange(List(asked))) == inCi,
      )
    ,
    test("a negation runs when its condition can be false on that file, and is refused when it is always true"):
      val paths       = List(On.Push(List(PushBranch.Default), Nil, List("docs/**")))
      val either      = Cond.AnyOf(List(Cond.Event(EventKind.PullRequest), Cond.Event(EventKind.Push)))
      val unscheduled = rendered(Cond.Not(Cond.Event(EventKind.Schedule)), List(On.Dispatch(Nil)))
      val notPush     = rendered(Cond.Not(Cond.DefaultBranchPush), List(On.Dispatch(Nil)), JobName("dispatched"))
      val notHeard    = rendered(Cond.Not(Cond.DefaultBranchPush), List(On.pushDefault), JobName("push"))
      val never       = rendered(Cond.Not(Cond.True), Nil, JobName("never"))
      val twice       = rendered(Cond.Not(Cond.Not(Cond.True)), Nil, JobName("twice"))
      val manual      = rendered(Cond.Not(either), Nil, JobName("manual"))
      val offPaths    = rendered(Cond.Not(Cond.DefaultBranchPush), paths, JobName("paths"))
      val cronOnly    = Rendered.schedule(base(JobName("cron"), Cond.Not(Cond.Event(EventKind.Schedule))))
      val cronBranch  = Rendered.schedule(base(JobName("branch"), Cond.Not(Cond.DefaultBranch)))
      val onBranch    = Rendered.schedule(base(JobName("scheduled"), Cond.DefaultBranch))
      val dispatched  =
        Rendered.schedule(base(nightly, Cond.Not(Cond.Event(EventKind.Schedule)), List(On.Dispatch(Nil))))
      assertTrue(
        fileOf(arrange(List(unscheduled))) == inCi,
        fileOf(arrange(List(notPush))) == inCi,
        fileOf(arrange(List(notHeard))) == inCi,
        arrange(List(never)) == unreachable(JobName("never")),
        fileOf(arrange(List(twice))) == inCi,
        fileOf(arrange(List(manual))) == inCi,
        arrange(List(offPaths)) == unreachable(JobName("paths")),
        arrange(List(cronOnly)) == unreachable(JobName("cron")),
        arrange(List(cronBranch)) == unreachable(JobName("branch")),
        fileOf(arrange(List(onBranch))) == Right(List(WorkflowFile.Named("histos-scheduled.yml"))),
        fileOf(arrange(List(dispatched))) == Right(List(WorkflowFile.Named("histos-nightly.yml"))),
      )
    ,
    test("a dispatch input conflict and a ci concurrency conflict name that file"):
      val mode       = InputKey("mode")
      val text       = DispatchInput(mode, InputKind.Text, Some("a"))
      val other      = DispatchInput(mode, InputKind.Text, Some("b"))
      val bool       = DispatchInput(mode, InputKind.Boolean, Some("true"))
      val same       = DispatchInput(mode, InputKind.Text, Some("a"))
      val left       = rendered(Cond.True, List(On.Dispatch(List(text))), JobName("left"))
      val right      = rendered(Cond.True, List(On.Dispatch(List(other))), JobName("right"))
      val typed      = rendered(Cond.True, List(On.Dispatch(List(bool))), JobName("right"))
      val copy       = rendered(Cond.True, List(On.Dispatch(List(same))), JobName("right"))
      val group      = GroupName("ship")
      val otherGroup = GroupName("docs")
      def grouped(name: JobName, concurrency: Concurrency): CiJob =
        TaskJob(name, TaskName("echo"), concurrency = Some(concurrency))
      val first    = grouped(JobName("left"), Concurrency(group, Cancel.InProgress))
      val second   = grouped(JobName("right"), Concurrency(otherGroup, Cancel.InProgress))
      val shared   = grouped(JobName("right"), Concurrency(group, Cancel.InProgress))
      val recancel = grouped(JobName("right"), Concurrency(group, Cancel.Finish))
      assertTrue(
        arrange(List(left, right)) == refused(CiFault.DispatchInputConflict("mode")),
        arrange(List(left, typed)) == refused(CiFault.DispatchInputConflict("mode")),
        fileOf(arrange(List(left, copy))) == Right(List(WorkflowFile.Ci, WorkflowFile.Ci)),
        headerOf(arrange(List(left, copy))).map(_.inputs) == Right(List(text)),
        arrange(List(first, second)) == refused(CiFault.ConcurrencyConflict("ci.yml")),
        arrange(List(first, recancel)) == refused(CiFault.ConcurrencyConflict("ci.yml")),
        fileOf(arrange(List(first, shared))) == Right(List(WorkflowFile.Ci, WorkflowFile.Ci)),
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
        arrange(List(missing)) == refused(CiFault.UnpinnedWorkflowRun("missing.yml")),
        fileOf(arrange(List(ci))) == Right(List(WorkflowFile.Named("histos-follow.yml"))),
        arrange(List(release)) == refused(CiFault.UnpinnedWorkflowRun("histos-release.yml")),
        catalog.flatMap(found => fileOf(arrange(List(release), catalog = found))) ==
          Right(List(WorkflowFile.Named("histos-follow.yml"))),
        JobCheck.knownWorkflows(ProjectGraph.trusted(Nil), Nil) == Set("ci.yml"),
        catalog.map(found => JobCheck.knownWorkflows(ProjectGraph.trusted(Nil, Splits.aggregate, found), Nil)) ==
          Right(Set("ci.yml", "histos-release.yml", "histos-promote.yml")),
      )
    ,
    test("an affected guard names a module, and a repeated target is refused"):
      def guarded(module: ProjectName): CiJob =
        TaskJob(nightly, TaskName("echo"), guard = Some(Guard.Affected(::(module, Nil), Guard.Unguarded)))
      val target   = Target(TargetName("one"))
      val fan      = Rendered.fan(base(JobName("fan")), ::(target, List(target)))
      val distinct = Rendered.fan(base(JobName("fan")), ::(target, List(Target(TargetName("two")))))
      assertTrue(
        arrange(List(guarded(ProjectName("missing")))) ==
          refused(CiFault.UnknownModule(nightly, ProjectName("missing"))),
        fileOf(arrange(List(guarded(ProjectName("core"))), projects = List(coreModule))) == inCi,
        arrange(List(fan)) == refused(CiFault.DuplicateTarget(JobName("fan"), TargetName("one"))),
        fileOf(arrange(List(distinct))) == inCi,
      )
    ,
    test("placement follows the shape, and a cron or a splitting event leaves ci.yml"):
      val scheduled = TaskJob.nightly(nightly, TaskName("echo"), Cron("0 6 * * 1"))
      val paths     = TaskJob(JobName("paths"), TaskName("echo"), trigger = Trigger.Paths(::("docs/**", Nil)))
      val pages     = SiteJob(JobName("docs"), TaskName("site"), Site(RepoPath("docs"), SitePublish.Manual))
      val merge     = rendered(Cond.Event(EventKind.MergeGroup), List(On.MergeGroup), JobName("merge"))
      assertTrue(
        fileOf(arrange(List(scheduled))) == Right(List(WorkflowFile.Named("histos-nightly.yml"))),
        fileOf(arrange(List(paths))) == Right(List(WorkflowFile.Named("histos-paths.yml"))),
        fileOf(arrange(List(pages))) == Right(List(WorkflowFile.Ci, WorkflowFile.Pages)),
        fileOf(arrange(List(merge))) == inCi,
        headerOf(arrange(List(merge))).map(_.extras) == Right(Set(CiExtra.MergeGroup)),
        arrange(Nil) == Right(Nil),
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

  private def refused(fault: CiFault): Either[BuildError, List[Placed]] =
    Left(BuildError.Ci(fault))

  private def unreachable(name: JobName): Either[BuildError, List[Placed]] =
    refused(CiFault.UnreachableCond(name))

  private def fileOf(built: Either[BuildError, List[Placed]]): Either[BuildError, List[WorkflowFile]] =
    built.map(_.map(_.file))

  private def headerOf(built: Either[BuildError, List[Placed]]): Either[BuildError, CiHeader] =
    built.map(JobCheck.header)

  private def base(name: JobName, cond: Cond = Cond.True, events: List[On] = Nil): TaskArrange =
    TaskArrange(name, TaskName("echo"), JobPermissions.read, Nil, cond, events)

  private def rendered(cond: Cond, events: List[On], name: JobName = nightly): CiJob =
    Rendered.task(base(name, cond, events))

  private def site(name: JobName, shapes: List[SiteShape]): CiJob =
    Rendered.site(name, shapes)

  private def runs(workflows: List[String]): CiJob =
    rendered(Cond.Event(EventKind.WorkflowRun), List(On.WorkflowRun(workflows)), JobName("follow"))
end JobCheckLawSpec

private object Rendered:
  def task(arranged: TaskArrange): CiJob =
    new CiJob:
      def name: JobName               = arranged.name
      def task: TaskName              = arranged.task
      def permissions: JobPermissions = arranged.permissions
      def needs: List[JobName]        = arranged.needs
      def render: List[JobRender]     = List(JobRender.Task(arranged))

  def schedule(arranged: TaskArrange): CiJob =
    new CiJob:
      def name: JobName               = arranged.name
      def task: TaskName              = arranged.task
      def permissions: JobPermissions = arranged.permissions
      def needs: List[JobName]        = arranged.needs
      def render: List[JobRender]     = List(JobRender.Schedule(Cron("0 6 * * 1"), arranged))

  def fan(arranged: TaskArrange, targets: ::[Target]): CiJob =
    new CiJob:
      def name: JobName               = arranged.name
      def task: TaskName              = arranged.task
      def permissions: JobPermissions = arranged.permissions
      def needs: List[JobName]        = arranged.needs
      def render: List[JobRender]     = List(JobRender.Fan(arranged, targets))

  def site(name: JobName, shapes: List[SiteShape]): CiJob =
    val arranged = SiteArrange(
      name,
      TaskName("site"),
      JobPermissions.read,
      Nil,
      Site(RepoPath("docs")),
      Cond.True,
      List(On.Dispatch(Nil)),
    )
    new CiJob:
      def name: JobName               = arranged.name
      def task: TaskName              = arranged.task
      def permissions: JobPermissions = arranged.permissions
      def needs: List[JobName]        = arranged.needs
      def render: List[JobRender]     = shapes.map(shape => JobRender.Site(arranged, shape))
end Rendered
