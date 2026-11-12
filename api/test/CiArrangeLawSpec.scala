package histos.plugin

import histos.ProjectName

import zio.test.*

object CiArrangeLawSpec extends ZIOSpecDefault:
  private val nightly = Cron("0 6 * * 1")
  private val docs    = JobName("docs")
  private val site    = TaskName("site")

  def spec = suite("ci arrange laws")(
    test("a task cron replaces the trigger with the events a cron file is allowed to keep"):
      val scheduled = TaskJob(JobName("nightly"), site, cron = Some(nightly))
      val dispatch  = TaskJob.nightly(JobName("nightly"), site, nightly)
      val tagged    = TaskJob(JobName("nightly"), site, trigger = Trigger.Tag("v*"), cron = Some(nightly))
      val labeled   = TaskJob(JobName("nightly"), site, trigger = Trigger.Label("ship"), cron = Some(nightly))
      assertTrue(
        renders(scheduled) == List("schedule"),
        condOf(scheduled) == Cond.True,
        eventsOf(scheduled).isEmpty,
        renders(dispatch) == List("schedule"),
        condOf(dispatch) == Cond.AnyOf(List(Trigger.Dispatch.cond, Cond.Event(EventKind.Schedule))),
        eventsOf(dispatch) == Trigger.Dispatch.on,
        condOf(tagged) == Cond.True,
        eventsOf(tagged).isEmpty,
        eventsOf(labeled) == Trigger.Label("ship").on,
      )
    ,
    test("a guard narrows the trigger, and an affected guard hears every named module"):
      val core     = ProjectName("core")
      val guarded  = TaskJob(docs, site, guard = Some(Guard.OnDefaultPush))
      val affected = TaskJob(docs, site, guard = Some(Guard.Affected(::(core, Nil), Guard.Unguarded)))
      assertTrue(
        condOf(guarded) == Cond.All(List(Trigger.Ci.cond, Cond.DefaultBranchPush)),
        Guard.cond(Guard.OnCi) == Trigger.Ci.cond,
        Guard.cond(Guard.Unguarded) == Cond.True,
        condOf(affected) == Cond.All(
          List(Trigger.Ci.cond, Cond.All(List(Cond.True, Cond.AnyOf(List(Cond.AffectedAll, Cond.Affected(core))))))
        ),
      )
    ,
    test("site publish chooses the shapes, and a site has no cron arm"):
      val directory = Site(RepoPath("docs"))
      val auto      = SiteJob(docs, site, directory.copy(publish = SitePublish.Automatic))
      val manual    = SiteJob(docs, site, directory.copy(publish = SitePublish.Manual))
      val both      = SiteJob(docs, site, directory)
      assertTrue(
        renders(auto) == List("deploy"),
        renders(manual) == List("build", "dispatch"),
        renders(both) == List("deploy", "dispatch"),
        condOf(auto) == Cond.DefaultBranchPush,
      )
    ,
    test("an event splits off ci.yml exactly when it would wake or cancel that file"):
      val opens   = On.PullRequest(On.defaultPull, Nil)
      val labeled = On.PullRequest(List(PullType.Labeled), Nil)
      val paths   = On.Push(List(PushBranch.Default), Nil, List("docs/**"))
      val tag     = On.Push(Nil, List("v*"), Nil)
      assertTrue(
        !On.splits(opens),
        On.splits(labeled),
        On.splits(On.PullRequest(On.defaultPull, List("docs/**"))),
        On.splits(paths),
        !On.splits(tag),
        !On.splits(On.pushDefault),
        On.splits(On.WorkflowRun(List("ci.yml"))),
        !On.splits(On.Dispatch(Nil)),
        !On.splits(On.MergeGroup),
        !On.splits(On.Release),
        On.withCron(On.Dispatch(Nil)),
        On.withCron(On.WorkflowRun(List("ci.yml"))),
        On.withCron(paths),
        !On.withCron(opens),
        !On.withCron(tag),
      )
    ,
    test("combining triggers keeps both event lists and hears either condition"):
      val combined = Trigger.Ci ++ Trigger.Dispatch
      assertTrue(
        combined.on == Trigger.Ci.on ++ Trigger.Dispatch.on,
        combined.cond == Cond.AnyOf(List(Trigger.Ci.cond, Trigger.Dispatch.cond)),
      )
    ,
    test("aws oidc asks for the token grant and the role inputs"):
      val action = AwsOidc(EnvValue.Plain("role"), EnvValue.Plain("us-east-1"), Some(EnvValue.Plain("registry")))
      assertTrue(
        action.name == AwsOidc.action,
        action.grants == Set(Grant.IdTokenWrite),
        action.inputs.map(_._1.value) == List("role-to-assume", "aws-region", "ecr-registry"),
      ),
  )

  private def renders(job: CiJob): List[String] =
    job.render
      .map:
        case JobRender.Task(_)                         => "task"
        case JobRender.Schedule(_, _)                  => "schedule"
        case JobRender.Fan(_, _)                       => "fan"
        case JobRender.Site(_, SiteShape.Build)        => "build"
        case JobRender.Site(_, SiteShape.DeployOnPush) => "deploy"
        case JobRender.Site(_, SiteShape.Dispatch)     => "dispatch"

  private def condOf(job: CiJob): Cond =
    job.render.map(view).headOption.getOrElse(Cond.True)

  private def eventsOf(job: CiJob): List[On] =
    job.render.flatMap(events)

  private def view(render: JobRender): Cond =
    render match
      case JobRender.Task(job)        => job.cond
      case JobRender.Schedule(_, job) => job.cond
      case JobRender.Fan(job, _)      => job.cond
      case JobRender.Site(job, _)     => job.cond

  private def events(render: JobRender): List[On] =
    render match
      case JobRender.Task(job)        => job.events
      case JobRender.Schedule(_, job) => job.events
      case JobRender.Fan(job, _)      => job.events
      case JobRender.Site(job, _)     => job.events
end CiArrangeLawSpec
