package histos

import histos.doctor.HygieneChecks
import histos.plugin.*
import histos.workflow.*

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A closed condition is printed. A site job is what makes pages part of the graph. */
object JobCondLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  private def trigger(events: List[On], condition: Cond): Trigger = new Trigger:
    def on: List[On] = events
    def cond: Cond   = condition

  private val pushOrDispatch = List(On.Push(List(PushBranch.Default), Nil, Nil), On.Dispatch(Nil))

  private val wide = trigger(
    List(
      On.PullRequest(List(PullType.Labeled), Nil),
      On.Push(List(PushBranch.Default), Nil, Nil),
      On.Dispatch(List(DispatchInput(InputKey("mode"), InputKind.Text, Some("fast")))),
      On.Release,
    ),
    // A label is on a pull request and an input is on a dispatch, so no one run has both.
    Cond.All(
      List(
        Cond.True,
        Cond.AnyOf(List(Cond.HasLabel("ready"), Cond.InputIs(InputKey("mode"), "fast"))),
        Cond.Affected(ProjectName("core")),
        Cond.AffectedAll,
        Cond.Not(Cond.Event(EventKind.Push)),
        Cond.AnyOf(List(Cond.True, Cond.Event(EventKind.Release))),
      )
    ),
  )

  def spec = suite("job condition laws")(
    test("a negation wraps its operand, so GitHub negates the comparison and not the field"):
      val job = TaskJob(
        JobName("gated"),
        TaskName("echo"),
        trigger = trigger(pushOrDispatch, Cond.Not(Cond.Event(EventKind.Push))),
      )
      for
        graph    <- ZIO.fromEither(graphOf(List(job)))
        rendered <- ZIO.fromEither(JobYaml.render(graph, "main"))
        lines = (rendered.ci ++ rendered.files.map(_.body)).flatMap(_.linesIterator)
      yield assertTrue(lines.contains("    if: \"!(github.event_name == 'push')\""))
    ,
    test("the condition prints a label, an input, affected modules, a release, and a negation"):
      val gated = TaskJob(
        JobName("gated"),
        TaskName("echo"),
        trigger = wide,
        guard = Some(Guard.Affected(::(ProjectName("core"), Nil), Guard.Unguarded)),
      )
      for
        graph    <- ZIO.fromEither(graphOf(List(gated)))
        rendered <- ZIO.fromEither(JobYaml.render(graph, "main"))
        body = (rendered.ci ++ rendered.files.map(_.body)).mkString("\n")
      yield assertTrue(
        body.contains("contains(github.event.pull_request.labels.*.name, 'ready')"),
        body.contains("github.event.inputs.mode == 'fast'"),
        body.contains("contains(fromJSON(needs.affected.outputs.modules), 'core')"),
        body.contains("contains(fromJSON(needs.affected.outputs.modules), 'all')"),
        body.contains("!(github.event_name == 'push')"),
        body.contains("github.event_name == 'release'"),
      )
    ,
    test("a site job makes the graph publish pages, and a task job alone does not"):
      val task = TaskJob(JobName("gated"), TaskName("echo"))
      val site = SiteJob(JobName("guide"), TaskName("echo"), Site(RepoPath.trusted("docs")))
      for
        withSite <- ZIO.fromEither(graphOf(List(task, site)))
        taskOnly <- ZIO.fromEither(graphOf(List(task)))
      yield assertTrue(HygieneChecks.publishesPages(withSite), !HygieneChecks.publishesPages(taskOnly)),
  )

  private def graphOf(jobs: List[CiJob]): Either[BuildError, ProjectGraph] =
    Projects
      .organization(organization"example.com")
      .ci(CiSettings.empty)(use(new CondEcho(jobs)), module("core", Jvm)(scala3))
end JobCondLawSpec

private final class CondEcho(jobs: List[CiJob]) extends HistosPlugin:
  def id: PluginId = PluginId("test.cond")

  def contribute: Contribution =
    Contribution(
      List(DeclaredTask(TaskName("echo"), Seal.Open, Set.empty, Nil, Nil, Nil)),
      Nil,
      jobs,
    )

  def run(request: RunRequest): zio.ZIO[zio.Scope, PluginRunError, PluginOutput] =
    zio.ZIO.succeed(PluginOutput(request.id.render, Nil))
end CondEcho
