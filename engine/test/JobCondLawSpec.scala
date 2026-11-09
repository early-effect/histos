package histos

import histos.doctor.HygieneChecks
import histos.plugin.*
import histos.workflow.*

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A closed condition is printed. A site job is what makes pages part of the graph. */
object JobCondLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  private val wide = new Trigger:
    def on: List[On] = List(
      On.PullRequest(List(PullType.Labeled), Nil),
      On.Push(List(PushBranch.Default), Nil, Nil),
      On.Dispatch(List(DispatchInput(InputKey("mode"), InputKind.Text, Some("fast")))),
      On.Release,
    )
    def cond: Cond = Cond.All(
      List(
        Cond.True,
        Cond.HasLabel("ready"),
        Cond.InputIs(InputKey("mode"), "fast"),
        Cond.Affected(ProjectName("core")),
        Cond.AffectedAll,
        Cond.Not(Cond.Event(EventKind.Push)),
        Cond.AnyOf(List(Cond.True, Cond.Event(EventKind.Release))),
      )
    )

  def spec = suite("job condition laws")(
    test("the condition prints a label, an input, affected modules, and a negation"):
      val gated = TaskJob(
        JobName("gated"),
        TaskName("echo"),
        trigger = wide,
        guard = Some(Guard.Affected(::(ProjectName("core"), Nil), Guard.Unguarded)),
      )
      val pages    = SiteJob(JobName("guide"), TaskName("echo"), Site(RepoPath.trusted("docs")))
      val withSite = Projects
        .organization(organization"example.com")
        .ci(CiSettings.empty)(use(new CondEcho(List(gated, pages))), module("core", Jvm)(scala3))
      val taskOnly = Projects
        .organization(organization"example.com")
        .ci(CiSettings.empty)(use(new CondEcho(List(gated))), module("core", Jvm)(scala3))
      (withSite, taskOnly) match
        case (Right(siteGraph), Right(taskGraph)) =>
          JobYaml.render(siteGraph, "main") match
            case Left(error)     => assertTrue(error.message == "")
            case Right(rendered) =>
              val body = (rendered.ci ++ rendered.files.map(_.body)).mkString("\n")
              assertTrue(
                body.contains("contains(github.event.pull_request.labels.*.name, 'ready')"),
                body.contains("github.event.inputs.mode == 'fast'"),
                body.contains("contains(fromJSON(needs.affected.outputs.modules), 'core')"),
                body.contains("contains(fromJSON(needs.affected.outputs.modules), 'all')"),
                body.contains("!github.event_name == 'push'"),
                body.contains("github.event_name == 'release'"),
                HygieneChecks.publishesPages(siteGraph),
                !HygieneChecks.publishesPages(taskGraph),
              )
        case (Left(error), _) => assertTrue(error.message == "")
        case (_, Left(error)) => assertTrue(error.message == "")
  )
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
