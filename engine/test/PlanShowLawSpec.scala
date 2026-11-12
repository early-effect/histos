package histos.preview

import histos.select.SelectError
import histos.workspace.Workspace
import histos.{Jvm, ProjectName, Projects, module, scala}

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, test}

/** The first plan names every source as stale, and a test plan with no classes skips nothing. */
object PlanShowLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  def spec = suite("plan show laws")(
    test("a selected module names its upstream, an unselected one is omitted, and every source is stale"):
      val core = module("core", Jvm)(scala3)
      val app  = module("app", Jvm)(scala3).dependsOn(core)
      val idle = module("idle", Jvm)(scala3)
      for
        graph <- ZIO.fromEither(Projects(core, app, idle))
        repo  <- Workspace.work("histos-plan-show")
        source = repo.resolve("app").resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
        _      <- Workspace.writeString(source, "object Hello\n")
        shown  <- Plan.show(repo, graph, List(ProjectName("app")))
        tested <- Plan.test(repo, graph, List(ProjectName("app"))).map(plan => Plan.renderTest(repo, plan))
        missed <- Plan.show(repo, graph, List(ProjectName("missing"))).either
      yield assertTrue(
        shown.contains("core 3.10.0 jvm upstream"),
        shown.contains("app 3.10.0 jvm selected"),
        shown.contains("upstreams core"),
        shown.contains("no previous compile"),
        shown.contains("sources ") && shown.contains("Hello.scala"),
        shown.contains("stale ") && shown.contains("Hello.scala"),
        !shown.contains("idle"),
        tested.contains("app 3.10.0 jvm selected"),
        tested.contains("quick: no suite would be skipped"),
        !tested.contains("quick skip"),
        !tested.contains("idle"),
        missed == Left(PreviewError.Target(SelectError.Unknown(ProjectName("missing"), Nil))),
      )
  )
end PlanShowLawSpec
