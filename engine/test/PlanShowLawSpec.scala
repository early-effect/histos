package histos.preview

import histos.select.SelectError
import histos.workspace.Workspace
import histos.{Jvm, Module, ProjectName, Projects, module, scala}

import java.nio.file.Path

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, test}

/** The first plan names every source as stale, and a test plan with no classes skips nothing. */
object PlanShowLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  def spec = suite("plan show laws")(
    test("a selected module names its upstream, and every source is stale until a compile exists"):
      val core  = module("core", Jvm)(scala3)
      val app   = module("app", Jvm)(scala3).dependsOn(core)
      val built = Projects(core, app)
      built match
        case Left(error) => ZIO.dieMessage(error.message)
        case Right(graph) =>
          graph.projects.collect { case item: Module => item } match
            case core :: app :: Nil =>
              Workspace.work("histos-plan-show").flatMap: repo =>
                val source = repo
                  .resolve(app.baseDirectory)
                  .resolve("src")
                  .resolve("main")
                  .resolve("scala")
                  .resolve("Hello.scala")
                for
                  _       <- Workspace.createDirectories(parentOf(source))
                  _       <- Workspace.writeString(source, "object Hello\n")
                  shown   <- Plan.show(repo, graph, List(app.name))
                  tested  <- Plan.test(repo, graph, List(app.name)).map(plan => Plan.renderTest(repo, plan))
                  missed  <- Plan.show(repo, graph, List(ProjectName.trusted("missing"))).either
                yield assertTrue(
                  shown.contains("core 3.10.0 jvm upstream"),
                  shown.contains("app 3.10.0 jvm selected"),
                  shown.contains("upstreams core"),
                  shown.contains("no previous compile"),
                  shown.contains("sources ") && shown.contains("Hello.scala"),
                  shown.contains("stale ") && shown.contains("Hello.scala"),
                  tested.contains("quick: no suite would be skipped"),
                  missed == Left(PreviewError.Target(SelectError.Unknown(ProjectName.trusted("missing"), Nil))),
                  core.name == ProjectName.trusted("core"),
                )
            case _ => ZIO.dieMessage("the graph did not keep core and app")
  )

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end PlanShowLawSpec
