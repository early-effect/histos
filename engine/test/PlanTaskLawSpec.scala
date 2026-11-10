package histos.preview

import histos.plugin.{
  Contribution,
  DeclaredTask,
  HistosPlugin,
  PluginId,
  RegisteredPlugin,
  RunRequest,
  Seal,
  TaskName,
}
import histos.version.Catalog
import histos.{Platform, ProjectGraph, ProjectNameError, Splits}

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, test}

/** A built-in name and a plugin task are runs. A platform on either name is refused. */
object PlanTaskLawSpec extends ZIOSpecDefault:
  private val plugin = PluginId("law")
  private val pack   = TaskName("pack")
  private val graph  = ProjectGraph.trusted(
    Nil,
    Splits.aggregate,
    Catalog.empty,
    None,
    Nil,
    plugins = List(
      RegisteredPlugin(
        plugin,
        Contribution(List(DeclaredTask(pack, Seal.Hermetic, Set.empty, Nil, Nil, Nil)), Nil, Nil),
      )(Unrun)
    ),
  )

  private object Unrun extends HistosPlugin:
    def id: PluginId             = plugin
    def contribute: Contribution = Contribution(Nil, Nil, Nil)
    def run(request: RunRequest) = ZIO.dieMessage("a plan never runs a plugin")

  def spec = suite("plan task laws")(
    test("a built-in name and a known plugin task are that run, and a platform is refused"):
      val known  = "law/pack"
      val absent = "law/missing"
      val shape  = "a/b/c"
      assertTrue(
        Plan.run(graph, "inspect", None) == Right(RunPlan(RunUnit.Task("inspect"))),
        Plan.renderRun(RunPlan(RunUnit.Task("inspect"))) == "inspect",
        Plan.run(graph, "inspect", Some(Platform.Jvm)) ==
          Left(PreviewError.Run(RunMiss.BadId("run inspect does not take a platform"))),
        Plan.run(graph, known, None) == Right(RunPlan(RunUnit.Task(known))),
        Plan.renderRun(RunPlan(RunUnit.Task(known))) == known,
        Plan.run(graph, known, Some(Platform.Jvm)) ==
          Left(PreviewError.Run(RunMiss.BadId(s"run $known does not take a platform"))),
        Plan.run(graph, absent, None) == Left(PreviewError.Run(RunMiss.UnknownTask(absent))),
        Plan.run(graph, shape, None) ==
          Left(PreviewError.Run(RunMiss.BadId(s"'$shape' is not a plugin task id"))),
        Plan.run(graph, "", None) == Left(PreviewError.Run(RunMiss.BadId(ProjectNameError.Empty.message))),
      )
  )
end PlanTaskLawSpec
