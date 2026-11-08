package histos.cli

import histos.plugin.{
  ApiDigest,
  CommandName,
  CommandSpec,
  Contribution,
  DeclaredInput,
  DeclaredTask,
  EntryName,
  EnvName,
  HistosPlugin,
  InputKey,
  JobName,
  Need,
  PluginDetail,
  PluginId,
  PluginLinked,
  RegisteredPlugin,
  RepoPath,
  RunRequest,
  Seal,
  TaskJob,
  TaskName,
}
import histos.load.PluginLink
import histos.version.Catalog
import histos.{
  Aggregate,
  Module,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
  Platform,
}

import zio.ZIO
import zio.test.*

object GraphTextLawSpec extends ZIOSpecDefault:
  private val digest =
    ApiDigest("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  private val id = PluginId("demo")

  private object Unrun extends HistosPlugin:
    def id: PluginId             = GraphTextLawSpec.id
    def contribute: Contribution = Contribution(Nil, Nil, Nil)
    def run(request: RunRequest) = ZIO.dieMessage("a graph listing never runs a plugin")

  def spec = suite("graph text laws")(
    test("a module and an aggregate render their tokens and a plugin hides env and text"):
      val text = GraphText.render(graph, List(PluginLink(id, PluginLinked.FromSource(digest, digest), Nil)))
      assertTrue(
        text.contains("api jvm 3.3.4"),
        text.contains("root aggregate api,core"),
        text.contains(s"plugin demo source ${digest.value} ${digest.value}"),
        text.contains("task demo/pack Hermetic Network src"),
        text.contains("command pack"),
        text.contains("job nightly"),
        !text.contains("secret"),
        !text.contains("HOME"),
      )
    ,
    test("a plugin with no link is binary and an empty graph is empty"):
      val binary = GraphText.render(graph, Nil)
      val empty  = GraphText.render(ProjectGraph.trusted(Nil), Nil)
      assertTrue(
        binary.contains("plugin demo binary"),
        !binary.contains("source"),
        empty.isEmpty,
      ),
  )

  private def graph: ProjectGraph =
    val api = Module(
      ProjectName("api"),
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion("3.3.4"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )
    val root = Aggregate(
      ProjectName("root"),
      List(ProjectName("api"), ProjectName("core")),
      SourcePos("histos.scala", 2, 1),
    )
    val task = DeclaredTask(
      TaskName("pack"),
      Seal.Hermetic,
      Set(Need.Network),
      List(
        DeclaredInput.File(RepoPath("src")),
        DeclaredInput.Text(InputKey("note"), "secret"),
        DeclaredInput.Env(EnvName("HOME")),
      ),
      Nil,
      Nil,
    )
    val command = CommandSpec(CommandName("pack"), Nil, TaskName("pack"), PluginDetail.trusted("packs it"))
    val plugin  = RegisteredPlugin(
      id,
      EntryName.trusted("demo.Pack"),
      Contribution(List(task), List(command), List(TaskJob(JobName("nightly"), TaskName("pack")))),
    )(Unrun)
    ProjectGraph.trusted(List(api, root), Splits.aggregate, Catalog.empty, None, Nil, List(plugin))
end GraphTextLawSpec
