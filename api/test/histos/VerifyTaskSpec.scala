package histos

import histos.plugin.{
  Contribution,
  DeclaredTask,
  HistosPlugin,
  JobCheck,
  JobName,
  PluginId,
  PluginOutput,
  RegisteredPlugin,
  RunRequest,
  Seal,
  TaskJob,
  TaskName,
}
import histos.suite.HistosSpec
import histos.version.Catalog

import zio.ZIO
import zio.test.*

object VerifyTaskSpec extends HistosSpec:
  private val host = VerifyRun.BuiltIn(TaskName("host"))

  private val api = module("api", Jvm)(scala"3.10.1")

  def spec = suite("verify tasks")(
    test("a build that does not ask has no verify task") {
      val built = Projects(api)
      assertTrue(
        built.map(_.verify) == Right(Nil),
        built.map(graph => JobCheck.generated(graph).contains(JobName("publish"))) == Right(false),
      )
    },
    test("verify appends a task and does not add a publish job") {
      val task  = VerifyTask(JobName("client"), host, ClientLink.Native)
      val built = Projects.verify(task)(api)
      assertTrue(
        built.map(_.verify) == Right(List(task)),
        built.map { graph =>
          val names = JobCheck.generated(graph)
          names.contains(JobName("client")) && !names.contains(JobName("publish"))
        } == Right(true),
      )
    },
    test("a second verify call appends") {
      val first  = VerifyTask(JobName("client"), host, ClientLink.Native)
      val second = VerifyTask(JobName("bench"), host)
      val built  = Projects.verify(first).verify(second)(api)
      assertTrue(built.map(_.verify) == Right(List(first, second)))
    },
    test("the same verify job twice is rejected") {
      val task  = VerifyTask(JobName("client"), host)
      val built = Projects.verify(task).verify(task)(api)
      assertTrue(built == Left(BuildError.Ci(CiFault.DuplicateVerify(JobName("client")))))
    },
    test("a verify job cannot take the test job") {
      val built = Projects.verify(VerifyTask(JobName("test"), host))(api)
      assertTrue(built == Left(BuildError.Ci(CiFault.VerifyJobTaken(JobName("test")))))
    },
    test("a plugin job cannot reuse a verify task's job name") {
      val plugin = new HistosPlugin:
        val id                       = PluginId("idle")
        def contribute: Contribution = Contribution(Nil, Nil, Nil)
        def run(request: RunRequest) = ZIO.succeed(PluginOutput("", Nil))
      val task       = DeclaredTask(TaskName("pack"), Seal.Hermetic, Set.empty, Nil, Nil, Nil)
      val registered = RegisteredPlugin(
        PluginId("idle"),
        Contribution(List(task), Nil, List(TaskJob(JobName("client"), TaskName("pack")))),
      )(plugin)
      val graph = ProjectGraph.trusted(
        Nil,
        Splits.aggregate,
        Catalog.empty,
        None,
        Nil,
        List(registered),
        verify = List(VerifyTask(JobName("client"), host)),
      )
      assertTrue(JobCheck.arrange(graph) == Left(BuildError.Ci(CiFault.RenderedJob(JobName("client")))))
    },
  )
end VerifyTaskSpec
