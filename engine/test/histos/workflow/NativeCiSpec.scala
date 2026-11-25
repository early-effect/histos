package histos.workflow

import histos.*
import histos.suite.HistosSpec

import zio.test.*

object NativeCiSpec extends HistosSpec:
  def spec = suite("native CI packages")(
    test("one tool calculation consumes selected stage platforms and a supplied producer closure") {
      val consumer =
        native(Set.empty).copy(name = ProjectName("consumer"), platforms = Platforms(Jvm, List(Js, Native)))
      val producer      = native(Set(NativeLibrary.Ssl)).copy(name = ProjectName("producer"))
      val graph         = ProjectGraph.trusted(List(consumer, producer))
      val tools         = RunnerTools(Some(NodeFloor(25)), Some(ToolRevision("21.1.8")), None)
      val consumerStage = CompilationStage(Cell(consumer.name, scala"3.9.0", Jvm), histos.plugin.SourceSet.Test)
      val producerStage = CompilationStage(Cell(producer.name, scala"3.9.0", Native), histos.plugin.SourceSet.Main)
      val selected      = Workflow.toolNeed(graph, List(consumerStage), tools)
      val closure       = Workflow.toolNeed(graph, List(consumerStage, producerStage), tools)
      assertTrue(
        selected == Workflow.ToolNeed(Set.empty),
        closure == Workflow.ToolNeed(Set(Workflow.Tool.Llvm, Workflow.Tool.OpenSsl)),
      )
    },
    test("openssl is installed when a native module links it") {
      assertTrue(NativeCi.openssl(List(native(Set(NativeLibrary.Ssl)))))
    },
    test("a JVM module does not ask for openssl") {
      val jvm = native(Set(NativeLibrary.Ssl)).copy(platforms = Platforms(Platform.Jvm, Nil), libraries = Set.empty)
      assertTrue(!NativeCi.openssl(List(jvm)))
    },
    test("the package list includes the linker and libssl") {
      assertTrue(NativeCi.packages.contains("libssl-dev"), NativeCi.packages.contains("clang"))
    },
  )

  private def native(libraries: Set[NativeLibrary]): Module =
    Module(
      ProjectName("core"),
      Platforms(Platform.Native, Nil),
      ScalaVersions(scala"3.9.0", Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
      libraries = libraries,
    )
end NativeCiSpec
