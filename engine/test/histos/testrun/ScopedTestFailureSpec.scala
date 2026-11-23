package histos.testrun

import histos.*
import histos.build.{BuildNode, ExecutionError}
import histos.plugin.{OutputKind, OutputName, PluginId, PluginTaskId, TaskName}
import histos.report.WireJson.given
import histos.zinc.{GeneratedRootId, StageRootError}
import histos.native.NativeError

import java.nio.file.Path

import zio.json.*
import zio.test.*

object ScopedTestFailureSpec extends ZIOSpecDefault:
  def spec = suite("scoped Test boundary errors")(
    test("root ownership and effective link identity survive typed adaptation and wire round trips") {
      val task   = PluginTaskId(PluginId("generated"), TaskName("resource"))
      val output = OutputName("file")
      val cell   = Cell(ProjectName("consumer"), ScalaVersion("3.10.1"), Js)
      val root   = TestFailures(
        ExecutionError.Roots(
          StageRootError.OutputKindMismatch(GeneratedRootId(task, output), OutputKind.File, OutputKind.Directory)
        )
      )
      val missing = TestFailures(ExecutionError.MissingProduct(BuildNode.Link(cell, JsOptimize.Full)))
      assertTrue(
        root == ModuleTestError.Scoped(
          ScopedTestFailure.OutputKindMismatch(task, output, OutputKind.File, OutputKind.Directory)
        ),
        missing == ModuleTestError.Scoped(
          ScopedTestFailure.Node(TestNodeFailure.Missing, TestFailureNode.Link(cell, JsOptimize.Full))
        ),
        root.toJson.fromJson[ModuleTestError] == Right(root),
        missing.toJson.fromJson[ModuleTestError] == Right(missing),
      )
    },
    test("Native input read-ownership failures preserve their exact paths") {
      val error = NativeError.InputsChanged(List(Path.of("member.jar")))
      assertTrue(error.toJson.fromJson[NativeError] == Right(error))
    },
  )
