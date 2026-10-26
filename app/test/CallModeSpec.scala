package histos.server

import histos.client.Session
import histos.plugin.PluginArgs
import histos.report.BuildFault
import histos.workflow.{WorkflowAction, WorkflowActionError}
import histos.{Platform, PlatformError}

import zio.json.*
import zio.test.*

object CallModeSpec extends ZIOSpecDefault:
  private val resolveMiss = ResolveActionError.Unknown("nope").message
  private val flowMiss    = WorkflowActionError.Unknown("nope").message

  def spec = suite("call mode")(
    test("resolve, workflow, and platform keep their tokens"):
      val resolve     = Call.Resolve(ResolveAction.Check)
      val flow        = Call.Workflow(WorkflowAction.Update)
      val run         = Call.Run("app", Some(Platform.Js))
      val preview     = Call.PreviewRun("app", Some(Platform.Native))
      val resolveJson = resolve.toJson
      val flowJson    = flow.toJson
      val runJson     = run.toJson
      val badResolve  = resolveJson.replace("\"check\"", "\"nope\"").fromJson[Call]
      val badFlow     = flowJson.replace("\"update\"", "\"nope\"").fromJson[Call]
      assertTrue(
        resolveJson.fromJson[Call] == Right(resolve),
        flowJson.fromJson[Call] == Right(flow),
        runJson.fromJson[Call] == Right(run),
        preview.toJson.fromJson[Call] == Right(preview),
        resolveJson.contains("\"check\""),
        flowJson.contains("\"update\""),
        runJson.contains("\"js\""),
        refused(badResolve, resolveMiss),
        refused(badFlow, flowMiss),
        ResolveAction.from("verify") == Right(ResolveAction.Verify),
        ResolveAction.from("update") == Right(ResolveAction.Update),
        ResolveAction.from("check") == Right(ResolveAction.Check),
        ResolveAction.from("nope") == Left(ResolveActionError.Unknown("nope")),
        WorkflowAction.from("check") == Right(WorkflowAction.Check),
        WorkflowAction.from("update") == Right(WorkflowAction.Update),
        WorkflowAction.from("nope") == Left(WorkflowActionError.Unknown("nope")),
        Session.runCall("app", Some("nope"), PluginArgs.empty) ==
          Left(BuildFault.Platform(PlatformError.Unknown("nope"))),
        Session.runCall("app", Some("js"), PluginArgs.empty) == Right(run),
      )
  )

  private def refused(parsed: Either[String, Call], message: String): Boolean =
    parsed match
      case Left(text) => text.contains(message)
      case Right(_)   => false
end CallModeSpec
