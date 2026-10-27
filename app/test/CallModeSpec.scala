package histos.server

import histos.cache.{CacheCleanMode, CacheCleanModeError}
import histos.client.Session
import histos.mcp.HistosMcp
import histos.plugin.PluginArgs
import histos.report.{BuildFault, Rejection}
import histos.task.{RunId, TaskError}
import histos.testrun.{SuitePattern, SuitePatternError, TestMode, TestModeError}
import histos.workflow.{WorkflowAction, WorkflowActionError}
import histos.{Platform, PlatformError, ProjectName, ProjectNameError}

import zio.ZIO
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
    ,
    test("project names, patterns, a run id, a test mode, and a cache-clean mode round-trip"):
      ZIO.fromEither(SuitePattern.from("com.acme.Foo*")).map { pattern =>
        val core     = ProjectName("core")
        val compile  = Call.Compile(List(core))
        val tested   = Call.Test(List(core), List(pattern), TestMode.Quick)
        val publish  = Call.PublishLocal(Some("repo"), List("core"))
        val snaps    = Call.PublishSnapshots(List("core"))
        val cleaned  = Call.Clean(List(core))
        val cancel   = Call.Cancel(RunId.trusted("run-1"))
        val dry      = Call.CacheClean(CacheCleanMode.DryRun)
        val preview  = Call.PreviewTest(List(core))
        val ran      = Call.Test(Nil, Nil, TestMode.Run)
        val removed  = Call.CacheClean(CacheCleanMode.Remove)
        val compiled = Call.PreviewCompile(List(core))
        val shipped  = Call.PreviewPublishLocal(None, List("core"))
        val looked   = Call.PreviewPublishSnapshots(List("core"))
        val wiped    = Call.PreviewClean(List(core))
        assertTrue(
          roundTrip(compile),
          roundTrip(tested),
          roundTrip(publish),
          roundTrip(snaps),
          roundTrip(cleaned),
          roundTrip(cancel),
          roundTrip(dry),
          roundTrip(preview),
          roundTrip(compiled),
          roundTrip(shipped),
          roundTrip(looked),
          roundTrip(wiped),
          roundTrip(ran),
          roundTrip(removed),
          json(tested).contains("\"quick\""),
          json(tested).contains("com.acme.Foo*"),
          json(dry).contains("\"dry-run\""),
          json(cancel).contains("\"run-1\""),
          refused(json(compile).replace("\"core\"", "\"1core\"").fromJson[Call], ProjectNameError.Bad("1core").message),
          refused(json(compile).replace("\"core\"", "\"a/b\"").fromJson[Call], ProjectNameError.Bad("a/b").message),
          refused(json(tested).replace("com.acme.Foo*", "").fromJson[Call], SuitePatternError.Empty.message),
          refused(
            json(tested).replace("com.acme.Foo*", "foo bar").fromJson[Call],
            SuitePatternError.Blank("foo bar").message,
          ),
          refused(json(tested).replace("\"quick\"", "\"nope\"").fromJson[Call], TestModeError.Unknown("nope").message),
          refused(json(cancel).replace("run-1", "").fromJson[Call], TaskError.EmptyId.message),
          refused(
            json(dry).replace("\"dry-run\"", "\"nope\"").fromJson[Call],
            CacheCleanModeError.Unknown("nope").message,
          ),
          ProjectName.from("1core") == Left(ProjectNameError.Bad("1core")),
          ProjectName.from("a/b") == Left(ProjectNameError.Bad("a/b")),
          SuitePattern.from("") == Left(SuitePatternError.Empty),
          SuitePattern.from("foo bar") == Left(SuitePatternError.Blank("foo bar")),
          RunId.from("") == Left(TaskError.EmptyId),
          TestMode.from("nope") == Left(TestModeError.Unknown("nope")),
          TestMode.from("run") == Right(TestMode.Run),
          TestMode.from("quick") == Right(TestMode.Quick),
          CacheCleanMode.from("nope") == Left(CacheCleanModeError.Unknown("nope")),
          CacheCleanMode.from("remove") == Right(CacheCleanMode.Remove),
          CacheCleanMode.from("dry-run") == Right(CacheCleanMode.DryRun),
        )
      }
    ,
    test("mcp nests a bad token as a rejected fault"):
      val name    = ProjectNameError.Bad("1core")
      val slash   = ProjectNameError.Bad("a/b")
      val pattern = SuitePatternError.Empty
      val mode    = TestModeError.Unknown("nope")
      val clean   = CacheCleanModeError.Unknown("nope")
      val empty   = TaskError.EmptyId
      assertTrue(
        HistosMcp.compileCall(List("core")) == Right(Call.Compile(List(ProjectName("core")))),
        HistosMcp.compileCall(List("1core")) == Left(Rejection.ProjectName(name)),
        HistosMcp.testCall(Nil, Nil, None) == Right(Call.Test(Nil, Nil, TestMode.Run)),
        HistosMcp.testCall(Nil, Nil, Some("quick")) == Right(Call.Test(Nil, Nil, TestMode.Quick)),
        HistosMcp.testCall(Nil, Nil, Some("nope")) == Left(Rejection.TestMode(mode)),
        HistosMcp.testCall(Nil, List(""), None) == Left(Rejection.SuitePattern(pattern)),
        HistosMcp.testCall(List("a/b"), Nil, None) == Left(Rejection.ProjectName(slash)),
        HistosMcp.cacheCall(None) == Right(Call.CacheClean(CacheCleanMode.Remove)),
        HistosMcp.cacheCall(Some("dry-run")) == Right(Call.CacheClean(CacheCleanMode.DryRun)),
        HistosMcp.cacheCall(Some("nope")) == Left(Rejection.CacheClean(clean)),
        HistosMcp.cancelCall("") == Left(Rejection.RunId(empty)),
        HistosMcp.cancelCall("run-1") == Right(Call.Cancel(RunId.trusted("run-1"))),
        nest(HistosMcp.testCall(List("1core"), Nil, None), Rejection.ProjectName(name)),
        nest(HistosMcp.testCall(Nil, List(""), None), Rejection.SuitePattern(pattern)),
        nest(HistosMcp.testCall(Nil, Nil, Some("nope")), Rejection.TestMode(mode)),
        nest(HistosMcp.cacheCall(Some("nope")), Rejection.CacheClean(clean)),
        nest(HistosMcp.cancelCall(""), Rejection.RunId(empty)),
        Rejection.ProjectName(name).message == name.message,
        Rejection.SuitePattern(pattern).message == pattern.message,
        Rejection.TestMode(mode).message == mode.message,
        Rejection.CacheClean(clean).message == clean.message,
        Rejection.RunId(empty).message == empty.message,
      ),
  )

  private def roundTrip(call: Call): Boolean =
    call.toJson.fromJson[Call] == Right(call)

  private def json(call: Call): String =
    call.toJson

  private def nest(parsed: Either[Rejection, Call], reason: Rejection): Boolean =
    parsed match
      case Left(found) =>
        Session.CallProblem.Fault(BuildFault.Rejected(found)) ==
          Session.CallProblem.Fault(BuildFault.Rejected(reason))
      case Right(_) => false

  private def refused(parsed: Either[String, Call], message: String): Boolean =
    parsed match
      case Left(text) => text.contains(message)
      case Right(_)   => false
end CallModeSpec
