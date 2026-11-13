package histos.server

import histos.cache.CacheCleanMode
import histos.central.DeploymentId
import histos.format.{FormatMode, FormatTargets}
import histos.plugin.{FlagName, NamedText, ParamName, PluginArgs}
import histos.publish.PublishToken
import histos.schedule.CacheNamespace
import histos.task.RunId
import histos.testrun.{SuitePattern, TestMode}
import histos.server.{CleanExtent, WatchPolicy}
import histos.workflow.WorkflowAction
import histos.{Platform, WireSamples}

import zio.NonEmptyChunk

/** Every `Call` constructor, plus the variants whose fields change a document. */
object CallSamples:
  import WireSamples.*

  private val dev = histos.schedule.DevName("russ")
  private val pr  = histos.version.PullRequestNumber("4")

  def oneEach: List[Call] =
    val names = List(core)
    val args  = PluginArgs(List(NamedText(param, "x")), Set(flag))
    List(
      Call.Compile(names),
      Call.Test(names, List(SuitePattern("App*")), TestMode.Run),
      Call.PublishLocal(List(PublishToken.trusted(ship.value))),
      Call.PublishSnapshots(Nil),
      Call.PublishRelease(List(ship.value), true),
      Call.PublishPromote(DeploymentId("id")),
      Call.PublishDrop(DeploymentId("id")),
      Call.PublishAssets(List("app")),
      Call.Graph,
      Call.Run(core.value, Some(Platform.Js)),
      Call.Task(taskId.render, args),
      Call.Cancel(RunId("run-1")),
      Call.Resolve(ResolveAction.Verify),
      Call.Workflow(WorkflowAction.Check),
      Call.Format(FormatTargets.Build, FormatMode.Check),
      Call.Assembly(List("app")),
      Call.Clean(names, CleanExtent.Products),
      Call.Reload,
      Call.Reboot(Successor(Some("histos 0.3.1"))),
      Call.Version,
      Call.CachePath,
      Call.CacheList,
      Call.CacheClean(CacheCleanMode.Remove),
      Call.CachePromote(CacheNamespace.Main, CacheNamespace.Dev(dev)),
      Call.CacheForget(ForgetTarget.Namespace(CacheNamespace.Main)),
      Call.CacheVerify(Nil),
      Call.Watch(Call.Compile(names), WatchPolicy.After),
      Call.Bsp,
      Call.PreviewCompile(names),
      Call.PreviewTest(names, Nil, TestMode.Run),
      Call.PreviewPublishLocal(Nil),
      Call.PreviewPublishSnapshots(List(ship.value)),
      Call.PreviewClean(names, CleanExtent.Products),
      Call.PreviewRun(core.value, None),
    )

  def every: List[Call] =
    oneEach.flatMap: call =>
      call match
        case Call.Compile(_) =>
          List(call, Call.Compile(Nil))
        case Call.Test(names, patterns, _) =>
          List(call, Call.Test(Nil, Nil, TestMode.Quick))
        case Call.PublishRelease(tokens, _) =>
          List(call, Call.PublishRelease(Nil, false))
        case Call.Run(id, _) =>
          List(call, Call.Run(id, None))
        case Call.Task(_, _) =>
          List(call, Call.Task(histos.task.TaskId.Inspect.value, PluginArgs.empty))
        case Call.Resolve(_) =>
          List(call, Call.Resolve(ResolveAction.Update), Call.Resolve(ResolveAction.Check))
        case Call.Workflow(_) =>
          List(call, Call.Workflow(WorkflowAction.Update))
        case Call.Format(_, _) =>
          List(
            call,
            Call.Format(FormatTargets.Projects(NonEmptyChunk(core)), FormatMode.Write),
            Call.Format(FormatTargets.Files(NonEmptyChunk(java.nio.file.Path.of("core/src/A.scala"))), FormatMode.Write),
          )
        case Call.Assembly(_) =>
          List(call, Call.Assembly(Nil))
        case Call.CacheClean(_) =>
          List(call, Call.CacheClean(CacheCleanMode.DryRun))
        case Call.CachePromote(_, _) =>
          List(call, Call.CachePromote(CacheNamespace.PullRequest(pr), CacheNamespace.Main))
        case Call.Clean(_, _) =>
          List(call, Call.Clean(Nil, CleanExtent.Full))
        case Call.PreviewClean(_, _) =>
          List(call, Call.PreviewClean(Nil, CleanExtent.Full))
        case Call.PreviewTest(_, _, _) =>
          List(call, Call.PreviewTest(Nil, Nil, TestMode.Quick))
        case Call.CacheForget(_) =>
          List(call, Call.CacheForget(ForgetTarget.Module(core)))
        case Call.CacheVerify(_) =>
          List(call, Call.CacheVerify(List(core)))
        case Call.Watch(_, _) =>
          List(
            call,
            Call.Watch(Call.Test(Nil, Nil, TestMode.Run), WatchPolicy.After),
            Call.Watch(Call.Run(core.value, None), WatchPolicy.Restart),
            Call.Watch(Call.Watch(Call.Compile(List(core)), WatchPolicy.After), WatchPolicy.After),
          )
        case Call.PreviewRun(id, _) =>
          List(call, Call.PreviewRun(id, Some(Platform.Native)))
        case Call.Reboot(_) =>
          List(call, Call.Reboot(Successor(None)))
        case other =>
          List(other)
end CallSamples
