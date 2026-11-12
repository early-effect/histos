package histos.server

import histos.cache.CacheCleanMode
import histos.format.{FormatMode, FormatTargets}
import histos.plugin.{FlagName, NamedText, ParamName, PluginArgs}
import histos.schedule.CacheNamespace
import histos.task.RunId
import histos.testrun.{SuitePattern, TestMode}
import histos.server.WatchPolicy
import histos.workflow.WorkflowAction
import histos.{Platform, WireSamples}

import zio.NonEmptyChunk

/** Every `Call` constructor, plus the variants whose fields change an exit or a document. */
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
      Call.PublishLocal(List(ship.value)),
      Call.PublishSnapshots(Nil),
      Call.PublishRelease(List(ship.value), true),
      Call.PublishPromote("id"),
      Call.PublishDrop("id"),
      Call.PublishAssets(List("app")),
      Call.Graph,
      Call.Run(core.value, Some(Platform.Js)),
      Call.Client,
      Call.Task(taskId.render, args),
      Call.Cancel(RunId.trusted("run-1")),
      Call.Toolchain,
      Call.Resolve(ResolveAction.Verify),
      Call.Workflow(WorkflowAction.Check),
      Call.Format(FormatTargets.Build, FormatMode.Check),
      Call.Assembly(Some("dist/app.jar")),
      Call.Clean(names),
      Call.CleanFull,
      Call.Reload,
      Call.Reboot,
      Call.Version,
      Call.CachePath,
      Call.CacheList,
      Call.CacheClean(CacheCleanMode.Remove),
      Call.CachePromote(CacheNamespace.Main, CacheNamespace.Dev(dev)),
      Call.Launcher(LauncherAction.Update),
      Call.Watch(Call.Compile(names), WatchPolicy.After),
      Call.Bsp,
      Call.PreviewCompile(names),
      Call.PreviewTest(names),
      Call.PreviewPublishLocal(Nil),
      Call.PreviewPublishSnapshots(List(ship.value)),
      Call.PreviewClean(names),
      Call.PreviewCleanFull,
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
          List(call, Call.Assembly(None))
        case Call.CacheClean(_) =>
          List(call, Call.CacheClean(CacheCleanMode.DryRun))
        case Call.CachePromote(_, _) =>
          List(call, Call.CachePromote(CacheNamespace.PullRequest(pr), CacheNamespace.Main))
        case Call.Launcher(_) =>
          List(call, Call.Launcher(LauncherAction.Latest))
        case Call.Watch(_, _) =>
          List(
            call,
            Call.Watch(Call.Test(Nil, Nil, TestMode.Run), WatchPolicy.After),
            Call.Watch(Call.Run(core.value, None), WatchPolicy.Restart),
            Call.Watch(Call.Watch(Call.Compile(List(core)), WatchPolicy.After), WatchPolicy.After),
          )
        case Call.PreviewRun(id, _) =>
          List(call, Call.PreviewRun(id, Some(Platform.Native)))
        case other =>
          List(other)
end CallSamples
