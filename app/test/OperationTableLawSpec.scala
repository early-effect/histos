package histos.cli

import histos.cache.CacheCleanMode
import histos.schedule.{CacheNamespace, DevName}
import histos.server.{Call, LauncherAction, ResolveAction}
import histos.task.{RunId, TaskId}
import histos.workflow.WorkflowAction
import histos.{Platform, ProjectName}

import zio.test.*

object OperationTableLawSpec extends ZIOSpecDefault:
  def spec = suite("operation table laws")(
    test("every remaining command is the call it names, with its arguments in that order"):
      val checked =
        for
          module <- TaskId.from("api")
          id     <- RunId.from("run-1")
        yield
          val dev  = CacheNamespace.Dev(DevName("local"))
          val main = CacheNamespace.Main
          val rows = List(
            Command.Graph                                    -> Right(Call.Graph),
            Command.Client                                   -> Right(Call.Client),
            Command.Clean(List(ProjectName("api")))          -> Right(Call.Clean(List(ProjectName("api")))),
            Command.Assembly(None)                           -> Right(Call.Assembly(None)),
            Command.Assembly(Some("dist/app.jar"))           -> Right(Call.Assembly(Some("dist/app.jar"))),
            Command.PublishLocal(List("core"))               -> Right(Call.PublishLocal(List("core"))),
            Command.PublishSnapshots(Nil)                    -> Right(Call.PublishSnapshots(Nil)),
            Command.PublishPromote("deployment-1")           -> Right(Call.PublishPromote("deployment-1")),
            Command.PublishDrop("deployment-1")              -> Right(Call.PublishDrop("deployment-1")),
            Command.PublishAssets(List("core"))              -> Right(Call.PublishAssets(List("core"))),
            Command.Toolchain                                -> Right(Call.Toolchain),
            Command.CachePath                                -> Right(Call.CachePath),
            Command.CacheList                                -> Right(Call.CacheList),
            Command.CacheClean(CacheCleanMode.DryRun)        -> Right(Call.CacheClean(CacheCleanMode.DryRun)),
            Command.CachePromote(dev, main)                  -> Right(Call.CachePromote(dev, main)),
            Command.Resolve(ResolveAction.Update)            -> Right(Call.Resolve(ResolveAction.Update)),
            Command.Workflow(WorkflowAction.Check)           -> Right(Call.Workflow(WorkflowAction.Check)),
            Command.Launcher(LauncherAction.Latest)          -> Right(Call.Launcher(LauncherAction.Latest)),
            Command.Reload                                   -> Right(Call.Reload),
            Command.Reboot                                   -> Right(Call.Reboot),
            Command.Cancel(id)                               -> Right(Call.Cancel(id)),
            Command.PreviewCompile(List(ProjectName("api"))) ->
              Right(Call.PreviewCompile(List(ProjectName("api")))),
            Command.PreviewTest(Nil)                       -> Right(Call.PreviewTest(Nil)),
            Command.PreviewPublishLocal(List("core"))      -> Right(Call.PreviewPublishLocal(List("core"))),
            Command.PreviewPublishSnapshots(Nil)           -> Right(Call.PreviewPublishSnapshots(Nil)),
            Command.PreviewClean(List(ProjectName("api"))) -> Right(Call.PreviewClean(List(ProjectName("api")))),
            Command.PreviewCleanFull                       -> Right(Call.PreviewCleanFull),
            Command.PreviewRun(module, Some(Platform.Js))  -> Right(Call.PreviewRun("api", Some(Platform.Js))),
          )
          rows.filter((command, expected) => Operation.operation(command) != expected).map(_._1.toString)
      assertTrue(checked == Right(Nil))
  )
