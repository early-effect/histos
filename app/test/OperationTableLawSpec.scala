package histos.cli

import histos.cache.CacheCleanMode
import histos.central.DeploymentId
import histos.publish.PublishToken
import histos.schedule.{CacheNamespace, DevName}
import histos.server.{Call, CleanExtent, ForgetTarget, ResolveAction}
import histos.task.{RunId, TaskId}
import histos.testrun.TestMode
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
            Command.Graph                                                 -> Right(Call.Graph),
            Command.Clean(List(ProjectName("api")), CleanExtent.Products) -> Right(
              Call.Clean(List(ProjectName("api")), CleanExtent.Products)
            ),
            Command.Clean(Nil, CleanExtent.Full)             -> Right(Call.Clean(Nil, CleanExtent.Full)),
            Command.Assembly(None)                           -> Right(Call.Assembly(None)),
            Command.Assembly(Some("dist/app.jar"))           -> Right(Call.Assembly(Some("dist/app.jar"))),
            Command.PublishLocal(List(PublishToken("core"))) -> Right(Call.PublishLocal(List(PublishToken("core")))),
            Command.PublishSnapshots(Nil)                    -> Right(Call.PublishSnapshots(Nil)),
            Command.PublishPromote(DeploymentId("deployment-1")) -> Right(
              Call.PublishPromote(DeploymentId("deployment-1"))
            ),
            Command.PublishDrop(DeploymentId("deployment-1")) -> Right(Call.PublishDrop(DeploymentId("deployment-1"))),
            Command.PublishAssets(List("core"))               -> Right(Call.PublishAssets(List("core"))),
            Command.CachePath                                 -> Right(Call.CachePath),
            Command.CacheList                                 -> Right(Call.CacheList),
            Command.CacheClean(CacheCleanMode.DryRun)         -> Right(Call.CacheClean(CacheCleanMode.DryRun)),
            Command.CachePromote(dev, main)                   -> Right(Call.CachePromote(dev, main)),
            Command.CacheForget(ForgetTarget.Module(ProjectName("api"))) ->
              Right(Call.CacheForget(ForgetTarget.Module(ProjectName("api")))),
            Command.CacheVerify(List(ProjectName("api")))    -> Right(Call.CacheVerify(List(ProjectName("api")))),
            Command.Resolve(ResolveAction.Update)            -> Right(Call.Resolve(ResolveAction.Update)),
            Command.Workflow(WorkflowAction.Check)           -> Right(Call.Workflow(WorkflowAction.Check)),
            Command.Reload                                   -> Right(Call.Reload),
            Command.Reboot                                   -> Left(CliParseError.Rejected),
            Command.Cancel(id)                               -> Right(Call.Cancel(id)),
            Command.PreviewCompile(List(ProjectName("api"))) ->
              Right(Call.PreviewCompile(List(ProjectName("api")))),
            Command.PreviewTest(Nil, AffectedChoice.Unset, TestMode.Run, Nil) -> Right(
              Call.PreviewTest(Nil, Nil, TestMode.Run)
            ),
            Command.PreviewPublishLocal(List(PublishToken("core"))) -> Right(
              Call.PreviewPublishLocal(List(PublishToken("core")))
            ),
            Command.PreviewPublishSnapshots(Nil) -> Right(Call.PreviewPublishSnapshots(Nil)),
            Command.PreviewClean(List(ProjectName("api")), CleanExtent.Products) -> Right(
              Call.PreviewClean(List(ProjectName("api")), CleanExtent.Products)
            ),
            Command.PreviewClean(Nil, CleanExtent.Full)   -> Right(Call.PreviewClean(Nil, CleanExtent.Full)),
            Command.PreviewRun(module, Some(Platform.Js)) -> Right(Call.PreviewRun("api", Some(Platform.Js))),
          )
          rows.filter((command, expected) => Operation.operation(command) != expected).map(_._1.toString)
      assertTrue(checked == Right(Nil))
  )
