package histos.server

import histos.bsp.{Bsp, Rpc}
import histos.cache.CacheCleanMode
import histos.plugin.PluginArgs
import histos.task.RunId
import histos.testrun.TestMode
import histos.workflow.WorkflowAction
import histos.ProjectName

import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, Promise, Queue, Ref, ZIO, durationInt}
import zio.test.*

object BspParallelSpec extends ZIOSpecDefault:
  private enum Fault:
    case Json(text: String)

  private final case class Failed(error: Rpc.ErrBody) derives JsonCodec

  private val CompileDone = "compile-finished"

  def spec =
    suite("bsp parallel")(
      test("one consumer writes whole frames in order"):
        ZIO.scoped:
          val first  = "frame-one"
          val second = "frame-two"
          for
            release    <- Promise.make[Nothing, Unit]
            entered    <- Promise.make[Nothing, Unit]
            finished   <- Promise.make[Nothing, Unit]
            active     <- Ref.make(false)
            overlapped <- Ref.make(false)
            seen       <- Ref.make(Chunk.empty[String])
            send = (line: String) =>
              active.get.flatMap { running =>
                ZIO.when(running)(overlapped.set(true)) *>
                  active.set(true) *>
                  entered.succeed(()) *>
                  release.await *>
                  seen.update(_ :+ line) *>
                  active.set(false) *>
                  seen.get.flatMap { lines =>
                    ZIO.when(lines.length == 2)(finished.succeed(())).unit
                  }
              }
            box     <- Daemon.outbox(send)
            _       <- box.offer(first)
            _       <- entered.await
            _       <- box.offer(second)
            pending <- seen.get
            clash   <- overlapped.get
            open    <- release.isDone
            _       <- release.succeed(()).unit
            _       <- finished.await
            lines   <- seen.get
            again   <- overlapped.get
          yield assertTrue(
            pending.isEmpty,
            clash == false,
            open == false,
            again == false,
            lines == Chunk(first, second),
          )
      ,
      test("a blocked answer still replies, and cancel leaves the loop usable"):
        ZIO.scoped:
          val compile = rpc(1, "buildTarget/compile")
          val query   = rpc(2, "workspace/buildTargets")
          val later   = rpc(3, "workspace/buildTargets")
          for
            inbound <- Queue.unbounded[Option[String]]
            replies <- Queue.unbounded[String]
            heard   <- Ref.make(List.empty[String])
            release <- Promise.make[Nothing, Unit]
            halted  <- Promise.make[Nothing, Unit]
            offer  = (line: String) => replies.offer(line) *> heard.update(_ :+ line).unit
            answer = (payload: String) =>
              payload.fromJson[Rpc.Incoming] match
                case Right(incoming) if incoming.method.contains("buildTarget/compile") =>
                  release.await.onInterrupt(halted.succeed(()).unit).as(Bsp.Answer.Reply(CompileDone))
                case _ =>
                  ZIO.succeed(Bsp.Answer.Reply(payload))
            idle          <- ZIO.unit.forkScoped
            loop          <- Daemon.requests(inbound.take, offer, answer, idle).forkScoped
            _             <- inbound.offer(Some(compile)) *> inbound.offer(Some(query))
            during        <- replies.take
            early         <- replies.poll
            _             <- inbound.offer(Some(cancel(Json.Num(1))))
            cancelledText <- replies.take
            cancelled     <- failed(cancelledText)
            _             <- halted.await
            _             <- inbound.offer(Some(later))
            laterText     <- replies.take
            _             <- inbound.offer(Some(cancel(Json.Num(99))))
            unknownText   <- replies.take
            unknown       <- failed(unknownText)
            _             <- inbound.offer(Some(cancelMissing))
            missingText   <- replies.take
            missing       <- failed(missingText)
            lines         <- heard.get
            _             <- inbound.offer(None)
            code          <- loop.join
          yield assertTrue(
            during == query,
            early.isEmpty,
            cancelled.code == -32800,
            cancelled.message == "cancelled",
            sameId(cancelledText, 1),
            laterText == later,
            unknown.code == -32602,
            unknown.message == "unknown request",
            sameId(unknownText, 99),
            missing.code == -32602,
            missing.message == Rpc.ParamsDecode.Missing.message,
            absentId(missingText),
            lines.contains(CompileDone) == false,
            code == 1,
          )
      ,
      test("cancel params are a typed id"):
        assertTrue(
          Rpc.cancelId(None) == Left(Rpc.ParamsDecode.Missing),
          Rpc.cancelId(Some(Json.Null)) == Left(Rpc.ParamsDecode.Rejected),
          Rpc.cancelId(Some(Json.Obj())) == Left(Rpc.ParamsDecode.Rejected),
          Rpc.cancelId(Some(Json.Arr())) == Left(Rpc.ParamsDecode.Rejected),
          Rpc.cancelId(Some(Json.Obj("id" -> Json.Null))) == Left(Rpc.ParamsDecode.Rejected),
          Rpc.cancelId(Some(Json.Obj("id" -> Json.Num(4)))) == Right(Rpc.RpcId.Num(4)),
          Rpc.cancelId(Some(Json.Obj("id" -> Json.Str("req-4")))) == Right(Rpc.RpcId.Text("req-4")),
          Rpc.ParamsDecode.Missing.message == "bsp params are required",
          Rpc.ParamsDecode.Rejected.message == "bsp params were rejected",
        )
      ,
      test("only output writers take the permit"):
        assertTrue(
          outputLaw.forall { case (call, writes) =>
            Daemon.writesOutputs(call) == writes
          }
        ),
    ) @@ TestAspect.timeout(5.seconds)

  private def outputLaw: List[(Call, Boolean)] =
    List(
      Call.Compile(Nil)                      -> true,
      Call.Test(Nil, Nil, TestMode.Run)      -> true,
      Call.PublishLocal(Some("repo"), Nil)   -> true,
      Call.PublishSnapshots(Nil)             -> true,
      Call.PublishRelease(Nil, false)        -> true,
      Call.PublishPromote("abc")             -> false,
      Call.PublishDrop("abc")                -> false,
      Call.PublishAssets(Nil)                -> false,
      Call.Run("demo", None)                 -> true,
      Call.Assembly(None)                    -> true,
      Call.Clean(Nil)                        -> true,
      Call.Clean(List(ProjectName("demo")))  -> true,
      Call.CleanFull                         -> true,
      Call.Graph                             -> false,
      Call.Task("inspect", PluginArgs.empty) -> false,
      Call.Cancel(RunId.trusted("run"))      -> false,
      Call.Toolchain                         -> false,
      Call.Resolve(ResolveAction.Verify)     -> false,
      Call.Workflow(WorkflowAction.Check)    -> false,
      Call.Reload                            -> false,
      Call.Reboot                            -> false,
      Call.Version                           -> false,
      Call.CachePath                         -> false,
      Call.CacheList                         -> false,
      Call.CacheClean(CacheCleanMode.DryRun) -> false,
      Call.LauncherUpdate                    -> false,
      Call.Watch(Call.Graph)                 -> false,
      Call.Bsp                               -> false,
      Call.PreviewCompile(Nil)               -> false,
      Call.PreviewTest(Nil)                  -> false,
      Call.PreviewPublishLocal(None, Nil)    -> false,
      Call.PreviewPublishSnapshots(Nil)      -> false,
      Call.PreviewClean(Nil)                 -> false,
      Call.PreviewCleanFull                  -> false,
      Call.PreviewRun("demo", None)          -> false,
    )

  private def rpc(id: Int, method: String): String =
    Rpc.Incoming(method = Some(method), id = Some(Rpc.RpcId.Num(id))).toJson

  private def cancel(id: Json): String =
    Json
      .Obj(
        "jsonrpc" -> Json.Str("2.0"),
        "method"  -> Json.Str("$/cancelRequest"),
        "params"  -> Json.Obj("id" -> id),
      )
      .toJson

  private def cancelMissing: String =
    Json.Obj("jsonrpc" -> Json.Str("2.0"), "method" -> Json.Str("$/cancelRequest")).toJson

  private def failed(text: String): ZIO[Any, Fault, Rpc.ErrBody] =
    ZIO.fromEither(text.fromJson[Failed]).mapBoth(_ => Fault.Json(text), _.error)

  private def field(text: String, name: String): Option[Json] =
    text.fromJson[Json] match
      case Right(Json.Obj(fields)) =>
        fields.collectFirst { case (`name`, value) => value }
      case _ => None

  private def sameId(text: String, id: Int): Boolean =
    field(text, "id") match
      case Some(Json.Num(value)) =>
        value.compareTo(java.math.BigDecimal.valueOf(id.toLong)) == 0
      case _ => false

  private def absentId(text: String): Boolean =
    field(text, "id") match
      case None | Some(Json.Null) => true
      case Some(_)                => false
end BspParallelSpec
