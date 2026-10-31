package histos.build

import histos.cli.CliExit
import histos.report.{BuildFault, CallResult, Rejection}
import histos.resolve.ResolveError
import histos.server.{Call, Perform, ResolveAction}
import histos.task.RunId
import histos.testrun.TestMode
import histos.zinc.GraphCompileError
import histos.ProjectName

import zio.Chunk
import zio.stream.ZStream
import zio.test.*

object BuildSpec extends ZIOSpecDefault:
  private val missing = ResolveError.NotFound("o", "n", "1")
  private val lock    = GraphCompileError.Lock(missing)

  def spec = suite("build")(
    test("a fault is the exit, and a heard line is not a second result"):
      val build = new Build[Any]:
        def perform(call: Call) =
          ZStream.fromIterable(
            List(
              Beat.Heard(BuildEvent.Err("still running")),
              Beat.Finished(CallResult.Fault(BuildFault.Rejected(Rejection.NotATool))),
            )
          )
      build.perform(Call.Version).runCollect.map { beats =>
        val found = Build.result(beats)
        assertTrue(
          beats.length == 2,
          found.contains(CallResult.Fault(BuildFault.Rejected(Rejection.NotATool))),
          found.map(result => CliExit.from(Call.Version, result)).contains(CliExit.Usage),
        )
      }
    ,
    test("a missing lock is 8 for compile, 5 for test and run, and 9 for publish"):
      val compile = Perform.jdkMissed(Call.Compile(Nil), lock)
      val test    = Perform.jdkMissed(Call.Test(Nil, Nil, TestMode.Run), lock)
      val run     = Perform.jdkMissed(Call.Run("app", None), lock)
      val publish = Perform.jdkMissed(Call.PublishLocal(Nil), lock)
      val snaps   = Perform.jdkMissed(Call.PublishSnapshots(Nil), lock)
      val release = Perform.jdkMissed(Call.PublishRelease(Nil, false), lock)
      assertTrue(
        compile == CallResult.Fault(BuildFault.Resolve(missing)),
        CliExit.from(Call.Compile(Nil), compile) == CliExit.CompileFailed,
        test == CallResult.Fault(BuildFault.Resolve(missing)),
        CliExit.from(Call.Test(Nil, Nil, TestMode.Run), test) == CliExit.ResolveFailed,
        run == CallResult.Fault(BuildFault.Resolve(missing)),
        CliExit.from(Call.Run("app", None), run) == CliExit.ResolveFailed,
        publish == CallResult.Fault(BuildFault.Compile(lock)),
        snaps == CallResult.Fault(BuildFault.Compile(lock)),
        CliExit.from(Call.PublishLocal(Nil), publish) == CliExit.PublishFailed,
        CliExit.from(Call.PublishSnapshots(Nil), snaps) == CliExit.PublishFailed,
        release == CallResult.Fault(BuildFault.Compile(lock)),
        CliExit.from(Call.PublishRelease(Nil, false), release) == CliExit.PublishFailed,
      )
    ,
    test("version, cancel, and an empty clean do not adopt a graph"):
      assertTrue(
        Perform.needsGraph(Call.Compile(Nil)),
        Perform.needsGraph(Call.Test(Nil, Nil, TestMode.Quick)),
        Perform.needsGraph(Call.Clean(List(ProjectName("core")))),
        !Perform.needsGraph(Call.Clean(Nil)),
        !Perform.needsGraph(Call.Version),
        !Perform.needsGraph(Call.Cancel(RunId.trusted("run"))),
        !Perform.needsGraph(Call.Resolve(ResolveAction.Check)),
        !Perform.needsGraph(Call.Watch(Call.Version)),
        !Perform.needsGraph(Call.Reload),
      ),
  )
end BuildSpec
