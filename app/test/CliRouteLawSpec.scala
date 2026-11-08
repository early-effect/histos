package histos.cli

import histos.doctor.FaultText
import histos.load.{LoadError, Loader}
import histos.report.{BuildFault, CallResult, Rejection}
import histos.server.{Call, Message, ServerError, WatchPolicy}
import slayer.{stub, stubbed}

import java.nio.file.Path

import zio.*
import zio.test.*

object CliRouteLawSpec extends ZIOSpecDefault:
  private val repo = Path.of("repo")
  private val jar  = Path.of("api.jar")

  def spec = suite("cli route laws")(
    test("a reported call keeps the exit and a missing frame says the call did not finish"):
      for
        done <- Cli.recorded(repo, Call.Version, say => say(Message.Done(CallResult.reported())))
        open <- Cli.recorded(repo, Call.Version, _ => ZIO.unit)
        err  <- TestConsole.outputErr
      yield assertTrue(
        done == Completed(CliExit.Success, "", "", Playback.Heard),
        open.exit == CliExit.TaskFailed,
        open.err == "the call did not finish",
        open.playback == Playback.Heard,
        err.exists(_.contains("the call did not finish")),
      )
    ,
    test("an interrupt is heard and a server failure is a held load failure"):
      for
        stopped <- Cli.recorded(repo, Call.Version, _ => ZIO.interrupt)
        missing <- Cli.recorded(repo, Call.Version, _ => ZIO.fail(ServerError.NoServer))
      yield assertTrue(
        stopped == Completed(CliExit.Interrupted, "", "", Playback.Heard),
        missing == Completed(CliExit.LoadFailed, "", ServerError.NoServer.message, Playback.Held),
      )
    ,
    test("a fault frame is heard with the fault text"):
      val why  = BuildFault.Interrupted
      val text = FaultText.show(why.diagnose)
      for done <- Cli.recorded(repo, Call.Version, say => say(Message.Done(CallResult.Fault(why))))
      yield assertTrue(
        done.exit == CliExit.Interrupted,
        done.err == text,
        done.out.isEmpty,
        done.playback == Playback.Heard,
      )
    ,
    test("a program status of 2 is usage even when the call is version"):
      for done <- Cli.recorded(
          repo,
          Call.Version,
          say => say(Message.Done(CallResult.reported(program = Some(2)))),
        )
      yield assertTrue(done.exit == CliExit.Usage, done.playback == Playback.Heard)
    ,
    test("an empty watch does not load the build"):
      val program =
        Cli
          .watchCall(Nil, repo, jar)
          .either
          .map: got =>
            assertTrue(got == Left(BuildFault.Rejected(Rejection.Watch("watch needs a command"))))
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Loader])
    ,
    test("watch of compile does not load the build"):
      val program =
        Cli
          .watchCall(List("compile"), repo, jar)
          .either
          .map: got =>
            assertTrue(got == Right(Call.Watch(Call.Compile(Nil), WatchPolicy.After)))
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Loader])
    ,
    test("watch of bsp is usage and does not load the build"):
      val program =
        Cli
          .watchCall(List("bsp"), repo, jar)
          .either
          .map: got =>
            assertTrue(got == Left(BuildFault.Rejected(Rejection.Watch(CliParseError.Rejected.message))))
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Loader])
    ,
    test("watch of an unknown root is the load fault"):
      val program =
        for
          _ <- stub[Loader](_.load(slayer.any[Path], slayer.any[Path]))((root: Path, _: Path) =>
            ZIO.fail(LoadError.MissingFile(root))
          )
          got <- Cli.watchCall(List("nope"), repo, jar).either
        yield assertTrue(got == Left(BuildFault.Load(LoadError.MissingFile(repo))))
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Loader]),
  )
end CliRouteLawSpec
