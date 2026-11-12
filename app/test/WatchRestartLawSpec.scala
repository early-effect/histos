package histos.server

import histos.build.BuildEvent
import histos.report.{BuildFault, CallResult, Diagnostic, Report, Severity}

import java.nio.file.Path

import zio.*
import zio.test.*

/** A restart replaces the program only when its compile succeeded and the change is one of the program's roots. The
  * compile's result reaches the watch, and its text still prints.
  */
object WatchRestartLawSpec extends ZIOSpecDefault:
  private val root    = Path.of("/work/repo/app/src")
  private val inside  = List(root.resolve("App.scala"))
  private val outside = List(Path.of("/work/repo/other/src/Other.scala"))
  private val watched = Watching.Watched(List(root), Set.empty)
  private val compile = Watching.compileFor(Call.Run("app", None))

  private val broken = CallResult.Reported(
    Report.empty.copy(diagnostics =
      Chunk(Diagnostic(Some("app/src/App.scala"), Some(3), Some(1), None, None, Severity.Error, "type mismatch"))
    )
  )

  def spec = suite("watch restart laws")(
    test("a restart compiles the program's module, or the build when the id is not a module"):
      assertTrue(
        compile == Call.Compile(List(histos.ProjectName("app"))),
        Watching.compileFor(Call.Run("specular/preview", None)) == Call.Compile(Nil),
        Watching.compileFor(Call.Compile(Nil)) == Call.Compile(Nil),
      )
    ,
    test("only a successful compile of a change under a root replaces the program"):
      assertTrue(
        Watching.restarts(compile, Some(CallResult.reported()), inside, watched),
        !Watching.restarts(compile, Some(CallResult.reported()), outside, watched),
        !Watching.restarts(compile, Some(broken), inside, watched),
        !Watching.restarts(compile, Some(CallResult.reported("app failed to compile")), inside, watched),
        !Watching.restarts(compile, Some(CallResult.Fault(BuildFault.Interrupted)), inside, watched),
        !Watching.restarts(compile, None, inside, watched),
      )
    ,
    test("the inner result lands in the ref, its text still prints, and the session does not end"):
      for
        sent     <- Ref.make(Chunk.empty[Message])
        finished <- Ref.make(Option.empty[CallResult])
        say = new Say(message => sent.update(_ :+ message)).dropExitInto(finished)
        _      <- say(Message.Event(BuildEvent.Out("compiling app")))
        _      <- say(Message.Done(broken))
        seen   <- sent.get
        result <- finished.get
      yield assertTrue(
        seen == Chunk(Message.Event(BuildEvent.Out("compiling app"))),
        result == Some(broken),
      ),
  )
end WatchRestartLawSpec
