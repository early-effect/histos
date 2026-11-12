package histos.client

import histos.build.BuildEvent
import histos.plugin.{NamedText, ParamName, PluginArgs}
import histos.report.{BuildFault, CallResult, Rejection, Report}
import histos.server.{Call, Feed, Message, ServerError}
import histos.{Platform, PlatformError}

import java.nio.file.Path

import zio.{Chunk, ZIO}
import zio.json.*
import zio.test.*

object SessionLawSpec extends ZIOSpecDefault:
  private val repo = Path.of("repo")
  private val args = PluginArgs(List(NamedText(ParamName("name"), "ada")), Set.empty)

  def spec = suite("session laws")(
    test("a main may name a platform and a task may not take arguments or a platform"):
      val rows = List(
        Session.runCall("api", None, PluginArgs.empty)          -> Right(Call.Run("api", None)),
        Session.runCall("api", Some("jvm"), PluginArgs.empty)   -> Right(Call.Run("api", Some(Platform.Jvm))),
        Session.runCall("api", Some("bogus"), PluginArgs.empty) -> Left(
          BuildFault.Platform(PlatformError.Unknown("bogus"))
        ),
        Session.runCall("api", Some("jvm"), args) -> Left(BuildFault.Rejected(Rejection.TaskArguments("api"))),
        Session.runCall("inspect", Some("jvm"), PluginArgs.empty) -> Left(
          BuildFault.Rejected(Rejection.TaskPlatform("inspect"))
        ),
        Session.runCall("inspect", None, args)                      -> Right(Call.Task("inspect", args)),
        Session.runCall("demo/pack", Some("jvm"), PluginArgs.empty) -> Left(
          BuildFault.Rejected(Rejection.TaskPlatform("demo/pack"))
        ),
        Session.runCall("demo/pack", None, args)           -> Right(Call.Task("demo/pack", args)),
        Session.runCall("", None, PluginArgs.empty)        -> Left(BuildFault.Rejected(Rejection.MissingRunName)),
        Session.runCall("", Some("jvm"), PluginArgs.empty) -> Left(BuildFault.Rejected(Rejection.MissingRunName)),
        Session.runCall("", None, args)                    -> Left(BuildFault.Rejected(Rejection.MissingRunName)),
      )
      val bad = rows.filter { (got, expected) => got != expected }
      assertTrue(bad == Nil)
    ,
    test("a missing frame did not finish and a fault is returned as it arrived"):
      val fault = CallResult.Fault(BuildFault.Interrupted)
      val live  = List(Feed.Event(BuildEvent.Out("live")), Feed.Done(CallResult.reported()))
      val kept  = CallResult.Reported(Report.empty.copy(out = Chunk("kept"), err = Chunk("e0")))
      val mixed = List(
        Feed.Event(BuildEvent.Out("live")),
        Feed.Event(BuildEvent.Out("kept")),
        Feed.Event(BuildEvent.Err("e0")),
        Feed.Event(BuildEvent.Err("e1")),
        Feed.Done(kept),
      )
      assertTrue(
        Session.spoken(repo, Nil) == Left(Session.TransportError.DidNotFinish),
        Session.spoken(repo, List(Feed.Done(fault))) == Right(fault),
        Session.spoken(repo, live) == Right(CallResult.Reported(Report.empty.copy(out = Chunk("live")))),
        Session.spoken(repo, mixed) == Right(
          CallResult.Reported(Report.empty.copy(out = Chunk("kept", "live"), err = Chunk("e0", "e1")))
        ),
        Session.TransportError.DidNotFinish.message == "the call did not finish",
        Session.TransportError.DifferentVersion.message == ServerError.DifferentVersion.message,
      )
    ,
    test("a report is the tool value and a fault or a transport error is the problem"):
      for
        reported <- Session.deliver(ZIO.succeed(CallResult.reported(summary = "boom"))).either
        faulted  <- Session.deliver(ZIO.succeed(CallResult.Fault(BuildFault.Interrupted))).either
        closed   <- Session.deliver(ZIO.fail(Session.TransportError.DidNotFinish)).either
      yield assertTrue(
        reported == Right(CallResult.reported(summary = "boom") match
          case CallResult.Reported(report) => report
          case CallResult.Fault(_)         => Report.empty),
        reported match
          case Right(report) => report.err == Chunk("boom")
          case Left(_)       => false
        ,
        faulted == Left(Session.CallProblem.Fault(BuildFault.Interrupted)),
        closed == Left(Session.CallProblem.Transport(Session.TransportError.DidNotFinish)),
      )
    ,
    test("an event continues, a done stops, and any other frame is protocol"):
      val out  = wire(Message.Event(BuildEvent.Out("x")))
      val done = wire(Message.Done(CallResult.reported()))
      assertTrue(
        Remote.event(out) == Right(Remote.Event.More(Feed.Event(BuildEvent.Out("x")))),
        Remote.event(done) == Right(Remote.Event.Stop(Feed.Done(CallResult.reported()))),
        Remote.event(wire(Message.Ready)) == Left(ServerError.Protocol),
        Remote.event(wire(Message.Marker(0))) == Left(ServerError.Protocol),
        Remote.event("nope") == Left(ServerError.Protocol),
      ),
  )

  private def wire(message: Message): String =
    message.toJson
end SessionLawSpec
