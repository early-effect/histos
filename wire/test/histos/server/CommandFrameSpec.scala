package histos.server

import histos.build.BuildEvent
import histos.report.CallResult

import zio.Chunk
import zio.json.*
import zio.test.*

object CommandFrameSpec extends ZIOSpecDefault:
  private val line  = BuildEvent.Out("heddle compiling")
  private val bytes = Chunk[Byte](1, 2, 3)
  private val done  = CallResult.reported()

  private def frame(message: Message): String = message.toJson

  def spec = suite("a command frame says whether the stream closed")(
    test("a closed stream is not an unknown frame") {
      val closed = ServerError.Closed
      assertTrue(
        CommandFrame.read(None) == Left(closed),
        CommandFrame.ready(None) == Left(closed),
        !closed.message.contains("does not understand"),
      )
    },
    test("a payload that is not an event, output, or done stays a protocol error") {
      val unknown = ServerError.Protocol
      assertTrue(
        CommandFrame.read(Some("nope")) == Left(unknown),
        CommandFrame.read(Some(frame(Message.Ready))) == Left(unknown),
        CommandFrame.read(Some(frame(Message.Marker(1)))) == Left(unknown),
        CommandFrame.read(Some(frame(Message.Output("%%%")))) == Left(unknown),
        CommandFrame.ready(Some(frame(Message.Event(line)))) == Left(unknown),
        unknown.message.contains("does not understand"),
      )
    },
    test("an event, output, and done are the command stream") {
      assertTrue(
        CommandFrame.read(Some(frame(Message.Event(line)))) == Right(Feed.Event(line)),
        CommandFrame.read(Some(frame(Message.Output(RunBytes.encode(bytes))))) == Right(Feed.Output(bytes)),
        CommandFrame.read(Some(frame(Message.Done(done)))) == Right(Feed.Done(done)),
        CommandFrame.ready(Some(frame(Message.Ready))) == Right(()),
      )
    },
  )
end CommandFrameSpec
