package histos.bsp

import histos.report.WireJson
import histos.server.{Call, CallSamples, ResolveAction}
import histos.workflow.WorkflowAction
import histos.{EnumLaw, Platform, WireSamples}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets

import zio.json.*
import zio.test.*

import WireJson.given

object FrameLawSpec extends ZIOSpecDefault:
  import WireSamples.*

  private val limit = 8 * 1024 * 1024

  def spec = suite("frame laws")(
    test("put then take is the payload, and the next read is the end"):
      check(Gen.string): payload =>
        val out = ByteArrayOutputStream()
        val put = Frame.put(out, payload)
        val in  = ByteArrayInputStream(out.toByteArray)
        assertTrue(
          put == Right(()),
          Frame.take(in) == Right(Some(payload)),
          Frame.take(in) == Right(None),
        )
    ,
    test("two frames keep their order, and unicode length is utf-8 bytes"):
      val accent = "é"
      val face   = "🙂"
      val body   = accent.getBytes(StandardCharsets.UTF_8)
      val header = s"Content-Length: ${body.length}\r\n\r\n".getBytes(StandardCharsets.US_ASCII)
      val short  = s"Content-Length: 1\r\n\r\n".getBytes(StandardCharsets.US_ASCII) ++ body.slice(0, 1)
      val out    = ByteArrayOutputStream()
      val wrote  = Frame.put(out, accent).flatMap(_ => Frame.put(out, face))
      val in     = ByteArrayInputStream(out.toByteArray)
      assertTrue(
        wrote == Right(()),
        Frame.take(in) == Right(Some(accent)),
        Frame.take(in) == Right(Some(face)),
        Frame.take(in) == Right(None),
        body.length == 2,
        Frame.take(ByteArrayInputStream(header ++ body)) == Right(Some(accent)),
        Frame.take(ByteArrayInputStream(short)) == Left(Frame.FrameError.Encoding),
      )
    ,
    test("a bad frame is the typed error"):
      val errors = List(
        Frame.FrameError.Header,
        Frame.FrameError.MissingLength,
        Frame.FrameError.BadLength,
        Frame.FrameError.Truncated,
        Frame.FrameError.TooLarge,
        Frame.FrameError.Encoding,
        Frame.FrameError.Io,
      )
      val ascii                                     = StandardCharsets.US_ASCII
      def frame(text: String): ByteArrayInputStream =
        ByteArrayInputStream(text.getBytes(ascii))
      val high = Array[Byte](0x80.toByte) ++ "\r\n\r\n".getBytes(ascii)
      val huge = new Array[Byte](8192)
      java.util.Arrays.fill(huge, 'a'.toByte)
      val early = new Array[Byte](8191)
      java.util.Arrays.fill(early, 'a'.toByte)
      val body = Array[Byte](0xff.toByte)
      assertTrue(
        EnumLaw.covered(errors),
        Frame.take(ByteArrayInputStream(Array.emptyByteArray)) == Right(None),
        Frame.take(frame("Content-Length: 0\r\n\r\n")) == Right(Some("")),
        Frame.take(frame("content-LENGTH: 1\r\n\r\nZ")) == Right(Some("Z")),
        Frame.take(frame("X-Ignore: no\r\nContent-Length: 1\r\n\r\nZ")) == Right(Some("Z")),
        Frame.take(frame("Content-Length:  2 \r\n\r\nHi")) == Right(Some("Hi")),
        Frame.take(frame("Content-Length: 00\r\n\r\n")) == Right(Some("")),
        Frame.take(ByteArrayInputStream(high)) == Left(Frame.FrameError.Header),
        Frame.take(frame(":\r\n\r\n")) == Left(Frame.FrameError.Header),
        Frame.take(frame("Content-Length\r\n\r\n")) == Left(Frame.FrameError.Header),
        Frame.take(frame("X-Other: 1\r\n\r\n")) == Left(Frame.FrameError.MissingLength),
        Frame.take(frame("\r\n\r\n")) == Left(Frame.FrameError.MissingLength),
        Frame.take(frame("Content-Length: 1\r\nContent-Length: 1\r\n\r\nZ")) == Left(Frame.FrameError.BadLength),
        Frame.take(frame("Content-Length: -1\r\n\r\n")) == Left(Frame.FrameError.BadLength),
        Frame.take(frame("Content-Length: 1a\r\n\r\n")) == Left(Frame.FrameError.BadLength),
        Frame.take(frame("Content-Length: 4\r\n\r\nab")) == Left(Frame.FrameError.Truncated),
        Frame.take(frame("Content-Length: 1")) == Left(Frame.FrameError.Truncated),
        Frame.take(ByteArrayInputStream(early)) == Left(Frame.FrameError.Truncated),
        Frame.take(ByteArrayInputStream(huge)) == Left(Frame.FrameError.TooLarge),
        Frame.take(frame(s"Content-Length: ${limit + 1}\r\n\r\n")) == Left(Frame.FrameError.TooLarge),
        Frame.take(frame(s"Content-Length: $limit\r\n\r\nx")) == Left(Frame.FrameError.Truncated),
        Frame.take(frame("Content-Length: 999999999999\r\n\r\n")) == Left(Frame.FrameError.TooLarge),
        Frame.put(ByteArrayOutputStream(), "a" * (limit + 1)) == Left(Frame.FrameError.TooLarge),
        Frame.take(ByteArrayInputStream("Content-Length: 1\r\n\r\n".getBytes(ascii) ++ body)) ==
          Left(Frame.FrameError.Encoding),
        Frame.take(PipedInputStream()) == Left(Frame.FrameError.Io),
        Frame.put(PipedOutputStream(), "a") == Left(Frame.FrameError.Io),
      )
    ,
    test("every call round-trips, and an action document is its token, not the case name"):
      val calls = CallSamples.every
      assertTrue(
        EnumLaw.covered(CallSamples.oneEach),
        calls.map(call => call.toJson.fromJson[Call]) == calls.map(Right(_)),
        Call.encode(Call.Resolve(ResolveAction.Verify)) == """{"Resolve":{"action":"verify"}}""",
        Call.encode(Call.Workflow(WorkflowAction.Update)) == """{"Workflow":{"action":"update"}}""",
        """{"Resolve":{"action":"Verify"}}""".fromJson[Call].isLeft,
        """{"Workflow":{"action":"Update"}}""".fromJson[Call].isLeft,
      )
    ,
    test("batch argv is the call it names, and a stray token is none"):
      val names = List(core, api)
      assertTrue(
        histos.client.Batch.parse(List("compile", "core", "api")) == Some(Call.Compile(names)),
        histos.client.Batch.parse(List("compile", "api", "core")) == Some(Call.Compile(List(api, core))),
        histos.client.Batch.parse(List("compile")) == Some(Call.Compile(Nil)),
        histos.client.Batch.parse(List("compile", "-core")) == None,
        histos.client.Batch.parse(List("compile", "core", "1core")) == None,
        histos.client.Batch.parse(List("test", "core")) ==
          Some(Call.Test(List(core), Nil, histos.testrun.TestMode.Run)),
        histos.client.Batch.parse(List("run", "core")) == Some(Call.Run("core", None)),
        histos.client.Batch.parse(List("run", "core", "js")) == Some(Call.Run("core", Some(Platform.Js))),
        histos.client.Batch.parse(List("run", "core", "nope")) == None,
        histos.client.Batch.parse(List("run", "core", "js", "extra")) == None,
        histos.client.Batch.parse(List("run", "inspect")) ==
          Some(Call.Task(histos.task.TaskId.Inspect.value, histos.plugin.PluginArgs.empty)),
        histos.client.Batch.parse(List("run", "versions")) ==
          Some(Call.Task(histos.task.TaskId.Versions.value, histos.plugin.PluginArgs.empty)),
        histos.client.Batch.parse(List("run", "doc")) ==
          Some(Call.Task(histos.task.TaskId.Doc.value, histos.plugin.PluginArgs.empty)),
        histos.client.Batch.parse(List("run", "inspect", "jvm")) == None,
        histos.client.Batch.parse(List("run", "demo/site")) ==
          Some(Call.Task(taskId.render, histos.plugin.PluginArgs.empty)),
        histos.client.Batch.parse(List("run", "demo/site", "jvm")) == None,
        histos.client.Batch.parse(List("run", "no/such/task")) == None,
        histos.client.Batch.parse(List("clean", "core", "api")) == Some(
          Call.Clean(names, histos.server.CleanExtent.Products)
        ),
        histos.client.Batch.parse(List("clean", "--full")) == Some(Call.Clean(Nil, histos.server.CleanExtent.Full)),
        histos.client.Batch.parse(List("cleanFull")) == None,
        histos.client.Batch.parse(List("clean", "--full", "core")) == None,
        histos.client.Batch.parse(List("graph")) == None,
        histos.client.Batch.parse(Nil) == None,
      ),
  )
end FrameLawSpec
