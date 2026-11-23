package histos.plugin

import java.nio.file.Path

import zio.{Queue, UIO, ZIO}
import zio.test.*

object ProgramSpec extends ZIOSpecDefault:
  private val java: String =
    Path.of(sys.props.getOrElse("java.home", "")).resolve("bin").resolve("java").toString

  def spec = suite("Program")(
    test("stderr arrives before the exit code, and a closed handle still returns it") {
      for
        seen  <- Queue.unbounded[String]
        code  <- Program.run(List(java, "-version"), Path.of("."), recording(seen))
        lines <- seen.takeAll
        quiet <- Program.run(List(java, "-version"), Path.of("."), PluginEvents.closedInvocation)
      yield assertTrue(code == 0, quiet == 0, lines.nonEmpty)
    },
    test("a missing program fails before it looks like a status") {
      for failed <- Program.run(Nil, Path.of("."), PluginEvents.closedInvocation).flip
      yield assertTrue(failed == PluginRunError.Failed(PluginDetail("a program needs a command")))
    },
  )

  private def recording(seen: Queue[String]): PluginEvents.Invocation =
    new PluginEvents.Invocation:
      def emit(event: InvocationPluginEvent): UIO[Unit] =
        event match
          case InvocationPluginEvent.Err(text)     => seen.offer(text.value).unit
          case InvocationPluginEvent.Message(text) => seen.offer(text.value).unit
          case InvocationPluginEvent.Diagnosis(_)  => ZIO.unit
end ProgramSpec
