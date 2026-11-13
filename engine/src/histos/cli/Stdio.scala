package histos.cli

import java.io.IOException

import zio.{UIO, ULayer, ZIO, ZLayer}

/** Where a standard stream goes. A pipe and a file are both redirected. */
enum Attached:
  case Terminal
  case Redirected

/** This process's standard input and output. Colour reads the output. A prompt needs both on a terminal, which is the
  * test `System.console()` makes. This is the one answer to whether a person is there: the command line, `new`, `init`,
  * and doctor all read it.
  */
final case class Stdio(input: Attached, output: Attached):
  /** A person can read a question and type the answer. */
  def prompts: Boolean = input == Attached.Terminal && output == Attached.Terminal

object Stdio:
  /** No person on either stream, as for a pipe, a file, or an MCP process, whose streams are the protocol. */
  val redirected: Stdio = Stdio(Attached.Redirected, Attached.Redirected)

  /** `test -t` on each inherited descriptor, once, when the layer is built. A probe that cannot start is redirected. */
  val probed: ULayer[Stdio] =
    ZLayer(attached(0).zipWithPar(attached(1))(Stdio(_, _)))

  private def attached(descriptor: Int): UIO[Attached] =
    ZIO
      .attemptBlocking(
        ProcessBuilder("/bin/sh", "-c", s"test -t $descriptor")
          .redirectInput(ProcessBuilder.Redirect.INHERIT)
          .redirectOutput(ProcessBuilder.Redirect.INHERIT)
          .redirectError(ProcessBuilder.Redirect.INHERIT)
          .start()
          .waitFor()
      )
      .refineToOrDie[IOException]
      .fold(_ => Attached.Redirected, status => if status == 0 then Attached.Terminal else Attached.Redirected)
end Stdio
