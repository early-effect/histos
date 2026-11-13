package histos.supply

import histos.doctor.HistosCommand

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{UIO, ULayer, ZIO, ZLayer}

/** What a fix's command did. `output` is what it printed when the runner keeps its output, and empty when the command
  * printed on this process's own streams.
  */
enum Ran:
  case Exited(status: Int, output: List[String])
  case Unstarted

/** Runs a doctor fix's histos command in a checkout, as `./histos` and the command's words, with no shell. */
trait CommandRunner:
  def run(repo: Path, command: HistosCommand): UIO[Ran]

object CommandRunner:
  /** The command reads and prints on this process's standard streams, for a person at a terminal. */
  val attached: ULayer[CommandRunner] = ZLayer.succeed(Attached)

  /** The command reads nothing and its output and errors come back as lines. An MCP process uses this, because its
    * standard streams are the protocol.
    */
  val captured: ULayer[CommandRunner] = ZLayer.succeed(Captured)

  private def builder(repo: Path, command: HistosCommand): ProcessBuilder =
    new ProcessBuilder(("./histos" :: command.words)*).directory(repo.toFile)

  private object Attached extends CommandRunner:
    def run(repo: Path, command: HistosCommand): UIO[Ran] =
      started(builder(repo, command).inheritIO()): process =>
        ZIO.attemptBlockingCancelable(Ran.Exited(process.waitFor(), Nil))(ended(process))

  /** No input, and errors merged into the output this process reads, so nothing reaches this process's streams. */
  private object Captured extends CommandRunner:
    def run(repo: Path, command: HistosCommand): UIO[Ran] =
      val quiet = builder(repo, command)
        .redirectInput(ProcessBuilder.Redirect.from(Path.of("/dev/null").toFile))
        .redirectErrorStream(true)
        .redirectOutput(ProcessBuilder.Redirect.PIPE)
      started(quiet): process =>
        ZIO.attemptBlockingCancelable {
          val text = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
          Ran.Exited(process.waitFor(), text.linesIterator.toList)
        }(ended(process))

  /** A command that cannot start is not run. A started child is ended however its run ends, including an interruption
    * that lands before the wait begins, so no child outlives its fix.
    */
  private def started(built: ProcessBuilder)(waited: Process => zio.Task[Ran]): UIO[Ran] =
    ZIO.acquireReleaseWith(ZIO.attemptBlocking(built.start()).option)(ZIO.foreachDiscard(_)(ended)):
      case None          => ZIO.succeed(Ran.Unstarted)
      case Some(process) => waited(process).orDie

  private def ended(process: Process): UIO[Unit] =
    ZIO.succeed(process.destroyForcibly()).unit
end CommandRunner
