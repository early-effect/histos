package histos.supply

import histos.doctor.HistosCommand

import java.nio.file.Path

import zio.{UIO, ULayer, ZIO, ZLayer}

/** Runs a doctor fix's histos command in a checkout. */
trait CommandRunner:
  def run(repo: Path, command: HistosCommand): UIO[Unit]

object CommandRunner:
  /** `./histos` and the command's words in the checkout, with no shell, on this process's standard streams. A command
    * that cannot start is not run.
    */
  val attached: ULayer[CommandRunner] = ZLayer.succeed(Attached)

  private object Attached extends CommandRunner:
    def run(repo: Path, command: HistosCommand): UIO[Unit] =
      ZIO
        .attemptBlockingInterrupt:
          val process = new ProcessBuilder(("./histos" :: command.words)*).directory(repo.toFile).inheritIO().start()
          process.waitFor()
          ()
        .ignore
end CommandRunner
