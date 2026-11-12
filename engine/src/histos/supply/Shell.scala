package histos.supply

import java.nio.file.Path

import zio.{UIO, ULayer, ZIO, ZLayer}

/** Runs a doctor fix's command line in a checkout. */
trait Shell:
  def run(repo: Path, line: String): UIO[Unit]

object Shell:
  /** `sh -c` in the checkout, on this process's standard streams. A line that cannot start is not run. */
  val live: ULayer[Shell] = ZLayer.succeed(Live)

  private object Live extends Shell:
    def run(repo: Path, line: String): UIO[Unit] =
      ZIO
        .attemptBlockingInterrupt:
          val process = new ProcessBuilder("sh", "-c", line).directory(repo.toFile).inheritIO().start()
          process.waitFor()
          ()
        .ignore
end Shell
