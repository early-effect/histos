package histos.process

import java.nio.charset.StandardCharsets

import zio.{IO, UIO, ZIO}

/** What a finished child wrote to stdout, and its status. */
final case class Ended(status: Int, output: String)

/** A short-lived child whose output is read to the end. */
object Child:
  /** The release kills the tree, and so does an interrupt while the child runs: a blocked pipe read ignores thread
    * interruption, so killing the writers is what ends it.
    */
  def run[E](start: IO[E, Process], failed: E): IO[E, Ended] =
    ZIO.scoped:
      ZIO
        .acquireRelease(start)(stop)
        .flatMap: process =>
          ZIO
            .attemptBlockingCancelable {
              val output = new String(
                process.getInputStream.readAllBytes,
                StandardCharsets.UTF_8,
              )
              Ended(process.waitFor(), output)
            }(stop(process))
            .orElseFail(failed)

  private def stop(process: Process): UIO[Unit] =
    ProcessTree.of(process).flatMap(_.destroy)
end Child
