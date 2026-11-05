package histos.doctor

import java.util.concurrent.TimeUnit

import zio.{UIO, ZIO}

/** What `gh auth` can see. A missing or refused `gh` is an absent probe, not a crash. */
object GitHubProbe:
  def authenticated: UIO[Boolean] =
    ZIO
      .attemptBlockingInterrupt:
        val process = new ProcessBuilder("gh", "api", "user", "--jq", ".login")
          .redirectError(ProcessBuilder.Redirect.DISCARD)
          .redirectOutput(ProcessBuilder.Redirect.DISCARD)
          .start()
        val finished = process.waitFor(2, TimeUnit.SECONDS)
        if !finished then
          process.destroyForcibly()
          false
        else process.exitValue() == 0
      .orElseSucceed(false)
