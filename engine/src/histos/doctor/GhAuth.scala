package histos.doctor

import java.nio.charset.StandardCharsets

import zio.{UIO, ULayer, ZIO, ZLayer}

/** The token the `gh` command line holds, for `hostname` or its own default host. */
trait GhAuth:
  def token(hostname: Option[String]): UIO[Option[String]]

object GhAuth:
  /** Runs `gh auth token` when the binary exists. A missing binary or a non-zero status is no token, not a crash. */
  val live: ULayer[GhAuth] = ZLayer.succeed(Live)

  private object Live extends GhAuth:
    def token(hostname: Option[String]): UIO[Option[String]] =
      ZIO.attemptBlocking(read(hostname)).orElseSucceed(None)

    private def read(hostname: Option[String]): Option[String] =
      val command = hostname match
        case Some(host) => List("gh", "auth", "token", "--hostname", host)
        case None       => List("gh", "auth", "token")
      val process = new ProcessBuilder(command*).redirectError(ProcessBuilder.Redirect.DISCARD).start()
      val text    = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).trim
      val code    = process.waitFor()
      if code == 0 && text.nonEmpty then Some(text) else None
  end Live
end GhAuth
