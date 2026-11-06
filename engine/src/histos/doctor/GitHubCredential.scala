package histos.doctor

import java.nio.charset.StandardCharsets

import zio.{UIO, ZIO}

/** A token in the order `gh` uses. The `gh` binary is optional. The API call uses the token. */
object GitHubCredential:
  final case class Token(value: String, host: String):
    def api: String =
      if host == "github.com" then "https://api.github.com"
      else s"https://$host/api/v3"

  /** `hostname` is set only when `GH_HOST` was set, so `gh` keeps its own default otherwise. */
  def resolve(env: Map[String, String], gh: Option[String] => UIO[Option[String]]): UIO[Option[Token]] =
    val host = env.get("GH_HOST").map(_.trim).filter(_.nonEmpty).getOrElse("github.com")
    fromEnv(env, host) match
      case Some(token) => ZIO.succeed(Some(Token(token, host)))
      case None        =>
        val named = env.get("GH_HOST").map(_.trim).filter(_.nonEmpty)
        gh(named).map(_.map(_.trim).filter(_.nonEmpty).map(Token(_, host)))

  /** `github.com`, `*.ghe.com`, and `localhost` read `GH_TOKEN` then `GITHUB_TOKEN`. Any other host reads the
    * enterprise pair.
    */
  def fromEnv(env: Map[String, String], host: String): Option[String] =
    val names =
      if githubHost(host) then List("GH_TOKEN", "GITHUB_TOKEN")
      else List("GH_ENTERPRISE_TOKEN", "GITHUB_ENTERPRISE_TOKEN")
    names.iterator.map(name => env.get(name)).collectFirst { case Some(value) if value.nonEmpty => value }

  def githubHost(host: String): Boolean =
    host == "github.com" || host == "localhost" || host.endsWith(".ghe.com")

  /** Runs `gh auth token` when the binary exists. A missing binary or a non-zero status is no token, not a crash. */
  def gh: Option[String] => UIO[Option[String]] =
    hostname => ZIO.attemptBlocking(readGh(hostname)).orElseSucceed(None)

  private def readGh(hostname: Option[String]): Option[String] =
    val command = hostname match
      case Some(host) => List("gh", "auth", "token", "--hostname", host)
      case None       => List("gh", "auth", "token")
    val process = new ProcessBuilder(command*).redirectError(ProcessBuilder.Redirect.DISCARD).start()
    val text    = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).trim
    val code    = process.waitFor()
    if code == 0 && text.nonEmpty then Some(text) else None
end GitHubCredential
