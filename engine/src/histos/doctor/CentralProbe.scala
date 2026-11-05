package histos.doctor

import histos.supply.{Net, Reply}

import java.nio.charset.StandardCharsets
import java.util.Base64

import zio.{UIO, ZIO}

/** One authenticated read of the Central publisher. Missing variables do not open a socket. */
object CentralProbe:
  enum Auth:
    case NotRun
    case Rejected(status: Int)
    case Accepted
    case Unreachable

  def read(net: Net, env: Map[String, String]): UIO[Auth] =
    val user     = env.get("SONATYPE_USERNAME").map(_.trim).filter(_.nonEmpty)
    val password = env.get("SONATYPE_PASSWORD").filter(_.nonEmpty)
    (user, password, root(env)) match
      case (Some(name), Some(secret), Some(base)) =>
        val token = Base64.getEncoder.encodeToString(s"$name:$secret".getBytes(StandardCharsets.UTF_8))
        net
          .send(
            "POST",
            s"$base/api/v1/publisher/status?id=histos-credentials-probe",
            "",
            List(
              "Authorization" -> s"Bearer $token",
              "User-Agent"    -> "histos",
              "Accept"        -> "application/json",
            ),
          )
          .map(classify)
          .catchAll(_ => ZIO.succeed(Auth.Unreachable))
      case _ => ZIO.succeed(Auth.NotRun)

  private def classify(reply: Reply): Auth =
    reply.status match
      case 401 | 403                               => Auth.Rejected(reply.status)
      case code if code >= 200 && code < 500      => Auth.Accepted
      case _                                       => Auth.Unreachable

  private def root(env: Map[String, String]): Option[String] =
    env.get("HISTOS_PORTAL").map(_.trim).filter(_.nonEmpty) match
      case None => Some("https://central.sonatype.com")
      case Some(raw) =>
        val text = raw.stripSuffix("/")
        if text.startsWith("https://") || text.startsWith("http://") then Some(text) else None
end CentralProbe
