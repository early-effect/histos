package histos.doctor

import histos.central.{Fingerprint, KeyServer, PreflightError}
import histos.publish.Signer

import zio.{URIO, ZIO}

/** The signing fingerprint, then the keyserver read [[histos.central.Preflight]] performs before an upload. */
object PgpProbe:
  enum Hit:
    case Unset
    case Bad
    case Present(fingerprint: String)
    case Missing(fingerprint: String)
    case Unreachable(fingerprint: String)

  def read(env: Map[String, String]): URIO[KeyServer, Hit] =
    named(env) match
      case Some(raw) =>
        Fingerprint.from(raw) match
          case Left(_)   => ZIO.succeed(Hit.Bad)
          case Right(fp) => lookup(fp)
      case None =>
        Signer
          .fromEnv(env)
          .flatMap(_.fingerprint)
          .either
          .flatMap:
            case Right(fp) => lookup(fp)
            case Left(_)   => ZIO.succeed(Hit.Unset)

  private def lookup(fingerprint: Fingerprint): URIO[KeyServer, Hit] =
    ZIO
      .serviceWithZIO[KeyServer](_.key(fingerprint))
      .either
      .map:
        case Right(_)                           => Hit.Present(fingerprint.value)
        case Left(PreflightError.KeyMissing(_)) => Hit.Missing(fingerprint.value)
        case Left(_)                            => Hit.Unreachable(fingerprint.value)

  private def named(env: Map[String, String]): Option[String] =
    env.get("PGP_KEY_HEX").map(_.trim).filter(_.nonEmpty)
end PgpProbe
