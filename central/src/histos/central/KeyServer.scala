package histos.central

import zio.{IO, ULayer, ZLayer}

/** The keyserver Central reads a signing key from before it publishes. */
trait KeyServer:
  /** Succeeds when the keyserver has the key. A miss is [[PreflightError.KeyMissing]]. */
  def key(fingerprint: Fingerprint): IO[PreflightError, Unit]

object KeyServer:
  /** The lookup [[Preflight]] makes before an upload, on Central's keyserver. */
  val central: ULayer[KeyServer] = ZLayer.succeed(Central)

  private object Central extends KeyServer:
    def key(fingerprint: Fingerprint): IO[PreflightError, Unit] =
      Preflight.key(fingerprint, Preflight.Hosts.central)
end KeyServer
