package histos

import java.nio.file.Path

import zio.{IO, ZIO}

/** The new client against the new jar. `Handshake` is the version line the client printed. */
enum PairError:
  case Exec(detail: String)
  case Handshake(wanted: String, got: String)
  case Load(detail: String)

  def message: String = this match
    case Exec(detail)           => detail
    case Handshake(wanted, got) => s"toolchain handshake wanted $wanted and the client reported $got"
    case Load(detail)           => detail
end PairError

/** One pair. `wanted` is the version line this publish packed. `jar` and `client` are the files just built. `checkout`
  * is a copy of HEAD.
  */
final case class PairAttempt(wanted: String, jar: Path, client: Path, checkout: Path)

/** Proves one pair. A build replaces it the way it replaces `sona`. */
trait PairProof:
  def prove(attempt: PairAttempt): IO[PairError, Unit]

object PairProof:
  /** A version line that is not `wanted` is [[PairError.Handshake]] and `graph` does not run. */
  def judge(wanted: String, reported: String, graph: IO[PairError, Unit]): IO[PairError, Unit] =
    if reported == wanted then graph
    else ZIO.fail(PairError.Handshake(wanted, reported))
