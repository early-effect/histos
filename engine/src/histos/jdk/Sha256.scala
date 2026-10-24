package histos.jdk

import java.security.MessageDigest

import zio.Chunk

opaque type Sha256 = String

object Sha256:
  given CanEqual[Sha256, Sha256] = CanEqual.derived

  def from(raw: String): Either[Sha256Error, Sha256] =
    val hex = raw.trim.toLowerCase(java.util.Locale.ROOT)
    if hex.length == 64 && hex.forall(isHex) then Right(hex)
    else Left(Sha256Error.Bad(raw))

  def of(bytes: Chunk[Byte]): Sha256 = fromDigest(
    MessageDigest.getInstance("SHA-256").digest(bytes.toArray)
  )

  private[histos] def fromDigest(bytes: Array[Byte]): Sha256 =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  extension (sha: Sha256) def value: String = sha

  private def isHex(c: Char): Boolean =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

enum Sha256Error:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"sha256 is not 64 hex digits: $raw"
