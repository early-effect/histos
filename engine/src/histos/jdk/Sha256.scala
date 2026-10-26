package histos.jdk

import java.security.MessageDigest

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

import zio.Chunk

opaque type Sha256 = String

object Sha256:
  given CanEqual[Sha256, Sha256] = CanEqual.derived

  inline def apply(inline raw: String): Sha256 = ${
    Sha256Macro.literal('raw)
  }

  def from(raw: String): Either[Sha256Error, Sha256] =
    val hex = raw.trim.toLowerCase(java.util.Locale.ROOT)
    if hex.length == 64 && hex.forall(isHex) then Right(trusted(hex))
    else Left(Sha256Error.Bad(raw))

  def of(bytes: Chunk[Byte]): Sha256 = fromDigest(
    MessageDigest.getInstance("SHA-256").digest(bytes.toArray)
  )

  private[histos] def fromDigest(bytes: Array[Byte]): Sha256 =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  extension (sha: Sha256) def value: String = sha

  @publicInBinary private[histos] def trusted(raw: String): Sha256 = raw

  private def isHex(c: Char): Boolean =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
end Sha256

enum Sha256Error:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"sha256 is not 64 hex digits: $raw"
end Sha256Error

private object Sha256Macro:
  def literal(raw: Expr[String])(using Quotes): Expr[Sha256] =
    constant(raw) match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Sha256(...) takes a literal. Use Sha256.from at runtime.",
          raw,
        )
      case Some(text) =>
        Sha256.from(text) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(digest) =>
            '{ Sha256.trusted(${ Expr(digest.value) }) }

  /** `"ab" * 32` is an `augmentString` call, not a string literal. */
  private def constant(raw: Expr[String])(using Quotes): Option[String] =
    import quotes.reflect.*
    def repeated(term: Term): Option[String] = term match
      case Inlined(_, _, expansion) => repeated(expansion)
      case Apply(
            Select(Apply(Ident("augmentString"), List(Literal(StringConstant(part)))), "*"),
            List(Literal(IntConstant(count))),
          ) =>
        Some(part * count)
      case _ => None
    raw.value.orElse(repeated(raw.asTerm))
end Sha256Macro
