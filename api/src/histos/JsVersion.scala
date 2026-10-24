package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** An exact Scala.js toolchain revision. */
opaque type JsVersion = String

object JsVersion:
  given CanEqual[JsVersion, JsVersion] = CanEqual.derived
  inline def apply(inline raw: String): JsVersion = ${
    JsVersionMacro.literal('raw)
  }
  def from(raw: String): Either[JsVersionError, JsVersion] =
    if raw.isEmpty then Left(JsVersionError.Empty)
    else if """\d+\.\d+\.\d+([-+][0-9A-Za-z.-]+)?""".r.matches(raw) then
      Right(trusted(raw))
    else Left(JsVersionError.NotExact(raw))
  extension (value: JsVersion) def value: String = value
  @publicInBinary private[histos] def trusted(raw: String): JsVersion = raw

enum JsVersionError:
  case Empty
  case NotExact(raw: String)
  def message: String = this match
    case Empty         => "a Scala.js toolchain revision is not empty"
    case NotExact(raw) => s"'$raw' is not an exact Scala.js toolchain revision"

private object JsVersionMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[JsVersion] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "JsVersion(...) takes a literal. Use JsVersion.from at runtime.",
          raw
        )
      case Some(text) =>
        JsVersion.from(text) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ JsVersion.trusted(${ Expr(text) }) }
