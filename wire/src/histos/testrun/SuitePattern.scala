package histos.testrun

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** A glob on a suite's binary name. `*` is any characters. `?` is one character. */
opaque type SuitePattern = String

object SuitePattern:
  given CanEqual[SuitePattern, SuitePattern] = CanEqual.derived

  inline def apply(inline raw: String): SuitePattern = ${
    SuitePatternMacro.literal('raw)
  }

  def from(raw: String): Either[SuitePatternError, SuitePattern] =
    if raw.isEmpty then Left(SuitePatternError.Empty)
    else if raw.exists(_.isWhitespace) then Left(SuitePatternError.Blank(raw))
    else Right(trusted(raw))

  extension (pattern: SuitePattern) def value: String = pattern

  @publicInBinary private[histos] def trusted(raw: String): SuitePattern = raw
end SuitePattern

enum SuitePatternError:
  case Empty
  case Blank(raw: String)

  def message: String = this match
    case Empty      => "a test pattern is not empty"
    case Blank(raw) => s"'$raw' is not a test pattern"
end SuitePatternError

private object SuitePatternMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[SuitePattern] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "SuitePattern(...) takes a literal. Use SuitePattern.from at runtime.",
          raw,
        )
      case Some(value) =>
        SuitePattern.from(value) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ SuitePattern.trusted(${ Expr(value) }) }
