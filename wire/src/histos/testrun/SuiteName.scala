package histos.testrun

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** A suite's binary name. Empty is not a name. This is the key of one quick record. */
opaque type SuiteName = String

object SuiteName:
  given CanEqual[SuiteName, SuiteName] = CanEqual.derived

  inline def apply(inline raw: String): SuiteName = ${
    SuiteNameMacro.literal('raw)
  }

  def from(raw: String): Either[SuiteNameError, SuiteName] =
    if raw.isEmpty then Left(SuiteNameError.Empty)
    else Right(trusted(raw))

  extension (name: SuiteName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): SuiteName = raw
end SuiteName

enum SuiteNameError:
  case Empty

  def message: String = this match
    case Empty => "a suite name is not empty"
end SuiteNameError

private object SuiteNameMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[SuiteName] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "SuiteName(...) takes a literal. Use SuiteName.from at runtime.",
          raw,
        )
      case Some(value) =>
        SuiteName.from(value) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ SuiteName.trusted(${ Expr(value) }) }
