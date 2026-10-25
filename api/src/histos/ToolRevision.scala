package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** An exact build tool revision. */
opaque type ToolRevision = String

object ToolRevision:
  given CanEqual[ToolRevision, ToolRevision]         = CanEqual.derived
  inline def apply(inline raw: String): ToolRevision = ${
    ToolRevisionMacro.literal('raw)
  }
  def from(raw: String): Either[ToolRevisionError, ToolRevision] =
    if raw.isEmpty then Left(ToolRevisionError.Empty)
    else if """\d+\.\d+\.\d+([-+][0-9A-Za-z.-]+)?""".r.matches(raw) then Right(trusted(raw))
    else Left(ToolRevisionError.NotExact(raw))
  extension (value: ToolRevision) def value: String                      = value
  @publicInBinary private[histos] def trusted(raw: String): ToolRevision = raw

enum ToolRevisionError:
  case Empty
  case NotExact(raw: String)
  def message: String = this match
    case Empty         => "a build tool revision is not empty"
    case NotExact(raw) => s"'$raw' is not an exact build tool revision"

private object ToolRevisionMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ToolRevision] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "ToolRevision(...) takes a literal. Use ToolRevision.from at runtime.",
          raw,
        )
      case Some(text) =>
        ToolRevision.from(text) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ ToolRevision.trusted(${ Expr(text) }) }
