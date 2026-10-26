package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** A project id. A letter, then letters, digits, `_`, or `-`. */
opaque type ProjectName = String

object ProjectName:
  given CanEqual[ProjectName, ProjectName] = CanEqual.derived

  inline def apply(inline raw: String): ProjectName = ${
    ProjectNameMacro.literal('raw)
  }

  def from(raw: String): Either[ProjectNameError, ProjectName] =
    raw.headOption match
      case None                           => Left(ProjectNameError.Empty)
      case Some(head) if valid(head, raw) => Right(trusted(raw))
      case Some(_)                        => Left(ProjectNameError.Bad(raw))

  extension (name: ProjectName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): ProjectName = raw

  private def valid(head: Char, raw: String): Boolean =
    head.isLetter && raw.forall(c => c.isLetterOrDigit || c == '_' || c == '-')
end ProjectName

enum ProjectNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a project name is not empty"
    case Bad(raw) => s"'$raw' is not a project name"
end ProjectNameError

private object ProjectNameMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ProjectName] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "ProjectName(...) takes a literal. Use ProjectName.from at runtime.",
          raw,
        )
      case Some(value) =>
        ProjectName.from(value) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ ProjectName.trusted(${ Expr(value) }) }
