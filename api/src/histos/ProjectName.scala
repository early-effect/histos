package histos

/** A project id. A letter, then letters, digits, `_`, or `-`. */
opaque type ProjectName = String

object ProjectName:
  given CanEqual[ProjectName, ProjectName] = CanEqual.derived

  def from(raw: String): Either[ProjectNameError, ProjectName] =
    raw.headOption match
      case None                           => Left(ProjectNameError.Empty)
      case Some(head) if valid(head, raw) => Right(raw)
      case Some(_)                        => Left(ProjectNameError.Bad(raw))

  extension (name: ProjectName) def value: String = name

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
