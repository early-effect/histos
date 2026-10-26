package histos

import _root_.scala.annotation.publicInBinary

/** A directory under the repository. The project id stays a name. Sources are `<path>/src`. */
opaque type ProjectPath = String

object ProjectPath:
  given CanEqual[ProjectPath, ProjectPath] = CanEqual.derived

  inline def apply(inline raw: String): ProjectPath = ${ ProjectPathMacro.literal('raw) }

  def from(raw: String): Either[ProjectPathError, ProjectPath] =
    if raw.isEmpty then Left(ProjectPathError.Empty)
    else if raw.startsWith("/") || raw.contains('\\') || raw.contains(':') then Left(ProjectPathError.Escapes(raw))
    else
      val parts = raw.split("/", -1).toList
      if parts.forall(segment) then Right(trusted(raw))
      else Left(ProjectPathError.Escapes(raw))

  extension (path: ProjectPath) def value: String = path

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): ProjectPath = raw

  private def segment(part: String): Boolean =
    part.headOption match
      case Some(head) if head.isLetter => part.forall(c => c.isLetterOrDigit || c == '_' || c == '-')
      case _                           => false
end ProjectPath

enum ProjectPathError:
  case Empty
  case Escapes(raw: String)

  def message: String = this match
    case Empty        => "a project directory is not empty"
    case Escapes(raw) => s"'$raw' is not a directory under the repository"
end ProjectPathError
