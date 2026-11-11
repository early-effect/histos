package histos

import _root_.scala.annotation.publicInBinary

/** A third-party Maven revision. One version. Not a ship revision and not a range. */
opaque type LibraryRevision = String

object LibraryRevision:
  given CanEqual[LibraryRevision, LibraryRevision] = CanEqual.derived

  def from(raw: String): Either[LibraryRevisionError, LibraryRevision] =
    if raw.isEmpty then Left(LibraryRevisionError.Empty)
    else if raw.exists(_.isWhitespace) || raw.contains(':') then Left(LibraryRevisionError.Bad(raw))
    else if isRange(raw) then Left(LibraryRevisionError.Ranged(raw))
    else Right(raw)

  /** A range is `latest…`, or a revision that contains `+`, a comma, or a bracket. */
  private[histos] def isRange(raw: String): Boolean =
    raw.startsWith("latest") || raw.exists(marked)

  extension (revision: LibraryRevision) def value: String = revision

  /** Spliced after `from` has accepted the text. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): LibraryRevision = raw

  private def marked(character: Char): Boolean =
    character == '[' || character == ']' || character == '(' || character == ')' || character == '+' || character == ','
end LibraryRevision

enum LibraryRevisionError:
  case Empty
  case Bad(raw: String)
  case Ranged(raw: String)

  def message: String = this match
    case Empty       => "a revision is not empty"
    case Bad(raw)    => s"'$raw' is not a revision"
    case Ranged(raw) => s"'$raw' is not a single revision"
end LibraryRevisionError
