package histos.testrun

/** A glob on a suite's binary name. `*` is any characters. `?` is one character. */
opaque type SuitePattern = String

object SuitePattern:
  given CanEqual[SuitePattern, SuitePattern] = CanEqual.derived

  def from(raw: String): Either[SuitePatternError, SuitePattern] =
    if raw.isEmpty then Left(SuitePatternError.Empty)
    else if raw.exists(_.isWhitespace) then Left(SuitePatternError.Blank(raw))
    else Right(raw)

  extension (pattern: SuitePattern) def value: String = pattern
end SuitePattern

enum SuitePatternError:
  case Empty
  case Blank(raw: String)

  def message: String = this match
    case Empty      => "a test pattern is not empty"
    case Blank(raw) => s"'$raw' is not a test pattern"
end SuitePatternError
