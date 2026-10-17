package histos

/** A clang major, such as `21`. The patch is whatever `clang` on `PATH` reports. */
opaque type ClangMajor = Int

object ClangMajor:
  given CanEqual[ClangMajor, ClangMajor] = CanEqual.derived

  /** A literal (`ClangMajor(21)`), rejected at compile time when it is not positive. */
  inline def apply(inline raw: Int): ClangMajor = ${ ClangMajorMacro.literal('raw) }

  def from(raw: Int): Either[ClangError, ClangMajor] =
    if raw > 0 then Right(trusted(raw)) else Left(ClangError.NotPositive(raw))

  private[histos] def trusted(raw: Int): ClangMajor = raw

  extension (major: ClangMajor) def value: Int = major

enum ClangError:
  case NotPositive(raw: Int)

  def message: String = this match
    case NotPositive(raw) => s"$raw is not a clang major"

/** Present only on a module that targets Native. `Unused` is not a missing major. */
enum ClangPin:
  case Unused
  case Major(major: ClangMajor)
