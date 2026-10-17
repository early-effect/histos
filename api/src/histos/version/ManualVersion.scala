package histos.version

import _root_.scala.annotation.publicInBinary

/** A version the build states. Histos does not derive it from git, and it does not rewrite it. */
opaque type ManualVersion = String

object ManualVersion:
  given CanEqual[ManualVersion, ManualVersion] = CanEqual.derived

  private val Shape = """[0-9A-Za-z][0-9A-Za-z._+\-]*""".r

  /** A literal (`ManualVersion("1.2.3")`), rejected at compile time when it is not a version. */
  inline def apply(inline raw: String): ManualVersion = ${ ManualVersionMacro.literal('raw) }

  def from(raw: String): Either[ManualVersionError, ManualVersion] =
    if raw.isEmpty then Left(ManualVersionError.Empty)
    else if raw.contains("..") || !Shape.matches(raw) then Left(ManualVersionError.NotAVersion(raw))
    else Right(trusted(raw))

  extension (version: ManualVersion) def value: String = version

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): ManualVersion = raw

enum ManualVersionError:
  case Empty
  case NotAVersion(raw: String)

  def message: String = this match
    case Empty            => "a manual version is not empty"
    case NotAVersion(raw) =>
      s"'$raw' is not a manual version. Use letters, digits, and . _ + - without '..'."
