package histos

import _root_.scala.annotation.publicInBinary

/** A Scala Native toolchain revision, such as `0.5.12` or `0.5.13-SNAPSHOT`. Not an ABI suffix. */
opaque type NativeVersion = String

object NativeVersion:
  given CanEqual[NativeVersion, NativeVersion] = CanEqual.derived

  private val Full = """\d+\.\d+\.\d+(-[0-9A-Za-z.]+)?""".r

  inline def apply(inline raw: String): NativeVersion = ${ NativeVersionMacro.literal('raw) }

  def from(raw: String): Either[NativeVersionError, NativeVersion] =
    if raw.isEmpty then Left(NativeVersionError.Empty)
    else if Full.matches(raw) then Right(trusted(raw))
    else Left(NativeVersionError.Bad(raw))

  extension (version: NativeVersion) def value: String = version

  @publicInBinary private[histos] def trusted(raw: String): NativeVersion = raw
end NativeVersion

enum NativeVersionError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a Scala Native version is not empty"
    case Bad(raw) => s"'$raw' is not a Scala Native version (major.minor.patch)"
end NativeVersionError
