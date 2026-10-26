package histos

import _root_.scala.annotation.publicInBinary

/** A full Scala version, such as `3.9.0` or `2.13.16`. A series such as `3` or `2.13` is not a pin. */
opaque type ScalaVersion = String

object ScalaVersion:
  given CanEqual[ScalaVersion, ScalaVersion] = CanEqual.derived

  private val Full = """\d+\.\d+\.\d+([-+][0-9A-Za-z.-]+)?""".r

  /** A literal (`ScalaVersion("3.9.0")`), rejected at compile time when it is not full. */
  inline def apply(inline raw: String): ScalaVersion = ${ ScalaVersionMacro.literal('raw) }

  def from(raw: String): Either[ScalaVersionError, ScalaVersion] =
    if raw.isEmpty then Left(ScalaVersionError.Empty)
    else if Full.matches(raw) then Right(trusted(raw))
    else Left(ScalaVersionError.NotFull(raw))

  extension (version: ScalaVersion) def value: String = version

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): ScalaVersion = raw
end ScalaVersion

enum ScalaVersionError:
  case Empty
  case NotFull(raw: String)

  def message: String = this match
    case Empty        => "a Scala version is not empty"
    case NotFull(raw) => s"'$raw' is not a full Scala version (major.minor.patch)"
end ScalaVersionError
