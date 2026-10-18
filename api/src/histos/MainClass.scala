package histos

import _root_.scala.annotation.publicInBinary

/** The binary name `histos run` starts. Not a test main and not discovered by scanning. */
opaque type MainClass = String

object MainClass:
  given CanEqual[MainClass, MainClass] = CanEqual.derived

  private val Name = """[A-Za-z_][A-Za-z0-9_$]*(\.[A-Za-z_][A-Za-z0-9_$]*)*""".r

  inline def apply(inline raw: String): MainClass = ${ MainClassMacro.literal('raw) }

  def from(raw: String): Either[MainClassError, MainClass] =
    if raw.isEmpty then Left(MainClassError.Empty)
    else if Name.matches(raw) then Right(trusted(raw))
    else Left(MainClassError.Bad(raw))

  extension (main: MainClass) def value: String = main

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): MainClass = raw

enum MainClassError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a main class is not empty"
    case Bad(raw) => s"'$raw' is not a main class"
