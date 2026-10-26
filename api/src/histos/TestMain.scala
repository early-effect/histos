package histos

import _root_.scala.annotation.publicInBinary

/** The binary name of the one main `histos test` runs for a module. Not a test framework. */
opaque type TestMain = String

object TestMain:
  given CanEqual[TestMain, TestMain] = CanEqual.derived

  private val Name = """[A-Za-z_][A-Za-z0-9_$]*(\.[A-Za-z_][A-Za-z0-9_$]*)*""".r

  inline def apply(inline raw: String): TestMain = ${ TestMainMacro.literal('raw) }

  def from(raw: String): Either[TestMainError, TestMain] =
    if raw.isEmpty then Left(TestMainError.Empty)
    else if Name.matches(raw) then Right(trusted(raw))
    else Left(TestMainError.Bad(raw))

  extension (main: TestMain) def value: String = main

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): TestMain = raw
end TestMain

enum TestMainError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a test main is not empty"
    case Bad(raw) => s"'$raw' is not a test main"
end TestMainError
