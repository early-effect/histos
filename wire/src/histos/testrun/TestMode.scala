package histos.testrun

/** What `histos test` does. The command token is `run` or `quick`. */
enum TestMode:
  case Run, Quick

  def token: String = this match
    case Run   => "run"
    case Quick => "quick"
end TestMode

object TestMode:
  private val known: List[TestMode] = List(Run, Quick)

  def from(raw: String): Either[TestModeError, TestMode] =
    known.find(_.token == raw).toRight(TestModeError.Unknown(raw))
end TestMode

enum TestModeError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a test mode"
end TestModeError
