package histos.js

import histos.zinc.CompileError
import histos.resolve.ResolveError

enum JsSuiteError:
  case Resolved(error: ResolveError)
  case Unavailable
  case Library(error: CompileError)
  case Run(text: String)

  def message: String = this match
    case Resolved(error) => error.message
    case Unavailable     => "the Scala.js test adapter could not be fetched"
    case Library(error)  => error.message
    case Run(text)       => text
end JsSuiteError
