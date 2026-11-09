package histos.js

import histos.zinc.CompileError
import histos.resolve.ResolveError

enum JsLinkError:
  case Resolved(error: ResolveError)
  case Unavailable
  case Library(error: CompileError)
  case Product(error: CompileError)
  case Linker(text: String)

  def message: String = this match
    case Resolved(error) => error.message
    case Unavailable     => "the Scala.js linker could not be fetched"
    case Library(error)  => error.message
    case Product(error)  => error.message
    case Linker(text)    => text
end JsLinkError
