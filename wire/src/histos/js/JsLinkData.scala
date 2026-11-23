package histos.js

import histos.zinc.CompileError
import histos.resolve.ResolveError
import histos.process.ChildError
import histos.workspace.IoFailure

enum JsLinkError:
  case Resolved(error: ResolveError)
  case Unavailable
  case Library(error: CompileError)
  case Product(error: CompileError)
  case Linker(text: String)
  case Child(error: ChildError)
  case Io(error: IoFailure)
  case Minifier(text: String)
  case Bridge
  case InputsChanged

  def message: String = this match
    case Resolved(error) => error.message
    case Unavailable     => "the Scala.js linker could not be fetched"
    case Library(error)  => error.message
    case Product(error)  => error.message
    case Linker(text)    => text
    case Child(error)    => error.message
    case Io(error)       => error.message
    case Minifier(text)  => text
    case Bridge          => "invalid Scala.js link child request or result"
    case InputsChanged   => "Scala.js link inputs changed during linking"
end JsLinkError
