package histos.js

import histos.zinc.{CompileError}
import histos.resolve.{ResolveError}
import java.nio.file.Path

enum JsRunError:
  case Resolved(error: ResolveError)
  case Unavailable
  case Compile(error: CompileError)
  case Write(path: Path)

  def message: String = this match
    case Resolved(error) => error.message
    case Unavailable     => "the JavaScript environment could not be fetched"
    case Compile(error)  => error.message
    case Write(path)     => s"could not write $path"
end JsRunError
