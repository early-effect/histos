package histos.native

import histos.zinc.CompileError
import histos.resolve.ResolveError
import java.nio.file.Path

enum NativeSuiteError:
  case Resolved(error: ResolveError)
  case Unavailable
  case Shapes(error: ClassShapeError)
  case Compile(error: CompileError)
  case Write(path: Path)

  def message: String = this match
    case Resolved(error) => error.message
    case Unavailable     => "the Scala Native test runner could not be fetched"
    case Shapes(error)   => error.message
    case Compile(error)  => error.message
    case Write(path)     => s"could not write $path"
end NativeSuiteError
