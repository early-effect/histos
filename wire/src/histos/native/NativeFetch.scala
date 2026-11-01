package histos.native

import histos.resolve.ResolveError
import histos.zinc.CompileError

enum NativeFetch:
  case Compiler(error: histos.zinc.CompileError)
  case Resolved(error: ResolveError)
  case Missing(artifact: String, revision: String)
  case Duplicated(artifact: String)

  def message: String = this match
    case Compiler(error)             => error.message
    case Resolved(error)             => error.message
    case Missing(artifact, revision) =>
      s"the Scala Native toolchain has no $artifact $revision"
    case Duplicated(artifact) =>
      s"the Scala Native toolchain has more than one $artifact"
end NativeFetch
