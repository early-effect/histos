package histos.native

import histos.zinc.CompileError
import histos.{ClangMajor, ProjectName}

enum LinkError:
  case Interrupted
  case Linker(text: String)
  case Unexpected(kind: String)

  def message: String = this match
    case Interrupted      => "the native link was interrupted"
    case Linker(text)     => text
    case Unexpected(kind) => s"the native link failed ($kind)"
end LinkError

enum NativeError:
  case Clang(miss: ClangMiss)
  case Fetch(error: NativeFetch)
  case Compile(error: CompileError)
  case Link(error: LinkError)

  def message: String = this match
    case Clang(miss)    => miss.message
    case Fetch(error)   => error.message
    case Compile(error) => error.message
    case Link(error)    => error.message
end NativeError

enum NativeDeclaration:
  case NotNative
  case Major(major: ClangMajor)
  case Missing(name: ProjectName)
  case Disagree(left: ProjectName, right: ProjectName)

  def message: String = this match
    case NotNative     => ""
    case Major(major)  => s"clang ${major.value}"
    case Missing(name) =>
      s"project '${name.value}' targets Native and does not name a clang major"
    case Disagree(left, right) =>
      s"projects '${left.value}' and '${right.value}' name different clang majors"
end NativeDeclaration
