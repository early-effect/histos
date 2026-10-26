package histos

/** The Scala.js linker module kind. Tests and `run` use the kind the module names. */
enum JsModuleKind:
  case NoModule
  case ESModule
  case CommonJS

  def token: String = this match
    case NoModule => "nomodule"
    case ESModule => "esmodule"
    case CommonJS => "commonjs"
end JsModuleKind
