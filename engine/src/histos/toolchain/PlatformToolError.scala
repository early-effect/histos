package histos.toolchain

import histos.js.{JsLinkError, JsRunError, JsSuiteError}
import histos.native.{NativeError, NativeSuiteError, NativeToolchainError}

enum PlatformToolError:
  case JsLink(error: JsLinkError)
  case JsRun(error: JsRunError)
  case JsSuite(error: JsSuiteError)
  case Native(error: NativeError)
  case NativeSuite(error: NativeSuiteError)
  case NativeOrigin(error: NativeToolchainError)

  def message: String = this match
    case JsLink(error)       => error.message
    case JsRun(error)        => error.message
    case JsSuite(error)      => error.message
    case Native(error)       => error.message
    case NativeSuite(error)  => error.message
    case NativeOrigin(error) => error.message
end PlatformToolError
