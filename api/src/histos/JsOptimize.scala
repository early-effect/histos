package histos

/** How a Scala.js link is finished. [[Fast]] is the linker with the optimizer off. [[Full]] turns the optimizer on and
  * then minifies the result to one classic script.
  */
enum JsOptimize:
  case Fast
  case Full

  def token: String = this match
    case Fast => "fast"
    case Full => "full"
end JsOptimize
