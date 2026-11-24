package histos

/** How a Scala.js link is finished. `Fast` is the linker with the optimizer off. `Full` turns the optimizer on and then
  * applies Splice production minification. The module kind is an independent declaration.
  */
enum JsOptimize:
  case Fast
  case Full

  def token: String = this match
    case Fast => "fast"
    case Full => "full"
end JsOptimize

object JsOptimize:
  def effective(tool: JsOptimize, module: Option[JsOptimize], task: Option[JsOptimize]): JsOptimize =
    task.orElse(module).getOrElse(tool)
