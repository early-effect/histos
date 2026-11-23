package histos.js.child

import java.nio.file.Path

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

import org.scalajs.jsenv.{Input, JSEnv, RunConfig}

/** Starts one linked script. `JS_ENV` is `node`, `jsdom`, or `firefox`.
  */
object RunMain:
  def main(args: Array[String]): Unit =
    val code =
      try body()
      catch
        case NonFatal(thrown) =>
          thrown.printStackTrace(System.err)
          1
    if code != 0 then System.exit(code)

  private def body(): Int =
    val script = Path.of(sys.env("JS_SCRIPT"))
    val kind   = sys.env.getOrElse("JS_MODULE", "nomodule")
    val input  = kind match
      case "esmodule" => Input.ESModule(script)
      case "commonjs" => Input.CommonJSModule(script)
      case _          => Input.Script(script)
    val started = jsEnv.start(Seq(input), RunConfig().withInheritOut(true).withInheritErr(true))
    try
      Await.result(started.future, Duration.Inf)
      0
    finally started.close()

  private def jsEnv: JSEnv =
    sys.env.getOrElse("JS_ENV", "node") match
      case "firefox" =>
        new chekhov.jsenv.ChekhovJSEnv(chekhov.ChekhovBrowser.Firefox, true, false, Map.empty)
      case "jsdom" =>
        throw new IllegalArgumentException("jsdom is not a supported JavaScript environment")
      case _ =>
        val config = org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withExecutable(sys.env("JS_NODE")).withSourceMap(false)
        new org.scalajs.jsenv.nodejs.NodeJSEnv(config)
end RunMain
