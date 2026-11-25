package histos.js

import histos.node.NodeBinary
import histos.pack.MainClass
import histos.suite.HistosSpec
import histos.{JsEnv, JsModuleKind, JsVersion, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.test.*

object JsArgsSpec extends HistosSpec:
  def spec = suite("program arguments on Scala.js")(
    test("an empty argument list keeps the link key") {
      val compared =
        MainClass
          .from("demo.Main")
          .map: main =>
            val version = ScalaVersion("3.10.0")
            val linker  = JsVersion("1.20.1")
            val bare    = JsKey.of(version, linker, JsModuleKind.NoModule, main, Nil)
            val again   = JsKey.of(version, linker, JsModuleKind.NoModule, main, Nil, arguments = Nil)
            val changed = JsKey.of(version, linker, JsModuleKind.NoModule, main, Nil, arguments = List("hello"))
            bare == again && bare != changed
      assertTrue(compared == Right(true))
    },
    test("arguments encode as a count and one base64 line, and an empty argument stays a blank line") {
      val encoded = JsKey.encoded(List("hello", ""))
      val lines   = encoded.split("\n", -1).toList
      val decoded = lines match
        case count :: hello :: blank :: Nil =>
          val text = new String(java.util.Base64.getDecoder.decode(hello), StandardCharsets.UTF_8)
          count == "2" && text == "hello" && blank.isEmpty
        case _ => false
      assertTrue(decoded)
    },
    test("bridge keys win over the program environment and JS_ARGS stays off the node process") {
      val node   = NodeBinary(Path.of("/histos/node"))
      val script = Path.of("/work/main.js")
      val caller = Map(
        "JS_NODE"                -> "/caller/node",
        "JS_SCRIPT"              -> "/caller/main.js",
        "JS_MODULE"              -> "commonjs",
        "PLAYWRIGHT_NODEJS_PATH" -> "/caller/playwright",
        "HOME"                   -> "/home",
      )
      val nodeEnv = JsRun.runEnvironment(caller, JsEnv.Node, node, script, JsModuleKind.NoModule)
      val foxEnv  = JsRun.runEnvironment(caller, JsEnv.Firefox, node, script, JsModuleKind.ESModule)
      assertTrue(
        nodeEnv.get("JS_NODE").contains("/histos/node"),
        nodeEnv.get("JS_SCRIPT").contains(script.toAbsolutePath.toString),
        nodeEnv.get("JS_MODULE").contains("nomodule"),
        nodeEnv.get("HOME").contains("/home"),
        !nodeEnv.contains("JS_ARGS"),
        nodeEnv.get("PLAYWRIGHT_NODEJS_PATH").contains("/caller/playwright"),
        foxEnv.get("PLAYWRIGHT_NODEJS_PATH").contains("/histos/node"),
        foxEnv.get("JS_MODULE").contains("esmodule"),
        foxEnv.get("JS_SCRIPT").contains(script.toAbsolutePath.toString),
        !foxEnv.contains("JS_ARGS"),
      )
    },
  )
