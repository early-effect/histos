package histos.splice

import histos.suite.HistosSpec

import zio.test.*

import java.nio.file.{Files, Path}

object SmokeSiteSpec extends HistosSpec:

  def spec =
    suite("smoke site")(
      test("writes spliced escape-string-regexp page under target/splice-smoke") {
        SmokeSite.prepare(publishSmoke = true).map { _ =>
          val js = Files.readString(Path.of("target/splice-smoke/fast.js"))
          assertTrue(
            Files.isRegularFile(Path.of("target/splice-smoke/index.html")),
            JsHost.evalExpr(js, "document.getElementById('out').textContent") == "hello\\?",
            !js.contains("""from "escape-string-regexp""""),
          )
        }
      }
    )
end SmokeSiteSpec
