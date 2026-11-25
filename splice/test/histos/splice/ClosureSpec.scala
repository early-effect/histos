package histos.splice

import histos.suite.HistosSpec

import zio.*
import zio.test.*

import java.nio.file.{Files, Path}

object ClosureSpec extends HistosSpec:

  def spec =
    suite("Closure")(
      test("programDigest is stable and changes when the libraries, the linker, or the output change") {
        check(Gen.alphaNumericString, Gen.alphaNumericString) { (libraries, linker) =>
          val out  = Path.of("full.js")
          val same = Closure.programDigest(libraries, linker, out, sourceMaps = false)
          assertTrue(
            same == Closure.programDigest(libraries, linker, out, sourceMaps = false),
            same != Closure.programDigest(libraries + "x", linker, out, sourceMaps = false),
            same != Closure.programDigest(libraries, linker + "x", out, sourceMaps = false),
            same != Closure.programDigest(libraries, linker, Path.of("other.js"), sourceMaps = false),
            same != Closure.programDigest(libraries, linker, out, sourceMaps = true),
            same.length == 64,
          )
        }
      },
      test("cacheHit is true only when stamp, digest, and output all match") {
        for
          dir <- tempDir
          out   = dir.resolve("full.js")
          stamp = dir.resolve("digest")
          _ <- write(out, "js")
          _ <- ZIO.succeed(Closure.storeCache(stamp, "abc"))
        yield assertTrue(
          Closure.cacheHit(stamp, "abc", out),
          !Closure.cacheHit(stamp, "other", out),
          !Closure.cacheHit(stamp, "abc", dir.resolve("missing.js")),
          !Closure.cacheHit(stamp, "abc", out, Some(dir.resolve("missing.js.map"))),
        )
      },
      test("optimize drops unused exports, minifies locals, and is smaller than concat") {
        val vendor = ProtocolFixtures.minifyVendor
        val linker = ProtocolFixtures.minifyLinker
        for
          got <- Closure.optimize(List("vendor.js" -> vendor, "linker.js" -> linker))
          out = JsHost.evalExpr(got.js, "document.getElementById('out').textContent")
        yield assertTrue(
          out == "1",
          got.js.length < vendor.length + linker.length,
          !got.js.contains(ProtocolFixtures.DeadCodeMarker),
          !got.js.contains(ProtocolFixtures.LongLocal),
        )
      },
      test("rewriteScalaJsNames replaces escape and raw U+FF3F") {
        val escaped = """$m_Ljava_nio_charset_ISO\uff3f8859\uff3f1$"""
        val upper   = """$m_Ljava_nio_charset_ISO\uFF3F8859\uFF3F1$"""
        val raw     =
          s"$$m_Ljava_nio_charset_ISO${Closure.FullwidthLowLine}8859${Closure.FullwidthLowLine}1$$"
        val standIn = "$m_Ljava_nio_charset_ISO$uFF3F8859$uFF3F1$"
        assertTrue(
          Closure.rewriteScalaJsNames(escaped) == standIn,
          Closure.rewriteScalaJsNames(upper) == standIn,
          Closure.rewriteScalaJsNames(raw) == standIn,
        )
      },
      test("optimize accepts Scala.js minify names and keeps host free-vars") {
        val linker =
          """function $m_Ljava_nio_charset_ISO\uff3f8859\uff3f1$() { return 1; }
            |onmessage = function () { return $m_Ljava_nio_charset_ISO\uff3f8859\uff3f1$(); };
            |attachEvent("onmessage", onmessage);
            |""".stripMargin
        for got <- Closure.optimize(List("linker.js" -> linker))
        yield assertTrue(
          !got.js.contains("\\uff3f"),
          !got.js.contains("\\uFF3F"),
          !got.js.contains(Closure.FullwidthLowLine.toString),
          got.js.contains("onmessage"),
          got.js.contains("attachEvent"),
        )
      },
      test("prefix chunks are prepended and extra extern names are not minified away") {
        val prefix =
          """const __splice_ext = (() => {
            |  const module = { exports: {} };
            |  module.exports.keep = function keep() { return "EXTERN_KEEP"; };
            |  return module.exports;
            |})();
            |""".stripMargin
        val linker = "const n = __splice_ext.keep();"
        for got <- Closure.optimize(
            inputs = List("linker.js" -> linker),
            prefix = List("ext.js" -> prefix),
            extraExterns = List("__splice_ext"),
          )
        yield assertTrue(
          got.js.startsWith("const __splice_ext"),
          got.js.contains("EXTERN_KEEP"),
        )
      },
      test("optimize stubs process so ZIO-shaped exitCode and env eval without a host process") {
        val linker =
          """onmessage = function () {
            |  process.exitCode = 7;
            |  var env = (typeof process !== "undefined" && typeof process.env !== "undefined")
            |    ? process.env : {};
            |  return String(process.exitCode) + (env.FOO || "none");
            |};
            |""".stripMargin
        for
          got <- Closure.optimize(List("linker.js" -> linker))
          ran  = JsHost.evalExpr(got.js, "onmessage()")
          host = JsHost.evalExpr(got.js, "typeof process")
        yield assertTrue(ran == "7none", host == "undefined")
      },
      test("optimize fails on a free-var that is not a host extern or Node stub") {
        val linker = "onmessage = function () { notAHostApi.foo = 1; };"
        for result <- Closure.optimize(List("linker.js" -> linker)).either
        yield assertTrue(
          result match
            case Left(SpliceError.Closure(detail)) => detail.contains("notAHostApi")
            case _                                 => false
        )
      },
      test("advanced mode runs a Scala.js class-extends render override and setState call") {
        for
          got <- Closure.optimize(
            List(
              "vendor.js"  -> ProtocolFixtures.classComponentVendor,
              "runtime.js" -> ProtocolFixtures.scalaJsRuntimePadding,
              "linker.js"  -> ProtocolFixtures.scalaJsClassSubclass,
            )
          )
          out = JsHost.evalExpr(got.js, "document.getElementById('out').textContent")
        yield assertTrue(
          out == ProtocolFixtures.classComponentOut,
          got.js.contains("setState"),
          got.js.contains("render"),
          got.js.contains("componentDidMount"),
          !got.js.contains(ProtocolFixtures.DeadCodeMarker),
        )
      },
      test("advanced mode keeps a named export like h") {
        for
          got <- Closure.optimize(
            List("vendor.js" -> ProtocolFixtures.classComponentVendor, "linker.js" -> ProtocolFixtures.hCallLinker)
          )
          out = JsHost.evalExpr(got.js, "document.getElementById('out').textContent")
        yield assertTrue(out == "h1")
      },
      test("advanced mode runs a custom-element connectedCallback override") {
        for
          got <- Closure.optimize(
            List(
              "vendor.js" -> ProtocolFixtures.customElementVendor,
              "linker.js" -> ProtocolFixtures.scalaJsCustomElementSubclass,
            )
          )
          out = JsHost.evalExpr(got.js, "document.getElementById('out').textContent")
        yield assertTrue(
          out == ProtocolFixtures.customElementOut,
          got.js.contains("connectedCallback"),
        )
      },
      test("advanced mode keeps Scala.js class extends Error and super instead of Error.call") {
        for
          got <- Closure.optimize(List("linker.js" -> ProtocolFixtures.errorSubclass))
          out = JsHost.evalExpr(got.js, "document.getElementById('out').textContent")
        yield assertTrue(
          out == ProtocolFixtures.errorSubclassOut,
          got.js.contains("extends Error"),
          !got.js.contains("Error.call("),
          !got.js.contains("this.message="),
          !got.js.contains("this.message ="),
        )
      },
    )

  private def tempDir: UIO[Path] =
    ZIO.attempt(Files.createTempDirectory("sbt-splice-closure-")).orDie

  private def write(path: Path, body: String): Task[Unit] =
    ZIO.attempt {
      Files.writeString(path, body)
      ()
    }
end ClosureSpec
