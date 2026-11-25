package histos.splice

import histos.suite.HistosSpec

import zio.test.*
import zio.*

import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.util.Using

object EsbuildNativeSpec extends HistosSpec:

  def spec = suite("EsbuildNative")(
    test("this host has a pinned esbuild") {
      assertTrue(EsbuildNative.currentPin.isRight)
    },
    test("minify keeps class extends Error and super, not Error.call") {
      for
        js <- EsbuildNative.optimize(
          ProtocolFixtures.errorSubclass,
          FileCacheDir,
          localOnly = false,
        )
        out = JsHost.evalExpr(js, "document.getElementById('out').textContent")
      yield assertTrue(
        out == ProtocolFixtures.errorSubclassOut,
        js.contains("extends Error"),
        !js.contains("Error.call("),
        !js.contains("this.message="),
        !js.contains("this.message ="),
      )
    },
    test("minify keeps a class-extends render override") {
      val src = ProtocolFixtures.classComponentVendor + ProtocolFixtures.scalaJsClassSubclass
      for
        js <- EsbuildNative.optimize(src, FileCacheDir, localOnly = false)
        out = JsHost.evalExpr(js, "document.getElementById('out').textContent")
      yield assertTrue(
        out == ProtocolFixtures.classComponentOut,
        js.contains("setState"),
        js.contains("render"),
      )
    },
    test("minify is smaller than concat") {
      val src = ProtocolFixtures.classComponentVendor + ProtocolFixtures.hCallLinker
      for js <- EsbuildNative.optimize(src, FileCacheDir, localOnly = false)
      yield assertTrue(js.length < src.length)
    },
    test("spliceFull minify path keeps Error subclass") {
      for
        dir <- tempDir
        out = dir.resolve("full.js")
        _ <- Splice.run(
          SpliceInput(
            linker = List(LinkerFile("main.js", ProtocolFixtures.errorSubclass)),
            libs = Map.empty,
            output = out,
            minify = Minify.Esbuild,
          )
        )
        body <- ZIO.attempt(Files.readString(out))
        got = JsHost.evalExpr(body, "document.getElementById('out').textContent")
      yield assertTrue(
        got == ProtocolFixtures.errorSubclassOut,
        !body.contains("Error.call("),
      )
    },
    test("parallel installs keep a readable binary") {
      val callers = 12
      for
        dir     <- tempDir
        _       <- EsbuildNative.optimize(ProtocolFixtures.errorSubclass, dir, localOnly = false)
        _       <- ZIO.attempt(deleteTree(dir.resolve("sbt-splice-esbuild")))
        results <- ZIO.foreachPar(Chunk.fromIterable(1 to callers)) { _ =>
          EsbuildNative.optimize(ProtocolFixtures.errorSubclass, dir, localOnly = false)
        }
      yield assertTrue(
        results.size == callers,
        results.forall(js => js.contains("extends Error") && !js.contains("Error.call(")),
      )
    },
    test("unknown host fails without fetching") {
      val err = EsbuildNative.pinFor("SerenityOS", "riscv64")
      assertTrue(
        err.isLeft,
        err.swap.exists(_.message.contains("darwin-arm64")),
      )
    },
  )

  private val FileCacheDir = coursier.cache.FileCache().location.toPath

  private def tempDir: Task[Path] =
    ZIO.attempt(Files.createTempDirectory("sbt-splice-esbuild-spec-"))

  /** Drops the extracted binary and leaves the Coursier tarball, so the next installs race on publish. */
  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      Using.resource(Files.walk(root)): walk =>
        walk.sorted(Comparator.reverseOrder[Path]()).forEach(Files.delete(_))
        ()
end EsbuildNativeSpec
