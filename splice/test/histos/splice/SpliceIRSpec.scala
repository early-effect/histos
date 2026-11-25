package histos.splice

import histos.suite.HistosSpec

import org.scalajs.ir.Trees.JSNativeLoadSpec
import zio.test.*

object SpliceIRSpec extends HistosSpec:

  def spec =
    suite("SpliceIR")(
      test("mapped Import becomes Global __splice_* and keeps the path") {
        val ns     = JSNativeLoadSpec.Import("preact", Nil)
        val dflt   = JSNativeLoadSpec.Import("preact", List("default"))
        val named  = JSNativeLoadSpec.Import("preact", List("h"))
        val mapped = Set("preact")
        assertTrue(
          SpliceIR.remapSpec(ns, mapped) == JSNativeLoadSpec.Global("__splice_preact", Nil),
          SpliceIR.remapSpec(dflt, mapped) ==
            JSNativeLoadSpec.Global("__splice_preact", List("default")),
          SpliceIR.remapSpec(named, mapped) ==
            JSNativeLoadSpec.Global("__splice_preact", List("h")),
        )
      },
      test("unmapped Import is left alone") {
        val spec = JSNativeLoadSpec.Import("other", List("x"))
        assertTrue(SpliceIR.remapSpec(spec, Set("preact")) == spec)
      },
    )
end SpliceIRSpec
