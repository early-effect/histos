package histos.splice

import histos.suite.HistosSpec

import zio.test.*

import java.nio.file.Path

object SourceMapsSpec extends HistosSpec:

  def spec =
    suite("SourceMaps")(
      test("lineOffset counts every newline including blanks") {
        assertTrue(
          SourceMaps.lineOffset("") == 0,
          SourceMaps.lineOffset("a") == 0,
          SourceMaps.lineOffset("a\n") == 1,
          SourceMaps.lineOffset("a\n\n") == 2,
          SourceMaps.lineOffset("a\n\nb\n") == 3,
        )
      },
      test("indexed map first section offset is the prepended line count") {
        val prefix = "const __splice_foo = (() => {\n  return {};\n})();\n\n"
        val map    = Path.of("/tmp/splice/fast-link/main.js.map")
        val dest   = Path.of("/tmp/splice/fast.js.map")
        val linker = List(LinkerFile("main.js", "const Foo = __splice_foo;\n", Some(map)))
        val secs   = SourceMaps.sectionsFor(prefix, linker, dest)
        val json   = SourceMaps.indexed("fast.js", secs)
        val first  = secs match
          case section :: Nil => Some(section)
          case _              => None
        assertTrue(
          first.exists(s => s.line == SourceMaps.lineOffset(prefix) && s.line == 4),
          first.exists(_.url == "fast-link/main.js.map"),
          json.contains("\"line\":4"),
          json.contains("fast-link/main.js.map"),
        )
      },
      test("retarget rewrites sources relative to the final map and leaves sourcesContent") {
        val json =
          """{"version":3,"sources":["../../proj/target/splice/fast-link/main.js","/proj/vendor/foo.js"],"sourcesContent":["keep \"sources\" here"],"mappings":""}"""
        val out = SourceMaps.retarget(
          json,
          Path.of("/tmp/esbuild-work"),
          Path.of("/proj/target/splice/fast.js.map"),
        )
        assertTrue(
          out.contains("\"fast-link/main.js\""),
          out.contains("\"../../vendor/foo.js\"") || out.contains("vendor/foo.js"),
          out.contains("keep \\\"sources\\\" here"),
          !out.contains("\"sections\""),
        )
      },
      test("metafile input keys are the top-level inputs object") {
        val meta =
          """{"inputs":{"lib.js":{"bytes":1,"imports":[]},"in.js":{"bytes":2,"inputs":{"nested.js":{}}}},"outputs":{"out.js":{"inputs":{"skip.js":{"bytesInOutput":1}}}}}"""
        assertTrue(JsonText.objectKeys(meta, "inputs") == List("lib.js", "in.js"))
      },
      test("a quoted key in inputs round-trips") {
        val meta = """{"inputs":{"a\"b":{"bytes":1}}}"""
        assertTrue(JsonText.objectKeys(meta, "inputs") == List("a\"b"))
      },
    )
end SourceMapsSpec
