package histos.splice

import zio.*
import zio.test.*

import java.nio.file.{Files, Path}
import java.security.MessageDigest

object SpliceSpec extends ZIOSpecDefault:

  def spec =
    suite("Splice")(
      test("unresolved bare specifier names the specifier and referring file") {
        for
          out <- tempOut
          err <- Splice
            .run(
              SpliceInput(
                linker = List(LinkerFile("main.js", """import * as foo from "foo";""")),
                libs = Map.empty,
                output = out,
              )
            )
            .flip
        yield assertTrue(
          err == SpliceError.Unresolved("foo", "main.js"),
          err.message == """sbt-splice: unresolved specifier "foo" in main.js""",
        )
      },
      test("missing mapped file names the specifier and path") {
        for
          out <- tempOut
          missing = Path.of("/no/such/foo.js")
          err <- Splice
            .run(
              SpliceInput(
                linker = List(LinkerFile("main.js", "")),
                libs = Map("foo" -> missing),
                output = out,
              )
            )
            .flip
        yield assertTrue(err == SpliceError.MissingFile("foo", missing.toString))
      },
      test("splices a namespace import so the mapped specifier does not remain") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(foo, """export function greet() { return "ok"; }""")
          out = dir.resolve("splice.js")
          path <- Splice.run(
            SpliceInput(
              linker = List(LinkerFile("main.js", use("foo", "Foo", "Foo.greet()"))),
              libs = Map("foo" -> foo),
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(path))
        yield assertTrue(
          path == out,
          outText(body) == "ok",
          !body.contains("""from "foo""""),
          !body.contains("""require("foo")"""),
        )
      },
      test("empty spliceLibs keeps an indexed source map") {
        for
          dir <- tempDir
          linkerMap = dir.resolve("main.js.map")
          _ <- write(linkerMap, """{"version":3,"file":"main.js","sources":["Hello.scala"],"mappings":"AAAA"}""")
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile(
                  "main.js",
                  """document.getElementById("out").textContent = "plain";
                    |""".stripMargin,
                  Some(linkerMap),
                )
              ),
              libs = Map.empty,
              output = out,
              sourceMaps = true,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
          map  <- ZIO.attempt(Files.readString(SourceMaps.mapPath(out)))
        yield assertTrue(
          outText(body) == "plain",
          map.contains("\"sections\""),
          body.contains("sourceMappingURL=splice.js.map"),
        )
      },
      test("a bundled source map is esbuild's and names the linker file") {
        for
          dir <- tempDir
          foo  = dir.resolve("foo.js")
          main = dir.resolve("main.js")
          js   = use("foo", "Foo", "Foo.greet()")
          _ <- write(foo, """export function greet() { return "ok"; }""")
          _ <- write(main, js)
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(LinkerFile("main.js", js, None, Some(main))),
              libs = Map("foo" -> foo),
              output = out,
              sourceMaps = true,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
          map  <- ZIO.attempt(Files.readString(SourceMaps.mapPath(out)))
        yield assertTrue(
          outText(body) == "ok",
          body.contains("sourceMappingURL=splice.js.map"),
          map.contains("\"sources\""),
          map.contains("\"mappings\""),
          map.contains("main.js"),
          !map.contains("\"sections\""),
        )
      },
      test("a linker sourceMappingURL does not replace the linker file in the bundle map") {
        val js =
          """import * as Foo from "foo";
            |document.getElementById("out").textContent = Foo.greet();
            |//# sourceMappingURL=main.js.map
            |""".stripMargin
        for
          dir <- tempDir
          linkDir = dir.resolve("target").resolve("splice").resolve("fast-link")
          main    = linkDir.resolve("main.js")
          foo     = dir.resolve("foo.js")
          out     = dir.resolve("app.js")
          _ <- ZIO.attempt(Files.createDirectories(linkDir))
          _ <- write(foo, """export function greet() { return "ok"; }""")
          _ <- write(main, js)
          _ <- write(
            linkDir.resolve("main.js.map"),
            """{"version":3,"file":"main.js","sources":["Hello.scala"],"mappings":"AAAA","names":[]}""",
          )
          _ <- Splice.run(
            SpliceInput(
              linker = List(LinkerFile("main.js", js, Some(linkDir.resolve("main.js.map")), Some(main))),
              libs = Map("foo" -> foo),
              output = out,
              sourceMaps = true,
            )
          )
          body     <- ZIO.attempt(Files.readString(out))
          map      <- ZIO.attempt(Files.readString(SourceMaps.mapPath(out)))
          restored <- ZIO.attempt(Files.readString(main))
        yield assertTrue(
          outText(body) == "ok",
          map.contains("\"sources\""),
          map.contains("fast-link/main.js"),
          !map.contains("\"sections\""),
          restored == js,
        )
      },
      test("splices two mapped specifiers into one file") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          bar = dir.resolve("bar.js")
          _ <- write(foo, """export function foo() { return 1; }""")
          _ <- write(bar, """export function bar() { return 2; }""")
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile(
                  "main.js",
                  """import * as Foo from "foo";
                    |import * as Bar from "bar";
                    |document.getElementById("out").textContent = "" + Foo.foo() + Bar.bar();
                    |""".stripMargin,
                )
              ),
              libs = Map("foo" -> foo, "bar" -> bar),
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          outText(body) == "12",
          !body.contains("""from "foo""""),
          !body.contains("""from "bar""""),
        )
      },
      test("resolves nested relative imports against the mapped file") {
        for
          dir <- tempDir
          util = dir.resolve("util.js")
          foo  = dir.resolve("foo.js")
          _ <- write(util, """export function helper() { return "ok"; }""")
          _ <- write(
            foo,
            """import { helper } from "./util.js";
                              |export function greet() { return helper(); }
                              |""".stripMargin,
          )
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(LinkerFile("main.js", use("foo", "Foo", "Foo.greet()"))),
              libs = Map("foo" -> foo),
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          outText(body) == "ok",
          !body.contains("""from "./util.js""""),
          !body.contains("""from "foo""""),
        )
      },
      test("splices pinned escape-string-regexp 5.0.0 and keeps the published sha256") {
        val jsBytes = vendorBytes("escape-string-regexp@5.0.0.js")
        val pinned  =
          String(vendorBytes("escape-string-regexp@5.0.0.js.sha256"), java.nio.charset.StandardCharsets.UTF_8).trim
        val digest = sha256(jsBytes)
        for
          dir <- tempDir
          js = dir.resolve("escape-string-regexp.js")
          _ <- write(js, String(jsBytes, java.nio.charset.StandardCharsets.UTF_8))
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile(
                  "main.js",
                  """import * as escapeStringRegexp from "escape-string-regexp";
                    |document.getElementById("out").textContent = escapeStringRegexp.default("hello?");
                    |""".stripMargin,
                )
              ),
              libs = Map("escape-string-regexp" -> js),
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          digest == pinned,
          pinned == "af2065ad2f2d2b91946c2121e21618daa3f4b18787af9226f8c953ca54cca2f5",
          outText(body) == "hello\\?",
          !body.contains("""from "escape-string-regexp""""),
        )
      },
      test("spliceClosure drops unused linker code and module syntax, and the library it calls still answers") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(foo, "export function used() { return \"used\"; }\n")
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile(
                  "main.js",
                  s"""function unused() { return "${ProtocolFixtures.DeadCodeMarker}"; }
                     |function shown(${ProtocolFixtures.LongLocal}) { return ${ProtocolFixtures.LongLocal}; }
                     |document.getElementById("out").textContent = shown(__splice_foo.used());
                     |export { shown };
                     |""".stripMargin,
                )
              ),
              libs = Map("foo" -> foo),
              output = out,
              minify = Minify.Closure,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          !body.contains(ProtocolFixtures.DeadCodeMarker),
          !body.contains(ProtocolFixtures.LongLocal),
          JsHost.evalExpr(body, "document.getElementById('out').textContent") == "used",
        )
      },
      test("a library esbuild cannot parse fails with the file and esbuild's reason") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _   <- write(foo, "const x = {")
          err <- Splice
            .run(
              SpliceInput(
                linker = List(LinkerFile("main.js", use("foo", "Foo", "Foo.greet()"))),
                libs = Map("foo" -> foo),
                output = dir.resolve("splice.js"),
              )
            )
            .flip
        yield assertTrue(
          err match
            case SpliceError.Bundle(Some("foo.js"), detail) => detail.nonEmpty
            case _                                          => false
        )
      },
      test("a library that reads import.meta, which a script cannot, fails naming the file") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _   <- write(foo, "export const here = import.meta.url;\n")
          err <- Splice
            .run(
              SpliceInput(
                linker = List(LinkerFile("main.js", use("foo", "Foo", "Foo.here"))),
                libs = Map("foo" -> foo),
                output = dir.resolve("splice.js"),
              )
            )
            .flip
        yield assertTrue(
          err match
            case SpliceError.Bundle(Some("foo.js"), detail) => detail.contains("import.meta")
            case _                                          => false
        )
      },
      test("optimize fails the program when Closure cannot parse the linker's output") {
        for
          dir <- tempDir
          err <- Splice
            .run(
              SpliceInput(
                linker = List(LinkerFile("main.js", "const x = {")),
                libs = Map.empty,
                output = dir.resolve("splice.js"),
                minify = Minify.Closure,
              )
            )
            .flip
        yield assertTrue(
          err match
            case SpliceError.Closure(detail) =>
              detail.nonEmpty && err.message.startsWith("sbt-splice: Closure compiler failed:")
            case _ => false
        )
      },
      test("a library's side-effect import of a package nobody mapped is an unresolved specifier") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _   <- write(foo, "import \"polyfill\";\nexport function greet() { return \"ok\"; }\n")
          err <- Splice
            .run(
              SpliceInput(
                linker = List(LinkerFile("main.js", use("foo", "Foo", "Foo.greet()"))),
                libs = Map("foo" -> foo),
                output = dir.resolve("splice.js"),
              )
            )
            .flip
        yield assertTrue(err == SpliceError.Unresolved("polyfill", "foo.js"))
      },
      test("linker output whose strings say from splices as it links") {
        for
          dir <- tempDir
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile(
                  "main.js",
                  """const m = ("Unable to obtain LocalDate from " + 1) + " from here";
                    |document.getElementById("out").textContent = m;
                    |""".stripMargin,
                )
              ),
              libs = Map.empty,
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          JsHost.evalExpr(
            body,
            "document.getElementById('out').textContent",
          ) == "Unable to obtain LocalDate from 1 from here"
        )
      },
      test("process.env.NODE_ENV is development for spliceFast and production for spliceFull") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(
            foo,
            "export function mode() { return process.env.NODE_ENV === \"production\" ? \"prod\" : \"dev\"; }\n",
          )
          fast   = dir.resolve("fast.js")
          full   = dir.resolve("full.js")
          linker = List(LinkerFile("main.js", use("foo", "Foo", "Foo.mode()")))
          _    <- Splice.run(SpliceInput(linker, Map("foo" -> foo), fast))
          _    <- Splice.run(SpliceInput(linker, Map("foo" -> foo), full, minify = Minify.Esbuild))
          dev  <- ZIO.attempt(Files.readString(fast))
          prod <- ZIO.attempt(Files.readString(full))
        yield assertTrue(outText(dev) == "dev", outText(prod) == "prod")
      },
      test("a library's side-effect imports run first, on a line of their own or minified mid-line") {
        check(Gen.elements("import \"./side.js\";\n", "var q=1;import\"./side.js\";")) { sideEffect =>
          for
            dir <- tempDir
            foo  = dir.resolve("foo.js")
            side = dir.resolve("side.js")
            _ <- write(side, "globalThis.sideRan = \"yes\";\n")
            _ <- write(foo, s"${sideEffect}export function greet() { return globalThis.sideRan; }\n")
            out = dir.resolve("splice.js")
            _ <- Splice.run(
              SpliceInput(
                linker = List(
                  LinkerFile(
                    "main.js",
                    use("foo", "Foo", "Foo.greet()"),
                  )
                ),
                libs = Map("foo" -> foo),
                output = out,
              )
            )
            body <- ZIO.attempt(Files.readString(out))
          yield assertTrue(
            !body.contains("import"),
            JsHost.evalExpr(body, "document.getElementById('out').textContent") == "yes",
          )
        }
      },
      test("a library that imports another mapped library finds it defined, whatever their names sort to") {
        check(Gen.elements(("a", "b"), ("b", "a"))) { (importer, imported) =>
          for
            dir <- tempDir
            user = dir.resolve(s"$importer.js")
            used = dir.resolve(s"$imported.js")
            _ <- write(used, "export function greet() { return \"greeted\"; }\n")
            _ <- write(user, s"import { greet } from \"$imported\";\nexport function hello() { return greet(); }\n")
            out = dir.resolve("splice.js")
            _ <- Splice.run(
              SpliceInput(
                linker = List(
                  LinkerFile(
                    "main.js",
                    use(importer, "User", "User.hello()"),
                  )
                ),
                libs = Map(importer -> user, imported -> used),
                output = out,
              )
            )
            body <- ZIO.attempt(Files.readString(out))
          yield assertTrue(JsHost.evalExpr(body, "document.getElementById('out').textContent") == "greeted")
        }
      },
      test("a library's re-exports reach whoever imports it") {
        val forms = Gen.elements(
          ("export { greet } from \"./impl.js\";\n", "Foo.greet()"),
          ("export { greet as hello } from \"./impl.js\";\n", "Foo.hello()"),
          ("export * as impl from \"./impl.js\";\n", "Foo.impl.greet()"),
          ("export * from \"./impl.js\";\n", "Foo.greet()"),
          ("var q=1;export{greet}from\"./impl.js\";", "Foo.greet()"),
        )
        check(forms) { (reexport, call) =>
          for
            dir <- tempDir
            foo  = dir.resolve("foo.js")
            impl = dir.resolve("impl.js")
            _ <- write(impl, "export function greet() { return \"re-exported\"; }\n")
            _ <- write(foo, reexport)
            out = dir.resolve("splice.js")
            _ <- Splice.run(
              SpliceInput(
                linker = List(
                  LinkerFile(
                    "main.js",
                    use("foo", "Foo", call),
                  )
                ),
                libs = Map("foo" -> foo),
                output = out,
              )
            )
            body <- ZIO.attempt(Files.readString(out))
          yield assertTrue(JsHost.evalExpr(body, "document.getElementById('out').textContent") == "re-exported")
        }
      },
      test("a module's exported declarations stay in scope for the rest of the module") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(
            foo,
            """export function a() { return b() + C.tag + x; }
              |export function b() { return "b"; }
              |export class C { static tag = "C"; }
              |export const x = "x";
              |""".stripMargin,
          )
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile(
                  "main.js",
                  use("foo", "Foo", "Foo.a() + Foo.b() + Foo.C.tag + Foo.x"),
                )
              ),
              libs = Map("foo" -> foo),
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(JsHost.evalExpr(body, "document.getElementById('out').textContent") == "bCxbCx")
      },
      test("a library whose strings say import, export, or from still wraps, and its strings are untouched") {
        val said = "export { a } from 'b'; import c from \"d\"; export this"
        val libs = Gen.elements(
          s"export function greet() { return ${quoted(said)}; }\n",
          s"module.exports.greet = function greet() { return ${quoted(said)}; };\n",
        )
        check(libs) { lib =>
          for
            dir <- tempDir
            foo = dir.resolve("foo.js")
            _ <- write(foo, lib)
            out = dir.resolve("splice.js")
            _ <- Splice.run(
              SpliceInput(
                linker = List(
                  LinkerFile(
                    "main.js",
                    use("foo", "Foo", "Foo.greet()"),
                  )
                ),
                libs = Map("foo" -> foo),
                output = out,
              )
            )
            body <- ZIO.attempt(Files.readString(out))
          yield assertTrue(JsHost.evalExpr(body, "document.getElementById('out').textContent") == said)
        }
      },
      test("a CommonJS library's default import is its module.exports, as ES interop has it") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(foo, "module.exports = { greet: function () { return \"cjs default\"; } };\n")
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile(
                  "main.js",
                  use("foo", "Foo", "Foo.default.greet()"),
                )
              ),
              libs = Map("foo" -> foo),
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(JsHost.evalExpr(body, "document.getElementById('out').textContent") == "cjs default")
      },
      test("a library that defines a custom element splices") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(
            foo,
            """export class Hello { greet() { return "defined"; } }
              |export function register(registry) { registry.define("x-hello", Hello); }
              |""".stripMargin,
          )
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile(
                  "main.js",
                  use("foo", "Foo", "new Foo.Hello().greet()"),
                )
              ),
              libs = Map("foo" -> foo),
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(JsHost.evalExpr(body, "document.getElementById('out').textContent") == "defined")
      },
      test("a cached full build is reused while nothing it was built from changes") {
        for
          dir <- tempDir
          foo   = dir.resolve("foo.js")
          out   = dir.resolve("full.js")
          stamp = dir.resolve("digest")
          _ <- write(foo, "export const x = \"one\";\n")
          input = SpliceInput(
            linker = List(LinkerFile("main.js", use("foo", "Foo", "Foo.x"))),
            libs = Map("foo" -> foo),
            output = out,
            minify = Minify.Esbuild,
            cache = Some(stamp),
          )
          _    <- Splice.run(input)
          _    <- write(out, "reused")
          _    <- Splice.run(input)
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(body == "reused")
      },
      test("a cached full build is rebuilt when a file a library imports changes") {
        for
          dir <- tempDir
          foo   = dir.resolve("foo.js")
          impl  = dir.resolve("impl.js")
          out   = dir.resolve("full.js")
          stamp = dir.resolve("digest")
          _ <- write(foo, "export { x } from \"./impl.js\";\n")
          _ <- write(impl, "export const x = \"one\";\n")
          input = SpliceInput(
            linker = List(LinkerFile("main.js", use("foo", "Foo", "Foo.x"))),
            libs = Map("foo" -> foo),
            output = out,
            minify = Minify.Esbuild,
            cache = Some(stamp),
          )
          _      <- Splice.run(input)
          first  <- ZIO.attempt(Files.readString(out))
          _      <- write(impl, "export const x = \"two\";\n")
          _      <- Splice.run(input)
          second <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          JsHost.evalExpr(first, "document.getElementById('out').textContent") == "one",
          JsHost.evalExpr(second, "document.getElementById('out').textContent") == "two",
        )
      },
      test("wraps CJS without rewriting exports and the binding is callable") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(foo, """module.exports.greet = function greet() { return "cjs"; };""")
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile("main.js", use("foo", "Foo", "Foo.greet()"))
              ),
              libs = Map("foo" -> foo),
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(outText(body) == "cjs")
      },
      test("wraps UMD through the CJS branch") {
        val umd =
          """(function (root, factory) {
            |  if (typeof exports === "object" && typeof module !== "undefined") module.exports = factory();
            |  else root.umdFoo = factory();
            |}(typeof globalThis !== "undefined" ? globalThis : this, function () {
            |  return { greet: function greet() { return "umd"; } };
            |}));
            |""".stripMargin
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(foo, umd)
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile("main.js", use("foo", "Foo", "Foo.greet()"))
              ),
              libs = Map("foo" -> foo),
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(outText(body) == "umd")
      },
      test("spliceClosure class-extends override and setState call work without a keep list") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(foo, ProtocolFixtures.classComponentEsm)
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(LinkerFile("main.js", ProtocolFixtures.scalaJsClassSubclass)),
              libs = Map("foo" -> foo),
              output = out,
              minify = Minify.Closure,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          JsHost.evalExpr(body, "document.getElementById('out').textContent") == ProtocolFixtures.classComponentOut,
          body.contains("setState"),
          body.contains("render"),
          !body.contains(ProtocolFixtures.DeadCodeMarker),
        )
      },
      test("spliceClosure custom-element connectedCallback override runs") {
        for
          dir <- tempDir
          el = dir.resolve("el.js")
          _ <- write(el, ProtocolFixtures.customElementEsm)
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(LinkerFile("main.js", ProtocolFixtures.scalaJsCustomElementSubclass)),
              libs = Map("el" -> el),
              output = out,
              minify = Minify.Closure,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          JsHost.evalExpr(body, "document.getElementById('out').textContent") == ProtocolFixtures.customElementOut,
          body.contains("connectedCallback"),
        )
      },
      test("spliceFast and spliceFull drop an unused export of a namespace import") {
        for
          dir <- tempDir
          lib = dir.resolve("lib.js")
          _ <- write(
            lib,
            """export function one() { return "one"; }
              |export const dead = "DEAD_EXPORT_MARKER";
              |""".stripMargin,
          )
          linker = List(LinkerFile("main.js", use("lib", "Lib", "Lib.one()")))
          fast <- readRun(SpliceInput(linker, Map("lib" -> lib), dir.resolve("fast.js")))
          full <- readRun(SpliceInput(linker, Map("lib" -> lib), dir.resolve("full.js"), minify = Minify.Esbuild))
        yield assertTrue(
          outText(fast) == "one",
          outText(full) == "one",
          !fast.contains("DEAD_EXPORT_MARKER"),
          !full.contains("DEAD_EXPORT_MARKER"),
        )
      },
      test("passing the namespace as a value keeps the unused export") {
        for
          dir <- tempDir
          lib = dir.resolve("lib.js")
          _ <- write(
            lib,
            """export function one() { return "one"; }
              |export const dead = "DEAD_EXPORT_MARKER";
              |""".stripMargin,
          )
          body <- readRun(
            SpliceInput(
              List(LinkerFile("main.js", use("lib", "Lib", "Lib.dead + Lib.one()"))),
              Map("lib" -> lib),
              dir.resolve("fast.js"),
            )
          )
        yield assertTrue(body.contains("DEAD_EXPORT_MARKER"), outText(body) == "DEAD_EXPORT_MARKERone")
      },
      test("a direct re-export of an unused binding is dropped") {
        for
          dir <- tempDir
          live  = dir.resolve("live.js")
          dead  = dir.resolve("dead.js")
          entry = dir.resolve("entry.js")
          _ <- write(live, """export function live() { return "live"; }""")
          _ <- write(dead, """export const dead = "DEAD_EXPORT_MARKER";""")
          _ <- write(
            entry,
            """export { live } from "./live.js";
              |export { dead } from "./dead.js";
              |""".stripMargin,
          )
          linker = List(LinkerFile("main.js", use("entry", "Lib", "Lib.live()")))
          fast <- readRun(SpliceInput(linker, Map("entry" -> entry), dir.resolve("fast.js")))
          full <- readRun(SpliceInput(linker, Map("entry" -> entry), dir.resolve("full.js"), minify = Minify.Esbuild))
        yield assertTrue(
          outText(fast) == "live",
          outText(full) == "live",
          !fast.contains("DEAD_EXPORT_MARKER"),
          !full.contains("DEAD_EXPORT_MARKER"),
        )
      },
      test("a pinned export * as namespace still runs the export the program calls") {
        for
          dir <- tempDir
          inner = dir.resolve("inner.js")
          star  = dir.resolve("star.js")
          _ <- write(
            inner,
            """export function live() { return "live"; }
              |export const dead = "STAR_MARKER";
              |""".stripMargin,
          )
          _    <- write(star, """export * as ns from "./inner.js";""")
          body <- readRun(
            SpliceInput(
              List(LinkerFile("main.js", use("star", "Lib", "Lib.ns.live()"))),
              Map("star" -> star),
              dir.resolve("fast.js"),
            )
          )
        yield assertTrue(outText(body) == "live")
      },
      test("spliceClosure runs the called export and keeps an unused library export") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(
            foo,
            """export function used() { return "used"; }
              |export const dead = "DEAD_EXPORT_MARKER";
              |""".stripMargin,
          )
          body <- readRun(
            SpliceInput(
              List(
                LinkerFile(
                  "main.js",
                  """document.getElementById("out").textContent = __splice_foo.used();
                    |""".stripMargin,
                )
              ),
              Map("foo" -> foo),
              dir.resolve("closure.js"),
              minify = Minify.Closure,
            )
          )
        yield assertTrue(outText(body) == "used", body.contains("DEAD_EXPORT_MARKER"))
      },
      test("spliceFast and spliceFull keep a class-extends render override") {
        val linker =
          """import * as $i_widget from "foo";
            |function $s(this$1) { this$1.setState({ "v": "from-will-mount" }); }
            |var $b;
            |function $a() {
            |  if (!$b) {
            |    $b = class $b extends $i_widget.Component {
            |      constructor() { super(); }
            |      "componentWillMount"() { $s(this); }
            |      "render"() { return "OVERRIDE_RENDER"; }
            |      "componentDidMount"() { return "OVERRIDE_DID_MOUNT"; }
            |    };
            |  }
            |  return $b;
            |}
            |document.getElementById("out").textContent = $i_widget.mount($a());
            |""".stripMargin
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(foo, ProtocolFixtures.classComponentEsm)
          files = List(LinkerFile("main.js", linker))
          fast <- readRun(SpliceInput(files, Map("foo" -> foo), dir.resolve("fast.js")))
          full <- readRun(SpliceInput(files, Map("foo" -> foo), dir.resolve("full.js"), minify = Minify.Esbuild))
        yield assertTrue(
          outText(fast) == ProtocolFixtures.classComponentOut,
          outText(full) == ProtocolFixtures.classComponentOut,
          fast.contains("setState"),
          fast.contains("render"),
          full.contains("setState"),
          full.contains("render"),
          !fast.contains(ProtocolFixtures.DeadCodeMarker),
          !full.contains(ProtocolFixtures.DeadCodeMarker),
        )
      },
      test("spliceFast keeps a connectedCallback override") {
        val linker =
          """import * as El from "el";
            |class HelloElement extends El.HtmlElement {
            |  constructor() { super(); }
            |  connectedCallback() { return "OVERRIDE_CONNECTED"; }
            |}
            |document.getElementById("out").textContent = El.upgrade(HelloElement);
            |""".stripMargin
        for
          dir <- tempDir
          el = dir.resolve("el.js")
          _    <- write(el, ProtocolFixtures.customElementEsm)
          body <- readRun(SpliceInput(List(LinkerFile("main.js", linker)), Map("el" -> el), dir.resolve("fast.js")))
        yield assertTrue(
          outText(body) == ProtocolFixtures.customElementOut,
          body.contains("connectedCallback"),
        )
      },
      test("an unmapped specifier next to a mapped one names the linker file") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _   <- write(foo, """export function greet() { return "ok"; }""")
          err <- Splice
            .run(
              SpliceInput(
                linker = List(
                  LinkerFile(
                    "main.js",
                    """import * as Foo from "foo";
                      |import * as Bar from "bar";
                      |document.getElementById("out").textContent = Foo.greet();
                      |""".stripMargin,
                  )
                ),
                libs = Map("foo" -> foo),
                output = dir.resolve("splice.js"),
              )
            )
            .flip
        yield assertTrue(
          err == SpliceError.Unresolved("bar", "main.js"),
          err.message == """sbt-splice: unresolved specifier "bar" in main.js""",
        )
      },
      test("a linker string that says from does not fail the bundle") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _    <- write(foo, """export function greet() { return "ok"; }""")
          body <- readRun(
            SpliceInput(
              List(
                LinkerFile(
                  "main.js",
                  """import * as Foo from "foo";
                    |const m = "Unable to obtain LocalDate from " + Foo.greet();
                    |document.getElementById("out").textContent = m;
                    |""".stripMargin,
                )
              ),
              Map("foo" -> foo),
              dir.resolve("splice.js"),
            )
          )
        yield assertTrue(outText(body) == "Unable to obtain LocalDate from ok")
      },
      test("more than one linker file on the bundle path fails") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _   <- write(foo, """export function greet() { return "ok"; }""")
          err <- Splice
            .run(
              SpliceInput(
                linker = List(LinkerFile("a.js", use("foo", "Foo", "Foo.greet()")), LinkerFile("b.js", "")),
                libs = Map("foo" -> foo),
                output = dir.resolve("splice.js"),
              )
            )
            .flip
        yield assertTrue(
          err == SpliceError.SeveralModules(List("a.js", "b.js")),
          err.message == "sbt-splice: expected one linker file to bundle, found a.js, b.js",
        )
      },
      test("a mapped library the linker never imports is absent") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _    <- write(foo, """export const dead = "DEAD_EXPORT_MARKER";""")
          body <- readRun(
            SpliceInput(
              List(
                LinkerFile(
                  "main.js",
                  """document.getElementById("out").textContent = "hi";
                    |""".stripMargin,
                )
              ),
              Map("foo" -> foo),
              dir.resolve("splice.js"),
            )
          )
        yield assertTrue(outText(body) == "hi", !body.contains("DEAD_EXPORT_MARKER"))
      },
      test("a cached full build is rebuilt when a specifier points at another file") {
        for
          dir <- tempDir
          foo   = dir.resolve("foo.js")
          bar   = dir.resolve("bar.js")
          out   = dir.resolve("full.js")
          stamp = dir.resolve("digest")
          _ <- write(foo, "export const x = \"one\";\n")
          _ <- write(bar, "export const x = \"one\";\n")
          first = SpliceInput(
            linker = List(LinkerFile("main.js", use("foo", "Foo", "Foo.x"))),
            libs = Map("foo" -> foo),
            output = out,
            minify = Minify.Esbuild,
            cache = Some(stamp),
          )
          _    <- Splice.run(first)
          _    <- write(out, "reused")
          _    <- Splice.run(first.copy(libs = Map("foo" -> bar)))
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(body != "reused")
      },
    )

  private def vendorBytes(name: String): Array[Byte] =
    val in = Option(getClass.getResourceAsStream(s"/vendor/$name"))
      .getOrElse(sys.error(s"missing resource /vendor/$name"))
    try in.readAllBytes()
    finally in.close()

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map("%02x".format(_)).mkString

  /** `s` as a JS double-quoted string literal. */
  private def quoted(s: String): String = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  /** Namespace import of `spec`, then write `expr` where a page script can read it. An iife does not leak the binding.
    */
  private def use(spec: String, binding: String, expr: String): String =
    s"""import * as $binding from "$spec";
       |document.getElementById("out").textContent = $expr;
       |""".stripMargin

  private def outText(js: String): String =
    JsHost.evalExpr(js, "document.getElementById('out').textContent")

  private def readRun(input: SpliceInput): IO[SpliceError, String] =
    Splice.run(input) *>
      ZIO.attempt(Files.readString(input.output)).mapError(e => SpliceError.Io(e.getMessage))

  private def tempDir: UIO[Path] =
    ZIO.attempt(Files.createTempDirectory("sbt-splice-")).orDie

  private def tempOut: UIO[Path] =
    tempDir.map(_.resolve("out.js"))

  private def write(path: Path, body: String): Task[Unit] =
    ZIO.attempt {
      Files.writeString(path, body)
      ()
    }
end SpliceSpec
