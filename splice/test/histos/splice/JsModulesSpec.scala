package histos.splice

import zio.test.*

object JsModulesSpec extends ZIOSpecDefault:

  def spec =
    suite("JsModules")(
      test("treats package names as bare, and paths and URLs as not") {
        assertTrue(
          JsModules.isBare("foo"),
          JsModules.isBare("escape-string-regexp"),
          JsModules.isBare("foo/plugin"),
          !JsModules.isBare("./util.js"),
          !JsModules.isBare("../x.js"),
          !JsModules.isBare("/abs.js"),
          !JsModules.isBare("https://example.com/x.js"),
        )
      },
      test("finds each import statement's specifier, and the specifier a re-export names") {
        val statements = Gen.elements(
          "import * as $i_foo from ",
          "import foo from ",
          "import { greet as $g } from ",
          "import {\n  a,\n  b as c,\n} from ",
          "import d, { e } from ",
          "import ",
          "export * from ",
          "export { x as y } from ",
        )
        val before  = Gen.elements("", "const q = 1;\n", "/* head */\n", "  ")
        val quotes  = Gen.elements("\"", "'")
        val modules = Gen.elements("foo", "@scope/pkg", "./util.js", "../x/y.js", "pkg/sub")
        check(before, statements, quotes, modules) { (lead, statement, quote, module) =>
          val js = s"$lead$statement$quote$module$quote;\nconst z = 2;"
          assertTrue(JsModules.specifiers(js) == List(module))
        }
      },
      test("a string that says import or from is not an import") {
        check(Gen.alphaNumericStringBounded(0, 12)) { word =>
          val js =
            s"""throw new Error(("Unable to obtain LocalDate from " + $$m_x().y(a_$$_lo)) + "$word");
               |const msg = "$word from " + v;
               |const s = 'import x from ' + "y";""".stripMargin
          assertTrue(JsModules.specifiers(js).isEmpty)
        }
      },
      test("dropExports removes ES module export lines") {
        val js =
          """const x = 1;
            |export { Hello as Hello };
            |export{Foo};
            |""".stripMargin
        val out = JsModules.dropExports(js)
        assertTrue(
          out.contains("const x = 1;"),
          !out.contains("export"),
        )
      },
    )
end JsModulesSpec
