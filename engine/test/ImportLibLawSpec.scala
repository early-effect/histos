package histos.scaffold

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A zipx `Lib` names a val, a test scope, or a coordinate this draft cannot place. */
object ImportLibLawSpec extends ZIOSpecDefault:
  def spec = suite("import lib laws")(
    test("a val is the coordinate, a test scope stays on the test list, and a stray scope is unmapped"):
      val versions =
        """val zio = "dev.zio::zio:2.1.27"
          |Lib(zio)
          |Lib(zio % Test)
          |Lib(zio % Provided)
          |Lib(missing)
          |""".stripMargin
      val draft = ImportDraft.zipx(versions, List("core"), "app")
      assertTrue(
        draft.contains(""".libraryDependencies(dep"dev.zio::zio:2.1.27")"""),
        draft.contains(""".testDependencies(dep"dev.zio::zio:2.1.27")"""),
        draft.contains("// unmapped: Lib(zio % Provided)"),
        draft.contains("// unmapped: Lib(missing)"),
        !draft.contains("// unmapped: Lib(zio)"),
        !draft.contains("// unmapped: Lib(zio % Test)"),
      )
  )
end ImportLibLawSpec
