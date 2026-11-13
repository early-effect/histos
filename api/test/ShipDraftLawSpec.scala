package histos

import histos.version.{ShipName, ShipNumber, ShipToolchain}

import zio.test.*

object ShipDraftLawSpec extends ZIOSpecDefault:
  private val number = ShipNumber.trusted(1, 4, 2)
  private val name   = ShipName.trusted("histos")
  private val here   = SourcePos("law", 2, 3)

  def spec = suite("ship draft laws")(
    test("a ship records no toolchain until the row names a launcher and a client"):
      val core   = module("core", Jvm)(scala"3.10.0")
      val app    = module("app", Jvm)(scala"3.10.0").assembly(MainClass("demo.Main"))
      val client = module("client", Native)(scala"3.10.0").clang(ClangMajor(21)).main(MainClass("demo.Client"))
      val row    = ShipDef.line(name.value, number, ::(core, List(app)), here)
      val asked  = row.toolchain(launcher = app, client = client)
      assertTrue(
        linked(row, core, app, client).isEmpty,
        linked(asked, core, app, client) == Some(ShipToolchain(ProjectName("app"), ProjectName("client"))),
      )
  )

  private def linked(
      row: ShipDef,
      core: ProjectDef,
      app: ProjectDef,
      client: ProjectDef,
  ): Option[ShipToolchain] =
    Projects(CatalogSpec(row), core, app, client).toOption.flatMap(_.catalog.get(name)).flatMap(_.toolchain)
end ShipDraftLawSpec
