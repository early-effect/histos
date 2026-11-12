package histos

import histos.version.{ShipName, ShipNumber}

import zio.test.*

object ShipDraftLawSpec extends ZIOSpecDefault:
  private val number = ShipNumber.trusted(1, 4, 2)
  private val name   = ShipName.trusted("histos")
  private val here   = SourcePos("law", 2, 3)

  def spec = suite("ship draft laws")(
    test("a ship builds clients on releases unless it asks for snapshots too, and the catalog row keeps that choice"):
      val core  = module("core", Jvm)(scala"3.10.0")
      val row   = ShipDef.line(name.value, number, ::(core, Nil), here)
      val asked = row.clients(ClientBuilds.ReleasesAndSnapshots)
      assertTrue(
        row.clients == ClientBuilds.Releases,
        asked.clients == ClientBuilds.ReleasesAndSnapshots,
        asked.description == row.description,
        linked(row, core) == Some(ClientBuilds.Releases),
        linked(asked, core) == Some(ClientBuilds.ReleasesAndSnapshots),
      )
  )

  private def linked(row: ShipDef, member: ProjectDef): Option[ClientBuilds] =
    Projects(CatalogSpec(row), member).toOption.flatMap(_.catalog.get(name)).map(_.clients)
end ShipDraftLawSpec
