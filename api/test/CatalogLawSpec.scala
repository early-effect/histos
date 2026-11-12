package histos.version

import histos.{BuildError, ProjectName, SourcePos}

import zio.test.*

object CatalogLawSpec extends ZIOSpecDefault:
  private val here  = SourcePos("histos.scala", 3, 5)
  private val there = SourcePos("histos.scala", 9, 5)
  private val api   = ProjectName.trusted("api")

  def spec = suite("catalog laws")(
    test(
      "a catalog is empty, or one row per ship and one ship per module, and a refusal names where its row was written"
    ):
      val core  = line("core", "api")
      val again = line("core", "app")
      val left  = line("left", "api")
      val right = line("right", "api")
      val twice = line("core", "api", "api")
      assertTrue(
        Catalog.from(Nil) == Right(Catalog.Empty),
        Catalog.from(List(core -> here)) == Right(Catalog.Lines(::(core, Nil))),
        Catalog.from(List(core -> here, again -> there)) ==
          Left(BuildError.DuplicateShip(core.name, here, there)),
        Catalog.from(List(left -> here, twice -> there)) ==
          Left(BuildError.RepeatedShipMember(twice.name, api, there)),
        Catalog.from(List(left -> here, right -> there)) ==
          Left(BuildError.MemberInTwoShips(api, left.name, right.name, there)),
      )
  )

  private def line(name: String, first: String, rest: String*): Ship.Line =
    Ship.Line(
      ShipName.trusted(name),
      ShipNumber.trusted(1, 0, 0),
      ::(ProjectName.trusted(first), rest.toList.map(ProjectName.trusted)),
    )
end CatalogLawSpec
