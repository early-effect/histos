package histos.version

import histos.ProjectName

import zio.test.*

object CatalogLawSpec extends ZIOSpecDefault:
  def spec = suite("catalog laws")(
    test("a catalog is empty, or one row per ship and one ship per module"):
      val api   = line("core", "api")
      val again = line("core", "app")
      val left  = line("left", "api")
      val right = line("right", "api")
      val twice = line("core", "api", "api")
      assertTrue(
        Catalog.from(Nil) == Right(Catalog.Empty),
        Catalog.from(List(api)).map(_.ships.map(_.name)) == Right(List(api.name)),
        Catalog.from(List(api, again)) == Left(CatalogError.DuplicateName(api.name)),
        Catalog.from(List(twice)) == Left(CatalogError.RepeatedMember(twice.name, ProjectName.trusted("api"))),
        Catalog.from(List(left, right)) == Left(
          CatalogError.InTwoShips(ProjectName.trusted("api"), left.name, right.name)
        ),
      )
  )

  private def line(name: String, first: String, rest: String*): Ship.Line =
    Ship.Line(
      ShipName.trusted(name),
      ShipNumber.trusted(1, 0, 0),
      ::(ProjectName.trusted(first), rest.toList.map(ProjectName.trusted)),
    )
end CatalogLawSpec
