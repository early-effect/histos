package histos.version

import zio.test.*

object ShipBumpLawSpec extends ZIOSpecDefault:
  private val nine  = ShipNumber.trusted(0, 2, 9)
  private val eight = ShipNumber.trusted(0, 2, 8)
  private val ship  = ShipName.trusted("histos")

  def spec = suite("ship bump")(
    test("a remote stable wins over a newer ivy2 local stable"):
      val listed = List(
        ListedStable(nine, StablePlace.Local),
        ListedStable(eight, StablePlace.Remote),
      )
      assertTrue(ShipBump.choose(listed).contains(ListedStable(eight, StablePlace.Remote)))
    ,
    test("ivy2 local is the stable when no maven repository listed one"):
      val listed = List(ListedStable(nine, StablePlace.Local))
      assertTrue(ShipBump.choose(listed).contains(ListedStable(nine, StablePlace.Local)))
    ,
    test("the catalog stays when it is already ahead"):
      val chosen = Some(ListedStable(eight, StablePlace.Remote))
      assertTrue(
        ShipBump.move(nine, chosen, BumpPart.Patch) == ShipMove.Ahead(nine, eight, StablePlace.Remote)
      )
    ,
    test("a caught catalog moves past the published stable, not past a stale catalog number"):
      val behind = ShipNumber.trusted(0, 2, 7)
      val chosen = Some(ListedStable(nine, StablePlace.Remote))
      assertTrue(
        ShipBump.move(nine, chosen, BumpPart.Patch) ==
          ShipMove.Due(nine, StablePlace.Remote, ShipNumber.trusted(0, 2, 10)),
        ShipBump.move(behind, chosen, BumpPart.Patch) ==
          ShipMove.Due(nine, StablePlace.Remote, ShipNumber.trusted(0, 2, 10)),
        ShipBump.move(nine, chosen, BumpPart.Minor) ==
          ShipMove.Due(nine, StablePlace.Remote, ShipNumber.trusted(0, 3, 0)),
        ShipBump.move(nine, chosen, BumpPart.Major) ==
          ShipMove.Due(nine, StablePlace.Remote, ShipNumber.trusted(1, 0, 0)),
        ShipBump.move(nine, None, BumpPart.Patch) == ShipMove.Unpublished,
      )
    ,
    test("next keeps the surrounding whitespace"):
      val source =
        """ships(Ship("histos").next(0, 2, 9)(api, app))
          |Ship("other")
          |  .next(
          |    1,
          |    0,
          |    0
          |  )
          |""".stripMargin
      val rewritten = ShipBump.rewrite(source, ship, ShipNumber.trusted(0, 2, 10))
      assertTrue(
        rewritten == Right(
          """ships(Ship("histos").next(0, 2, 10)(api, app))
            |Ship("other")
            |  .next(
            |    1,
            |    0,
            |    0
            |  )
            |""".stripMargin
        ),
        ShipBump.rewrite("val x = 1\n", ship, nine) == Left(ShipBump.Rewrite.Missing(ship)),
        ShipBump.rewrite("""Ship("histos").released""", ship, nine) == Left(ShipBump.Rewrite.Shape(ship)),
      ),
  )
end ShipBumpLawSpec
