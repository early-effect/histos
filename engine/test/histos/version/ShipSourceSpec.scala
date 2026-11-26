package histos.version

import histos.suite.HistosSpec

import zio.test.*

object ShipSourceSpec extends HistosSpec:
  def spec = suite("ship source")(
    test("a val, an alias, a block, and parentheses keep a multiline call and its comment") {
      val rewritten = ShipSource.rewrite(
        source,
        List(
          ShipName("histos") -> ShipNumber("10.6.2"),
          ShipName("other")  -> ShipNumber("9.0.0"),
          ShipName("alias")  -> ShipNumber("2.3.0"),
          ShipName("block")  -> ShipNumber("3.0.4"),
          ShipName("paren")  -> ShipNumber("4.5.6"),
        ),
      )
      assertTrue(rewritten == Right(expected))
    },
    test("a ship with no .next call is missing") {
      val text = scala(
        """object build:
          |  def graph =
          |    val ship = Ship("histos")
          |    val n = 1
          |"""
      )
      assertTrue(
        ShipSource.rewrite(text, List(ShipName("histos") -> ShipNumber("0.6.2"))) == Left(
          ShipBump.Rewrite.Missing(ShipName("histos"))
        )
      )
    },
    test("a .next argument that is not an integer literal is refused") {
      val text = scala(
        """object build:
          |  def graph =
          |    val ship = Ship("histos")
          |    ship.next(major, 2, 3)
          |"""
      )
      assertTrue(
        ShipSource.rewrite(text, List(ShipName("histos") -> ShipNumber("0.6.2"))) == Left(
          ShipBump.Rewrite.Shape(ShipName("histos"))
        )
      )
    },
  )

  private def scala(text: String): String = text.stripMargin

  private val source = scala(
    """object build:
      |  def graph =
      |    // ships
      |    val ship = Ship("histos")
      |    val library =
      |      ship.next(
      |        0, // major
      |        6,
      |        1,
      |      )(histos)
      |    val direct = Ship("other").next(1, 0, 0)(core)
      |    val origin = Ship("alias")
      |    val again = origin
      |    val aliased = again.next(2, 0, 0)(core)
      |    val blocked = { val inner = Ship("block"); inner }.next(3, 0, 0)(core)
      |    val wrapped = (Ship("paren")).next(4, 0, 0)(core)
      |"""
  )

  private val expected = scala(
    """object build:
      |  def graph =
      |    // ships
      |    val ship = Ship("histos")
      |    val library =
      |      ship.next(
      |        10, // major
      |        6,
      |        2,
      |      )(histos)
      |    val direct = Ship("other").next(9, 0, 0)(core)
      |    val origin = Ship("alias")
      |    val again = origin
      |    val aliased = again.next(2, 3, 0)(core)
      |    val blocked = { val inner = Ship("block"); inner }.next(3, 0, 4)(core)
      |    val wrapped = (Ship("paren")).next(4, 5, 6)(core)
      |"""
  )
end ShipSourceSpec
