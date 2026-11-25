package histos.publish

import histos.suite.HistosSpec
import histos.version.{Catalog, Ship, ShipName, ShipNumber}
import histos.{
  Dep,
  DepKind,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import zio.test.*

object PublishSelectSpec extends HistosSpec:
  private val plugin = ProjectName("plugin")
  private val ship   = ShipName.trusted("plugin")

  def spec = suite("publish selection")(
    test("a compile-only snapshot does not refuse the ship") {
      val picked = select(_.compileOnly)
      assertTrue(picked.exists(row => row.refused.isEmpty && row.roots.contains(plugin)))
    },
    test("a pom snapshot dependency refuses the ship") {
      val published = select(identity)
      val provided  = select(_.provided)
      assertTrue(
        published.exists(_.refused.exists(row => row.ship == ship && row.dependency.contains("1.0.0-SNAPSHOT"))),
        provided.exists(_.refused.exists(_.ship == ship)),
      )
    },
  )

  private def select(link: Dep => Dep): Either[LocalPublishError, Picked] =
    val host  = link(Dep.trusted("com.example", "host", "1.0.0-SNAPSHOT", DepKind.Java))
    val graph = ProjectGraph.trusted(
      List(
        Module(
          plugin,
          Platforms(Platform.Jvm, Nil),
          ScalaVersions(ScalaVersion("3.10.1"), Nil),
          List(host),
          Nil,
          SourcePos("histos.scala", 1, 1),
        )
      ),
      Splits.aggregate,
      Catalog.Lines(
        ::(Ship.Line(ship, ShipNumber.trusted(0, 0, 1), ::(plugin, Nil)), Nil)
      ),
    )
    PublishSelect.pick(graph, List(PublishName.Ship(ship)), List(ShipWrite(ship, List(plugin), snapshot = true)))
end PublishSelectSpec
