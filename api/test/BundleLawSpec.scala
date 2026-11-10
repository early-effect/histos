package histos.plugin

import _root_.scala.compiletime.testing.typeCheckErrors

import zio.{Scope, ZIO, ZLayer}
import zio.test.*

trait Home:
  def name: String

final class HomeLive(val name: String) extends Home

trait Named extends HistosPlugin:
  def id: PluginId = PluginId("test.named")

  def contribute: Contribution = Contribution(Nil, Nil, Nil)

  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput("named", Nil))
end Named

object Named extends Named

trait NeedsSite extends HistosPlugin:
  def label: String

  def id: PluginId = PluginId("test.needs")

  def contribute: Contribution = Contribution(Nil, Nil, Nil)

  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput(label, Nil))
end NeedsSite

object NeedsSite:
  def layer: ZLayer[Home, Nothing, NeedsSite] =
    pluginWith[NeedsSite, Home](site => Live(site.name))

  private final class Live(val label: String) extends NeedsSite
end NeedsSite

object Pair derives Bundle:
  val named: Named.type     = Named
  val needs: NeedsSite.type = NeedsSite

trait NoLayer extends HistosPlugin:
  def id: PluginId = PluginId("test.none")

  def contribute: Contribution = Contribution(Nil, Nil, Nil)

  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput("none", Nil))
end NoLayer

object BundleLawSpec extends ZIOSpecDefault:
  def spec = suite("plugins are layers")(
    test("a bundle provides each service and Plugins"):
      val program =
        for
          plugins <- ZIO.service[Plugins]
          named   <- ZIO.service[Named.type]
          needs   <- ZIO.service[NeedsSite]
        yield assertTrue(
          plugins.values.map(_.id.value).toList == List("test.named", "test.needs"),
          named.id.value == "test.named",
          needs.label == "home",
        )
      program.provide(Pair.layer, ZLayer.succeed[Home](HomeLive("home")))
    ,
    test("wire selects the types it names"):
      val program =
        for
          plugins <- ZIO.service[Plugins]
          needs   <- ZIO.service[NeedsSite]
        yield assertTrue(
          plugins.values.map(_.id.value).toList == List("test.needs"),
          needs.label == "home",
        )
      program.provide(wire[NeedsSite], ZLayer.succeed[Home](HomeLive("home")))
    ,
    test("a trait with no layer is not a wire"):
      val errors = typeCheckErrors("histos.plugin.wire[histos.plugin.NoLayer]")
      assertTrue(errors.nonEmpty)
    ,
    test("a class is not a bundle"):
      val errors = typeCheckErrors("import histos.plugin.*\nclass Box derives Bundle\n")
      assertTrue(errors.nonEmpty)
    ,
    test("a bundle val that is not a plugin is refused"):
      val errors = typeCheckErrors(
        "import histos.plugin.*\nobject Box derives Bundle:\n  val n = 1\nval layer = Box.layer\n"
      )
      assertTrue(errors.nonEmpty),
  )
