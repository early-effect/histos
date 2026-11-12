package histos.load

import histos.cache.ContentStore
import histos.plugin.{PluginCoordinate, PluginLoadError}
import histos.resolve.Resolve
import histos.workspace.Workspace
import histos.ProjectName

import slayer.stubbed

import zio.{Scope, ZIO, ZLayer}
import zio.test.*

object LoaderPluginLawSpec extends ZIOSpecDefault:
  def spec = suite("plugin modules compile before the wiring")(
    test("a plugin module and the module it depends on are on the wiring classpath"):
      load(coordinated, List(coreSource, pluginSource, otherSource), Some(wiringSeesPlugin)).map: found =>
        val named = found.graph.coordinates.collect:
          case PluginCoordinate.Module(name) => name.value
        assertTrue(named == List("plugins"), found.plugins.links.map(_.id.value) == List("law.mark"))
    ,
    test("a bundle builds every plugin val in the module"):
      load(coordinated, List(coreSource, pluginSource, otherSource), Some(wiringBundle)).map: found =>
        assertTrue(found.plugins.links.map(_.id.value) == List("law.mark", "law.other"))
    ,
    test("a module that is not a coordinate stays off the wiring classpath"):
      load(plain, List(coreSource, pluginSource), Some(wiringSeesPlugin)).either.map: found =>
        val refused = found match
          case Left(LoadError.CompileFailed(diagnostics)) => diagnostics.nonEmpty
          case _                                          => false
        assertTrue(refused)
    ,
    test("a plugin module that does not compile fails the load"):
      load(coordinated, List(coreSource, pluginBroken), None).either.map: found =>
        val refused = found match
          case Left(LoadError.CompileFailed(diagnostics)) =>
            diagnostics.exists(message => message.text.contains("String"))
          case _ => false
        assertTrue(refused)
    ,
    test("a plugin that still needs a service fails the wiring compile"):
      load(coordinated, List(coreSource, needsSource), Some(wiringNeeds)).either.map: found =>
        val texts = found match
          case Left(LoadError.CompileFailed(diagnostics)) => diagnostics.map(_.text).mkString("\n")
          case other                                      => other.toString
        assertTrue(texts.contains("Home"))
    ,
    test("a wiring that provides the service builds the plugin"):
      load(coordinated, List(coreSource, needsSource), Some(wiringProvides)).map: found =>
        assertTrue(found.plugins.links.map(_.id.value) == List("law.needs"))
    ,
    test("the host api stays the boundary jar, so a broken copy is not compiled"):
      load(hostBuild, List(hostSource, hostPlugin), Some(wiringSeesPlugin)).map: found =>
        assertTrue(found.plugins.links.map(_.id.value) == List("law.mark"))
    ,
    test("a plugin module with no sources fails the load"):
      load(coordinated, Nil, None).either.map: found =>
        val missing = found match
          case Left(LoadError.Plugin(PluginLoadError.NoSource(PluginCoordinate.Module(name)))) =>
            name == ProjectName("plugins")
          case _ => false
        assertTrue(missing),
  )

  private val coordinated =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val core = module("core", Jvm)(scala"3.10.0")
      |    val plugins = module("plugins", Jvm)(scala"3.10.0").dependsOn(core)
      |    val app = module("app", Jvm)(scala"3.10.0")
      |    Projects.plugins(PluginSources.module(plugins))(core, plugins, app)
      |""".stripMargin

  private val hostBuild =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val api = module("api", Jvm)(scala"3.10.0")
      |    val plugins = module("plugins", Jvm)(scala"3.10.0").dependsOn(api)
      |    Projects.plugins(PluginSources.module(plugins))(api, plugins)
      |""".stripMargin

  private val hostSource =
    "api/src/main/scala/histos/Host.scala" ->
      """package histos
        |
        |trait HistosBuild
        |object Broken:
        |  val no: Int = "no"
        |""".stripMargin

  private val hostPlugin =
    "plugins/src/main/scala/law/Mark.scala" ->
      """package law
        |
        |import histos.plugin.*
        |import zio.*
        |
        |object Mark extends HistosPlugin:
        |  def id: PluginId = PluginId("law.mark")
        |  def contribute: Contribution = Contribution(Nil, Nil, Nil)
        |  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
        |    ZIO.succeed(PluginOutput("mark", Nil))
        |""".stripMargin

  private val plain =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val core = module("core", Jvm)(scala"3.10.0")
      |    val plugins = module("plugins", Jvm)(scala"3.10.0").dependsOn(core)
      |    val app = module("app", Jvm)(scala"3.10.0")
      |    Projects(core, plugins, app)
      |""".stripMargin

  private val coreSource =
    "core/src/main/scala/core/Token.scala" ->
      """package core
        |
        |object Token:
        |  val value: String = "core"
        |""".stripMargin

  private val pluginSource =
    "plugins/src/main/scala/law/Mark.scala" ->
      """package law
        |
        |import histos.plugin.*
        |import zio.*
        |
        |object Mark extends HistosPlugin:
        |  def id: PluginId = PluginId("law.mark")
        |  def contribute: Contribution = Contribution(Nil, Nil, Nil)
        |  val token: String = core.Token.value
        |  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
        |    ZIO.succeed(PluginOutput(token, Nil))
        |""".stripMargin

  private val otherSource =
    "plugins/src/main/scala/law/Other.scala" ->
      """package law
        |
        |import histos.plugin.*
        |import zio.*
        |
        |object Other extends HistosPlugin:
        |  def id: PluginId = PluginId("law.other")
        |  def contribute: Contribution = Contribution(Nil, Nil, Nil)
        |  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
        |    ZIO.succeed(PluginOutput("other", Nil))
        |""".stripMargin

  private val needsSource =
    "plugins/src/main/scala/law/Needs.scala" ->
      """package law
        |
        |import histos.plugin.*
        |import zio.*
        |
        |trait Home
        |
        |object Needs extends HistosPlugin:
        |  def id: PluginId = PluginId("law.needs")
        |  def contribute: Contribution = Contribution(Nil, Nil, Nil)
        |  def layer = pluginWith[Needs.type, Home](_ => Needs)
        |  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
        |    ZIO.succeed(PluginOutput("needs", Nil))
        |""".stripMargin

  private val pluginBroken =
    "plugins/src/main/scala/law/Mark.scala" ->
      """package law
        |
        |object Mark:
        |  val token: Int = "no"
        |""".stripMargin

  private val wiringSeesPlugin =
    """import histos.plugin.*
      |
      |object HistosWiring:
      |  def layer = wire[law.Mark.type]
      |""".stripMargin

  private val wiringBundle =
    """import histos.plugin.*
      |
      |object OurPlugins derives Bundle:
      |  val mark  = law.Mark
      |  val other = law.Other
      |
      |object HistosWiring:
      |  def layer = OurPlugins.layer
      |""".stripMargin

  private val wiringNeeds =
    """import histos.plugin.*
      |
      |object HistosWiring:
      |  def layer = wire[law.Needs.type]
      |""".stripMargin

  private val wiringProvides =
    """import histos.plugin.*
      |import zio.*
      |
      |object HistosWiring:
      |  def layer = ZLayer.succeed[law.Home](new law.Home {}) >>> wire[law.Needs.type]
      |""".stripMargin

  private def load(
      build: String,
      files: List[(String, String)],
      wiring: Option[String],
  ) =
    Workspace
      .work("histos-load-plugin")
      .mapError(LoadError.Io(_))
      .flatMap: root =>
        val layer =
          (stubbed[Resolve] ++ ZLayer.succeed(ContentStore.in(root.resolve("store")))) >>> Loader.live
        val run =
          for
            api <- LoaderApiJar.of(classOf[histos.HistosBuild], root.resolve("histos-api.jar"))
            _   <- Workspace.writeString(root.resolve("histos.scala"), build).mapError(LoadError.Io(_))
            _   <- ZIO.foreachDiscard(files):
              case (path, text) =>
                Workspace.writeString(root.resolve(path), text).mapError(LoadError.Io(_))
            _ <- wiring match
              case None       => ZIO.unit
              case Some(text) =>
                Workspace.writeString(root.resolve("histos.wiring.scala"), text).mapError(LoadError.Io(_))
            found <- ZIO.serviceWithZIO[Loader](_.load(root, api))
          yield found
        run.provideSomeLayer[Scope](layer)
end LoaderPluginLawSpec
