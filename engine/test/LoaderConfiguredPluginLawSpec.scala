package histos.load

import histos.cache.ContentStore
import histos.plugin.{
  EntryName,
  HistosPlugin,
  PluginArgs,
  PluginId,
  PluginLoadError,
  PluginOutput,
  PluginTaskId,
  TaskName,
}
import histos.resolve.{DependencyLock, LockedArtifact, ResolutionService, Resolve}
import histos.schedule.{ActionCache, RemoteCache}
import histos.workspace.Workspace

import java.nio.file.Path

import slayer.{any, stub, stubbed}

import zio.{Scope, ZIO, ZLayer}
import zio.test.*

/** `use` hands the loader a plugin value. That value, and not the entry object's `MODULE$`, is what `run` is called on.
  * The jar still names one entry object, and the loader still checks it.
  */
object LoaderConfiguredPluginLawSpec extends ZIOSpecDefault:
  private val cfg      = PluginId.trusted("law.cfg")
  private val stranger = PluginId.trusted("law.stranger")
  private val go       = TaskName.trusted("go")

  private final case class Site(root: Path, api: Path, store: ContentStore)

  def spec =
    (suite("a plugin value passed to use")(
      test("a configured value is the instance that runs"):
        execute("Cfg.configured(\"x\")").map(found => assertTrue(found == Right(PluginOutput("x", Nil))))
      ,
      test("an object plugin with no configuration still runs"):
        execute("Cfg").map(found => assertTrue(found == Right(PluginOutput("object", Nil))))
      ,
      test("a value whose id differs from its entry object is refused"):
        refused("Cfg.stranger").map: found =>
          assertTrue(found == Left(LoadError.Plugin(PluginLoadError.IdMismatch(stranger, cfg))))
      ,
      test("a value whose entry class is outside every plugin jar is refused"):
        refused("Cfg.outside").map: found =>
          assertTrue(
            found == Left(
              LoadError.Plugin(PluginLoadError.ForeignEntry(cfg, EntryName.trusted("java.lang.String")))
            )
          )
      ,
      test("a path plugin whose entry lives under resources still loads and runs"):
        executeResource.map(found => assertTrue(found == Right(PluginOutput("noted", Nil)))),
    ) @@ TestAspect.sequential @@ TestAspect.withLiveSystem).provideSomeLayerShared[TestEnvironment](site)

  private val site: ZLayer[Any, Nothing, Site] =
    ZLayer.scoped:
      for
        root <- Workspace.work("histos-configured-plugin").catchAll(error => ZIO.dieMessage(error.toString))
        api  <- LoaderApiJar.of(classOf[histos.HistosBuild], root.resolve("histos-api.jar")).catchAll(die)
        _    <- Workspace
          .writeString(root.resolve("plugins/cfg/Cfg.scala"), pluginSource)
          .catchAll(failure => ZIO.dieMessage(failure.toString))
        _ <- Workspace
          .writeString(root.resolve(s"plugins/cfg/${HistosPlugin.EntryResourceName}"), "cfg.Cfg$\n")
          .catchAll(failure => ZIO.dieMessage(failure.toString))
      yield Site(root, api, ContentStore.in(root.resolve("store")))

  private def die(error: LoadError): ZIO[Any, Nothing, Nothing] = ZIO.dieMessage(error.message)

  private val loaderLayer: ZLayer[Site, Nothing, Loader] =
    ZLayer.service[Site].flatMap(env => (stubbed[Resolve] ++ ZLayer.succeed(env.get.store)) >>> Loader.live)

  private val runLayer =
    ZLayer
      .service[Site]
      .flatMap: env =>
        ZLayer.succeed(ActionCache.miss) ++ RemoteCache.off ++ stubbed[Resolve] ++ ZLayer.succeed(env.get.store) ++
          stubbed[ResolutionService]

  private def attempt(uses: String): ZIO[Loader & Site & Scope, LoadError, LoadedBuild] =
    for
      env   <- ZIO.service[Site]
      _     <- Workspace.writeString(env.root.resolve("histos.scala"), buildFile(uses)).mapError(LoadError.Io(_))
      found <- ZIO.serviceWithZIO[Loader](_.load(env.root, env.api))
    yield found

  private def refused(uses: String) =
    ZIO.scoped(attempt(uses).unit.either).provideSome[Site](loaderLayer)

  private def execute(uses: String) =
    ZIO
      .scoped:
        for
          _      <- stub[ResolutionService](_.current)(ZIO.succeed(DependencyLock(Nil)))
          _      <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.dieMessage("fetched"))
          found  <- attempt(uses).mapError(Left(_))
          output <- found.plugins.run(PluginTaskId(cfg, go), PluginArgs.empty).mapError(Right(_))
        yield output
      .either
      .provideSome[Site](loaderLayer ++ runLayer)

  /** The entry is under `resources/`, the layout a module publishes from. The path root stays the Scala tree. */
  private def executeResource =
    ZIO
      .scoped:
        for
          env <- ZIO.service[Site]
          _   <- Workspace
            .writeString(env.root.resolve("plugins/res/src/main/scala/Res.scala"), resourceSource)
            .mapError(LoadError.Io(_))
          _ <- Workspace
            .writeString(
              env.root.resolve(s"plugins/res/src/main/resources/${HistosPlugin.EntryResourceName}"),
              "res.Res$\n",
            )
            .mapError(LoadError.Io(_))
          _      <- stub[ResolutionService](_.current)(ZIO.succeed(DependencyLock(Nil)))
          _      <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.dieMessage("fetched"))
          found  <- loadFile(resourceBuild).mapError(Left(_))
          output <- found.plugins.run(PluginTaskId(resId, go), PluginArgs.empty).mapError(Right(_))
        yield output
      .either
      .provideSome[Site](loaderLayer ++ runLayer)

  private val resId = PluginId.trusted("law.res")

  private def loadFile(text: String): ZIO[Loader & Site & Scope, LoadError, LoadedBuild] =
    for
      env   <- ZIO.service[Site]
      _     <- Workspace.writeString(env.root.resolve("histos.scala"), text).mapError(LoadError.Io(_))
      found <- ZIO.serviceWithZIO[Loader](_.load(env.root, env.api))
    yield found

  private def buildFile(uses: String): String =
    s"""plugin(path"plugins/cfg")
       |import histos.*
       |import cfg.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val loaderlaw = module("loaderlaw", Jvm)(scala"3.10.0")
       |    Projects(use($uses), loaderlaw)
       |""".stripMargin

  private val pluginSource: String =
    """package cfg
      |
      |import histos.plugin.*
      |import zio.{Scope, ZIO}
      |
      |object Cfg extends HistosPlugin:
      |  val id: PluginId = PluginId("law.cfg")
      |
      |  def contribute: Contribution =
      |    Contribution(List(DeclaredTask(TaskName("go"), Seal.Hermetic, Set.empty, Nil, Nil, Nil)), Nil, Nil)
      |
      |  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      |    ZIO.succeed(PluginOutput("object", Nil))
      |
      |  def configured(tag: String): HistosPlugin = Configured(tag)
      |  def stranger: HistosPlugin                = Stranger()
      |  def outside: HistosPlugin                 = Outside()
      |end Cfg
      |
      |final class Configured(tag: String) extends HistosPlugin:
      |  def id: PluginId                      = Cfg.id
      |  override def entryClass: Class[?]     = Cfg.getClass
      |  def contribute: Contribution          = Cfg.contribute
      |
      |  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      |    ZIO.succeed(PluginOutput(tag, Nil))
      |
      |final class Stranger extends HistosPlugin:
      |  def id: PluginId                      = PluginId("law.stranger")
      |  override def entryClass: Class[?]     = Cfg.getClass
      |  def contribute: Contribution          = Cfg.contribute
      |
      |  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      |    ZIO.succeed(PluginOutput("stranger", Nil))
      |
      |final class Outside extends HistosPlugin:
      |  def id: PluginId                      = Cfg.id
      |  override def entryClass: Class[?]     = classOf[String]
      |  def contribute: Contribution          = Cfg.contribute
      |
      |  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      |    ZIO.succeed(PluginOutput("outside", Nil))
      |""".stripMargin

  private val resourceBuild =
    """plugin(path"plugins/res/src/main/scala")
      |import histos.*
      |import res.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val loaderlaw = module("loaderlaw", Jvm)(scala"3.10.0")
      |    Projects(use(Res.noted), loaderlaw)
      |""".stripMargin

  private val resourceSource =
    """package res
      |
      |import histos.plugin.*
      |import zio.{Scope, ZIO}
      |
      |object Res extends HistosPlugin:
      |  val id: PluginId = PluginId("law.res")
      |
      |  def contribute: Contribution =
      |    Contribution(List(DeclaredTask(TaskName("go"), Seal.Hermetic, Set.empty, Nil, Nil, Nil)), Nil, Nil)
      |
      |  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      |    ZIO.succeed(PluginOutput("object", Nil))
      |
      |  def noted: HistosPlugin = Noted()
      |end Res
      |
      |final class Noted extends HistosPlugin:
      |  def id: PluginId                  = Res.id
      |  override def entryClass: Class[?] = Res.getClass
      |  def contribute: Contribution      = Res.contribute
      |
      |  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      |    ZIO.succeed(PluginOutput("noted", Nil))
      |""".stripMargin
end LoaderConfiguredPluginLawSpec
