package histos.load

import histos.*
import histos.jdk.{Machine, Sha256Digest}
import histos.plugin.{PluginCoordinate, PluginLoadError}
import histos.resolve.{
  BuildResolvers,
  Classpath,
  ClasspathScope,
  DependencyLock,
  LockedTools,
  Repositories,
  Repository,
  ResolutionService,
  Resolve,
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
  ToolRequest,
}
import histos.schedule.RemoteCache
import histos.cache.ContentStore
import histos.workspace.Workspace
import histos.zinc.{GraphCompile, GraphCompileError, ScalacClasspath}

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object ApiPinSpec extends ZIOSpecDefault:
  private val version  = scala"3.10.1"
  private val sentence =
    "plugin plug depends on histos-api. A plugin tracks the toolchain pin and does not declare that dependency."
  private val apiDep = dep"rocks.earlyeffect::histos-api:0.5.0-SNAPSHOT".compileOnly

  def spec = suite("a plugin tracks the toolchain pin")(
    test("a plugin library with no histos-api dependency compiles against the pin") {
      ZIO
        .scoped {
          Workspace
            .work("api-pin-compile")
            .flatMap: repo =>
              val source =
                repo
                  .resolve("plug")
                  .resolve("src")
                  .resolve("main")
                  .resolve("scala")
                  .resolve("demo")
                  .resolve("Mark.scala")
              val defined = module("plug", Jvm)(version).pluginLibrary
              for
                _        <- hostFacts
                _        <- write(source, pinSource)
                graph    <- built(defined)
                machine  <- Machine.load(repo).catchAll(error => ZIO.dieMessage(error.message))
                jdk      <- GraphCompile.jdk(machine).catchAll(error => ZIO.dieMessage(error.message))
                compiled <- GraphCompile
                  .run(repo, graph, List(ProjectName("plug")), jdk, machine.native, Some(Platform.Jvm), _ => ZIO.unit)
                  .either
                item      <- only(graph)
                opened    <- ResolutionService.open(repo, graph).catchAll(error => ZIO.dieMessage(error.message))
                lock      <- opened.current
                tools     <- LockedTools.make(lock, graph, opened)
                host      <- ScalacClasspath.hostApi(repo).catchAll(error => ZIO.dieMessage(error.message))
                classpath <- ScalacClasspath
                  .of(
                    repo,
                    item,
                    Nil,
                    version,
                    Platform.Jvm,
                    lock,
                    BuildResolvers(graph.resolvers),
                    ClasspathScope.Compile,
                  )
                  .provideSomeLayer(ZLayer.succeed(tools))
                  .catchAll(error => ZIO.dieMessage(error.message))
              yield assertTrue(
                compiled.isRight,
                classpath.permitted == List(host),
                classpath.paths.contains(host),
              )
        }
        .provide(Resolve.coursier, store, RemoteCache.off)
    },
    test("a plugin library that declares histos-api fails before resolve") {
      val defined = module("plug", Jvm)(version).libraryDependencies(apiDep).pluginLibrary
      for
        graph <- built(defined)
        item  <- only(graph)
        asked <- Ref.make(false)
        resolve = Refusing(asked)
        tools   = LockedTools(DependencyLock(Nil), graph.tools, BuildResolvers(graph.resolvers), resolve)
        exit <- ScalacClasspath
          .of(
            Path.of("repo"),
            item,
            Nil,
            version,
            Platform.Jvm,
            DependencyLock(Nil),
            BuildResolvers(graph.resolvers),
            ClasspathScope.Compile,
          )
          .provide(ZLayer.succeed(tools), ZLayer.succeed(resolve))
          .either
        called <- asked.get
      yield assertTrue(
        exit == Left(GraphCompileError.ApiDependency(ProjectName("plug"))),
        exit.fold(_.message == sentence, _ => false),
        !called,
      )
    },
    test("plugin load refuses a direct histos-api dependency before resolve") {
      ZIO.scoped {
        Workspace
          .work("api-pin-load")
          .flatMap: repo =>
            val host    = ScalaVersion.trusted(_root_.scala.util.Properties.versionNumberString)
            val defined = module("plug", Jvm)(host).libraryDependencies(apiDep).pluginLibrary
            val source  =
              repo.resolve("plug").resolve("src").resolve("main").resolve("scala").resolve("demo").resolve("Mark.scala")
            for
              graph <- built(defined)
              named = ProjectGraph.trusted(
                graph.projects,
                graph.splits,
                graph.catalog,
                graph.organization,
                graph.resolvers,
                coordinates = List(PluginCoordinate.Module(ProjectName("plug"))),
              )
              _      <- write(source, "package demo\n\nobject Mark\n")
              asked  <- Ref.make(false)
              exit   <- PluginModules.compile(repo, named, Nil, Refusing(asked)).either
              called <- asked.get
            yield assertTrue(
              exit == Left(LoadError.Plugin(PluginLoadError.ApiDependency(ProjectName("plug")))),
              exit.fold(_.message == sentence, _ => false),
              !called,
            )
      }
    },
    test("a module that declares histos-api keeps the resolved jar") {
      ZIO
        .scoped {
          Workspace
            .work("api-pin-keep")
            .flatMap: repo =>
              val jar     = repo.resolve("histos-api_3.jar")
              val defined = module("lib", Jvm)(version).libraryDependencies(apiDep)
              for
                _     <- hostFacts
                _     <- writeBytes(jar, Array[Byte](1, 2, 3))
                graph <- built(defined)
                item  <- only(graph)
                asked <- Ref.make(0)
                inner <- ZIO.service[Resolve]
                wrapped = Keeping(inner, jar, apiDep, asked)
                opened <- ResolutionService
                  .open(repo, Resolution.Solve)
                  .provide(ZLayer.succeed(wrapped))
                  .catchAll(error => ZIO.dieMessage(error.message))
                lock <- opened.current
                tools = LockedTools(lock, graph.tools, BuildResolvers(graph.resolvers), wrapped, Some(opened))
                host      <- ScalacClasspath.hostApi(repo).catchAll(error => ZIO.dieMessage(error.message))
                classpath <- ScalacClasspath
                  .of(
                    repo,
                    item,
                    Nil,
                    version,
                    Platform.Jvm,
                    lock,
                    BuildResolvers(graph.resolvers),
                    ClasspathScope.Compile,
                  )
                  .provideSomeLayer(ZLayer.succeed(tools))
                hits <- asked.get
              yield assertTrue(
                classpath.paths.contains(jar),
                !classpath.paths.contains(host),
                hits == 1,
              )
        }
        .provide(Resolve.coursier)
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(8.minutes)

  private val pinSource =
    """package demo
      |
      |object Mark:
      |  def token: Class[?] = classOf[histos.plugin.Guard]
      |""".stripMargin

  private val store: ZLayer[Any, Nothing, ContentStore] =
    ZLayer.fromZIO(
      ZIO.succeed(ContentStore.in(Path.of(java.lang.System.getProperty("java.io.tmpdir"), "api-pin-store")))
    )

  private def built(defined: ProjectDef): UIO[ProjectGraph] =
    ZIO.fromEither(Projects(defined)).catchAll(error => ZIO.dieMessage(error.message))

  private def only(graph: ProjectGraph): UIO[Module] =
    graph.projects.collect { case item: Module => item } match
      case item :: Nil => ZIO.succeed(item)
      case _           => ZIO.dieMessage("expected one module")

  private def hostFacts: UIO[Unit] =
    ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")): key =>
      Option(java.lang.System.getProperty(key)) match
        case Some(value) if value.nonEmpty => TestSystem.putProperty(key, value)
        case _                             => ZIO.dieMessage(s"the test JVM has no $key")

  private def write(path: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(dir => Files.createDirectories(dir))
      Files.writeString(path, text)
      ()
    }.orDie

  private def writeBytes(path: Path, bytes: Array[Byte]): UIO[Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(dir => Files.createDirectories(dir))
      Files.write(path, bytes)
      ()
    }.orDie

  private final class Refusing(asked: Ref[Boolean]) extends Resolve:
    def resolveTool(request: ToolRequest): zio.IO[ResolveError, Classpath]                          = refuse
    def resolve(cell: ResolveCell): zio.IO[ResolveError, Classpath]                                 = refuse
    def fetch(artifact: histos.resolve.LockedArtifact): zio.IO[ResolveError, ResolvedArtifact]      = refuse
    def resolvePlugin(dependency: Dep, repositories: Repositories): zio.IO[ResolveError, Classpath] = refuse
    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository,
    ): zio.IO[ResolveError, ResolvedArtifact] = refuse

    private def refuse: UIO[Nothing] =
      asked.set(true) *> ZIO.dieMessage("resolve")
  end Refusing

  private final class Keeping(inner: Resolve, jar: Path, dependency: Dep, asked: Ref[Int]) extends Resolve:
    def resolveTool(request: ToolRequest): zio.IO[ResolveError, Classpath] =
      inner.resolveTool(request)

    def resolve(cell: ResolveCell): zio.IO[ResolveError, Classpath] =
      if cell.dependencies.exists(_.name == "histos-api") then
        asked.update(_ + 1) *> ZIO.succeed(Classpath(List(resolved), Nil))
      else inner.resolve(cell)

    def fetch(artifact: histos.resolve.LockedArtifact): zio.IO[ResolveError, ResolvedArtifact] =
      inner.fetch(artifact)

    def resolvePlugin(dependency: Dep, repositories: Repositories): zio.IO[ResolveError, Classpath] =
      inner.resolvePlugin(dependency, repositories)

    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository,
    ): zio.IO[ResolveError, ResolvedArtifact] =
      inner.sources(organization, name, revision, repository)

    private def resolved: ResolvedArtifact =
      ResolvedArtifact(
        dependency.organization,
        histos.resolve.ArtifactName.of(dependency, version, Platform.Jvm),
        dependency.revision.value,
        jar,
        Sha256Digest.of(Chunk.fromArray(Array[Byte](1, 2, 3))),
        Repository.Central,
      )
  end Keeping
end ApiPinSpec
