package histos.load

import histos.*
import histos.jdk.{Machine, Sha256Digest}
import histos.plugin.PluginCoordinate
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
import histos.suite.HistosSpec
import histos.workspace.Workspace
import histos.zinc.{GraphCompile, ScalacClasspath}

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object ApiPinSpec extends HistosSpec:
  private val version = scala"3.10.1"
  private val apiDep  = dep"rocks.earlyeffect::histos-api:0.5.0-SNAPSHOT".compileOnly

  def spec = suite("a plugin compiles against this process's API")(
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
                pinned <- ScalacClasspath.substituted(repo, item).catchAll(error => ZIO.dieMessage(error.message))
              yield assertTrue(
                compiled.isRight,
                classpath.permitted == List(host),
                classpath.paths.contains(host),
                pinned == List(host),
              )
        }
        .provide(Resolve.coursier, store, RemoteCache.off)
    },
    test("a declared histos-api coordinate is the host jar") {
      ZIO
        .scoped {
          Workspace
            .work("api-pin-declared")
            .flatMap: repo =>
              val jar     = repo.resolve("histos-api_3.jar")
              val defined = module("plug", Jvm)(version).libraryDependencies(apiDep).pluginLibrary
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
                pinned <- ScalacClasspath.substituted(repo, item).catchAll(error => ZIO.dieMessage(error.message))
              yield assertTrue(
                classpath.permitted == List(host),
                classpath.paths.contains(host),
                !classpath.paths.contains(jar),
                pinned == List(host),
              )
        }
        .provide(Resolve.coursier)
    },
    test("a module named as a plugin compiles against the host jar") {
      ZIO
        .scoped {
          Workspace
            .work("api-pin-named")
            .flatMap: repo =>
              val defined = module("plug", Jvm)(version)
              val named   = Set(ProjectName("plug"))
              for
                _         <- hostFacts
                graph     <- built(defined)
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
                    named,
                  )
                  .provideSomeLayer(ZLayer.succeed(tools))
                  .catchAll(error => ZIO.dieMessage(error.message))
                pinned <- ScalacClasspath
                  .substituted(repo, item, named)
                  .catchAll(error => ZIO.dieMessage(error.message))
              yield assertTrue(
                item.pluginLibrary.isEmpty,
                classpath.permitted == List(host),
                classpath.paths.contains(host),
                pinned == List(host),
              )
        }
        .provide(Resolve.coursier)
    },
    test("plugin load keeps the boundary jar when histos-api is declared") {
      ZIO
        .scoped {
          Workspace
            .work("api-pin-load")
            .flatMap: repo =>
              val host    = ScalaVersion.trusted(_root_.scala.util.Properties.versionNumberString)
              val defined = module("plug", Jvm)(host).libraryDependencies(apiDep).pluginLibrary
              val source  =
                repo
                  .resolve("plug")
                  .resolve("src")
                  .resolve("main")
                  .resolve("scala")
                  .resolve("demo")
                  .resolve("Mark.scala")
              val jar = repo.resolve("histos-api_3.jar")
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
                _        <- hostFacts
                _        <- write(source, pinSource)
                _        <- writeBytes(jar, Array[Byte](1, 2, 3))
                asked    <- Ref.make(0)
                inner    <- ZIO.service[Resolve]
                api      <- ScalacClasspath.hostApi(repo).catchAll(error => ZIO.dieMessage(error.message))
                boundary <- Boundary.jars(api).catchAll(error => ZIO.dieMessage(error.message))
                exit     <- PluginModules
                  .compile(repo, named, boundary, Keeping(inner, jar, apiDep, asked))
                  .either
              yield assertTrue(
                exit.isRight,
                exit.exists(paths => !paths.contains(jar)),
              )
        }
        .provide(Resolve.coursier)
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
