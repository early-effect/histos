package histos.assembly

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Machine, Sha256}
import histos.native.NativeOrigin
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.toolchain.RuntimeIdentity
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  Module,
  Platform,
  Platforms,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}
import histos.version.Catalog

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import slayer.stubbed

import zio.{Chunk, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** The launcher jar is `app`, the classes it embeds from `api`, and the modules `app` depends on.
  */
object AssembleLawSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val api    = ProjectName.trusted("api")
  private val app    = ProjectName.trusted("app")

  def spec = suite("assemble laws")(
    test("a graph without app is refused, and app without api is refused, before a compiler starts"):
      val jdk = JdkRuntime(
        Path.of("jdk"),
        JdkVersion.trusted("21", JdkMajor.trusted(21)),
        RuntimeIdentity.Local(Path.of("jdk"), Sha256.of(Chunk.empty)),
      )
      val missing =
        Assemble
          .run(Path.of("missing"), empty, jdk, NativeOrigin.Central, Path.of("out.jar"), _ => ZIO.unit)
          .either
      val only =
        Assemble
          .run(Path.of("missing"), appOnly, jdk, NativeOrigin.Central, Path.of("out.jar"), _ => ZIO.unit)
          .either
      missing
        .zip(only)
        .provide(stubbed[Resolve] ++ stubbed[ContentStore] ++ stubbed[ResolutionService])
        .map { (gone, bare) =>
          assertTrue(
            gone == Left(AssembleError.NoModule("app")),
            bare == Left(AssembleError.NoModule("api")),
          )
        }
    ,
    test("app and api compile into one jar, and that jar contains both class files"):
      ZIO.scoped {
        Workspace.work("histos-assemble").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[DependencyLock] { lock =>
                val destination = repo.resolve("dist").resolve("histos.jar")
                val run =
                  for
                    _ <- Workspace
                      .writeString(
                        repo.resolve("api").resolve("src").resolve("main").resolve("scala").resolve("Api.scala"),
                        "object ApiLib:\n  def ready: Int = 1\n",
                      )
                      .catchAll(error => ZIO.dieMessage(error.message))
                    _ <- Workspace
                      .writeString(
                        repo
                          .resolve("app")
                          .resolve("src")
                          .resolve("main")
                          .resolve("scala")
                          .resolve("histos")
                          .resolve("cli")
                          .resolve("Main.scala"),
                        "package histos.cli\nobject Main:\n  def main(args: Array[String]): Unit = ()\n",
                      )
                      .catchAll(error => ZIO.dieMessage(error.message))
                    assembled <- Assemble
                      .run(repo, graph(List(ProjectDep(api, None))), jdk, machine.native, destination, _ => ZIO.unit)
                      .either
                    packed <- ZIO.attemptBlocking(readJar(destination)).orDie
                  yield assertTrue(
                    assembled.map(jar => jar.path == destination && jar.entries > 0) == Right(true),
                    packed.contains("ApiLib.class"),
                    packed.contains("histos/cli/Main.class"),
                    packed.contains(Assemble.ApiEntry),
                  )
                run.provideSomeLayer[Resolve](
                  ResolutionService.pinned(pinned(lock)) ++
                    ZLayer.succeed(ContentStore.in(repo.resolve("store")))
                )
              }
            }
          }
        }
      },
  ).provideSomeLayerShared[zio.test.TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def empty: ProjectGraph =
    ProjectGraph.trusted(Nil, Splits.aggregate, Catalog.empty, None, Nil, resolution = Resolution.Lock)

  private def appOnly: ProjectGraph =
    ProjectGraph.trusted(
      List(appModule(Nil)),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def appModule(dependsOn: List[ProjectDep]): Module =
    Module(
      app,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      dependsOn,
      SourcePos("app.scala", 1, 1),
    )

  private def graph(dependsOn: List[ProjectDep]): ProjectGraph =
    val apiModule = Module(
      api,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("api.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(apiModule, appModule(dependsOn)),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def pinned(lock: DependencyLock): DependencyLock =
    val drop = Set(api, app)
    val kept = lock.cells.filterNot(cell => drop.contains(cell.module))
    val added = List(api, app).map: name =>
      LockedCell(name, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile)
    lock.copy(cells = added ++ kept)

  private def readJar(path: Path): Set[String] =
    if !Files.isRegularFile(path) then Set.empty
    else
      val zip = new java.util.zip.ZipFile(path.toFile)
      try zip.entries().asScala.map(_.getName).toSet
      finally zip.close()

  private def opened =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++
      ZLayer.fromZIO(Resolve.readLock(repository))

  private def repository: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)
end AssembleLawSpec
