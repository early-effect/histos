package histos.assembly

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Machine, Sha256, Sha256Digest}
import histos.native.NativeOrigin
import histos.resolve.{ResolutionService, Resolve}
import histos.toolchain.RuntimeIdentity
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  CompilerClosure,
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
  private val api = ProjectName.trusted("api")
  private val app = ProjectName.trusted("app")

  def spec = suite("assemble laws")(
    test("a graph without app is refused, and app without api is refused, before a compiler starts"):
      val jdk = JdkRuntime(
        Path.of("jdk"),
        JdkVersion.trusted("21", JdkMajor.trusted(21)),
        RuntimeIdentity.Local(Path.of("jdk"), Sha256Digest.of(Chunk.empty)),
      )
      ZIO.serviceWithZIO[CompilerClosure] { closure =>
        val missing =
          Assemble
            .run(Path.of("missing"), empty, jdk, NativeOrigin.Central, Path.of("out.jar"), _ => ZIO.unit)
            .either
        val only =
          Assemble
            .run(
              Path.of("missing"),
              appOnly(closure.scalaVersion),
              jdk,
              NativeOrigin.Central,
              Path.of("out.jar"),
              _ => ZIO.unit,
            )
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
      }
    ,
    test("app and api compile into one jar, and that jar contains both class files"):
      ZIO.scoped {
        Workspace.work("histos-assemble").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[CompilerClosure] { closure =>
                val scala3      = closure.scalaVersion
                val destination = repo.resolve("dist").resolve("histos.jar")
                val run         =
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
                      .run(
                        repo,
                        graph(scala3, List(ProjectDep(api, None))),
                        jdk,
                        machine.native,
                        destination,
                        _ => ZIO.unit,
                      )
                      .either
                    packed <- ZIO.attemptBlocking(readJar(destination)).orDie
                  yield assertTrue(
                    assembled.map(jar => jar.path == destination && jar.entries > 0) == Right(true),
                    packed.contains("ApiLib.class"),
                    packed.contains("histos/cli/Main.class"),
                    packed.contains(Assemble.ApiEntry),
                  )
                run.provideSomeLayer[Resolve](
                  ResolutionService.pinned(closure.pinning(api, app)) ++
                    ZLayer.succeed(ContentStore.in(repo.resolve("store")))
                )
              }
            }
          }
        }
      }
    ,
    test("an app over an older api packs the classes of api's own cell"):
      ZIO.scoped {
        Workspace.work("histos-assemble-older").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[CompilerClosure] { closure =>
                val newest      = closure.scalaVersion
                val destination = repo.resolve("dist").resolve("histos.jar")
                val run         =
                  for
                    older <- closure.previous
                    _     <- Workspace.writeString(
                      repo.resolve("api").resolve("src").resolve("main").resolve("scala").resolve("Api.scala"),
                      "package histos.cli\nobject ApiLib:\n  def ready: Int = 1\n",
                    )
                    _ <- Workspace.writeString(
                      repo.resolve("app").resolve("src").resolve("main").resolve("scala").resolve("Main.scala"),
                      "package histos.cli\nobject Main:\n  def main(args: Array[String]): Unit = println(ApiLib.ready)\n",
                    )
                    assembled <- Assemble
                      .run(
                        repo,
                        graphAt(older, newest),
                        jdk,
                        machine.native,
                        destination,
                        _ => ZIO.unit,
                      )
                      .either
                      .provideSomeLayer[Resolve](
                        ResolutionService.pinned(closure.pinningAt(api -> older, app -> newest)) ++
                          ZLayer.succeed(ContentStore.in(repo.resolve("store")))
                      )
                    packed <- ZIO.attemptBlocking(readJar(destination)).orDie
                  yield assertTrue(
                    assembled.map(_.path) == Right(destination),
                    packed.contains("histos/cli/ApiLib.class"),
                    packed.contains("histos/cli/Main.class"),
                    packed.contains(Assemble.ApiEntry),
                  )
                run
              }
            }
          }
        }
      },
  ).provideSomeLayerShared[zio.test.TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  /** `api` at `older`, and `app` at `newest` over it. */
  private def graphAt(older: ScalaVersion, newest: ScalaVersion): ProjectGraph =
    val apiModule =
      Module(api, Platforms(Platform.Jvm, Nil), ScalaVersions(older, Nil), Nil, Nil, SourcePos("api.scala", 1, 1))
    ProjectGraph.trusted(
      List(apiModule, appModule(newest, List(ProjectDep(api, None)))),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def empty: ProjectGraph =
    ProjectGraph.trusted(Nil, Splits.aggregate, Catalog.empty, None, Nil, resolution = Resolution.Lock)

  private def appOnly(scala3: ScalaVersion): ProjectGraph =
    ProjectGraph.trusted(
      List(appModule(scala3, Nil)),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def appModule(scala3: ScalaVersion, dependsOn: List[ProjectDep]): Module =
    Module(
      app,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      dependsOn,
      SourcePos("app.scala", 1, 1),
    )

  private def graph(scala3: ScalaVersion, dependsOn: List[ProjectDep]): ProjectGraph =
    val apiModule = Module(
      api,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("api.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(apiModule, appModule(scala3, dependsOn)),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def readJar(path: Path): Set[String] =
    if !Files.isRegularFile(path) then Set.empty
    else
      val zip = new java.util.zip.ZipFile(path.toFile)
      try zip.entries().asScala.map(_.getName).toSet
      finally zip.close()
end AssembleLawSpec
