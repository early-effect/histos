package histos.load

import histos.cache.ContentStore
import histos.central.Publisher
import histos.jdk.{Machine, Sha256, Sha256Digest}
import histos.plugin.ApiDigest
import histos.publish.{LawGit, LocalPublish, PublishWrite}
import histos.resolve.{Classpath, DependencyLock, Resolve, ResolveCell, ResolveError, ResolvedArtifact, ToolRequest}
import histos.workspace.{IoFault, Workspace}
import histos.zinc.GraphCompile
import histos.{CompilerClosure, Dep, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import zio.{Chunk, Scope, ZIO, ZLayer}
import zio.test.*

/** The producer publishes the module it wired. The consumer compiles against that jar. The class directory is gone. The
  * ship pin is a stub, not Coursier.
  */
object LoaderPublishLawSpec extends ZIOSpecDefault:
  def spec = suite("a published plugin module is the next build's jar")(
    test("the consumer compiles against the published jar after the class directory is deleted"):
      ZIO.scoped {
        Workspace.work("histos-plugin-publish").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              val maven  = repo.resolve("maven")
              val store  = repo.resolve("store")
              val probe  = repo.resolve("probe").resolve("MarkSpec.scala")
              val probed = repo.resolve("probe-out")
              for
                closure <- ZIO.service[CompilerClosure]
                scala3  = closure.scalaVersion
                classes = GraphCompile.classesDir(repo, ProjectName("plugins"), scala3)
                api    <- LoaderApiJar.of(classOf[histos.HistosBuild], repo.resolve("histos-api.jar"))
                digest <- digestOf(api)
                _ <- Workspace.writeString(repo.resolve("histos.scala"), producer(scala3)).mapError(LoadError.Io(_))
                _ <- Workspace.writeString(repo.resolve("histos.wiring.scala"), wiring).mapError(LoadError.Io(_))
                _ <- Workspace.writeString(
                  repo
                    .resolve("plugins")
                    .resolve("src")
                    .resolve("main")
                    .resolve("scala")
                    .resolve("law")
                    .resolve(
                      "Mark.scala"
                    ),
                  mark,
                )
                _ <- Workspace.writeString(
                  repo
                    .resolve("app")
                    .resolve("src")
                    .resolve("main")
                    .resolve("scala")
                    .resolve("app")
                    .resolve(
                      "Use.scala"
                    ),
                  use,
                )
                produced <- ZIO
                  .serviceWithZIO[Loader](_.load(repo, api))
                  .provideSomeLayer[Resolve & Scope](Loader.live)
                unpublished <- ZIO.attemptBlocking(Files.exists(maven)).orDie
                _           <- Workspace.writeString(
                  repo.resolve("dependencies.lock"),
                  DependencyLock.render(closure.pinning(ProjectName("plugins"), ProjectName("app"))),
                )
                _      <- LawGit.ready(repo)
                output <- LocalPublish
                  .run(repo, produced.graph, Nil, jdk, Map.empty, PublishWrite.Local, digest)
                  .provideSomeLayer[Resolve](Publisher.local(maven) ++ ZLayer.succeed(ContentStore.in(store)))
                line     <- pluginOf(output)
                jar      <- jarOf(line)
                _        <- Workspace.createDirectories(probed)
                _        <- Workspace.writeString(probe, probeSource)
                boundary <- Boundary.jars(api)
                _        <- BuildFile.compileSources(List(probe), classes :: boundary, probed)
                _        <- Workspace.delete(repo.resolve(".histos").resolve("classes")).mapError(LoadError.Io(_))
                gone     <- ZIO.attemptBlocking(Files.exists(classes)).orDie
                stamp    <- zipText(jar, ApiDigest.resource)
                consumer <- Workspace.work("histos-plugin-consume")
                _ <- Workspace.writeString(consumer.resolve("histos.scala"), consumerBuild(scala3, line.revision))
                _ <- Workspace.writeString(consumer.resolve("histos.wiring.scala"), wiring)
                consumed <- ZIO
                  .serviceWithZIO[Loader](_.load(consumer, api))
                  .provideSomeLayer[Scope](ZLayer.succeed(JarResolve(jar)) >>> Loader.live)
                pinnedDep = consumed.graph.coordinates.collect:
                  case histos.plugin.PluginCoordinate.Published(dependency, _) => dependency
              yield assertTrue(
                produced.plugins.links.isEmpty,
                !unpublished,
                line.module == ProjectName("plugins"),
                !gone,
                stamp.contains(s"${digest.value}\n"),
                pinnedDep.map(dep => (dep.organization, dep.name, dep.revision.value)) == List(
                  ("example.com", "plugins", line.revision)
                ),
                consumed.plugins.links.isEmpty,
              )
            }
          }
        }
      }
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private def producer(scala3: ScalaVersion): String =
    s"""import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val plugins = module("plugins", Jvm)(scala"${scala3.value}")
       |    val app = module("app", Jvm)(scala"${scala3.value}").dependsOn(plugins)
       |    Projects
       |      .organization(organization"example.com")
       |      .plugins(PluginSources.module(plugins))
       |      .locked(ships(Ship("demo").next(1, 2, 3)(plugins, app)), plugins, app)
       |""".stripMargin

  private val mark =
    """package law
      |
      |object Mark:
      |  val token: String = "mark"
      |""".stripMargin

  private val use =
    """package app
      |
      |object Use:
      |  def ready: Int = law.Mark.token.length
      |""".stripMargin

  private val wiring =
    """import histos.plugin.*
      |import zio.*
      |
      |object HistosWiring:
      |  val token: String = law.Mark.token
      |  def layer = ZLayer.succeed(Plugins(Chunk.empty))
      |""".stripMargin

  private val probeSource =
    """package law
      |
      |object MarkSpec:
      |  val seen: String = Mark.token
      |""".stripMargin

  private def consumerBuild(scala3: ScalaVersion, revision: String): String =
    s"""import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"${scala3.value}")
       |    Projects.plugins(PluginSources.ship(dep"example.com::plugins:$revision"))(app)
       |""".stripMargin

  private def digestOf(api: Path) =
    ZIO
      .attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(api))))
      .mapError(BuildFile.failed(api, IoFault.Read))
      .flatMap: sha =>
        ApiDigest.from(sha.value) match
          case Right(digest) => ZIO.succeed(digest)
          case Left(_)       => ZIO.fail(LoadError.Io(histos.workspace.IoFailure(api, IoFault.Stat)))

  private def pluginOf(output: histos.publish.PublishOutput) =
    output.modules.find(_.module == ProjectName("plugins")) match
      case Some(line) => ZIO.succeed(line)
      case None       => ZIO.dieMessage("plugins was not published")

  private def jarOf(line: histos.publish.PublishedModule) =
    line.jar match
      case Some(path) => ZIO.succeed(path)
      case None       => ZIO.dieMessage("plugins published no jar")

  private def zipText(path: Path, name: String) =
    ZIO
      .attemptBlocking:
        val zip = new ZipFile(path.toFile)
        try
          zip.getEntry(name) match
            case null  => None
            case entry =>
              val stream = zip.getInputStream(entry)
              try Some(new String(stream.readAllBytes(), StandardCharsets.UTF_8))
              finally stream.close()
        finally zip.close()
      .orDie

  /** The consumer's ship pin. `resolvePlugin` returns the published jar and nothing else. */
  private final class JarResolve(jar: Path) extends Resolve:
    def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
      ZIO
        .succeed(repositories)
        .zipRight(
          ZIO.succeed(
            Classpath(
              List(
                ResolvedArtifact(
                  dependency.organization,
                  dependency.name,
                  dependency.revision.value,
                  jar,
                  Sha256.trusted("aa"),
                  histos.resolve.Repository.Central,
                )
              ),
              Nil,
            )
          )
        )

    def resolveTool(request: ToolRequest) =
      ZIO.fail(ResolveError.NotFound("unused", request.id.toString, "0"))

    def resolve(cell: ResolveCell) =
      ZIO.fail(ResolveError.NotFound("unused", cell.module.value, "0"))

    def fetch(artifact: histos.resolve.LockedArtifact) =
      ZIO.fail(ResolveError.NotFound("unused", artifact.name, artifact.revision))

    def sources(organization: String, name: String, revision: String, repository: histos.resolve.Repository) =
      ZIO.fail(ResolveError.NotFound(organization, name, revision))
  end JarResolve
end LoaderPublishLawSpec
