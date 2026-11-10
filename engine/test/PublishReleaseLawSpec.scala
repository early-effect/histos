package histos.publish

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.plugin.ApiDigest
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, Resolve}
import histos.version.VersionTask
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  Developer,
  FollowUp,
  HistosBuild,
  Jvm,
  License,
  Projects,
  PublishFault,
  PublishReceipt,
  PublishRequest,
  Publishing,
  Scm,
  Ship,
  connection,
  developer,
  license,
  module,
  organization,
  pom,
  ships,
  url,
}
import histos.`scala`

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A release classifies the build's receipt. Credentials fail before that, and a ship with no asset does not upload. */
object PublishReleaseLawSpec extends ZIOSpecDefault:
  private val hostApi = ApiDigest.trusted("ab" * 32)
  private val scala3  = scala"3.10.0"
  private val group   = organization"example.com"
  private val named   = module("releaselaw", Jvm)(scala3)

  def spec = suite("publish release laws")(
    test("snapshots and promote name the missing credentials, and assets with nothing to upload stay empty"):
      Workspace
        .work("histos-release-gate")
        .flatMap: dir =>
          val program =
            for
              graph <- ZIO
                .fromEither(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(named)), named))
                .catchAll(error => ZIO.dieMessage(error.message))
              loaded  <- Machine.load(dir).either
              machine <- loaded match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value.copy(env = Map.empty))
              found <- GraphCompile.jdk(machine).either
              jdk   <- found match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              snapped <- PublishRun
                .snapshots(
                  dir,
                  graph,
                  HistosBuild.fixed(graph),
                  Nil,
                  jdk,
                  Map.empty,
                  hostApi,
                  _ => ZIO.unit,
                  _ => ZIO.unit,
                )
                .either
              promoted <- PublishRun.promote(dir, Map.empty, "nope", _ => ZIO.unit).either
              assets   <- PublishRun.assets(dir, graph, Nil, Map.empty).either
            yield assertTrue(
              snapped match
                case Left(error) => error.message == LocalPublishError.Credentials.message
                case Right(_)    => false
              ,
              promoted == Left(ReleaseError.Credentials),
              assets match
                case Right(report) => report.failed.isEmpty && report.uploaded.isEmpty
                case Left(_)       => false,
            )
          program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
    ,
    test("a receipt that says the ship is already published is held"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-release-held")
          .flatMap: dir =>
            val source =
              dir.resolve("releaselaw").resolve("src").resolve("main").resolve("scala").resolve("Ready.scala")
            val program =
              for
                graph <- ZIO
                  .fromEither(released(named))
                  .catchAll(error => ZIO.dieMessage(error.message))
                loaded  <- Machine.load(dir).either
                machine <- loaded match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value.copy(env = Map.empty))
                found <- GraphCompile.jdk(machine).either
                jdk   <- found match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                seen <- Ref.make(Option.empty[Seen])
                _    <- Workspace.writeString(VersionTask.path(dir), "")
                _    <- Workspace.createDirectories(parentOf(source))
                _    <- Workspace.writeString(source, "object Ready:\n  def n: Int = 1\n")
                _    <- Workspace.writeString(dir.resolve("dependencies.lock"), DependencyLock.render(pinned(lock)))
                ran  <- PublishRun
                  .release(
                    dir,
                    graph,
                    recording(graph, seen),
                    Nil,
                    jdk,
                    Map("HISTOS_SESSION" -> "release"),
                    false,
                    hostApi,
                    _ => ZIO.unit,
                    _ => ZIO.unit,
                  )
                  .either
                recorded <- seen.get
              yield
                val detail = ran.fold(_.message, report => report.held.mkString("\n"))
                val line   = recorded.fold("")(_.line)
                assertTrue(
                  detail == line,
                  line.contains(" is already published"),
                  line.nonEmpty,
                  ran match
                    case Right(report) => report.failed.isEmpty && report.refused.isEmpty && report.deployed.isEmpty
                    case Left(_)       => false
                  ,
                  recorded.exists(_.jars),
                  recorded.exists(_.artifacts.exists(_.contains("releaselaw"))),
                )
            program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store")))),
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def released(member: histos.ProjectDef): Either[histos.BuildError, histos.ProjectGraph] =
    Projects
      .organization(group)
      .license(License(license"Apache-2.0", url"https://www.apache.org/licenses/LICENSE-2.0.txt"))
      .developer(Developer(developer"early-effect", pom"Early Effect"))
      .scm(
        Scm(
          url"https://github.com/early-effect/histos",
          connection"scm:git:git@github.com:early-effect/histos.git",
        )
      )(ships(Ship("demo").next(1, 2, 3)(member)), member)

  private final case class Seen(artifacts: List[String], jars: Boolean, line: String)

  private def recording(graph: histos.ProjectGraph, seen: Ref[Option[Seen]]): HistosBuild =
    val found = graph
    new HistosBuild:
      def graph: Either[histos.BuildError, histos.ProjectGraph]          = Right(found)
      override def sona: ZLayer[histos.SonaPublish, Nothing, Publishing] =
        ZLayer.succeed(new Publishing:
          def publish(request: PublishRequest): zio.IO[PublishFault, PublishReceipt] =
            val artifacts = request.modules.map(_.artifact).toList
            val jars      =
              request.modules.forall(module => Files.isRegularFile(module.jar) && Files.isRegularFile(module.pom))
            val line = request.modules.headOption match
              case Some(module) => s"${module.ship.value} ${module.revision} is already published"
              case None         => ""
            seen.set(Some(Seen(artifacts, jars, line))).as(PublishReceipt(Chunk(line), Chunk.empty))
          def secretNames: List[String] = Nil
          def followUp: FollowUp        = FollowUp.Finished)

  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells =
      LockedCell(
        histos.ProjectName.trusted("releaselaw"),
        scala3,
        histos.Platform.Jvm,
        Nil,
        Nil,
        Nil,
        ClasspathScope.Compile,
      ) :: lock.cells
    )

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

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end PublishReleaseLawSpec
