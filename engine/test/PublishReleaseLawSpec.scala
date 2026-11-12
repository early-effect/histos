package histos.publish

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.plugin.ApiDigest
import histos.resolve.{DependencyLock, Resolve}
import histos.version.VersionTask
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  CompilerClosure,
  Developer,
  FollowUp,
  HistosBuild,
  Jvm,
  License,
  ProjectName,
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

import java.nio.file.Files

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A release classifies the build's receipt. Credentials fail before that, and a ship with no asset does not upload. */
object PublishReleaseLawSpec extends ZIOSpecDefault:
  private val hostApi = ApiDigest.trusted("ab" * 32)
  private val group   = organization"example.com"

  def spec = suite("publish release laws")(
    test("snapshots name the missing credentials, and assets with nothing to upload stay empty"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val named = module("releaselaw", Jvm)(closure.scalaVersion)
        Workspace
          .work("histos-release-gate")
          .flatMap: dir =>
            val program =
              for
                graph   <- ZIO.fromEither(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(named)), named))
                machine <- Machine.load(dir).map(_.copy(env = Map.empty))
                jdk     <- GraphCompile.jdk(machine)
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
                assets <- PublishRun.assets(dir, graph, Nil, Map.empty)
              yield assertTrue(
                snapped == Left(LocalPublishError.Credentials),
                assets.failed.isEmpty,
                assets.uploaded.isEmpty,
              )
            program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
    ,
    test("a receipt that says the ship is already published is held"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val named = module("releaselaw", Jvm)(closure.scalaVersion)
        Workspace
          .work("histos-release-held")
          .flatMap: dir =>
            val source =
              dir.resolve("releaselaw").resolve("src").resolve("main").resolve("scala").resolve("Ready.scala")
            val program =
              for
                graph   <- ZIO.fromEither(released(named))
                machine <- Machine.load(dir).map(_.copy(env = Map.empty))
                jdk     <- GraphCompile.jdk(machine)
                seen    <- Ref.make(Option.empty[Seen])
                _       <- Workspace.writeString(VersionTask.path(dir), "")
                _       <- Workspace.writeString(source, "object Ready:\n  def n: Int = 1\n")
                _       <- Workspace.writeString(
                  dir.resolve("dependencies.lock"),
                  DependencyLock.render(closure.pinning(ProjectName("releaselaw"))),
                )
                report <- PublishRun
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
                recorded <- seen.get
              yield assertTrue(
                report == ReleaseReport(Nil, List("demo 1.2.3 is already published"), Nil, Nil),
                recorded == Some(Seen(List("releaselaw_3"), true)),
              )
            program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store")))),
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

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

  private final case class Seen(artifacts: List[String], jars: Boolean)

  /** The receipt says every staged ship is already published. */
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
            val lines = request.modules.map(module => s"${module.ship.value} ${module.revision} is already published")
            seen.set(Some(Seen(artifacts, jars))).as(PublishReceipt(lines, Chunk.empty))
          def secretNames: List[String] = Nil
          def followUp: FollowUp        = FollowUp.Finished)
end PublishReleaseLawSpec
