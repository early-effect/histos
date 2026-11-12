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

import zio.{Chunk, Ref, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A snapshot with no asset still stages. The channel plan is empty, so nothing is uploaded. */
object PublishSnapshotLawSpec extends ZIOSpecDefault:
  private val hostApi  = ApiDigest.trusted("ab" * 32)
  private val group    = organization"example.com"
  private val sha      = "0123456789abcdef0123456789abcdef01234567"
  private val revision = "1.2.3-SNAPSHOT"

  def spec = suite("publish snapshot laws")(
    test("a snapshot with no asset stages and does not upload a channel"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val named = module("snaplaw", Jvm)(closure.scalaVersion)
        Workspace
          .work("histos-snapshot-channel")
          .flatMap: dir =>
            val source = dir.resolve("snaplaw").resolve("src").resolve("main").resolve("scala").resolve("Ready.scala")
            val env    = Map(
              "HISTOS_SESSION" -> "snapshot",
              "HISTOS_SHA"     -> sha,
              "HISTOS_BRANCH"  -> "default",
            )
            val program =
              for
                graph   <- ZIO.fromEither(released(named))
                machine <- Machine.load(dir).map(_.copy(env = Map.empty))
                jdk     <- GraphCompile.jdk(machine)
                seen    <- Ref.make(Option.empty[Seen])
                notes   <- Ref.make(List.empty[String])
                _       <- Workspace.writeString(VersionTask.path(dir), "")
                _       <- Workspace.writeString(source, "object Ready:\n  def n: Int = 1\n")
                _       <- Workspace.writeString(
                  dir.resolve("dependencies.lock"),
                  DependencyLock.render(closure.pinning(ProjectName("snaplaw"))),
                )
                ran <- PublishRun
                  .snapshots(
                    dir,
                    graph,
                    recording(graph, seen),
                    Nil,
                    jdk,
                    env,
                    hostApi,
                    _ => ZIO.unit,
                    line => notes.update(line :: _),
                  )
                recorded <- seen.get
                told     <- notes.get
              yield assertTrue(
                recorded == Some(Seen(revision, true)),
                ran.lines.contains(s"published $revision"),
                told.exists(_.startsWith("staged ")),
                !told.exists(_.contains("uploading")),
              )
            program.provideSomeLayer[Resolve & zio.Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
  ).provideSomeLayerShared[TestEnvironment & zio.Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

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

  private final case class Seen(revision: String, jars: Boolean)

  private def recording(graph: histos.ProjectGraph, seen: Ref[Option[Seen]]): HistosBuild =
    val found = graph
    new HistosBuild:
      def graph: Either[histos.BuildError, histos.ProjectGraph]          = Right(found)
      override def sona: ZLayer[histos.SonaPublish, Nothing, Publishing] =
        ZLayer.succeed(new Publishing:
          def publish(request: PublishRequest): zio.IO[PublishFault, PublishReceipt] =
            val revision = request.modules.headOption.fold("")(_.revision)
            val jars     =
              request.modules.forall(module => Files.isRegularFile(module.jar) && Files.isRegularFile(module.pom))
            seen.set(Some(Seen(revision, jars))).as(PublishReceipt(Chunk(s"published $revision"), Chunk.empty))
          def secretNames: List[String] = Nil
          def followUp: FollowUp        = FollowUp.Finished)
end PublishSnapshotLawSpec
