package histos.publish

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.plugin.ApiDigest
import histos.resolve.{DependencyLock, Resolve}
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  CompilerClosure,
  FollowUp,
  HistosBuild,
  Jvm,
  ProjectName,
  Projects,
  PublishFault,
  PublishReceipt,
  PublishRequest,
  Publishing,
  Ship,
  module,
  organization,
  ships,
}

import java.nio.file.Files

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** Local publish hands the staged jars to the build. It does not install them. */
object PublishRecordLawSpec extends ZIOSpecDefault:
  private val hostApi = ApiDigest.trusted("ab" * 32)
  private val group   = organization"example.com"

  def spec = suite("publish record laws")(
    test("no organization is refused before a class file exists"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val named = module("recordlaw", Jvm)(closure.scalaVersion)
        Workspace
          .work("histos-record-org")
          .flatMap: dir =>
            val program =
              for
                _       <- LawGit.ready(dir)
                graph   <- ZIO.fromEither(Projects(ships(Ship("demo").next(1, 2, 3)(named)), named))
                machine <- Machine.load(dir)
                jdk     <- GraphCompile.jdk(machine)
                ran     <- PublishRun
                  .local(
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
                classes <- ZIO.attemptBlocking(Files.exists(dir.resolve(".histos").resolve("classes"))).orDie
              yield assertTrue(ran == Left(LocalPublishError.NoOrganization), !classes)
            program.provideSomeLayer[Resolve & Scope](
              ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++ ZLayer.succeed(IvyRoot.at(dir.resolve("ivy")))
            )
    ,
    test("a staged jar and its pom at the commit coordinate are the request the build publishes"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val named = module("recordlaw", Jvm)(closure.scalaVersion)
        Workspace
          .work("histos-record-local")
          .flatMap: dir =>
            val source = dir.resolve("recordlaw").resolve("src").resolve("main").resolve("scala").resolve("Ready.scala")
            val program =
              for
                sha     <- LawGit.ready(dir)
                graph   <- ZIO.fromEither(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(named)), named))
                machine <- Machine.load(dir)
                jdk     <- GraphCompile.jdk(machine)
                seen    <- Ref.make(Option.empty[Seen])
                _       <- Workspace.writeString(source, "object Ready:\n  def n: Int = 1\n")
                _       <- Workspace.writeString(
                  dir.resolve("dependencies.lock"),
                  DependencyLock.render(closure.pinning(ProjectName("recordlaw"))),
                )
                ran <- PublishRun
                  .local(dir, graph, recording(graph, seen), Nil, jdk, Map.empty, hostApi, _ => ZIO.unit, _ => ZIO.unit)
                recorded <- seen.get
              yield assertTrue(
                ran.lines == List("recordlaw_3"),
                recorded == Some(Seen(List("recordlaw_3"), List(s"1.2.3-${sha.abbreviate.value}-SNAPSHOT"), true)),
              )
            program.provideSomeLayer[Resolve & Scope](
              ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++ ZLayer.succeed(IvyRoot.at(dir.resolve("ivy")))
            ),
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private final case class Seen(artifacts: List[String], revisions: List[String], jars: Boolean)

  private def recording(graph: histos.ProjectGraph, seen: Ref[Option[Seen]]): HistosBuild =
    val found = graph
    new HistosBuild:
      def graph: Either[histos.BuildError, histos.ProjectGraph]            = Right(found)
      override def local: ZLayer[histos.LocalPublish, Nothing, Publishing] =
        ZLayer.succeed(new Publishing:
          def publish(request: PublishRequest): zio.IO[PublishFault, PublishReceipt] =
            val artifacts = request.modules.map(_.artifact).toList
            val revisions = request.modules.map(_.revision).toList
            val jars      =
              request.modules.forall(module => Files.isRegularFile(module.jar) && Files.isRegularFile(module.pom))
            seen
              .set(Some(Seen(artifacts, revisions, jars)))
              .as(PublishReceipt(Chunk.fromIterable(artifacts), Chunk.empty))
          def secretNames: List[String] = Nil
          def followUp: FollowUp        = FollowUp.Finished)
end PublishRecordLawSpec
