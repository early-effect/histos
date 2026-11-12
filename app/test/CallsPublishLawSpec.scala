package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.plugin.ApiDigest
import histos.publish.IvyRoot
import histos.report.CallResult
import histos.resolve.{DependencyLock, Resolve}
import histos.version.VersionTask
import histos.workspace.Workspace
import histos.{
  CompilerClosure,
  FollowUp,
  HistosBuild,
  Jvm,
  LawGit,
  LawMachine,
  ProjectGraph,
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
import histos.`scala`
import slayer.stubbed

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, Ref, Scope, ZIO, ZLayer}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Publish calls report the build's receipt. A missing organization and missing credentials fail before anything is
  * staged, and the ivy root is the law's own directory.
  */
object CallsPublishLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("publishlaw")
  private val group   = organization"example.com"
  private val hostApi = ApiDigest.trusted("ab" * 32)

  def spec = suite("calls publish laws")(
    test("a missing organization and missing credentials are faults before anything compiles"):
      Workspace
        .work("histos-calls-refuse")
        .flatMap: dir =>
          val program =
            for
              bare    <- graphOf(scala"3.10.0", None)
              machine <- LawMachine.at(dir)
              _       <- headCommit(dir)
              jdk = LawMachine.fixtureJdk
              (missed, missedHeard) <- speak(say =>
                Calls.publishLocal(dir, bare, HistosBuild.fixed(bare), machine, jdk, Nil, say, hostApi)
              )
              (released, releasedHeard) <- speak(say =>
                Calls.publishRelease(dir, bare, HistosBuild.fixed(bare), machine, jdk, Nil, false, say, hostApi)
              )
              (dropped, droppedHeard) <- speak(say => Calls.drop(dir, bare, machine, "nope", say))
            yield assertTrue(
              missed == Right(()),
              faultText(missedHeard).contains(
                "a publish needs an organization. Name it with Projects.organization(organization\"com.example\")"
              ),
              released == Right(()),
              faultText(releasedHeard).contains("SONATYPE_USERNAME and SONATYPE_PASSWORD are required"),
              dropped == Right(()),
              faultText(droppedHeard).contains("SONATYPE_USERNAME and SONATYPE_PASSWORD are required"),
            )
          program.provideSomeLayer[Scope](
            stubbed[Resolve] ++ stubbed[ContentStore] ++ ZLayer.succeed(IvyRoot.at(dir.resolve("ivy")))
          )
    ,
    test("a staged jar and pom reach the build's publisher, and its receipt is the report"):
      received.provideSomeLayer[Scope](CompilerClosure.layer),
  )

  private def received =
    ZIO.serviceWithZIO[CompilerClosure]: closure =>
      Workspace
        .work("histos-calls-publish")
        .flatMap: dir =>
          val source =
            dir.resolve("publishlaw").resolve("src").resolve("main").resolve("scala").resolve("Ready.scala")
          val program =
            for
              graph   <- graphOf(closure.scalaVersion, Some(group))
              machine <- LawMachine.at(dir)
              jdk     <- LawMachine.jdk(machine)
              seen    <- Ref.make(Option.empty[Seen])
              _       <- headCommit(dir)
              _       <- Workspace.writeString(source, "object Ready:\n  def n: Int = 1\n")
              _       <- Workspace.writeString(
                dir.resolve("dependencies.lock"),
                DependencyLock.render(closure.pinning(lawName)),
              )
              (published, heard) <- speak(say =>
                Calls.publishLocal(dir, graph, recording(graph, seen), machine, jdk, Nil, say, hostApi)
              )
              recorded <- seen.get
            yield assertTrue(
              published == Right(()),
              recorded == Some(Seen(List("publishlaw_3"), staged = true)),
              outs(heard).linesIterator.contains("received publishlaw_3"),
              done(heard).exists:
                case CallResult.Reported(_) => true
                case CallResult.Fault(_)    => false,
            )
          program.provideSomeLayer[Resolve & Scope](
            ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++ ZLayer.succeed(IvyRoot.at(dir.resolve("ivy")))
          )

  private final case class Seen(artifacts: List[String], staged: Boolean)

  /** A snapshot coordinate is the commit at HEAD. An empty releases file is a checked registry. */
  private def headCommit(dir: Path) =
    Workspace.writeString(dir.resolve("ready"), "ready\n") *>
      LawGit.commit(dir, "topic", "ready") *>
      Workspace.writeString(VersionTask.path(dir), "")

  private def graphOf(version: histos.ScalaVersion, owner: Option[histos.Organization]): IO[Nothing, ProjectGraph] =
    val named   = module("publishlaw", Jvm)(version)
    val catalog = ships(Ship("demo").next(1, 2, 3)(named))
    val built   = owner match
      case Some(found) => Projects.organization(found)(catalog, named)
      case None        => Projects(catalog, named)
    ZIO.fromEither(built).orDieWith(error => IllegalStateException(error.message))

  /** The build's local publisher records what it was handed and answers one receipt line per artifact. */
  private def recording(graph: ProjectGraph, seen: Ref[Option[Seen]]): HistosBuild =
    val found = graph
    new HistosBuild:
      def graph: Either[histos.BuildError, ProjectGraph]                   = Right(found)
      override def local: ZLayer[histos.LocalPublish, Nothing, Publishing] =
        ZLayer.succeed(new Publishing:
          def publish(request: PublishRequest): IO[PublishFault, PublishReceipt] =
            val artifacts = request.modules.map(_.artifact).toList
            val staged    =
              request.modules.forall(module => Files.isRegularFile(module.jar) && Files.isRegularFile(module.pom))
            seen
              .set(Some(Seen(artifacts, staged)))
              .as(PublishReceipt(Chunk.fromIterable(artifacts.map(name => s"received $name")), Chunk.empty))
          def secretNames: List[String] = Nil
          def followUp: FollowUp        = FollowUp.Finished)

  private def faultText(heard: Chunk[Message]): Option[String] =
    done(heard).collect { case CallResult.Fault(found) => found.message }

  private def done(heard: Chunk[Message]): Option[CallResult] =
    heard.collect { case Message.Done(result) => result }.lastOption

  private def outs(heard: Chunk[Message]): String =
    heard.collect { case Message.Event(BuildEvent.Out(text)) => text }.mkString("\n")

  private def speak[R, E](use: Say => ZIO[R, E, Unit]): ZIO[R, Nothing, (Either[E, Unit], Chunk[Message])] =
    for
      messages <- Ref.make(Chunk.empty[Message])
      say = new Say(message => messages.update(_ :+ message))
      result <- use(say).either
      heard  <- messages.get
    yield (result, heard)
end CallsPublishLawSpec
