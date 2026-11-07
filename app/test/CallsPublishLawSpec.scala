package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.jdk.Machine
import histos.report.CallResult
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, Resolve}
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  FollowUp,
  HistosBuild,
  Jvm,
  Projects,
  PublishFault,
  PublishReceipt,
  PublishRequest,
  Publishing,
  Ship,
  module,
  organization,
  ship,
  ships,
}
import histos.`scala`

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** Publish calls report the build's receipt. Credentials fail before a socket opens. */
object CallsPublishLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val group  = organization"example.com"
  private val named  = module("publishlaw", Jvm)(scala3)

  def spec = suite("calls publish laws")(
    test("a missing organization and missing credentials are faults, and a staged jar is the receipt"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace.work("histos-calls-publish").flatMap: dir =>
          val source =
            dir.resolve("publishlaw").resolve("src").resolve("main").resolve("scala").resolve("Ready.scala")
          val program =
            for
              bare <- ZIO
                .fromEither(Projects(ships(Ship("demo").next(1, 2, 3)(named)), named))
                .catchAll(error => ZIO.dieMessage(error.message))
              graph <- ZIO
                .fromEither(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(named)), named))
                .catchAll(error => ZIO.dieMessage(error.message))
              loaded <- Machine.load(dir).either
              machine <- loaded match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value.copy(env = Map.empty))
              found <- GraphCompile.jdk(machine).either
              jdk <- found match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              (missed, missedHeard) <- speak(say =>
                Calls.publishLocal(dir, bare, HistosBuild.fixed(bare), machine, jdk, Nil, say)
              )
              (released, releasedHeard) <- speak(say =>
                Calls.publishRelease(dir, bare, HistosBuild.fixed(bare), machine, jdk, Nil, false, say)
              )
              (dropped, droppedHeard) <- speak(say => Calls.drop(dir, bare, machine, "nope", say))
              seen <- Ref.make(Option.empty[Seen])
              _ <- Workspace.createDirectories(parentOf(source))
              _ <- Workspace.writeString(source, "object Ready:\n  def n: Int = 1\n")
              _ <- Workspace.writeString(dir.resolve("dependencies.lock"), DependencyLock.render(pinned(lock)))
              (published, heard) <- speak(say =>
                Calls.publishLocal(dir, graph, recording(graph, seen), machine, jdk, Nil, say)
              )
              recorded <- seen.get
            yield
              val text = outs(heard)
              val artifacts = recorded.fold(List.empty[String])(_.artifacts)
              assertTrue(
                missed == Right(()),
                fault(missedHeard, "a publish needs an organization"),
                released == Right(()),
                fault(releasedHeard, "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"),
                dropped == Right(()),
                fault(droppedHeard, "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"),
                published == Right(()),
                done(heard).exists {
                  case CallResult.Reported(_) => true
                  case _                      => false
                },
                text.contains("publishlaw"),
                artifacts.exists(_.contains("publishlaw")),
                recorded.exists(_.jars),
              )
          program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private final case class Seen(artifacts: List[String], jars: Boolean)

  private def recording(graph: histos.ProjectGraph, seen: Ref[Option[Seen]]): HistosBuild =
    val found = graph
    new HistosBuild:
      def graph: Either[histos.BuildError, histos.ProjectGraph] = Right(found)
      override def local: ZLayer[histos.LocalPublish, Nothing, Publishing] =
        ZLayer.succeed(new Publishing:
          def publish(request: PublishRequest): zio.IO[PublishFault, PublishReceipt] =
            val artifacts = request.modules.map(_.artifact).toList
            val jars =
              request.modules.forall(module => Files.isRegularFile(module.jar) && Files.isRegularFile(module.pom))
            seen.set(Some(Seen(artifacts, jars))).as(PublishReceipt(Chunk.fromIterable(artifacts), Chunk.empty))
          def secretNames: List[String] = Nil
          def followUp: FollowUp        = FollowUp.Finished
        )

  private def fault(heard: Chunk[Message], text: String): Boolean =
    done(heard) match
      case Some(CallResult.Fault(found)) => found.message.contains(text)
      case _                             => false

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

  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells =
      LockedCell(
        histos.ProjectName.trusted("publishlaw"),
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
end CallsPublishLawSpec
