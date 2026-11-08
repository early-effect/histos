package histos.publish

import histos.cache.ContentStore
import histos.jdk.Machine
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

/** Local publish hands the staged jars to the build. It does not install them. */
object PublishRecordLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val group  = organization"example.com"
  private val named  = module("recordlaw", Jvm)(scala3)

  def spec = suite("publish record laws")(
    test("no organization is refused before a class file exists"):
      Workspace.work("histos-record-org").flatMap: dir =>
        val program =
          for
            _ <- LawGit.ready(dir)
            graph <- ZIO
              .fromEither(Projects(ships(Ship("demo").next(1, 2, 3)(named)), named))
              .catchAll(error => ZIO.dieMessage(error.message))
            loaded <- Machine.load(dir).either
            machine <- loaded match
              case Left(error)  => ZIO.dieMessage(error.message)
              case Right(value) => ZIO.succeed(value)
            found <- GraphCompile.jdk(machine).either
            jdk <- found match
              case Left(error)  => ZIO.dieMessage(error.message)
              case Right(value) => ZIO.succeed(value)
            ran <- PublishRun
              .local(dir, graph, HistosBuild.fixed(graph), Nil, jdk, Map.empty, _ => ZIO.unit, _ => ZIO.unit)
              .either
            classes <- ZIO.attempt(Files.exists(dir.resolve(".histos").resolve("classes"))).orDie
          yield assertTrue(
            ran == Left(LocalPublishError.NoOrganization),
            ran match
              case Left(error) => error.message == LocalPublishError.NoOrganization.message
              case Right(_)   => false
            ,
            !classes,
          )
        program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
    ,
    test("a staged jar is the request the build publishes"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace.work("histos-record-local").flatMap: dir =>
          val source = dir.resolve("recordlaw").resolve("src").resolve("main").resolve("scala").resolve("Ready.scala")
          val program =
            for
              _ <- LawGit.ready(dir)
              graph <- ZIO
                .fromEither(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(named)), named))
                .catchAll(error => ZIO.dieMessage(error.message))
              loaded <- Machine.load(dir).either
              machine <- loaded match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              found <- GraphCompile.jdk(machine).either
              jdk <- found match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              seen <- Ref.make(Option.empty[Seen])
              _ <- Workspace.createDirectories(parentOf(source))
              _ <- Workspace.writeString(source, "object Ready:\n  def n: Int = 1\n")
              _ <- Workspace.writeString(dir.resolve("dependencies.lock"), DependencyLock.render(pinned(lock)))
              ran <- PublishRun
                .local(dir, graph, recording(graph, seen), Nil, jdk, Map.empty, _ => ZIO.unit, _ => ZIO.unit)
                .either
              recorded <- seen.get
            yield
              val detail = ran.fold(_.message, report => report.lines.mkString("\n"))
              val artifacts = recorded.fold(List.empty[String])(_.artifacts)
              assertTrue(
                detail.contains("recordlaw"),
                ran.isRight,
                artifacts.exists(_.contains("recordlaw")),
                recorded.exists(_.jars),
              )
          program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
    ,
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
            val jars = request.modules.forall(module => Files.isRegularFile(module.jar) && Files.isRegularFile(module.pom))
            seen.set(Some(Seen(artifacts, jars))).as(PublishReceipt(Chunk.fromIterable(artifacts), Chunk.empty))
          def secretNames: List[String] = Nil
          def followUp: FollowUp        = FollowUp.Finished
        )

  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells =
      LockedCell(
        histos.ProjectName.trusted("recordlaw"),
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
end PublishRecordLawSpec
