package histos.publish

import histos.cache.ContentStore
import histos.jdk.Machine
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

import zio.{Chunk, Ref, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A snapshot with no asset still stages. The channel plan is empty, so nothing is uploaded. */
object PublishSnapshotLawSpec extends ZIOSpecDefault:
  private val scala3   = scala"3.10.0"
  private val group    = organization"example.com"
  private val named    = module("snaplaw", Jvm)(scala3)
  private val sha      = "0123456789abcdef0123456789abcdef01234567"
  private val revision = "1.2.3-SNAPSHOT"

  def spec = suite("publish snapshot laws")(
    test("a snapshot with no asset stages and does not upload a channel"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
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
                graph   <- ZIO.fromEither(released(named)).catchAll(error => ZIO.dieMessage(error.message))
                loaded  <- Machine.load(dir).either
                machine <- loaded match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value.copy(env = Map.empty))
                found <- GraphCompile.jdk(machine).either
                jdk   <- found match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                seen  <- Ref.make(Option.empty[Seen])
                notes <- Ref.make(List.empty[String])
                _ <- Workspace.writeString(VersionTask.path(dir), "").catchAll(error => ZIO.dieMessage(error.message))
                _ <- Workspace.createDirectories(parentOf(source)).catchAll(error => ZIO.dieMessage(error.message))
                _ <- Workspace
                  .writeString(source, "object Ready:\n  def n: Int = 1\n")
                  .catchAll(error => ZIO.dieMessage(error.message))
                _ <- Workspace
                  .writeString(dir.resolve("dependencies.lock"), DependencyLock.render(pinned(lock)))
                  .catchAll(error => ZIO.dieMessage(error.message))
                ran <- PublishRun
                  .snapshots(
                    dir,
                    graph,
                    recording(graph, seen),
                    Nil,
                    jdk,
                    env,
                    _ => ZIO.unit,
                    line => notes.update(line :: _),
                  )
                  .either
                recorded <- seen.get
                told     <- notes.get
              yield
                val lines = ran.fold(_.message, _.lines.mkString("\n"))
                assertTrue(
                  ran.isRight,
                  recorded.exists(_.revision == revision),
                  recorded.exists(_.jars),
                  lines.contains(s"published $revision"),
                  told.exists(_.startsWith("staged ")),
                  told.forall(line => !line.contains("uploading")),
                )
            program.provideSomeLayer[Resolve & zio.Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
  ).provideSomeLayerShared[TestEnvironment & zio.Scope](opened) @@ TestAspect.withLiveSystem

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

  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells =
      LockedCell(
        histos.ProjectName.trusted("snaplaw"),
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
end PublishSnapshotLawSpec
