package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.jdk.Machine
import histos.load.Loader
import histos.report.CallResult
import histos.resolve.Resolve
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  HistosBuild,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import java.nio.file.{Files, Path, StandardCopyOption}

import zio.{Duration, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** Reload forces a load. A missing build file is the fault, and the server still answers. */
object DaemonMoreLawSpec extends ZIOSpecDefault:
  private val version = "0.0.0-law"
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("reloadlaw")

  def spec = suite("daemon reload laws")(
    test("reload of a repo with no build file is a fault, and version still answers"):
      ZIO
        .scoped {
          for
            repo <- ZIO.acquireRelease(
              ZIO.attemptBlocking(Files.createTempDirectory(Path.of("/tmp"), "hd")).orDie
            )(path => Workspace.delete(path).catchAll(error => ZIO.dieMessage(error.message)))
            _ <- ZIO
              .attemptBlocking(
                Files.copy(
                  repository.resolve("dependencies.lock"),
                  repo.resolve("dependencies.lock"),
                  StandardCopyOption.REPLACE_EXISTING,
                )
              )
              .orDie
            api = repo.resolve("api.jar")
            _       <- ZIO.attemptBlocking(Files.write(api, Array.emptyByteArray)).orDie
            loaded  <- Machine.load(repo).either
            machine <- loaded match
              case Left(error)  => ZIO.dieMessage(error.message)
              case Right(value) => ZIO.succeed(value)
            found <- GraphCompile.jdk(machine).either
            jdk   <- found match
              case Left(error)  => ZIO.dieMessage(error.message)
              case Right(value) => ZIO.succeed(value)
            running <- Daemon.open(
              repo,
              graph,
              HistosBuild.fixed(graph),
              jdk,
              version,
              api,
              Nil,
              ZIO.dieMessage("reload loads through the loader"),
              Calls.quiet,
              machine,
              _ => ZIO.unit,
            )
            socket = running.record.socket
            reloaded     <- Client.talk(socket, Call.Reload).runCollect
            versionFeeds <- Client.talk(socket, Call.Version).runCollect
            message = done(reloaded) match
              case Some(CallResult.Fault(fault)) => fault.message
              case other                         => other.fold("no done")(_.toString)
          yield assertTrue(
            message.contains("no build file at"),
            message.contains("histos.scala"),
            outs(versionFeeds) == version,
            done(versionFeeds).contains(CallResult.reported()),
          )
        }
        .timeout(Duration.fromSeconds(30))
        .flatMap {
          case Some(result) => ZIO.succeed(result)
          case None         => ZIO.dieMessage("daemon did not answer")
        }
        .provideSomeLayer[Scope](opened)
  ) @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock

  private def outs(heard: zio.Chunk[Feed]): String =
    heard.collect { case Feed.Event(BuildEvent.Out(text)) => text }.mkString("\n")

  private def done(heard: zio.Chunk[Feed]): Option[CallResult] =
    heard.collect { case Feed.Done(result) => result }.lastOption

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def opened =
    (Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++
      ZLayer.succeed(ContentStore.in(Path.of("/tmp").resolve("hd-store")))) >+> Loader.live

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
end DaemonMoreLawSpec
