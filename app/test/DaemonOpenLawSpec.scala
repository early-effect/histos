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

/** A server on its own socket answers version and cache, and an empty clean removes products. */
object DaemonOpenLawSpec extends ZIOSpecDefault:
  private val version = "0.0.0-law"
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("daemonlaw")

  def spec = suite("daemon open laws")(
    test("version is the server's version, cache names the store, and clean removes the product"):
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
              ZIO.dieMessage("version does not load a build"),
              Calls.quiet,
              machine,
              _ => ZIO.unit,
            )
            socket = running.record.socket
            versionFeeds <- Client.talk(socket, Call.Version).runCollect
            cacheFeeds   <- Client.talk(socket, Call.CachePath).runCollect
            clazz = repo.resolve(".histos").resolve("classes").resolve(lawName.value).resolve("Kept.class")
            _ <- (Workspace.createDirectories(parentOf(clazz)) *> Workspace.writeString(clazz, "kept\n"))
              .catchAll(error => ZIO.dieMessage(error.message))
            cleanFeeds <- Client.talk(socket, Call.Clean(Nil)).runCollect
            gone       <- ZIO.attempt(Files.isRegularFile(clazz)).orDie
            missed     <- Client.talk(socket, Call.PreviewCompile(List(lawName))).runCollect
            assembled  <- Client.talk(socket, Call.Assembly(None)).runCollect
            ran        <- Client.talk(socket, Call.Run(lawName.value, None)).runCollect
            rebooted   <- Client.talk(socket, Call.Reboot).runCollect
            closed     <- ZIO.attempt(Files.exists(socket)).orDie
          yield assertTrue(
            outs(versionFeeds) == version,
            done(versionFeeds).contains(CallResult.reported()),
            outs(cacheFeeds) == Path.of("/tmp").resolve("hd-store").toString,
            outs(cleanFeeds) == "cleaned",
            !gone,
            outs(missed).contains("daemonlaw 3.10.0 jvm selected"),
            done(assembled).exists(faultHas("'app' is not a module")),
            done(ran).exists(faultHas("'daemonlaw' does not name a main")),
            done(rebooted).contains(CallResult.reported()),
            !closed,
          )
        }
        .timeout(Duration.fromSeconds(30))
        .flatMap {
          case Some(result) => ZIO.succeed(result)
          case None         => ZIO.dieMessage("daemon did not answer")
        }
        .provideSomeLayer[Scope](opened)
  ) @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock

  private def faultHas(text: String)(result: CallResult): Boolean =
    result match
      case CallResult.Fault(fault) => fault.message.contains(text)
      case _                       => false

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

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end DaemonOpenLawSpec
