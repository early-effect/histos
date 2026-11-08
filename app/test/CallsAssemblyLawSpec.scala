package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.jdk.{Machine, Sha256Digest}
import histos.report.CallResult
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  Module,
  Platform,
  Platforms,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** Assembly writes `dist/histos.jar` and its checksum. A destination that is not a path is refused. */
object CallsAssemblyLawSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val api    = ProjectName.trusted("api")
  private val app    = ProjectName.trusted("app")

  def spec = suite("calls assembly laws")(
    test("the jar contains api and the launcher main, and a broken destination is a fault"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-calls-assembly")
          .flatMap: dir =>
            val jar     = dir.resolve("dist").resolve("histos.jar")
            val pin     = dir.resolve(".histos").resolve("assembly.sha256")
            val program =
              for
                loaded  <- Machine.load(dir).either
                machine <- loaded match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                found <- GraphCompile.jdk(machine).either
                jdk   <- found match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                (refused, refusedHeard) <- speak(say => Calls.assembly(dir, graph, jdk, machine, Some("\u0000"), say))
                _                       <- Workspace.writeString(
                  dir.resolve("api").resolve("src").resolve("main").resolve("scala").resolve("Api.scala"),
                  "object ApiLib:\n  def ready: Int = 1\n",
                )
                _ <- Workspace.writeString(
                  dir
                    .resolve("app")
                    .resolve("src")
                    .resolve("main")
                    .resolve("scala")
                    .resolve("histos")
                    .resolve("cli")
                    .resolve("Main.scala"),
                  "package histos.cli\nobject Main:\n  def main(args: Array[String]): Unit = ()\n",
                )
                (assembled, heard) <- speak(say => Calls.assembly(dir, graph, jdk, machine, None, say))
                names              <- ZIO.attempt(entryNames(jar)).orDie
                digest             <- ZIO
                  .attempt(if Files.isRegularFile(jar) then
                    Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(jar))).value
                  else "")
                  .orDie
                written <- ZIO.attempt(if Files.isRegularFile(pin) then Files.readString(pin) else "").orDie
              yield
                val text = outs(heard)
                assertTrue(
                  refused == Right(()),
                  done(refusedHeard).exists(faultHas("assembly path is not a path")),
                  assembled == Right(()),
                  modulesOf(done(heard)) == List("api", "app"),
                  text == "files compiled: 2 skipped: 0; modules: 2; classes: 4\ndist/histos.jar",
                  names.contains("ApiLib.class"),
                  names.contains("histos/cli/Main.class"),
                  written == s"$digest\n",
                  digest.nonEmpty,
                )
            program.provideSomeLayer[Resolve & Scope](session(dir, lock))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def faultHas(text: String)(result: CallResult): Boolean =
    result match
      case CallResult.Fault(fault) => fault.message.contains(text)
      case _                       => false

  private def modulesOf(result: Option[CallResult]): List[String] =
    result match
      case Some(CallResult.Reported(report)) =>
        report.observation.toList.flatMap(_.modules.map(_.module).toList).sorted
      case _ => Nil

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

  private def graph: ProjectGraph =
    val apiModule = Module(
      api,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("api.scala", 1, 1),
    )
    val appModule = Module(
      app,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      List(ProjectDep(api, None)),
      SourcePos("app.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(apiModule, appModule),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def session(dir: Path, lock: DependencyLock) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      ResolutionService.pinned(pinned(lock))

  private def pinned(lock: DependencyLock): DependencyLock =
    val drop  = Set(api, app)
    val kept  = lock.cells.filterNot(cell => drop.contains(cell.module))
    val added = List(api, app).map: name =>
      LockedCell(name, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile)
    lock.copy(cells = added ++ kept)

  private def entryNames(path: Path): Set[String] =
    if !Files.isRegularFile(path) then Set.empty
    else
      val zip = new java.util.zip.ZipFile(path.toFile)
      try zip.entries().asScala.map(_.getName).toSet
      finally zip.close()

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
end CallsAssemblyLawSpec
