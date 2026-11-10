package histos.testrun

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, Repositories, ResolutionService, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers, GraphCompile}
import histos.{
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
  TestMain,
}

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** Compiles a module and runs its main. The compiler jars are the locked ones, read offline. */
object ModuleTestLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("law")

  def spec = suite("module test laws")(
    test("a test main that returns is a pass, and the compiled class is that main"):
      Workspace
        .work("histos-module-test")
        .flatMap: dir =>
          Machine
            .load(dir)
            .either
            .flatMap:
              case Left(error)    => ZIO.succeed(assertTrue(error.message == ""))
              case Right(machine) =>
                GraphCompile
                  .jdk(machine)
                  .either
                  .flatMap:
                    case Left(error) => ZIO.succeed(assertTrue(error.message == ""))
                    case Right(jdk)  =>
                      ZIO.serviceWithZIO[DependencyLock]: lock =>
                        val main  = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
                        val probe = dir.resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
                        val ran   =
                          for
                            runtimes <- Runtimes.read(machine, None)
                            _        <- Workspace.writeString(main, "object Hello:\n  def ready: Int = 1\n")
                            _        <- Workspace.writeString(
                              probe,
                              "object Probe:\n  def main(args: Array[String]): Unit = ()\n",
                            )
                            collected <- ModuleTest
                              .sessionOn(TestRun(dir, graph, List(lawName), jdk, runtimes, TestMode.Run))
                              .runCollect
                              .either
                            detail   = collected.fold(_.message, _ => "")
                            statuses = collected.toOption.toList.flatMap(chunk =>
                              chunk.collect { case SessionFeed.Test(TestFeed.Done(cell)) => cell.status }.toList
                            )
                            err = collected.toOption.toList.flatMap(chunk =>
                              chunk.collect { case SessionFeed.Test(TestFeed.Err(_, _, _, text)) => text }.toList
                            )
                            noise = if statuses == List(TestStatus.Passed) then "" else err.mkString("\n")
                          yield assertTrue(
                            detail == "",
                            statuses == List(TestStatus.Passed),
                            noise == "",
                          )
                        ran.provideSomeLayer[TestEnvironment & Scope & Resolve](
                          graphEnv(lock, dir.resolve("store"))
                        )
    ,
    test("a test reads its own resource and a main resource"):
      Workspace
        .work("histos-module-resource")
        .flatMap: dir =>
          Machine
            .load(dir)
            .either
            .flatMap:
              case Left(error)    => ZIO.succeed(assertTrue(error.message == ""))
              case Right(machine) =>
                GraphCompile
                  .jdk(machine)
                  .either
                  .flatMap:
                    case Left(error) => ZIO.succeed(assertTrue(error.message == ""))
                    case Right(jdk)  =>
                      ZIO.serviceWithZIO[DependencyLock]: lock =>
                        val main  = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
                        val probe = dir.resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
                        val note  = dir.resolve("src").resolve("main").resolve("resources").resolve("note.txt")
                        val own   = dir.resolve("src").resolve("test").resolve("resources").resolve("probe.txt")
                        val ran   =
                          for
                            runtimes  <- Runtimes.read(machine, None)
                            _         <- Workspace.writeString(main, "object Hello:\n  def ready: Int = 1\n")
                            _         <- Workspace.writeString(probe, resourceProbe)
                            _         <- Workspace.writeString(note, "main")
                            _         <- Workspace.writeString(own, "test")
                            collected <- ModuleTest
                              .sessionOn(TestRun(dir, graph, List(lawName), jdk, runtimes, TestMode.Run))
                              .runCollect
                              .either
                            mainText <- Workspace.readIfPresent(
                              GraphCompile.classesDir(dir, lawName, scala3).resolve("note.txt")
                            )
                            testText <- Workspace.readIfPresent(
                              TestCompile.testClasses(dir, lawName, scala3, Platform.Jvm).resolve("probe.txt")
                            )
                            detail   = collected.fold(_.message, _ => "")
                            statuses = collected.toOption.toList.flatMap(chunk =>
                              chunk.collect { case SessionFeed.Test(TestFeed.Done(cell)) => cell.status }.toList
                            )
                            err = collected.toOption.toList.flatMap(chunk =>
                              chunk.collect { case SessionFeed.Test(TestFeed.Err(_, _, _, text)) => text }.toList
                            )
                            noise     = if statuses == List(TestStatus.Passed) then "" else err.mkString("\n")
                            mainFound = mainText.getOrElse("")
                            testFound = testText.getOrElse("")
                          yield assertTrue(
                            detail == "",
                            statuses == List(TestStatus.Passed),
                            noise == "",
                            mainFound == "main",
                            testFound == "test",
                          )
                        ran.provideSomeLayer[TestEnvironment & Scope & Resolve](
                          graphEnv(lock, dir.resolve("store"))
                        ),
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private val resourceProbe =
    """object Probe:
      |  def main(args: Array[String]): Unit =
      |    def text(name: String): String =
      |      Option(Probe.getClass.getResourceAsStream(name)) match
      |        case Some(stream) =>
      |          try new String(stream.readAllBytes, java.nio.charset.StandardCharsets.UTF_8)
      |          finally stream.close()
      |        case None =>
      |          sys.error("missing " + name)
      |    val note  = text("/note.txt")
      |    val probe = text("/probe.txt")
      |    if note != "main" || probe != "test" then sys.error(note + " " + probe)
      |""".stripMargin

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      test = Some(TestMain("Probe")),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells = LockedCell(lawName, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile) :: lock.cells)

  private def graphEnv(lock: DependencyLock, store: Path) =
    ResolutionService.pinned(pinned(lock)) ++
      CompileMemo.live ++
      Compilers.live(1) ++
      ZLayer.succeed(ContentStore.in(store))

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
end ModuleTestLawSpec
