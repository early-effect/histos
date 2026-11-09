package histos.load

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.testrun.TestCompile
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileSession, GraphCompile}
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
}

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A plugin cell compiles its main and its tests. A type error is a compile fault. */
object PluginCompileCellLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("pluglaw")

  def spec = suite("plugin compile cell laws")(
    test("the cell writes main and test classes, and a type error is a compile fault"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-plugin-cell")
          .flatMap: dir =>
            val main  = dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Plug.scala")
            val probe =
              dir.resolve(lawName.value).resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
            val mainClass = GraphCompile.classesDir(dir, lawName, scala3, Platform.Jvm).resolve("Plug.class")
            val testClass = TestCompile.testClasses(dir, lawName, scala3, Platform.Jvm).resolve("Probe.class")
            val cell      = PluginCells.Cell(module, scala3, Platform.Jvm)
            val program   =
              for
                loaded  <- Machine.load(dir).either
                machine <- loaded match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                found <- GraphCompile.jdk(machine).either
                jdk   <- found match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                _ <- Workspace.createDirectories(parentOf(main))
                _ <- Workspace.createDirectories(parentOf(probe))
                _ <- Workspace.writeString(main, "object Plug:\n  def ready: Int = 1\n")
                _ <- Workspace.writeString(probe, "object Probe:\n  def n: Int = Plug.ready\n")
                session = CompileSession(dir, graph, jdk, NativeOrigin.read(machine.env), pinned(lock))
                compiled  <- PluginCells.compile(session, cell).either
                wroteMain <- ZIO.attempt(Files.isRegularFile(mainClass)).orDie
                wroteTest <- ZIO.attempt(Files.isRegularFile(testClass)).orDie
                _         <- Workspace.writeString(main, "object Plug:\n  def ready: Int = \"no\"\n")
                broken    <- PluginCells.compile(session, cell).either
              yield
                val detail = broken.fold(_.message, _ => "")
                assertTrue(
                  compiled == Right(()),
                  wroteMain,
                  wroteTest,
                  detail.contains("'pluglaw' 3.10.0 jvm failed to compile"),
                )
            program.provideSomeLayer[Resolve & Scope](sessionLayer(dir, lock))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def module: Module =
    Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )

  private def graph: ProjectGraph =
    ProjectGraph.trusted(
      List(module),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def sessionLayer(dir: Path, lock: DependencyLock) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      ResolutionService.pinned(pinned(lock))

  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells = LockedCell(lawName, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile) :: lock.cells)

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
end PluginCompileCellLawSpec
