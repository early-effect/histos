package histos.testrun

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers, GraphCompile}
import histos.{
  Dep,
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

import zio.*
import zio.test.*

/** A module with no test main launches the ZIO provider. The jars are the locked ones, read offline. */
object ProviderLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("probelaw")

  def spec = suite("provider laws")(
    test("a zio spec with no test main passes through the provider"):
      Workspace
        .work("histos-provider")
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
                        donor(lock) match
                          case None =>
                            ZIO.dieMessage("central 3.10.0 jvm test cell is not in the lock")
                          case Some(held) =>
                            val main = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
                            val spec = dir.resolve("src").resolve("test").resolve("scala").resolve("Ready.scala")
                            val ran  =
                              for
                                runtimes  <- Runtimes.read(machine, None)
                                _         <- Workspace.writeString(main, hello)
                                _         <- Workspace.writeString(spec, ready)
                                collected <- ModuleTest
                                  .sessionOn(TestRun(dir, graph(held), List(lawName), jdk, runtimes, TestMode.Run))
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
                              graphEnv(pinned(lock, held), dir.resolve("store"))
                            )
    @@ TestAspect.withLiveSystem
  ).provideSomeLayerShared[TestEnvironment & Scope](opened)

  private val hello =
    """object Hello:
      |  def ready: Int = 1
      |""".stripMargin

  private val ready =
    """import zio.test.*
      |
      |object Ready extends ZIOSpecDefault:
      |  def spec = suite("ready")(
      |    test("one")(assertTrue(1 == 1))
      |  )
      |""".stripMargin

  private def graph(held: LockedCell): ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      testDependencies =
        held.directs.map(direct => Dep.trusted(direct.organization, direct.name, direct.revision, direct.kind)),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def donor(lock: DependencyLock): Option[LockedCell] =
    lock.cells.find: cell =>
      cell.module.value == "central" &&
        cell.scope == ClasspathScope.Test &&
        cell.scalaVersion == scala3 &&
        cell.platform == Platform.Jvm

  private def pinned(lock: DependencyLock, held: LockedCell): DependencyLock =
    val compile = LockedCell(lawName, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile)
    val tests   = held.copy(module = lawName)
    lock.copy(cells = compile :: tests :: lock.cells)

  private def graphEnv(lock: DependencyLock, store: Path) =
    ResolutionService.pinned(lock) ++
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
end ProviderLawSpec
