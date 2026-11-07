package histos.testrun

import histos.cache.ContentStore
import histos.coverage.{CoverageCompile, Excludes, Flags, Gather}
import histos.jdk.Machine
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileFeed, CompileMemo, Compilers, GraphCompile}
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
  TestMain,
  ToolRevision,
}

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** A coverage run finishes an upstream cell's tests before a dependent cell compiles. The dependent's test compile
  * restores measurement files. That restore must see the hits the upstream tests already wrote.
  */
object CoverageOrderLawSpec extends ZIOSpecDefault:
  private val scala3   = ScalaVersion("3.10.0")
  private val coreName = ProjectName.trusted("core")
  private val appName  = ProjectName.trusted("app")

  def spec = suite("coverage order laws")(
    test("a dependent cell compiles only after the upstream tests have recorded"):
      Workspace
        .work("histos-coverage-order")
        .flatMap: dir =>
          val marker = dir.resolve("core-tested")
          for
            saw      <- Ref.make(false)
            machine  <- Machine.load(dir)
            jdk      <- GraphCompile.jdk(machine)
            lock     <- ZIO.service[DependencyLock]
            runtimes <- Runtimes.read(machine, None)
            _        <- write(dir, "core", "main", "Core.scala", "object Core:\n  def ready: Int = 1\n")
            _        <- write(
              dir,
              "core",
              "test",
              "Probe.scala",
              s"""object Probe:
                 |  def main(args: Array[String]): Unit =
                 |    val n = Core.ready
                 |    java.nio.file.Files.writeString(java.nio.file.Path.of("${marker}"), n.toString)
                 |""".stripMargin,
            )
            _ <- write(dir, "app", "main", "AppMain.scala", "object AppMain:\n  def ready: Int = 1\n")
            _ <- write(dir, "app", "test", "Probe.scala", "object Probe:\n  def main(args: Array[String]): Unit = ()\n")
            collected <- ModuleTest
              .sessionOn(
                TestRun(
                  dir,
                  graph,
                  List(coreName, appName),
                  jdk,
                  runtimes,
                  TestMode.Run,
                  coverage = Some(CoverageCompile(Excludes(Nil, Nil), ToolRevision.trusted("plugin"), Nil)),
                ),
                note(saw, marker),
              )
              .runCollect
              .provideSomeLayer[TestEnvironment & Scope & Resolve](graphEnv(lock, dir.resolve("store")))
            found    <- saw.get
            measured <- Gather.read(Flags.classesDir(dir, coreName, scala3, Platform.Jvm))
            rows     = measured._1
            ids      = measured._2
            core     = rows.filter(row => row.source.endsWith("Core.scala") && !row.ignored).map(_.id)
            compiled = collected.collect { case SessionFeed.Compile(CompileFeed.Done(cell)) =>
              cell.module.value
            }.toList
            refused  = collected.collect { case SessionFeed.Compile(CompileFeed.Failed(error)) => error.message }.toList
            statuses = collected.collect { case SessionFeed.Test(TestFeed.Done(cell)) => cell.status }.toList
            err      = collected.collect { case SessionFeed.Test(TestFeed.Err(_, _, _, text)) => text }.toList
            noise    =
              if found && refused.isEmpty && statuses == List(TestStatus.Passed, TestStatus.Passed) &&
                core.exists(ids.contains)
              then ""
              else (refused ++ err).mkString("\n")
          yield assertTrue(
            found,
            refused.isEmpty,
            compiled == List(coreName.value, appName.value),
            statuses == List(TestStatus.Passed, TestStatus.Passed),
            core.nonEmpty,
            core.exists(ids.contains),
            noise == "",
          )
    @@ TestAspect.withLiveSystem
  ).provideSomeLayerShared[TestEnvironment & Scope](opened)

  private def note(saw: Ref[Boolean], marker: Path)(name: ProjectName): UIO[Unit] =
    if name != appName then ZIO.unit
    else ZIO.attemptBlocking(Files.isRegularFile(marker)).orElseSucceed(false).flatMap(found => saw.set(found))

  private def write(dir: Path, module: String, kind: String, name: String, text: String) =
    Workspace.writeString(dir.resolve(module).resolve("src").resolve(kind).resolve("scala").resolve(name), text)

  private def graph: ProjectGraph =
    val core = Module(
      coreName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("core.scala", 1, 1),
      test = Some(TestMain("Probe")),
    )
    val app = Module(
      appName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      List(ProjectDep(coreName, None)),
      SourcePos("app.scala", 1, 1),
      test = Some(TestMain("Probe")),
    )
    ProjectGraph.trusted(
      List(core, app),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def pinned(lock: DependencyLock): DependencyLock =
    val cells = List(coreName, appName).map: name =>
      LockedCell(name, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile)
    lock.copy(cells = cells ++ lock.cells)

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
end CoverageOrderLawSpec
