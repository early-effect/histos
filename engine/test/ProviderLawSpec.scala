package histos.testrun

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers, GraphCompile}
import histos.{
  CompilerClosure,
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

import zio.*
import zio.test.*

/** A module with no test main launches the ZIO provider. The jars are the locked ones, read offline. The test cell is
  * the one this checkout locks for `central`, so its zio-test jars are the ones `./histos resolve` fetched.
  */
object ProviderLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("probelaw")

  def spec = suite("provider laws")(
    test("a zio spec with no test main passes through the provider"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-provider")
          .flatMap: dir =>
            val main = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val spec = dir.resolve("src").resolve("test").resolve("scala").resolve("Ready.scala")
            for
              held <- ZIO
                .fromOption(donor(closure.lock, scala3))
                .orElse(ZIO.dieMessage(s"central ${scala3.value} jvm test cell is not in the lock"))
              machine   <- Machine.load(dir)
              jdk       <- GraphCompile.jdk(machine)
              runtimes  <- Runtimes.read(machine, None)
              _         <- Workspace.writeString(main, hello)
              _         <- Workspace.writeString(spec, ready)
              collected <- ModuleTest
                .sessionOn(TestRun(dir, graph(scala3, held), List(lawName), jdk, runtimes, TestMode.Run))
                .runCollect
                .provideSomeLayer[TestEnvironment & Scope & Resolve](
                  ResolutionService.pinned(pinned(closure, held)) ++
                    CompileMemo.live ++
                    Compilers.live(1) ++
                    ZLayer.succeed(ContentStore.in(dir.resolve("store")))
                )
            yield
              val statuses = collected.collect { case SessionFeed.Test(TestFeed.Done(cell)) => cell.status }.toList
              val err      = collected.collect { case SessionFeed.Test(TestFeed.Err(_, _, _, text)) => text }.toList
              // A failed run shows its stderr; a JVM warning beside a pass is not a failure.
              val noise = if statuses == List(TestStatus.Passed) then Nil else err
              assertTrue(statuses == List(TestStatus.Passed), noise == Nil)
    @@ TestAspect.withLiveSystem
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer)

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

  private def graph(scala3: ScalaVersion, held: LockedCell): ProjectGraph =
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

  private def donor(lock: DependencyLock, scala3: ScalaVersion): Option[LockedCell] =
    lock.cells.find: cell =>
      cell.module.value == "central" &&
        cell.scope == ClasspathScope.Test &&
        cell.scalaVersion == scala3 &&
        cell.platform == Platform.Jvm

  private def pinned(closure: CompilerClosure, held: LockedCell): DependencyLock =
    val compiled = closure.pinning(lawName)
    compiled.copy(cells = held.copy(module = lawName) :: compiled.cells)
end ProviderLawSpec
