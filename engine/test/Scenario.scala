package histos

import histos.cache.ContentStore
import histos.clean.Products
import histos.jdk.{Machine, Sha256}
import histos.resolve.{BuildResolvers, DependencyLock, LockedCell, ResolutionService, Resolve, ResolveError}
import histos.schedule.CacheError
import histos.testrun.{ModuleTest, ModuleTestError, TestFeed, TestMode, TestRun}
import histos.toolchain.Runtimes
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompileFeed, CompilerStarts, GraphCompile, GraphCompileError}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, Ref, Scope, UIO, ZEnvironment, ZIO, ZLayer}

/** One content store, one compiler-start count, and the checkouts that share them. Not
  * [[histos.zinc.CompileReplay.processStore]]: that store is one directory for the test JVM.
  */
final class Scenario private (
    env: ZEnvironment[Resolve & ContentStore & CompilerStarts],
    tools: DependencyLock,
):
  /** Compiler starts that [[Checkout.compile]] counted. A test run's compiles keep their own count. */
  def starts: UIO[Int] = env.get[CompilerStarts].get

  /** Whether the scenario store holds a blob under `sha`. */
  def holds(sha: Sha256): IO[CacheError, Boolean] =
    env.get[ContentStore].list.map(_.exists(_.digest.value == sha.value))

  /** A work tree of `build`. The lock `build` reads pins every cell with no jars, plus this repository's tools. A
    * module that names dependencies drifts from that pin.
    */
  def checkout(build: ProjectGraph): ZIO[Scope, IoFailure, Checkout] =
    for
      root <- Workspace.work("histos-scenario")
      _    <- Workspace.writeString(ResolutionService.file(root, build.resolution), DependencyLock.render(pin(build)))
    yield Checkout(root, build, env)

  private def pin(build: ProjectGraph): DependencyLock =
    val cells = Resolve
      .cells(build, BuildResolvers(build.resolvers))
      .map(cell => LockedCell(cell.module, cell.scalaVersion, cell.platform, Nil, Nil, Nil, cell.scope))
    DependencyLock(cells, tools = tools.tools)
end Scenario

object Scenario:
  /** The store dies with the scope. The lock in the environment names the compiler tools every checkout pins. */
  def open: ZIO[Scope & Resolve & DependencyLock, IoFailure, Scenario] =
    for
      dir     <- Workspace.work("histos-scenario-cas")
      count   <- Ref.make(0)
      resolve <- ZIO.service[Resolve]
      tools   <- ZIO.service[DependencyLock]
    yield Scenario(
      ZEnvironment[Resolve, ContentStore, CompilerStarts](resolve, ContentStore.in(dir), CompilerStarts(count)),
      tools,
    )

  /** Jars from the coursier cache, never the network, and the lock of the repository the test runs in. */
  val tools: ZLayer[Any, ResolveError | IoFailure, Resolve & DependencyLock] =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++
      ZLayer.fromZIO(repository.flatMap(Resolve.readLock))

  /** The nearest directory with a build file and a lock. The test JVM may run below the repository root. */
  private def repository: IO[IoFailure, Path] =
    val here = Path.of("").toAbsolutePath.normalize
    ZIO
      .attemptBlocking(
        Iterator
          .unfold(Option(here))(dir => dir.map(found => (found, Option(found.getParent))))
          .find(dir => Files.isRegularFile(Resolve.lockPath(dir)) && Files.isRegularFile(dir.resolve("histos.scala")))
          .getOrElse(here)
      )
      .mapError(_ => IoFailure(here, IoFault.Stat))
end Scenario

/** One work tree of one build. `compile` and `test` run it against the scenario store. */
final class Checkout private[histos] (
    val root: Path,
    build: ProjectGraph,
    env: ZEnvironment[Resolve & ContentStore & CompilerStarts],
):
  def write(relative: String, text: String): IO[IoFailure, Unit] =
    Workspace.writeString(root.resolve(relative), text)

  /** `./histos clean`: this work tree's products go and the scenario store stays. */
  def clean: IO[IoFailure, Unit] =
    Products.wipe(root)

  /** Every module of the build. Starts count toward [[Scenario.starts]]. */
  def compile: IO[GraphCompileError, Chunk[CompileFeed]] =
    for
      machine <- here
      jdk     <- GraphCompile.jdk(machine)
      feeds   <- GraphCompile
        .consoleCounted(root, build, Nil, jdk, machine.native, None)
        .runCollect
        .provideEnvironment(env)
    yield feeds

  /** Every module's tests. A quick record is written under this work tree. */
  def test(mode: TestMode): IO[ModuleTestError | GraphCompileError, Chunk[TestFeed]] =
    for
      machine  <- here
      jdk      <- GraphCompile.jdk(machine)
      runtimes <- Runtimes.read(machine, None)
      feeds    <- ModuleTest
        .console(TestRun(root, build, Nil, jdk, runtimes, mode))
        .runCollect
        .provideEnvironment(env)
    yield feeds

  /** Needs `TestAspect.withLiveSystem`. */
  private def here: IO[GraphCompileError, Machine] =
    Machine.load(root).mapError(GraphCompileError.Runtime(_))
end Checkout
