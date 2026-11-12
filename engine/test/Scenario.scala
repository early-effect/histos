package histos

import histos.cache.ContentStore
import histos.clean.Products
import histos.jdk.{Machine, Sha256}
import histos.program.{Program, ProgramError, ProgramFeed}
import histos.resolve.{BuildResolvers, DependencyLock, LockedCell, ResolutionService, Resolve, ResolveError}
import histos.schedule.{CacheError, Parallelism}
import histos.testrun.{ModuleTest, ModuleTestError, TestFeed, TestMode, TestRun}
import histos.toolchain.Runtimes
import histos.workspace.{IoFailure, Workspace}
import histos.zinc.{CompileFeed, CompileMemo, CompilerStarts, Compilers, GraphCompile, GraphCompileError}

import java.nio.file.Path

import zio.{Chunk, IO, Ref, Scope, UIO, ZEnvironment, ZIO, ZLayer}
import zio.stream.ZStream

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

  /** Delete every blob in the scenario store. A compile that still needs one after this starts the compiler. */
  def emptyStore: IO[CacheError | IoFailure, Unit] =
    env.get[ContentStore].list.flatMap(entries => ZIO.foreachDiscard(entries)(entry => Workspace.delete(entry.path)))

  /** A work tree of `build`. The lock `build` reads pins every cell with no jars, plus this repository's tools. A
    * module that names dependencies drifts from that pin. The checkout keeps its compilers and its input memo for the
    * scope, the way that checkout's server keeps them between commands.
    */
  def checkout(build: ProjectGraph): ZIO[Scope, IoFailure, Checkout] =
    for
      root <- Workspace.work("histos-scenario")
      _    <- Workspace.writeString(ResolutionService.file(root, build.resolution), DependencyLock.render(pin(build)))
      resident <- (Compilers.live(Parallelism.default.cpu) ++ CompileMemo.live).build
    yield Checkout(root, build, env ++ resident)

  private def pin(build: ProjectGraph): DependencyLock =
    val cells = Resolve
      .cells(build, BuildResolvers(build.resolvers))
      .map(cell => LockedCell(cell.module, cell.scalaVersion, cell.platform, Nil, Nil, Nil, cell.scope))
    DependencyLock(cells, tools = tools.tools)
end Scenario

object Scenario:
  /** The store dies with the scope. Every checkout pins the compiler tools of [[CompilerClosure.lock]]. */
  def open: ZIO[Scope & Resolve & CompilerClosure, IoFailure, Scenario] =
    for
      dir     <- Workspace.work("histos-scenario-cas")
      count   <- Ref.make(0)
      resolve <- ZIO.service[Resolve]
      closure <- ZIO.service[CompilerClosure]
    yield Scenario(
      ZEnvironment[Resolve, ContentStore, CompilerStarts](resolve, ContentStore.in(dir), CompilerStarts(count)),
      closure.lock,
    )

/** One work tree of one build. `compile` and `test` run it against the scenario store. */
final class Checkout private[histos] (
    val root: Path,
    build: ProjectGraph,
    env: ZEnvironment[Resolve & ContentStore & CompilerStarts & Compilers & CompileMemo],
):
  def write(relative: String, text: String): IO[IoFailure, Unit] =
    Workspace.writeString(root.resolve(relative), text)

  /** `./histos clean`: this work tree's products go, the server drops its memo and compilers, and the scenario store
    * stays.
    */
  def clean: IO[IoFailure, Unit] =
    Products.wipe(root) *> env.get[CompileMemo].forget *> env.get[Compilers].drop

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

  /** The module's main, compiled and started, with each feed as the program produces it. */
  def run(name: String): ZStream[Any, ProgramError | GraphCompileError | ResolveError, ProgramFeed] =
    ZStream.unwrap:
      for
        machine  <- here
        jdk      <- GraphCompile.jdk(machine)
        runtimes <- Runtimes.read(machine, None)
        lock     <- Resolve.readLock(root)
      yield Program
        .console(root, build, name, jdk, runtimes, None, _ => ZIO.unit, ZIO.unit)
        .provideLayer(ZLayer.succeedEnvironment(env) >+> ResolutionService.pinned(lock))

  /** Needs `TestAspect.withLiveSystem`. */
  private def here: IO[GraphCompileError, Machine] =
    Machine.load(root).mapError(GraphCompileError.Runtime(_))
end Checkout
