package histos

import histos.cache.ContentStore
import histos.native.NativeOrigin
import histos.resolve.{Resolve, ResolveError}
import histos.testrun.{ModuleTest, ModuleTestError, TestFeed, TestMode, TestRun}
import histos.workspace.{IoFailure, Workspace}
import histos.zinc.{CompileFeed, Compiler, GraphCompile, GraphCompileError}

import java.nio.file.Path

import zio.{Chunk, Ref, Scope, UIO, ZIO, ZLayer}
import zio.stream.ZStream

/** One content store and the checkouts that share it. Not [[histos.zinc.CompileReplay.processStore]]: that store is one
  * directory for the test JVM.
  */
object Scenario:
  def open: ZIO[Scope, IoFailure, Store] =
    for
      dir    <- Workspace.work("histos-scenario-cas")
      starts <- Ref.make(0)
    yield Store(ContentStore.in(dir), starts)

/** The store for one scenario. Checkouts are separate work trees. */
final class Store private[histos] (val content: ContentStore, starts: Ref[Int]):
  def opened: UIO[Int] = starts.get

  /** Live Zinc. Each start increments [[opened]]. */
  def compiler: ZLayer[histos.resolve.LockedTools, Nothing, Compiler] =
    Compiler.counted(starts)

  def checkout: ZIO[Scope, IoFailure, Checkout] =
    Workspace.work("histos-scenario").map(root => Checkout(root))
end Store

/** One work tree. `compile` and `test` take their services from the environment. */
final class Checkout private[histos] (val root: Path):
  def write(relative: String, text: String): ZIO[Any, IoFailure, Unit] =
    Workspace.writeString(root.resolve(relative), text)

  def compile(
      graph: ProjectGraph
  ): ZIO[
    histos.resolve.LockedTools & Resolve & ContentStore & Compiler,
    GraphCompileError | ResolveError | IoFailure,
    Chunk[CompileFeed],
  ] =
    ZIO.scoped:
      GraphCompile.jdkHere.flatMap: jdk =>
        ToolFixture
          .streamAt(root, graph)(
            GraphCompile.consoleCounted(root, graph, Nil, jdk, NativeOrigin.Central, None)
          )
          .runCollect

  /** Quick unless `mode` says otherwise. The record is written under this checkout. */
  def test(
      graph: ProjectGraph,
      mode: TestMode = TestMode.Quick,
  ): ZIO[
    histos.resolve.LockedTools & Resolve & ContentStore & Compiler,
    ModuleTestError | ResolveError | IoFailure | GraphCompileError,
    Chunk[TestFeed],
  ] =
    for
      jdk  <- GraphCompile.jdkHere
      here <- TestMachine.runtimes(root)
      asked = TestRun(root, graph, Nil, jdk, here, mode)
      feeds <- ToolFixture
        .streamAt(root, graph)(
          ZStream.unwrap(
            GraphCompile
              .runCounted(root, graph, Nil, jdk, here.native, None)
              .mapError(ModuleTestError.Compile(_))
              .map(compiled => ModuleTest.suites(asked, compiled))
          )
        )
        .runCollect
    yield feeds
end Checkout
