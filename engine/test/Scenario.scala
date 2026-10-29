package histos

import histos.cache.ContentStore
import histos.native.NativeOrigin
import histos.resolve.{Resolve, ResolveError}
import histos.testrun.{ModuleTest, ModuleTestError, TestFeed, TestMode, TestRun}
import histos.workspace.{IoFailure, Workspace}
import histos.zinc.{CompileFeed, Compiler, GraphCompile, GraphCompileError}

import java.nio.file.Path

import zio.{Chunk, Ref, Scope, UIO, ZIO, ZLayer}

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
final class Store private[histos] (content: ContentStore, starts: Ref[Int]):
  def opened: UIO[Int] = starts.get

  def checkout: ZIO[Scope, IoFailure, Checkout] =
    Workspace.work("histos-scenario").map(root => Checkout(root, content, starts))
end Store

/** One work tree. `compile` and `test` use the scenario store. */
final class Checkout private[histos] (
    val root: Path,
    content: ContentStore,
    starts: Ref[Int],
):
  def write(relative: String, text: String): ZIO[Any, IoFailure, Unit] =
    Workspace.writeString(root.resolve(relative), text)

  private def base: ZLayer[histos.resolve.LockedTools, Nothing, Resolve & ContentStore] =
    ZLayer.fromZIO(ZIO.serviceWith[histos.resolve.LockedTools](_.resolve)) ++
      ZLayer.succeed(content)

  def compile(
      graph: ProjectGraph
  ): ZIO[histos.resolve.LockedTools, GraphCompileError | ResolveError | IoFailure, Chunk[CompileFeed]] =
    ZIO.scoped:
      GraphCompile.jdkHere.flatMap: jdk =>
        ToolFixture
          .streamAt(root, graph)(
            GraphCompile.consoleCounted(root, graph, Nil, jdk, NativeOrigin.Central, None)
          )
          .runCollect
          .provideSomeLayer[histos.resolve.LockedTools & Scope](base ++ Compiler.counted(starts))

  /** Quick unless `mode` says otherwise. The record is written under this checkout. */
  def test(
      graph: ProjectGraph,
      mode: TestMode = TestMode.Quick,
  ): ZIO[
    histos.resolve.LockedTools,
    ModuleTestError | ResolveError | IoFailure | GraphCompileError,
    Chunk[TestFeed],
  ] =
    for
      jdk   <- GraphCompile.jdkHere
      here  <- TestMachine.runtimes(root)
      feeds <- ToolFixture
        .streamAt(root, graph)(
          ModuleTest.console(TestRun(root, graph, Nil, jdk, here, mode))
        )
        .runCollect
        .provideSomeLayer[histos.resolve.LockedTools](base)
    yield feeds
end Checkout
