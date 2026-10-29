package histos.testrun

import histos.jdk.{JdkRuntime, Sha256}
import histos.process.Process
import histos.resolve.{LockedTools, ResolveError, ToolInput}
import histos.toolchain.{RuntimeIdentity, Runtimes}
import histos.zinc.CompiledCell
import histos.{Module, Platform}

import zio.ZIO

/** Verified compilation and execution inputs checked before a cached test success.
  */
final case class TestContext(
    tools: List[ToolInput],
    runtimes: List[RuntimeIdentity],
):
  def hashes: List[Sha256] =
    tools.map(_.fingerprint) ++ runtimes.map(SuiteCover.tokenHash)

object TestContext:
  val empty: TestContext = TestContext(Nil, Nil)

  def make(
      module: Module,
      cell: CompiledCell,
      jdk: JdkRuntime,
      runtimes: Runtimes,
  ): ZIO[LockedTools & Process, ModuleTestError, TestContext] =
    ZIO.serviceWithZIO[LockedTools]: view =>
      for
        compilation <- view
          .compilation(module, cell.version, cell.platform, runtimes.native)
          .mapError(ModuleTestError.Lock(_))
        requests <- ZIO
          .fromEither(
            LockedTools.executionRequests(
              module,
              cell.platform,
              runtimes.native,
              view.catalog,
              view.repositories,
            )
          )
          .mapError:
            case ResolveError.NativeOrigin(error) =>
              ModuleTestError.Tool(
                cell.module,
                cell.version,
                histos.toolchain.PlatformToolError.NativeOrigin(error),
              )
            case other => ModuleTestError.Lock(other)
        execution <- ZIO
          .foreach(requests.distinct)(view.selected)
          .mapError(ModuleTestError.Lock(_))
        node <-
          if cell.platform == Platform.Js then
            runtimes.nodeRuntime
              .map(runtime => List(runtime.identity))
              .mapError(error => ModuleTestError.NoNode(cell.module, cell.version, error))
          else ZIO.succeed(Nil)
      yield TestContext(
        (compilation ++ execution).distinctBy(_.id),
        jdk.identity :: node,
      )
end TestContext
