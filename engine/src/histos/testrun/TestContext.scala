package histos.testrun

import histos.jdk.JdkRuntime
import histos.{Module, Platform, ScalaVersion}
import histos.jdk.Sha256
import histos.native.NativeToolchain
import histos.resolve.{LockedTools, ToolInput}
import histos.toolchain.{Runtimes, RuntimeIdentity}
import histos.zinc.CompiledCell
import java.nio.charset.StandardCharsets
import zio.{Chunk, ZIO}

/** Verified compilation and execution inputs checked before a cached test success.
  */
final case class TestContext(
    tools: List[ToolInput],
    runtimes: List[RuntimeIdentity],
):
  def hashes: List[Sha256] =
    tools.map(_.fingerprint) ++ runtimes.map(identity =>
      Sha256.of(
        Chunk.fromArray(
          Runtimes.identityToken(identity).getBytes(StandardCharsets.UTF_8)
        )
      )
    )

object TestContext:
  val empty: TestContext = TestContext(Nil, Nil)

  def make(
      module: Module,
      cell: CompiledCell,
      jdk: JdkRuntime,
      runtimes: Runtimes,
  ): ZIO[LockedTools, ModuleTestError, TestContext] =
    ZIO.serviceWithZIO[LockedTools]: view =>
      val provider                                = module.test.isEmpty
      def compilers(versions: List[ScalaVersion]) =
        versions.distinct.flatMap(version =>
          histos.zinc.CompilerRequests
            .compiler(version, view.repositories) :: Option
            .when(version.value.startsWith("2.13."))(
              histos.zinc.CompilerRequests
                .semanticdb(version, view.catalog, view.repositories)
            )
            .toList
        )
      for
        compilation <- view
          .compilation(module, cell.version, cell.platform, runtimes.native)
          .mapError(ModuleTestError.Lock(_))
        requests <- cell.platform match
          case Platform.Jvm => ZIO.succeed(Nil)
          case Platform.Js  =>
            val revision = module.jsVersion.getOrElse(view.catalog.js.revision)
            val selected = if provider then
              List(
                histos.js.JsRequests.suite(
                  revision,
                  module.jsEnv,
                  view.catalog,
                  view.repositories,
                ),
                histos.js.JsRequests
                  .testBridge(revision, view.catalog, view.repositories),
              )
            else
              List(
                histos.js.JsRequests.linker(
                  revision,
                  view.catalog.js.bridgeScala,
                  view.repositories,
                ),
                histos.js.JsRequests.environment(
                  module.jsEnv,
                  view.catalog,
                  view.repositories,
                ),
              )
            ZIO.succeed(
              selected ++ compilers(
                List(
                  view.catalog.js.bridgeScala,
                  histos.js.JsRequests.compiler(module.jsEnv, view.catalog),
                )
              )
            )
          case Platform.Native =>
            ZIO
              .fromEither(
                NativeToolchain.of(
                  module.nativeVersion.getOrElse(view.catalog.native.revision),
                  runtimes.native,
                )
              )
              .mapError(error =>
                ModuleTestError.Tool(
                  cell.module,
                  cell.version,
                  histos.toolchain.PlatformToolError.NativeOrigin(error),
                )
              )
              .map(toolchain =>
                List(
                  histos.native.NativeRequests
                    .linker(toolchain, view.catalog, view.repositories)
                ) ++ Option
                  .when(provider)(
                    histos.native.NativeRequests
                      .suite(toolchain, view.catalog, view.repositories)
                  )
                  .toList ++ compilers(List(view.catalog.native.bridgeScala))
              )
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
