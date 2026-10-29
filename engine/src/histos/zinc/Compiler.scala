package histos.zinc

import histos.cache.ContentStore
import histos.jdk.Sha256
import histos.resolve.{LockedTools, ToolInput}

import java.nio.file.Path

import zio.{IO, Ref, ZIO, ZLayer}

/** One invocation of the Scala compiler. The key is already decided. A hit never builds one of these. */
final case class CompileCall(
    request: CompileRequest,
    runtime: List[Path],
    compiler: List[Path],
    bridge: Path,
    inputs: List[ToolInput],
    extra: List[String],
    hear: CompileProblem => Unit,
    key: Sha256,
    rows: List[CompileRow],
)

/** The compiler `GraphCompile` calls after a cache miss. The live layer is Zinc. */
trait Compiler:
  def compile(call: CompileCall): ZIO[ContentStore, CompileError, CompileOutput]

object Compiler:
  val live: ZLayer[LockedTools, Nothing, Compiler] =
    ZLayer.scoped(CompilerPool.make.map(pool => ZincCompiler(pool)))

  /** Live Zinc, counting each `compile`. The ref is the start count. */
  def counted(count: Ref[Int]): ZLayer[LockedTools, Nothing, Compiler] =
    ZLayer.scoped {
      CompilerPool.make.map: pool =>
        val inner = ZincCompiler(pool)
        new Compiler:
          def compile(call: CompileCall): ZIO[ContentStore, CompileError, CompileOutput] =
            count.update(_ + 1) *> inner.compile(call)
    }

  private[zinc] def retain(
      store: ContentStore,
      call: CompileCall,
      done: CompileOutput,
  ): IO[CompileError, Unit] =
    CompileReplay.save(store, done.key, call.request) *>
      CompileInputs.remember(store, call.request.analysis, call.rows)
end Compiler

private[zinc] final class ZincCompiler(pool: CompilerPool) extends Compiler:
  def compile(call: CompileCall): ZIO[ContentStore, CompileError, CompileOutput] =
    pool
      .compile(
        call.request,
        CompilerArtifacts(call.runtime, call.compiler, call.bridge, call.inputs),
        call.extra,
        call.hear,
        call.key,
      )
      .tap: done =>
        ZIO.serviceWithZIO[ContentStore](store => Compiler.retain(store, call, done))
