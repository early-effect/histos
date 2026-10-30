package histos.zinc

import histos.cache.ContentStore
import histos.resolve.LockedTools

import zio.{Ref, UIO, ZIO, ZLayer}

/** The live compiler, plus the pool's opening count. [[Compiler]] does not expose that count. */
object PoolCount:
  final class Token private[zinc] (pool: CompilerPool):
    def openings: UIO[Int] = pool.openings

  def layer(
      starts: Ref[Int],
      token: Ref[Option[Token]],
  ): ZLayer[LockedTools, Nothing, Compiler] =
    ZLayer.scoped:
      CompilerPool.make.flatMap: pool =>
        val inner = ZincCompiler(pool)
        token.set(Some(Token(pool))).as {
          new Compiler:
            def compile(call: CompileCall): ZIO[ContentStore, CompileError, CompileOutput] =
              starts.update(_ + 1) *> inner.compile(call)
        }
end PoolCount
