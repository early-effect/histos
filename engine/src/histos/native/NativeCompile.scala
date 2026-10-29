package histos.native

import histos.zinc.{CompileCall, CompileError, CompileOutput, CompileProblem, CompileRequest, JvmCompile}
import histos.{NativeVersion, Platform}
import histos.resolve.LockedTools

import java.nio.file.Path

import zio.{IO, ZIO}

/** Compile a Native cell with that revision's compiler plugin. Linking is separate. The engine does not load the
  * plugin.
  */
object NativeCompile:
  def compile(
      request: CompileRequest,
      revision: NativeVersion,
      origin: NativeOrigin,
      extra: List[String] = Nil,
      hear: CompileProblem => Unit = _ => (),
  ): ZIO[LockedTools & histos.cache.ContentStore, CompileError, CompileOutput] =
    prepared(request, revision, origin).flatMap: (updated, plugin) =>
      JvmCompile.compilePrepared(updated, List(plugin), extra, hear)

  /** Native libraries and the compiler plugin, ready for a cache check. This does not compile. */
  private[histos] def stage(
      request: CompileRequest,
      revision: NativeVersion,
      origin: NativeOrigin,
      extra: List[String] = Nil,
      hear: CompileProblem => Unit = _ => (),
  ): ZIO[LockedTools, CompileError, CompileCall] =
    prepared(request, revision, origin).flatMap: (updated, plugin) =>
      JvmCompile.stage(updated, List(plugin), extra, hear, None)

  private def prepared(
      request: CompileRequest,
      revision: NativeVersion,
      origin: NativeOrigin,
  ): ZIO[LockedTools, CompileError, (CompileRequest, Path)] =
    if request.toolchain.platform != Platform.Native then
      ZIO.fail(CompileError.UnsupportedPlatform(request.toolchain.platform))
    else
      for
        toolchain <- ZIO
          .fromEither(NativeToolchain.of(revision, origin))
          .mapError(error => CompileError.NativeOrigin(error))
        libs <- NativeClasspath
          .fetch(toolchain, Some(request.toolchain.version))
          .mapError(CompileError.NativeFetch(_))
      yield (
        request.copy(
          classpath = libs.libraries ++ request.classpath,
          compilationTools = request.compilationTools :+ libs.input,
          compilerPlugins =
            request.compilerPlugins :+ libs.input.copy(artifacts = libs.input.artifacts.filter(_.path == libs.plugin)),
        ),
        libs.plugin,
      )
end NativeCompile
