package histos.native

import histos.zinc.{CompileError, CompileOutput, CompileProblem, CompileRequest, JvmCompile}
import histos.{NativeVersion, Platform}
import histos.resolve.LockedTools

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
  ): ZIO[LockedTools & histos.cache.ContentStore & histos.zinc.CompilerStarts, CompileError, CompileOutput] =
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
        compiled <- JvmCompile.compilePrepared(
          request.copy(
            classpath = libs.libraries ++ request.classpath,
            compilationTools = request.compilationTools :+ libs.input,
            compilerPlugins =
              request.compilerPlugins :+ libs.input.copy(artifacts = libs.input.artifacts.filter(_.path == libs.plugin)),
          ),
          List(libs.plugin),
          extra,
          hear,
        )
      yield compiled
