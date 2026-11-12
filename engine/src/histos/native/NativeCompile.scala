package histos.native

import histos.zinc.{CompileError, CompileOutput, CompileProblem, CompileRequest, JvmCompile}
import histos.{NativeVersion, Platform}
import histos.resolve.LockedTools

import zio.{IO, ZIO}

/** Compile a Native cell with that revision's compiler plugin. Linking is separate. The engine does not load the
  * plugin. The compiler keeps its own `scala-library`. A resolved copy of that jar, and a resolved native library at
  * any other revision, is dropped. The toolchain libraries are already first.
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
            classpath = libs.libraries ++ NativeLink
              .withoutLibraries(request.classpath, libs.libraries)
              .filterNot(JvmCompile.isStandardLibrary),
            compilationTools = request.compilationTools :+ libs.input,
            compilerPlugins =
              request.compilerPlugins :+ NativeClasspath.plugin(libs.input, toolchain, request.toolchain.version),
          ),
          List(libs.plugin),
          extra,
          hear,
        )
      yield compiled
