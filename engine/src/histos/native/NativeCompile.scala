package histos.native

import histos.zinc.{CompileError, CompileOutput, CompileProblem, CompileRequest, JvmCompile}
import histos.{NativeVersion, Platform}

import zio.{IO, ZIO}

/** Compile a Native cell with that revision's compiler plugin. Linking is separate. The engine does not load the plugin. */
object NativeCompile:
  def compile(
      request: CompileRequest,
      revision: NativeVersion,
      origin: NativeOrigin,
      extra: List[String] = Nil,
      hear: CompileProblem => Unit = _ => (),
  ): IO[CompileError, CompileOutput] =
    if request.toolchain.platform != Platform.Native then ZIO.fail(CompileError.UnsupportedPlatform(request.toolchain.platform))
    else if request.toolchain.version != JvmCompile.Scala390 then
      ZIO.fail(CompileError.NativeLibrary(s"native compile uses the Scala ${JvmCompile.Scala390.value} compiler plugin, not ${request.toolchain.version.value}"))
    else
      for
        toolchain <- ZIO.fromEither(NativeToolchain.of(revision, origin)).mapError(error => CompileError.NativeLibrary(error.message))
        libs <- NativeClasspath.fetch(toolchain).mapError(error => CompileError.NativeLibrary(error.message))
        compiled <- JvmCompile.compilePrepared(
                      request.copy(classpath = libs.libraries ++ request.classpath),
                      List(libs.plugin),
                      extra,
                      hear,
                    )
      yield compiled
