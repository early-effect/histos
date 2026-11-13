package histos.native

import histos.zinc.{CellPlatform, CompileError, CompileOutput, CompileProblem, CompileRequest, JvmCompile}
import histos.Platform
import histos.resolve.LockedTools

import zio.ZIO

/** Compile a Native cell with that revision's compiler plugin. Linking is separate. The engine does not load the
  * plugin. The compiler keeps its own `scala-library`. A resolved copy of that jar, and a resolved native library at
  * any other revision, is dropped. The toolchain libraries are already first. The key's native inputs come from
  * [[histos.zinc.CellTools]], as recall's do. This fetch is for their paths.
  */
object NativeCompile:
  def compile(
      request: CompileRequest,
      native: CellPlatform.Native,
      extra: List[String] = Nil,
      hear: CompileProblem => Unit = _ => (),
  ): ZIO[
    LockedTools & histos.cache.ContentStore & histos.zinc.CompilerStarts & histos.schedule.RemoteCache,
    CompileError,
    CompileOutput,
  ] =
    if request.toolchain.platform != Platform.Native then
      ZIO.fail(CompileError.UnsupportedPlatform(request.toolchain.platform))
    else
      for
        libs <- NativeClasspath
          .fetch(native.toolchain, Some(request.toolchain.version))
          .mapError(CompileError.NativeFetch(_))
        compiled <- JvmCompile.compilePrepared(
          request.copy(
            classpath = libs.libraries ++ NativeLink
              .withoutLibraries(request.classpath, libs.libraries)
              .filterNot(JvmCompile.isStandardLibrary)
          ),
          native,
          List(libs.plugin),
          extra,
          hear,
        )
      yield compiled
