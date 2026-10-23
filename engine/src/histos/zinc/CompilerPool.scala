package histos.zinc

import java.net.URLClassLoader

import scala.util.control.NonFatal

import sbt.internal.inc.{ScalaInstance, ZincUtil}
import xsbti.compile.{ClasspathOptionsUtil, CompilerCache, Compilers, GlobalsCache}

import zio.{IO, Ref, Scope, Semaphore, UIO, ZIO}

/** One Scala compiler, reused for a toolchain. The pool closes its class loaders.
  * A keyed semaphore, not `ZKeyedPool`: that pool's shrink timer waits on the test clock.
  */

private[zinc] final class Prepared(
    val compilers: Compilers,
    val cache: GlobalsCache,
    val bridge: java.nio.file.Path,
    val runtime: List[java.nio.file.Path],
    val compilerJars: List[java.nio.file.Path],
    loaders: List[URLClassLoader],
    val id: Int,
):
  private[zinc] def close(): Unit =
    loaders.foreach(loader => try loader.close() catch case NonFatal(_) => ())

/** Version, JDK, and the jars the compiler itself loads. The project's classpath is not part of the key. */
private[zinc] final case class CompilerKey(
    version: String,
    jdk: String,
    runtime: List[String],
    compiler: List[String],
    bridge: String,
)

private[zinc] final class CompilerPool(
    made: Ref[Int],
    held: Ref[Map[CompilerKey, Prepared]],
    permits: Ref[Map[CompilerKey, Semaphore]],
    guard: Semaphore,
):
  def openings: UIO[Int] = made.get

  def compile(
      request: CompileRequest,
      runtime: List[java.nio.file.Path],
      compilerJars: List[java.nio.file.Path],
      bridge: java.nio.file.Path,
      extra: List[String],
      hear: CompileProblem => Unit,
      digest: histos.jdk.Sha256,
  ): IO[CompileError, CompileOutput] =
    val id = CompilerKey(
      request.toolchain.version.value,
      request.toolchain.jdk.home.toString,
      runtime.map(_.toString),
      compilerJars.map(_.toString),
      bridge.toString,
    )
    permit(id).flatMap: sem =>
      sem.withPermit:
        prepared(id).flatMap: ready =>
          ZIO
            .attemptBlockingInterrupt(JvmCompile.once(request, ready, extra, hear, digest))
            .mapError(_ => CompileError.Io(CompileIo.Compile))
            .flatMap(ZIO.fromEither)

  private def prepared(id: CompilerKey): IO[CompileError, Prepared] =
    held.get.flatMap: have =>
      have.get(id) match
        case Some(found) => ZIO.succeed(found)
        case None =>
          CompilerPool.open(id).tap: created =>
            held.update(_.updated(id, created)) *> made.update(_ + 1)

  private def permit(id: CompilerKey): UIO[Semaphore] =
    permits.get.flatMap: have =>
      have.get(id) match
        case Some(found) => ZIO.succeed(found)
        case None =>
          guard.withPermit:
            permits.get.flatMap: again =>
              again.get(id) match
                case Some(found) => ZIO.succeed(found)
                case None =>
                  Semaphore.make(1).tap(created => permits.update(_.updated(id, created)))

private[zinc] object CompilerPool:
  def make: ZIO[Scope, Nothing, CompilerPool] =
    for
      made <- Ref.make(0)
      held <- Ref.make(Map.empty[CompilerKey, Prepared])
      permits <- Ref.make(Map.empty[CompilerKey, Semaphore])
      guard <- Semaphore.make(1)
      _ <- ZIO.addFinalizer(held.get.flatMap(have => ZIO.foreachDiscard(have.values)(item => ZIO.succeed(item.close()))))
    yield CompilerPool(made, held, permits, guard)

  private[zinc] def open(key: CompilerKey): IO[CompileError, Prepared] =
    val version = histos.ScalaVersion.trusted(key.version)
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala3) =>
        Scala3.jars(version).flatMap(toolchain => ZIO.attempt(scala3(key, toolchain)).mapError(failed))
      case Some(ScalaSeries.Scala2(13)) =>
        Scala213.jars(version).flatMap(toolchain => ZIO.attempt(scala213(key, toolchain)).mapError(failed))
      case _ =>
        ZIO.fail(CompileError.UnsupportedScala(version))

  private def failed(thrown: Throwable): CompileError =
    val text = Option(thrown.getMessage).filter(_.nonEmpty).getOrElse(thrown.getClass.getName)
    CompileError.Failed(List(CompileDiagnostic.fromCompiler(text)))

  private def scala3(key: CompilerKey, toolchain: Scala3.Toolchain): Prepared =
    val loader = toolchain.openLoader(getClass.getClassLoader)
    val library = libraryLoader(key.runtime)
    val instance = instanceOf(key, loader, library)
    val options = ClasspathOptionsUtil.noboot(key.version)
    val scalac = ZincUtil.scalaCompiler(instance, toolchain.bridgeJar, options)
    val zinc = new sbt.internal.inc.IncrementalCompilerImpl
    Prepared(
      zinc.compilers(instance, options, Some(java.nio.file.Path.of(key.jdk)), scalac),
      CompilerCache.fresh,
      toolchain.bridgeJar,
      key.runtime.map(java.nio.file.Path.of(_)),
      key.compiler.map(java.nio.file.Path.of(_)),
      List(loader, library),
      Prepared.next,
    )

  private def scala213(key: CompilerKey, toolchain: Scala213.Toolchain): Prepared =
    val loader = toolchain.openLoader()
    val library = libraryLoader(key.runtime)
    val instance = instanceOf(key, loader, library)
    val options = ClasspathOptionsUtil.noboot(key.version)
    val scalac = ZincUtil.scalaCompiler(instance, toolchain.bridgeJar, options)
    val zinc = new sbt.internal.inc.IncrementalCompilerImpl
    Prepared(
      zinc.compilers(instance, options, Some(java.nio.file.Path.of(key.jdk)), scalac),
      CompilerCache.fresh,
      toolchain.bridgeJar,
      key.runtime.map(java.nio.file.Path.of(_)),
      key.compiler.map(java.nio.file.Path.of(_)),
      List(loader, library),
      Prepared.next,
    )

  private def libraryLoader(jars: List[String]): URLClassLoader =
    new URLClassLoader(jars.map(path => java.nio.file.Path.of(path).toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)

  private def instanceOf(key: CompilerKey, compiler: ClassLoader, library: ClassLoader): ScalaInstance =
    val runtime = key.runtime.map(java.nio.file.Path.of(_).toFile).toArray
    val jars = key.compiler.map(java.nio.file.Path.of(_).toFile).toArray
    new ScalaInstance(key.version, compiler, compiler, library, runtime, jars, runtime ++ jars, Some(key.version))

private[zinc] object Prepared:
  private val ids = new java.util.concurrent.atomic.AtomicInteger
  def next: Int = ids.incrementAndGet()
