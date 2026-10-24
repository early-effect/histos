package histos.zinc

import histos.resolve.LockedTools
import histos.workspace.{IoFailure, IoFault}

import java.net.URLClassLoader
import java.nio.file.Path

import sbt.internal.inc.{ScalaInstance, ZincUtil}
import xsbti.compile.{
  ClasspathOptionsUtil,
  CompilerCache,
  Compilers,
  GlobalsCache
}

import zio.{IO, Ref, Scope, Semaphore, UIO, ZIO}

/** One Scala compiler, reused for a toolchain. The pool closes its class
  * loaders. A keyed semaphore, not `ZKeyedPool`: that pool's shrink timer waits
  * on the test clock.
  */

private[zinc] final class Prepared(
    val compilers: Compilers,
    val cache: GlobalsCache,
    val bridge: java.nio.file.Path,
    val runtime: List[java.nio.file.Path],
    val compilerJars: List[java.nio.file.Path],
    val id: Int,
    val toolFingerprint: String
)

/** Version, JDK, and the jars the compiler itself loads. The project's
  * classpath is not part of the key.
  */
private[zinc] final case class CompilerKey(
    version: histos.ScalaVersion,
    jdk: Path,
    runtime: List[Path],
    compiler: List[Path],
    bridge: Path,
    hashes: List[histos.jdk.Sha256],
    jdkIdentity: histos.toolchain.RuntimeIdentity
)

private[zinc] final case class CompilerArtifacts(
    runtime: List[Path],
    compiler: List[Path],
    bridge: Path,
    inputs: List[histos.resolve.ToolInput]
)

private[zinc] final class CompilerPool(
    scope: Scope,
    tools: LockedTools,
    made: Ref[Int],
    held: Ref[Map[CompilerKey, Prepared]],
    permits: Ref[Map[CompilerKey, Semaphore]],
    guard: Semaphore
):
  def openings: UIO[Int] = made.get

  def compile(
      request: CompileRequest,
      artifacts: CompilerArtifacts,
      extra: List[String],
      hear: CompileProblem => Unit,
      digest: histos.jdk.Sha256
  ): IO[CompileError, CompileOutput] =
    val id = CompilerKey(
      request.toolchain.version,
      request.toolchain.jdk.home,
      artifacts.runtime,
      artifacts.compiler,
      artifacts.bridge,
      artifacts.inputs.map(_.fingerprint),
      request.toolchain.jdk.identity
    )
    permit(id).flatMap: sem =>
      sem.withPermit:
        prepared(id).flatMap: ready =>
          ZIO
            .attemptBlockingInterrupt(
              JvmCompile.once(request, ready, extra, hear, digest)
            )
            .mapError(_ =>
              CompileError.Io(IoFailure(request.classes, IoFault.Write))
            )
            .flatMap(ZIO.fromEither)

  private def prepared(id: CompilerKey): IO[CompileError, Prepared] =
    held.get.flatMap: have =>
      have.get(id) match
        case Some(found) => ZIO.succeed(found)
        case None        =>
          scope
            .extend(
              CompilerPool.open(id, tools)
            )
            .tap: created =>
              held.update(_.updated(id, created)) *> made.update(_ + 1)

  private def permit(id: CompilerKey): UIO[Semaphore] =
    permits.get.flatMap: have =>
      have.get(id) match
        case Some(found) => ZIO.succeed(found)
        case None        =>
          guard.withPermit:
            permits.get.flatMap: again =>
              again.get(id) match
                case Some(found) => ZIO.succeed(found)
                case None        =>
                  Semaphore
                    .make(1)
                    .tap(created => permits.update(_.updated(id, created)))

private[zinc] object CompilerPool:
  def make: ZIO[Scope & LockedTools, Nothing, CompilerPool] =
    for
      scope <- ZIO.service[Scope]
      tools <- ZIO.service[LockedTools]
      made <- Ref.make(0)
      held <- Ref.make(Map.empty[CompilerKey, Prepared])
      permits <- Ref.make(Map.empty[CompilerKey, Semaphore])
      guard <- Semaphore.make(1)
    yield CompilerPool(scope, tools, made, held, permits, guard)

  private[zinc] def open(
      key: CompilerKey,
      tools: LockedTools
  ): ZIO[Scope, CompileError, Prepared] =
    val version = key.version
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala3) =>
        Scala3
          .jars(version)
          .provide(zio.ZLayer.succeed(tools))
          .flatMap(toolchain => scala3(key, toolchain))
      case Some(ScalaSeries.Scala2(13)) =>
        Scala213
          .jars(version)
          .provide(zio.ZLayer.succeed(tools))
          .flatMap(toolchain => scala213(key, toolchain))
      case _ =>
        ZIO.fail(CompileError.UnsupportedScala(version))

  private def failed(thrown: Throwable): CompileError =
    val text = Option(thrown.getMessage)
      .filter(_.nonEmpty)
      .getOrElse(thrown.getClass.getName)
    CompileError.Failed(List(CompileDiagnostic.fromCompiler(text)))

  private def scala3(
      key: CompilerKey,
      toolchain: Scala3.Toolchain
  ): ZIO[Scope, CompileError, Prepared] =
    for
      loader <- acquireLoader(toolchain.openLoader(getClass.getClassLoader))
      library <- acquireLoader(libraryLoader(key.runtime))
      ready <- ZIO
        .attemptBlocking {
          val instance = instanceOf(key, loader, library)
          val options = ClasspathOptionsUtil.noboot(key.version.value)
          val scalac =
            ZincUtil.scalaCompiler(instance, toolchain.bridgeJar, options)
          val zinc = new sbt.internal.inc.IncrementalCompilerImpl
          Prepared(
            zinc.compilers(
              instance,
              options,
              Some(key.jdk),
              scalac
            ),
            CompilerCache.fresh,
            toolchain.bridgeJar,
            key.runtime,
            key.compiler,
            Prepared.next,
            key.version.value + "\n" + histos.toolchain.Runtimes
              .identityToken(key.jdkIdentity) + "\n" + key.hashes
              .map(_.value)
              .mkString("\n")
          )
        }
        .mapError(failed)
    yield ready

  private def scala213(
      key: CompilerKey,
      toolchain: Scala213.Toolchain
  ): ZIO[Scope, CompileError, Prepared] =
    for
      loader <- acquireLoader(toolchain.openLoader())
      library <- acquireLoader(libraryLoader(key.runtime))
      ready <- ZIO
        .attemptBlocking {
          val instance = instanceOf(key, loader, library)
          val options = ClasspathOptionsUtil.noboot(key.version.value)
          val scalac =
            ZincUtil.scalaCompiler(instance, toolchain.bridgeJar, options)
          val zinc = new sbt.internal.inc.IncrementalCompilerImpl
          Prepared(
            zinc.compilers(
              instance,
              options,
              Some(key.jdk),
              scalac
            ),
            CompilerCache.fresh,
            toolchain.bridgeJar,
            key.runtime,
            key.compiler,
            Prepared.next,
            key.version.value + "\n" + histos.toolchain.Runtimes
              .identityToken(key.jdkIdentity) + "\n" + key.hashes
              .map(_.value)
              .mkString("\n")
          )
        }
        .mapError(failed)
    yield ready

  private[zinc] def acquireLoader(
      make: => URLClassLoader
  ): ZIO[Scope, CompileError, URLClassLoader] =
    ZIO.acquireRelease(ZIO.attemptBlocking(make).mapError(failed))(loader =>
      ZIO.attemptBlocking(loader.close()).ignore
    )

  private def libraryLoader(jars: List[Path]): URLClassLoader =
    new URLClassLoader(
      jars.map(_.toUri.toURL).toArray,
      ClassLoader.getPlatformClassLoader
    )

  private def instanceOf(
      key: CompilerKey,
      compiler: ClassLoader,
      library: ClassLoader
  ): ScalaInstance =
    val runtime = key.runtime.map(_.toFile).toArray
    val jars = key.compiler.map(_.toFile).toArray
    new ScalaInstance(
      key.version.value,
      compiler,
      compiler,
      library,
      runtime,
      jars,
      runtime ++ jars,
      Some(key.version.value)
    )

private[zinc] object Prepared:
  private val ids = new java.util.concurrent.atomic.AtomicInteger
  def next: Int = ids.incrementAndGet()
