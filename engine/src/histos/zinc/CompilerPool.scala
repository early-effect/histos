package histos.zinc

import histos.workspace.{IoFailure, IoFault}

import java.net.URLClassLoader
import java.nio.file.Path

import sbt.internal.inc.{ScalaInstance, ZincUtil}
import xsbti.compile.{AnalysisContents, ClasspathOptionsUtil, CompilerCache, Compilers, GlobalsCache}

import zio.{IO, Ref, Scope, Semaphore, UIO, ZIO}

/** Scala compilers for one toolchain. One instance compiles one cell. The pool grows to `width` and closes its class
  * loaders. A keyed semaphore, not `ZKeyedPool`: that pool's shrink timer waits on the test clock.
  */

private[zinc] final class Prepared(
    val compilers: Compilers,
    val cache: GlobalsCache,
    val bridge: java.nio.file.Path,
    val runtime: List[java.nio.file.Path],
    val compilerJars: List[java.nio.file.Path],
    val id: Int,
    val toolFingerprint: String,
)

/** Version, JDK, and the jars the compiler itself loads. The project's classpath is not part of the key.
  */
private[zinc] enum LoaderBoundary:
  case Engine
  case Platform

private[zinc] final case class CompilerKey(
    version: histos.ScalaVersion,
    jdk: Path,
    runtime: List[Path],
    compiler: List[Path],
    bridge: Path,
    hashes: List[histos.jdk.Sha256],
    jdkIdentity: histos.toolchain.RuntimeIdentity,
    loader: List[Path],
    boundary: LoaderBoundary,
)

/** An analysis a pool compile wrote, and the sha256 of the `inc.zip` bytes it wrote. A store restore writes other bytes
  * into that directory, so a compile that finds other bytes reads the file.
  */
private[zinc] final case class Remembered(bytes: histos.jdk.Sha256, contents: AnalysisContents)

private[zinc] final case class CompilerArtifacts(
    runtime: List[Path],
    compiler: List[Path],
    bridge: Path,
    inputs: List[histos.resolve.ToolInput],
    loader: List[Path],
    boundary: LoaderBoundary,
)

private[zinc] final class CompilerPool(
    scope: Scope,
    width: Int,
    made: Ref[Int],
    idle: Ref[Map[CompilerKey, List[Prepared]]],
    live: Ref[Map[CompilerKey, Int]],
    permits: Ref[Map[CompilerKey, Semaphore]],
    guard: Semaphore,
    remembered: Ref[Map[Path, Remembered]],
):
  def openings: UIO[Int] = made.get

  /** `admitted` runs after this compile holds an instance and before that instance returns.
    */
  def compile(
      request: CompileRequest,
      artifacts: CompilerArtifacts,
      extra: List[String],
      hear: CompileProblem => Unit,
      digest: histos.jdk.Sha256,
      admitted: UIO[Unit] = ZIO.unit,
  ): IO[CompileError, CompileOutput] =
    val id = CompilerKey(
      request.toolchain.version,
      request.toolchain.jdk.home,
      artifacts.runtime,
      artifacts.compiler,
      artifacts.bridge,
      artifacts.inputs.map(_.fingerprint),
      request.toolchain.jdk.identity,
      artifacts.loader,
      artifacts.boundary,
    )
    permit(id).flatMap: sem =>
      sem.withPermit:
        checkout(id).flatMap: ready =>
          (admitted *>
            remembered.get.flatMap { known =>
              ZIO
                .attemptBlockingInterrupt(
                  JvmCompile.once(request, ready, extra, hear, digest, known)
                )
                .mapError(_ => CompileError.Io(IoFailure(request.classes, IoFault.Write)))
                .flatMap(ZIO.fromEither)
                .flatMap { (output, stored) =>
                  stored match
                    case Some(entry) =>
                      val dir = request.analysis.toAbsolutePath.normalize
                      remembered.update(_.updated(dir, entry)).as(output)
                    case None => ZIO.succeed(output)
                }
            }).ensuring(checkin(id, ready))

  /** An idle instance, or a new one while this key is still under `width`. The guard is not held across `open`.
    */
  private def checkout(id: CompilerKey): IO[CompileError, Prepared] =
    guard
      .withPermit {
        idle.modify { have =>
          have.getOrElse(id, Nil) match
            case ready :: rest => (Some(ready), have.updated(id, rest))
            case Nil           => (None, have)
        }
      }
      .flatMap {
        case Some(ready) => ZIO.succeed(ready)
        case None        =>
          guard
            .withPermit {
              live.modify { counts =>
                val n = counts.getOrElse(id, 0)
                if n < width then (true, counts.updated(id, n + 1))
                else (false, counts)
              }
            }
            .flatMap { create =>
              if create then
                scope
                  .extend(CompilerPool.open(id))
                  .tapError(_ => release(id))
                  .tap(_ => made.update(_ + 1))
              else ZIO.dieMessage("a compile held a permit and found no compiler")
            }
      }

  private def checkin(id: CompilerKey, ready: Prepared): UIO[Unit] =
    guard.withPermit:
      idle.update(have => have.updated(id, ready :: have.getOrElse(id, Nil)))

  private def release(id: CompilerKey): UIO[Unit] =
    guard.withPermit:
      live.update { counts =>
        val n = counts.getOrElse(id, 0)
        if n <= 1 then counts.removed(id) else counts.updated(id, n - 1)
      }

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
                    .make(width)
                    .tap(created => permits.update(_.updated(id, created)))
end CompilerPool

private[zinc] object CompilerPool:
  def make(width: Int): ZIO[Scope, Nothing, CompilerPool] =
    for
      scope      <- ZIO.service[Scope]
      made       <- Ref.make(0)
      idle       <- Ref.make(Map.empty[CompilerKey, List[Prepared]])
      live       <- Ref.make(Map.empty[CompilerKey, Int])
      permits    <- Ref.make(Map.empty[CompilerKey, Semaphore])
      guard      <- Semaphore.make(1)
      remembered <- Ref.make(Map.empty[Path, Remembered])
    yield CompilerPool(scope, width.max(1), made, idle, live, permits, guard, remembered)

  /** Opens from the jars on the key. The request already resolved them, so a later lock does not rebuild this compiler.
    */
  private[zinc] def open(key: CompilerKey): ZIO[Scope, CompileError, Prepared] =
    ScalaSeries.of(key.version) match
      case Some(ScalaSeries.Scala3) | Some(ScalaSeries.Scala2(13)) => prepare(key)
      case _ => ZIO.fail(CompileError.UnsupportedScala(key.version))

  private def prepare(key: CompilerKey): ZIO[Scope, CompileError, Prepared] =
    for
      loader  <- acquireLoader(compilerLoader(key))
      library <- acquireLoader(libraryLoader(key.runtime))
      ready   <- ZIO
        .attemptBlocking {
          val instance = instanceOf(key, loader, library)
          val options  = ClasspathOptionsUtil.noboot(key.version.value)
          val scalac   = ZincUtil.scalaCompiler(instance, key.bridge, options)
          val zinc     = new sbt.internal.inc.IncrementalCompilerImpl
          Prepared(
            zinc.compilers(instance, options, Some(key.jdk), scalac),
            CompilerCache.fresh,
            key.bridge,
            key.runtime,
            key.compiler,
            Prepared.next,
            key.version.value + "\n" + histos.toolchain.Runtimes
              .identityToken(key.jdkIdentity) + "\n" + key.hashes.map(_.value).mkString("\n"),
          )
        }
        .mapError(failed)
    yield ready

  private def compilerLoader(key: CompilerKey): URLClassLoader =
    val parent = key.boundary match
      case LoaderBoundary.Engine   => new EngineBoundary(getClass.getClassLoader)
      case LoaderBoundary.Platform => ClassLoader.getPlatformClassLoader
    new URLClassLoader(key.loader.map(_.toUri.toURL).toArray, parent)

  private def failed(thrown: Throwable): CompileError =
    val text = Option(thrown.getMessage)
      .filter(_.nonEmpty)
      .getOrElse(thrown.getClass.getName)
    CompileError.Failed(List(CompileDiagnostic.fromCompiler(text)))

  private[zinc] def acquireLoader(
      make: => URLClassLoader
  ): ZIO[Scope, CompileError, URLClassLoader] =
    ZIO.acquireRelease(ZIO.attemptBlocking(make).mapError(failed))(loader => ZIO.attemptBlocking(loader.close()).ignore)

  private def libraryLoader(jars: List[Path]): URLClassLoader =
    new URLClassLoader(
      jars.map(_.toUri.toURL).toArray,
      ClassLoader.getPlatformClassLoader,
    )

  private def instanceOf(
      key: CompilerKey,
      compiler: ClassLoader,
      library: ClassLoader,
  ): ScalaInstance =
    val runtime = key.runtime.map(_.toFile).toArray
    val jars    = key.compiler.map(_.toFile).toArray
    new ScalaInstance(
      key.version.value,
      compiler,
      compiler,
      library,
      runtime,
      jars,
      runtime ++ jars,
      Some(key.version.value),
    )
end CompilerPool

private[zinc] object Prepared:
  private val ids = new java.util.concurrent.atomic.AtomicInteger
  def next: Int   = ids.incrementAndGet()
