package histos.zinc

import histos.jdk.Sha256
import histos.resolve.{Classpath, ClasspathScope, DependencyLock, Repositories, Resolve, ResolveCell}
import histos.{Dep, Module, Platform, ScalaVersion}

import java.nio.file.Path

import zio.ZIO

/** The scalac classpath of one cell. Compile and BSP both call this.
  *
  * `paths` is the runtime library, then the lock jars, then the upstream class directories. On the JVM the lock's copy
  * of the standard library is dropped, because that runtime is already first. A class directory is named even when it
  * does not exist yet: import runs before compile. `jars` is what compile places after the upstream directories.
  * `JvmCompile` prepends the output directory and the runtime, and an upstream class has to beat a lock jar.
  */
final case class ScalacClasspath(paths: List[Path], jars: List[Path], hashes: List[Sha256])

object ScalacClasspath:
  def of(
      repo: Path,
      modules: List[Module],
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
      scope: ClasspathScope,
  ): ZIO[Resolve & histos.resolve.LockedTools, GraphCompileError, ScalacClasspath] =
    val chain = Module.chain(module, modules, platform)
    val asked = chain.map(item => item -> ClasspathScope.Compile) :+ (module -> scope)
    for
      fetched <- ZIO
        .foreach(asked):
          case (item, itemScope) => load(item, version, platform, lock, repositories, itemScope)
        .map(merge)
      kept = if platform == Platform.Jvm then withoutStdlib(fetched) else fetched
      runtime <- runtimeOf(module, version, platform)
    yield
      val dirs = directories(repo, module, chain, version, platform, scope)
      ScalacClasspath(runtime ++ kept.paths ++ dirs, kept.paths, kept.hashes)

  private def runtimeOf(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
  ): ZIO[histos.resolve.LockedTools, GraphCompileError, List[Path]] =
    platform match
      case Platform.Jvm =>
        JvmCompile
          .runtime(version)
          .mapError(error => GraphCompileError.Failed(module.name, version, platform, error))
      case Platform.Js | Platform.Native => ZIO.succeed(Nil)

  /** Test compile reads this module's main classes. They are not an upstream module, and they may not exist yet.
    */
  private def directories(
      repo: Path,
      module: Module,
      chain: List[Module],
      version: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
  ): List[Path] =
    val upstream = chain.map(item => GraphCompile.classesDir(repo, item.name, version, platform))
    scope match
      case ClasspathScope.Compile => upstream
      case ClasspathScope.Test    =>
        GraphCompile.classesDir(repo, module.name, version, platform) :: upstream

  private def load(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
      scope: ClasspathScope,
  ): ZIO[Resolve, GraphCompileError, Fetched] =
    val compileDeps              = Dep.select(module.dependencies, platform)
    val (dependencies, resolved) = scope match
      case ClasspathScope.Compile => (compileDeps, ClasspathScope.Compile)
      case ClasspathScope.Test    =>
        val testDeps = Dep.select(module.testDependencies, platform)
        if testDeps.isEmpty then (compileDeps, ClasspathScope.Compile)
        else (compileDeps ++ testDeps, ClasspathScope.Test)
    val cell = ResolveCell(module.name, dependencies, version, platform, repositories, resolved)
    Resolve.materialize(cell, lock).mapBoth(GraphCompileError.Lock(_), fetched)

  /** JVM compile prepends `JvmCompile.runtime`. The lock's copy of that library has to stay off.
    */
  private def withoutStdlib(fetched: Fetched): Fetched =
    val kept = fetched.paths
      .zip(fetched.hashes)
      .filterNot:
        case (path, _) => JvmCompile.isStandardLibrary(path)
    Fetched(kept.map(_._1), kept.map(_._2))

  private def merge(fetched: List[Fetched]): Fetched =
    val paired             = fetched.flatMap(item => item.paths.zip(item.hashes))
    val (paths, hashes, _) =
      paired.foldLeft((List.empty[Path], List.empty[Sha256], Set.empty[Path])):
        case ((paths, hashes, seen), (path, _)) if seen.contains(path) => (paths, hashes, seen)
        case ((paths, hashes, seen), (path, hash))                     => (paths :+ path, hashes :+ hash, seen + path)
    Fetched(paths, hashes)

  private final case class Fetched(paths: List[Path], hashes: List[Sha256])

  private def fetched(classpath: Classpath): Fetched =
    Fetched(classpath.artifacts.map(_.path), classpath.artifacts.map(_.sha256))
end ScalacClasspath
