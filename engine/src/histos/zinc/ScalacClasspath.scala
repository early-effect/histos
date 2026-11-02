package histos.zinc

import histos.jdk.Sha256
import histos.resolve.{Classpath, ClasspathScope, DependencyLock, LockedArtifact, Repositories, Resolve, ResolveCell}
import histos.{Dep, Module, Platform, ScalaVersion}

import java.nio.file.Path

import zio.ZIO

/** The scalac classpath of one cell. Compile and BSP both call this.
  *
  * `paths` is the runtime library, then the lock jars, then the upstream class directories. On the JVM and on Native
  * the lock's copy of the JVM standard library is dropped, because the compiler already puts that runtime first. A
  * class directory is named even when it does not exist yet: import runs before compile. `jars` is what compile places
  * after the upstream directories. `JvmCompile` prepends the output directory and the runtime, and an upstream class
  * has to beat a lock jar.
  */
final case class ScalacClasspath(paths: List[Path], jars: List[Path], hashes: List[Sha256])

object ScalacClasspath:
  /** Checksums already in the lock, in the order [[of]] would fetch them. Nothing is opened. None when a cell is
    * absent, so the caller fetches instead of inventing a key.
    */
  private[zinc] def lockHashes(
      lock: DependencyLock,
      modules: List[Module],
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
  ): Option[List[histos.jdk.Sha256]] =
    val chain = Module.chain(module, modules, platform)
    val asked = chain.map(item => item -> ClasspathScope.Compile) :+ (module -> scope)
    val found = asked.map: (item, itemScope) =>
      lock.cells.find: cell =>
        cell.module == item.name &&
          cell.scalaVersion == version &&
          cell.platform == platform &&
          cell.scope == itemScope
    if found.exists(_.isEmpty) then None
    else
      val artifacts = found.flatMap(_.toList.flatMap(_.artifacts))
      val unique    = artifacts
        .foldLeft((List.empty[LockedArtifact], Set.empty[String])):
          case ((kept, seen), artifact) =>
            val id =
              s"${artifact.organization}\n${artifact.name}\n${artifact.revision}\n${artifact.classifier}\n${artifact.sha256.value}"
            if seen.contains(id) then (kept, seen) else (kept :+ artifact, seen + id)
      val kept = if dropJvmLibrary(platform) then unique._1.filterNot(standardLibrary) else unique._1
      Some(kept.map(_.sha256))

  private def standardLibrary(artifact: LockedArtifact): Boolean =
    artifact.name == "scala-library" || artifact.name.startsWith("scala3-library_")

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
      kept = if dropJvmLibrary(platform) then withoutStdlib(fetched) else fetched
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
  ): ZIO[Resolve & histos.resolve.LockedTools, GraphCompileError, Fetched] =
    val compileDeps              = Dep.select(module.dependencies, platform)
    val (dependencies, resolved) = scope match
      case ClasspathScope.Compile => (compileDeps, ClasspathScope.Compile)
      case ClasspathScope.Test    =>
        val testDeps = Dep.select(module.testDependencies, platform)
        if testDeps.isEmpty then (compileDeps, ClasspathScope.Compile)
        else (compileDeps ++ testDeps, ClasspathScope.Test)
    val cell = ResolveCell(module.name, dependencies, version, platform, repositories, resolved)
    ZIO
      .serviceWithZIO[histos.resolve.LockedTools]: tools =>
        val view = if tools.service.isDefined then tools else tools.copy(lock = lock)
        view.module(cell)
      .mapBoth(GraphCompileError.Lock(_), fetched)

  /** JVM and Native compile do not keep a resolved `scala-library`. JavaScript supplies its own through `compileWith`.
    */
  private def dropJvmLibrary(platform: Platform): Boolean =
    platform match
      case Platform.Jvm | Platform.Native => true
      case Platform.Js                    => false

  /** JVM compile prepends `JvmCompile.runtime`. Native compile does too. The lock's copy of that library stays off.
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
