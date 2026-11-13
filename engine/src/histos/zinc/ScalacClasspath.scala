package histos.zinc

import histos.jdk.Sha256
import histos.pack.JarError
import histos.resolve.{
  ArtifactName,
  Classpath,
  ClasspathScope,
  DependencyLock,
  LockedArtifact,
  Repositories,
  Resolve,
  ResolveCell,
}
import histos.{Dep, Module, Platform, ScalaVersion}

import java.nio.file.Path

import zio.{IO, ZIO}

/** The scalac classpath of one cell over the upstream cells [[UpstreamCells.read]] picked. Compile, BSP, and scaladoc
  * call this. Each upstream's lock jars and classes directory are its own cell's, at that cell's Scala version.
  *
  * `paths` is the runtime library, then the lock jars, then the upstream class directories. On the JVM and on Native
  * the lock's copy of the JVM standard library is dropped, because the compiler already puts that runtime first. A
  * class directory is named even when it does not exist yet: import runs before compile. `jars` is what compile places
  * after the upstream directories. `JvmCompile` prepends the output directory and the runtime, and an upstream class
  * has to beat a lock jar.
  */
final case class ScalacClasspath(
    paths: List[Path],
    jars: List[Path],
    hashes: List[Sha256],
    permitted: List[Path],
)

object ScalacClasspath:
  /** Checksums already in the lock, in the order [[of]] would fetch them. Nothing is opened. None when a cell is
    * absent, so the caller fetches instead of inventing a key.
    */
  private[zinc] def lockHashes(
      lock: DependencyLock,
      module: Module,
      upstream: List[UpstreamCell],
      version: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
  ): Option[List[histos.jdk.Sha256]] =
    val found = asked(module, upstream, version, scope).map: (item, itemVersion, itemScope) =>
      lock.cells.find: cell =>
        cell.module == item.name &&
          cell.scalaVersion == itemVersion &&
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

  /** Each upstream's compile cell at its own version, then `module`'s `scope` cell at `version`. */
  private def asked(
      module: Module,
      upstream: List[UpstreamCell],
      version: ScalaVersion,
      scope: ClasspathScope,
  ): List[(Module, ScalaVersion, ClasspathScope)] =
    upstream.map(cell => (cell.module, cell.version, ClasspathScope.Compile)) :+ (module, version, scope)

  def of(
      repo: Path,
      module: Module,
      upstream: List[UpstreamCell],
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
      scope: ClasspathScope,
  ): ZIO[Resolve & histos.resolve.LockedTools, GraphCompileError, ScalacClasspath] =
    for
      fetched <- ZIO
        .foreach(asked(module, upstream, version, scope)):
          case (item, itemVersion, itemScope) => load(item, itemVersion, platform, lock, repositories, itemScope)
        .map(merge)
      kept = if dropJvmLibrary(platform) then withoutStdlib(fetched) else fetched
      runtime <- runtimeOf(module, version, platform)
      api     <-
        if asks(dependenciesOf(module, platform, scope), version, platform) then hostApi(repo).map(Some(_))
        else ZIO.none
    yield
      val dirs = directories(repo, module, upstream, version, platform, scope)
      val jars = kept.paths ++ api.toList
      ScalacClasspath(runtime ++ jars ++ dirs, jars, kept.hashes, api.toList)

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
      upstream: List[UpstreamCell],
      version: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
  ): List[Path] =
    val dirs = upstream.map(_.classes)
    scope match
      case ClasspathScope.Compile => dirs
      case ClasspathScope.Test    =>
        GraphCompile.classesDir(repo, module.name, version, platform) :: dirs

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
      .mapBoth(
        GraphCompileError.Lock(_),
        classpath => fetched(classpath),
      )

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

  /** A compile-only `histos-api` is the api this process already loaded. The resolved jar is a second copy.
    */
  private def fetched(classpath: Classpath): Fetched =
    val artifacts = classpath.artifacts.filterNot(artifact => artifact.name.startsWith("histos-api"))
    Fetched(artifacts.map(_.path), artifacts.map(_.sha256))

  private def dependenciesOf(module: Module, platform: Platform, scope: ClasspathScope): List[Dep] =
    val compileDeps = Dep.select(module.dependencies, platform)
    scope match
      case ClasspathScope.Compile => compileDeps
      case ClasspathScope.Test    =>
        val testDeps = Dep.select(module.testDependencies, platform)
        if testDeps.isEmpty then compileDeps else compileDeps ++ testDeps

  /** A direct `histos-api` dependency compiles against the api this process loaded. [[histos.load.Boundary]] already
    * supplies that jar.
    */
  private[histos] def asks(dependencies: List[Dep], version: ScalaVersion, platform: Platform): Boolean =
    dependencies.exists: dep =>
      dep.name == "histos-api" || ArtifactName.of(dep, version, platform).startsWith("histos-api")

  /** The pure api jar inside this process. An assembly carries it at `bootstrap/histos-api.jar`. Class files are packed
    * into the repo cache.
    */
  private[histos] def hostApi(repo: Path): IO[GraphCompileError, Path] =
    codeSource match
      case None         => ZIO.fail(GraphCompileError.RootRead(repo))
      case Some(source) =>
        histos.load.BuildClasspath(source, repo.resolve(".histos")).mapError(hostFailure(repo))

  private def hostFailure(repo: Path)(error: histos.load.LoadError): GraphCompileError =
    error match
      case histos.load.LoadError.Runtime(inner)    => inner
      case histos.load.LoadError.MissingApi(path)  => GraphCompileError.RootRead(path)
      case histos.load.LoadError.MissingFile(path) => GraphCompileError.RootRead(path)
      case histos.load.LoadError.Io(failure)       => GraphCompileError.RootRead(failure.path)
      case histos.load.LoadError.CompileFailed(_)  => GraphCompileError.RootRead(repo)
      case histos.load.LoadError.NoEntry           => GraphCompileError.RootRead(repo)
      case histos.load.LoadError.AmbiguousEntry(_) => GraphCompileError.RootRead(repo)
      case histos.load.LoadError.BrokenGraph(_)    => GraphCompileError.RootRead(repo)
      case histos.load.LoadError.Plugin(_)         => GraphCompileError.RootRead(repo)
      case histos.load.LoadError.Resolve(_)        => GraphCompileError.RootRead(repo)
      case histos.load.LoadError.NoHome            => GraphCompileError.RootRead(repo)
      case histos.load.LoadError.Jar(error)        => GraphCompileError.RootRead(jarPath(repo, error))

  private def jarPath(repo: Path, error: JarError): Path =
    error match
      case JarError.NotADirectory(path)     => path
      case JarError.Empty(path)             => path
      case JarError.DestinationInside(path) => path
      case JarError.Io(op)                  => op.location
      case JarError.Escaped(_)              => repo
      case JarError.Duplicate(_)            => repo
      case JarError.Scaladoc(_)             => repo

  private def codeSource: Option[Path] =
    Option(classOf[histos.HistosBuild].getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
      .flatMap(url => scala.util.Try(Path.of(url.toURI)).toOption)
end ScalacClasspath
