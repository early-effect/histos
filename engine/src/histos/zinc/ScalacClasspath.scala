package histos.zinc

import histos.jdk.Sha256
import histos.pack.JarError
import histos.resolve.{Classpath, ClasspathScope, DependencyLock, LockedArtifact, Repositories, Resolve, ResolveCell}
import histos.{Dep, Module, Platform, ScalaVersion}

import java.nio.file.Path

import zio.{IO, ZIO}

/** The scalac classpath of one cell over the upstream cells [[UpstreamCells.read]] picked. Compile, BSP, and scaladoc
  * call this. Each upstream's lock jars and classes directory are its own cell's, at that cell's Scala version.
  *
  * `paths` is the runtime library, then the lock jars, then the upstream class directories. Tool-owned platform
  * libraries are dropped from dependency jars and hashes. JS compile and link supply the requested library tool cell. A
  * class directory is named even when it does not exist yet: import runs before compile. `jars` is what compile places
  * after the upstream directories. `JvmCompile` prepends the output directory and the runtime, and an upstream class
  * has to beat a lock jar.
  *
  * A plugin's API artifact is this process's jar. The author does not write that coordinate, and the lock does not
  * carry it. The jar is `permitted`, and its bytes are a compile input, so a moved host pin recompiles the plugin.
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
      plugins: Set[histos.ProjectName] = Set.empty,
  ): Option[List[histos.jdk.Sha256]] =
    val pin   = histos.load.ApiDep.plugin(module, plugins)
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
      val named =
        if pin then unique._1.filterNot(artifact => histos.load.ApiDep.resolvedName(artifact.name)) else unique._1
      val kept = named.filterNot(artifact => toolLibrary(artifact.organization, artifact.name, version, platform))
      Some(kept.map(_.sha256))

  /** Platform libraries come from the requested tool cell, never a dependency's transitive runtime. */
  private[histos] def toolLibrary(
      organization: String,
      name: String,
      version: ScalaVersion,
      platform: Platform,
  ): Boolean =
    platform match
      case Platform.Jvm | Platform.Native =>
        name == "scala-library" || name.startsWith("scala3-library_")
      case Platform.Js =>
        histos.js.JsRequests
          .libraryRoots(version, histos.ToolCatalog.default.js.revision)
          .exists(root => root.organization == organization && root.name == name)

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
      plugins: Set[histos.ProjectName] = Set.empty,
  ): ZIO[Resolve & histos.resolve.LockedTools, GraphCompileError, ScalacClasspath] =
    assemble(repo, module, upstream, version, platform, lock, repositories, scope, plugins)

  private def assemble(
      repo: Path,
      module: Module,
      upstream: List[UpstreamCell],
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
      scope: ClasspathScope,
      plugins: Set[histos.ProjectName],
  ): ZIO[Resolve & histos.resolve.LockedTools, GraphCompileError, ScalacClasspath] =
    val pin = histos.load.ApiDep.plugin(module, plugins)
    for
      fetched <- ZIO
        .foreach(asked(module, upstream, version, scope)):
          case (item, itemVersion, itemScope) => load(item, itemVersion, platform, lock, repositories, itemScope)
        .map(merge)
      runtime <- runtimeOf(module, version, platform)
      host    <- if pin then hostApi(repo).map(Some(_)) else ZIO.none
    yield
      val mine = host.fold(fetched)(pickHost(fetched, _))
      val dirs = directories(repo, module, upstream, version, platform, scope)
      val jars = mine.paths
      ScalacClasspath(runtime ++ jars ++ dirs, jars, mine.hashes, host.toList)

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
        classpath =>
          fetched(
            classpath.copy(artifacts =
              classpath.artifacts
                .filterNot(artifact => toolLibrary(artifact.organization, artifact.name, version, platform))
            )
          ),
      )

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

  /** The plugin's API artifact is `host`. A resolved `histos-api` path is not that artifact. */
  private def pickHost(fetched: Fetched, host: Path): Fetched =
    val kept = fetched.paths.zip(fetched.hashes).filterNot((path, _) => histos.load.ApiDep.resolvedJar(path))
    Fetched(kept.map(_._1) :+ host, kept.map(_._2))

  /** The toolchain jar a plugin compiles against. Its bytes are the compile input that moves with the host pin. */
  private[histos] def substituted(
      repo: Path,
      module: Module,
      plugins: Set[histos.ProjectName] = Set.empty,
  ): IO[GraphCompileError, List[Path]] =
    if histos.load.ApiDep.plugin(module, plugins) then hostApi(repo).map(path => List(path))
    else ZIO.succeed(Nil)

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
      case histos.load.LoadError.Bootstrap(_)      => GraphCompileError.RootRead(repo)
      case histos.load.LoadError.HostCompiler(_)   => GraphCompileError.RootRead(repo)

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
