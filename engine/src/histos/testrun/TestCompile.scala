package histos.testrun

import histos.jdk.JdkRuntime
import histos.resolve.LockedTools
import histos.jdk.Sha256
import histos.js.{JsLink, JsLinkError, JsLinkRequest, JsSuite, JsSuiteRequest, ScalaJs}
import histos.native.{
  NativeCompile,
  NativeLink,
  NativeLinkRequest,
  NativeOrigin,
  NativeSuite,
  NativeSuiteRequest,
  NativeToolchain,
}
import histos.node.{NodeBinary, NodeMiss}

import histos.toolchain.Runtimes
import histos.resolve.{
  BuildResolvers,
  Classpath,
  ClasspathScope,
  DependencyLock,
  Repositories,
  Resolve,
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
}
import histos.process.{ChildClasspath, ChildError, ChildEvent, ChildProcess, ChildRequest}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{
  ApiDigest,
  CellPlatform,
  CompileError,
  CompileFacts,
  CompileProblem,
  CompileRequest,
  CompileToolchain,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  JvmCompile,
  ScalacFlags,
  SemanticDb,
  Sources,
  ToolchainMiss,
  UpstreamCell,
  UpstreamCells,
}
import histos.pack.{AssetSync, MainClass, MainClassError, Resources}
import histos.{ClangPin, Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion, TestMain, TestProvider}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Ref, UIO, ZIO}
import zio.stream.ZStream

private[histos] object TestCompile:
  /** The framework a test target launches, or a refusal when that library is not a test dependency. A named test main
    * opts out of the framework.
    */
  private[histos] def launchProvider(
      module: Module
  ): Either[ModuleTestError, Option[TestProvider]] =
    module.test match
      case Some(_) => Right(None)
      case None    =>
        val provider = module.provider.getOrElse(TestProvider.Zio)
        if SuiteLaunch.satisfied(module, provider) then Right(Some(provider))
        else Left(ModuleTestError.ProviderDependency(module.name, provider))

  /** The work directory stays empty. `HISTOS_REPO` finds project files. `PATH` finds clang. `HOME` finds the Playwright
    * cache. `env` is the machine's.
    */
  private[histos] def suiteEnv(
      repo: Path,
      env: Map[String, String],
  ): Map[String, String] =
    val root = Map("HISTOS_REPO" -> repo.toAbsolutePath.normalize.toString)
    val kept = List("PATH", "HOME", "XDG_CACHE_HOME").flatMap: key =>
      env.get(key).filter(_.nonEmpty).map(key -> _)
    root ++ kept

  /** `src/test` inside the source root, plus `<module>/test`. The repository `test` is only for a graph with one
    * module.
    */
  private[histos] def testSources(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
  ): IO[ModuleTestError, List[Path]] =
    for
      base <- GraphCompile
        .sourceBase(repo, module, modules)
        .mapError(ModuleTestError.Compile(_))
      inside <- base match
        case Some(root) =>
          Sources
            .tests(root, version, platform)
            .mapError(error => ModuleTestError.Sources(module.name, version, error))
        case None => ZIO.succeed(Nil)
      sibling <- testBase(repo, module, modules)
      outside <- sibling match
        case Some(root) =>
          Sources
            .select(root, version, platform)
            .mapError(error => ModuleTestError.Sources(module.name, version, error))
        case None => ZIO.succeed(Nil)
    yield (inside ++ outside).distinct

  /** `<module>/test` wins. The repository `test` is only for a graph with one module.
    */
  private[testrun] def testBase(
      repo: Path,
      module: Module,
      modules: List[Module],
  ): IO[ModuleTestError, Option[Path]] =
    ZIO
      .attemptBlocking {
        val named  = repo.resolve(module.baseDirectory).resolve("test")
        val shared = repo.resolve("test")
        if Files.isDirectory(named) then Some(named)
        else if modules.length == 1 && Files.isDirectory(shared) then Some(shared)
        else None
      }
      .mapError(_ => ModuleTestError.Root(repo))

  /** Each upstream cell's classes directory, or its measured copy under coverage. A directory no compile wrote is a
    * missing upstream.
    */
  private def upstreamClasses(
      repo: Path,
      module: Module,
      upstream: List[UpstreamCell],
      platform: Platform,
      coverage: Option[histos.coverage.CoverageCompile],
  ): IO[ModuleTestError, List[Path]] =
    ZIO.foreach(upstream): cell =>
      val measured = coverage.isDefined && histos.coverage.Cell.measures(cell.version, platform)
      val path     =
        if measured then histos.coverage.Flags.classesDir(repo, cell.module.name, cell.version, platform)
        else cell.classes
      ZIO
        .attemptBlocking(Files.isDirectory(path))
        .mapError(_ => ModuleTestError.Root(path))
        .flatMap: found =>
          if found then ZIO.succeed(path)
          else ZIO.fail(ModuleTestError.MissingUpstream(module.name, cell.module.name, cell.version))

  private[testrun] final case class Fetched(
      paths: List[Path],
      hashes: List[Sha256],
      needsHost: Boolean,
  )

  /** Compiler library first, then locked jars. The first jar on the classpath defines `scala.*`.
    */
  /** `hashes` are this module's locked jars. `upstream` are compile jars of upstream modules, without the hashes
    * already in `hashes`. `classDirs` contribute their API digests, not their class bytes. `cells` are the upstream
    * cells [[UpstreamCells.read]] picked.
    */
  private[histos] final case class Linked(
      paths: List[Path],
      hashes: List[Sha256],
      upstream: List[Sha256],
      classDirs: List[Path],
      permitted: List[Path],
      cells: List[UpstreamCell],
  )

  private[histos] def runtimeClasspath(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      lock: DependencyLock,
      repositories: Repositories,
      coverage: Option[histos.coverage.CoverageCompile] = None,
  ): ZIO[Resolve & LockedTools, ModuleTestError, Linked] =
    for
      fetched <- classpath(
        module,
        cell.version,
        cell.platform,
        lock,
        repositories,
      ).mapError(ModuleTestError.Lock(_))
      cells <- UpstreamCells
        .read(
          module,
          Module.chain(module, modules, cell.platform),
          cell.version,
          cell.platform,
          UpstreamCells.declared(repo, modules, cell.platform),
        )
        .mapError(ModuleTestError.Compile(_))
      upstream  <- upstreamClasses(repo, module, cells, cell.platform, coverage)
      libraries <- dependencyJars(cells, cell.platform, lock, repositories)
      library   <- standardLibrary(cell)
      api       <-
        if fetched.needsHost then histos.zinc.ScalacClasspath.hostApi(repo).mapBoth(ModuleTestError.Compile(_), Some(_))
        else ZIO.none
      drop = dropJvmLibrary(cell.platform)
      kept =
        if drop then
          fetched.paths
            .zip(fetched.hashes)
            .filterNot((path, _) => JvmCompile.isStandardLibrary(path))
        else fetched.paths.zip(fetched.hashes)
      libraryPaths = libraries.map(_.path)
      tail         =
        if drop then libraryPaths.filterNot(JvmCompile.isStandardLibrary)
        else libraryPaths
      jars           = (kept.map(_._1) ++ tail ++ api.toList).distinct
      own            = kept.map(_._2).toSet
      upstreamHashes =
        libraries.map(_.sha256).distinct.filterNot(own.contains)
      classes <- extendRuntime(coverage, cell, cell.classes :: (upstream ++ library ++ jars))
    yield Linked(
      classes,
      kept.map(_._2),
      upstreamHashes,
      cell.classes :: upstream,
      api.toList,
      cells,
    )

  /** A resolved JVM `scala-library` is a second copy of the compiler runtime. JavaScript supplies its own library list.
    */
  private def dropJvmLibrary(platform: Platform): Boolean =
    platform match
      case Platform.Jvm | Platform.Native => true
      case Platform.Js                    => false

  /** The JVM suite needs `scala-library` on the child classpath. Js and Native put their own library on the compiler
    * classpath.
    */
  private[testrun] def standardLibrary(
      cell: CompiledCell
  ): ZIO[LockedTools, ModuleTestError, List[Path]] =
    cell.platform match
      case Platform.Jvm =>
        JvmCompile
          .runtime(cell.version)
          .mapError(error => ModuleTestError.Sources(cell.module, cell.version, error))
      case Platform.Js | Platform.Native => ZIO.succeed(Nil)

  private def readApi(module: ProjectName, version: ScalaVersion, classes: Path): IO[ModuleTestError, Sha256] =
    val found = GraphCompile.analysisDirOf(classes) match
      case Some(dir) => ApiDigest.read(dir)
      case None      => Left(CompileError.Io(IoFailure(classes, IoFault.Stat)))
    ZIO.fromEither(found).mapError(error => ModuleTestError.Sources(module, version, error))

  private[histos] def compiledTests(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      jdk: JdkRuntime,
      native: NativeOrigin,
      lock: DependencyLock,
      repositories: Repositories,
      hear: CompileProblem => Unit = _ => (),
      onCompiled: CompileFacts => UIO[Unit] = _ => ZIO.unit,
      coverage: Option[histos.coverage.CoverageCompile] = None,
  ): ZIO[
    Resolve & LockedTools & histos.cache.ContentStore & histos.zinc.Compilers & histos.schedule.RemoteCache,
    ModuleTestError,
    Linked,
  ] =
    val classes  = testClasses(repo, module.name, cell.version, cell.platform)
    val analysis = testAnalysis(repo, module.name, cell.version, cell.platform)
    for
      linked <- runtimeClasspath(
        repo,
        modules,
        module,
        cell,
        lock,
        repositories,
        coverage,
      )
      apis <- ZIO.foreach(linked.classDirs)(readApi(module.name, cell.version, _))
      request = CompileRequest(
        CompileToolchain(cell.version, cell.platform, jdk, module.flags),
        sources,
        linked.paths,
        classes,
        analysis,
        linked.hashes,
        apis,
        permitted = linked.permitted,
        checkout = repo,
      )
      held  <- holdMeasured(coverage, linked.classDirs, module.name, cell.version)
      facts <- compileSuite(
        module,
        request,
        native,
        SemanticDb.argv(repo, cell.version),
        hear,
      )
      _ <- onCompiled(facts)
      _ <- restoreMeasured(coverage, held, module.name, cell.version)
      _ <- syncTests(repo, modules, module, cell, classes, analysis)
    yield linked

  /** `src/test/resources` only. Main resources stay in the main class directory, which is already on `linked.paths`. */
  private def syncTests(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      classes: Path,
      analysis: Path,
  ): ZIO[Any, ModuleTestError, Unit] =
    val refused = (error: histos.AssetError) =>
      ModuleTestError.Compile(GraphCompileError.Assets(module.name, cell.version, cell.platform, error))
    GraphCompile
      .sourceBase(repo, module, modules)
      .mapError(ModuleTestError.Compile(_))
      .flatMap:
        case None      => ZIO.unit
        case Some(src) =>
          Resources
            .filesAt(src.resolve("test").resolve("resources"))
            .mapError(refused)
            .flatMap: found =>
              AssetSync
                .sync(classes, Some(analysis.resolve(AssetSync.Manifest)), found, Nil, repo)
                .mapError(refused)

  /** Scala 2.13 runs the instrumented classes with the scoverage runtime. Scala 3 already has it in the compiler. */
  private def extendRuntime(
      coverage: Option[histos.coverage.CoverageCompile],
      cell: CompiledCell,
      classes: List[Path],
  ): IO[ModuleTestError, List[Path]] =
    coverage match
      case None       => ZIO.succeed(classes)
      case Some(spec) =>
        val runtime = spec.plugin(cell.version).map(_.runtime)
        ZIO
          .fromEither(histos.coverage.Classpath.tests(classes, cell.version, runtime))
          .mapError:
            case histos.coverage.ClasspathGap.MissingRuntime =>
              ModuleTestError.Sources(
                cell.module,
                cell.version,
                CompileError.Toolchain(ToolchainMiss.Coverage(cell.version)),
              )

  /** A test compile loads upstream classes. Those classes record into their own directories, on top of the measurements
    * their tests already wrote. Hold the files first and put them back, so the compile's own records are the only ones
    * that disappear.
    */
  private def holdMeasured(
      coverage: Option[histos.coverage.CoverageCompile],
      dirs: List[Path],
      module: ProjectName,
      version: ScalaVersion,
  ): IO[ModuleTestError, histos.coverage.Measurements.Held] =
    coverage match
      case None    => ZIO.succeed(histos.coverage.Measurements.Held(Nil, Nil))
      case Some(_) =>
        histos.coverage.Measurements
          .hold(dirs)
          .mapError(fault => ModuleTestError.Sources(module, version, CompileError.Io(fault)))

  private def restoreMeasured(
      coverage: Option[histos.coverage.CoverageCompile],
      held: histos.coverage.Measurements.Held,
      module: ProjectName,
      version: ScalaVersion,
  ): IO[ModuleTestError, Unit] =
    coverage match
      case None    => ZIO.unit
      case Some(_) =>
        histos.coverage.Measurements
          .restore(held)
          .mapError(fault => ModuleTestError.Sources(module, version, CompileError.Io(fault)))

  private[testrun] def compileSuite(
      module: Module,
      request: CompileRequest,
      native: NativeOrigin,
      extra: List[String],
      hear: CompileProblem => Unit,
  ): ZIO[
    LockedTools & histos.cache.ContentStore & histos.zinc.Compilers & histos.schedule.RemoteCache,
    ModuleTestError,
    CompileFacts,
  ] =
    ZIO.serviceWithZIO[LockedTools]: tools =>
      type Keep =
        LockedTools & histos.cache.ContentStore & histos.zinc.Compilers & histos.schedule.RemoteCache
      def keep(
          effect: ZIO[Keep & histos.zinc.CompilerStarts, CompileError, histos.zinc.CompileOutput]
      ): ZIO[Keep, CompileError, histos.zinc.CompileOutput] =
        effect.provideSomeLayer[Keep](histos.zinc.CompilerStarts.live)
      val compiled: ZIO[Keep, CompileError, histos.zinc.CompileOutput] =
        ZIO
          .fromEither(CellPlatform.of(module, request.toolchain.platform, tools.catalog, native))
          .flatMap:
            case CellPlatform.Jvm            => keep(JvmCompile.compilePooled(request, extra, hear, ZIO.unit))
            case js: CellPlatform.Js         => keep(ScalaJs.compile(request, js, extra, hear))
            case native: CellPlatform.Native => keep(NativeCompile.compile(request, native, extra, hear))
      compiled
        .mapBoth(
          error =>
            ModuleTestError
              .Sources(module.name, request.toolchain.version, error),
          _.facts,
        )

  /** Compile jars of `dependsOn`, not their tests. The child runs those classes.
    */
  private def dependencyJars(
      upstream: List[UpstreamCell],
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve & LockedTools, ModuleTestError, List[ResolvedArtifact]] =
    ZIO
      .foreach(upstream)(cell => compileArtifacts(cell.module, cell.version, platform, lock, repositories))
      .map(_.flatten.distinct)

  private[testrun] def compileArtifacts(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve & LockedTools, ModuleTestError, List[ResolvedArtifact]] =
    val cell = ResolveCell(
      module.name,
      Dep.select(module.dependencies, platform),
      version,
      platform,
      repositories,
      ClasspathScope.Compile,
    )
    held(lock, cell).mapBoth(ModuleTestError.Lock(_), _.artifacts)

  private[testrun] def classpath(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve & LockedTools, ResolveError, Fetched] =
    val (dependencies, scope) =
      val compileDeps = Dep.select(module.dependencies, platform)
      val testDeps    = Dep.select(module.testDependencies, platform)
      if testDeps.isEmpty then (compileDeps, ClasspathScope.Compile)
      else (compileDeps ++ testDeps, ClasspathScope.Test)
    val cell = ResolveCell(
      module.name,
      dependencies,
      version,
      platform,
      repositories,
      scope,
    )
    held(lock, cell).map: classpath =>
      val host      = histos.zinc.ScalacClasspath.asks(dependencies, version, platform)
      val artifacts = classpath.artifacts.filterNot(artifact => artifact.name.startsWith("histos-api"))
      Fetched(artifacts.map(_.path), artifacts.map(_.sha256), host)

  /** The service solves a miss. A tools value without one fetches the snapshot.
    */
  private def held(
      lock: DependencyLock,
      cell: ResolveCell,
  ): ZIO[Resolve & LockedTools, ResolveError, Classpath] =
    ZIO.serviceWithZIO[LockedTools]: tools =>
      val view = if tools.service.isDefined then tools else tools.copy(lock = lock)
      view.module(cell)

  private[testrun] def readLock(
      repo: Path
  ): IO[ModuleTestError, DependencyLock] =
    Resolve
      .readLock(repo)
      .mapError:
        case ResolveError.Io(failure) => ModuleTestError.LockRead(failure.path)
        case other                    => ModuleTestError.Lock(other)

  private[testrun] def fresh(work: Path): IO[ModuleTestError, Path] =
    Workspace.recreate(work).mapError(_ => ModuleTestError.Work(work))

  private[histos] def testClasses(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    repo
      .resolve(".histos")
      .resolve("test-classes")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)

  private[testrun] def testAnalysis(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    repo
      .resolve(".histos")
      .resolve("test-analysis")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)

  private[histos] def testWork(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    repo
      .resolve(".histos")
      .resolve("test-work")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)

  private[testrun] def linkWork(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    repo
      .resolve(".histos")
      .resolve("link")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)

end TestCompile
