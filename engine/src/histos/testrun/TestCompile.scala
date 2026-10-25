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
  ClasspathScope,
  DependencyLock,
  Repositories,
  Resolve,
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
}
import histos.workflow.CiError
import histos.process.{ChildClasspath, ChildError, ChildEvent, ChildProcess, ChildRequest}
import histos.workspace.Workspace
import histos.zinc.{
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
}
import histos.pack.{MainClass, MainClassError}
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
    yield distinct(inside ++ outside)

  private[testrun] def distinct(paths: List[Path]): List[Path] =
    paths
      .foldLeft((List.empty[Path], Set.empty[Path])):
        case ((kept, seen), path) if seen.contains(path) => (kept, seen)
        case ((kept, seen), path)                        => (kept :+ path, seen + path)
      ._1

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

  private[testrun] def upstreamClasses(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
  ): IO[ModuleTestError, List[Path]] =
    ZIO.foreach(Module.chain(module, modules, platform)): dependency =>
      val path =
        GraphCompile.classesDir(repo, dependency.name, version, platform)
      ZIO
        .attemptBlocking(Files.isDirectory(path))
        .mapError(_ => ModuleTestError.Root(path))
        .flatMap: found =>
          if found then ZIO.succeed(path)
          else
            ZIO.fail(
              ModuleTestError
                .MissingUpstream(module.name, dependency.name, version)
            )

  private[testrun] final case class Fetched(
      paths: List[Path],
      hashes: List[Sha256],
  )

  /** Compiler library first, then locked jars. The first jar on the classpath defines `scala.*`.
    */
  /** `hashes` are this module's locked jars. `upstream` are compile jars of upstream modules, without the hashes
    * already in `hashes`.
    */
  private[histos] final case class Linked(
      paths: List[Path],
      hashes: List[Sha256],
      upstream: List[Sha256],
  )

  private[histos] def runtimeClasspath(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve & LockedTools, ModuleTestError, Linked] =
    for
      fetched <- classpath(
        module,
        cell.version,
        cell.platform,
        lock,
        repositories,
      ).mapError(ModuleTestError.Lock(_))
      upstream <- upstreamClasses(
        repo,
        module,
        modules,
        cell.version,
        cell.platform,
      )
      libraries <- dependencyJars(
        module,
        modules,
        cell.version,
        cell.platform,
        lock,
        repositories,
      )
      library <- standardLibrary(cell)
      jvm  = cell.platform == Platform.Jvm
      kept =
        if jvm then
          fetched.paths
            .zip(fetched.hashes)
            .filterNot((path, _) => JvmCompile.isStandardLibrary(path))
        else fetched.paths.zip(fetched.hashes)
      libraryPaths = libraries.map(_.path)
      tail         =
        if jvm then libraryPaths.filterNot(JvmCompile.isStandardLibrary)
        else libraryPaths
      jars           = (kept.map(_._1) ++ tail).distinct
      own            = kept.map(_._2).toSet
      upstreamHashes =
        libraries.map(_.sha256).distinct.filterNot(own.contains)
    yield Linked(
      cell.classes :: (upstream ++ library ++ jars),
      kept.map(_._2),
      upstreamHashes,
    )

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
  ): ZIO[Resolve & LockedTools, ModuleTestError, Linked] =
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
      )
      request = CompileRequest(
        CompileToolchain(cell.version, cell.platform, jdk, module.flags),
        sources,
        linked.paths,
        classes,
        analysis,
        linked.hashes,
      )
      facts <- compileSuite(
        module,
        request,
        native,
        SemanticDb.argv(repo, cell.version),
        hear,
      )
      _ <- onCompiled(facts)
    yield linked

  private[testrun] def compileSuite(
      module: Module,
      request: CompileRequest,
      native: NativeOrigin,
      extra: List[String],
      hear: CompileProblem => Unit,
  ): ZIO[LockedTools, ModuleTestError, CompileFacts] =
    ZIO.serviceWithZIO[LockedTools]: tools =>
      val compiled = request.toolchain.platform match
        case Platform.Jvm => JvmCompile.compile(request, extra, hear)
        case Platform.Js  =>
          ScalaJs.compile(request, extra, hear, module.jsVersion)
        case Platform.Native =>
          val revision =
            module.nativeVersion.getOrElse(tools.catalog.native.revision)
          NativeCompile.compile(request, revision, native, extra, hear)
      compiled
        .mapBoth(
          error =>
            ModuleTestError
              .Sources(module.name, request.toolchain.version, error),
          _.facts,
        )

  /** Compile jars of `dependsOn`, not their tests. The child runs those classes.
    */
  private[testrun] def dependencyJars(
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ModuleTestError, List[ResolvedArtifact]] =
    val found = Module.chain(module, modules, platform)
    ZIO
      .foreach(found)(
        compileArtifacts(_, version, platform, lock, repositories)
      )
      .map(_.flatten.distinct)

  private[testrun] def compileArtifacts(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ModuleTestError, List[ResolvedArtifact]] =
    val cell = ResolveCell(
      module.name,
      Dep.select(module.dependencies, platform),
      version,
      platform,
      repositories,
      ClasspathScope.Compile,
    )
    Resolve
      .materialize(cell, lock)
      .mapBoth(ModuleTestError.Lock(_), _.artifacts)

  private[testrun] def classpath(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ResolveError, Fetched] =
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
    Resolve
      .materialize(cell, lock)
      .map(fetched => Fetched(fetched.artifacts.map(_.path), fetched.artifacts.map(_.sha256)))

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
