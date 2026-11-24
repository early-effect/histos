package histos.testrun

import histos.jdk.JdkRuntime
import histos.resolve.LockedTools
import histos.jdk.Sha256
import histos.js.ScalaJs
import histos.native.{NativeCompile, NativeOrigin}
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
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{
  ApiDigest,
  CellPlatform,
  CompileError,
  CompileInputs,
  CompileFacts,
  CompileProblem,
  CompilePulse,
  CompileRequest,
  CompileToolchain,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  JvmCompile,
  SemanticDb,
  Sources,
  ToolchainMiss,
  UpstreamCell,
  UpstreamCells,
}
import histos.pack.{AssetSync, Resources}
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion, TestProvider}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

import zio.{IO, UIO, ZIO}

private[histos] object TestCompile:
  /** Compile-only Test leaf. All scoped dependencies and generated products are already completed. */
  def stage(
      execution: histos.build.ExecutionRequest,
      stage: histos.CompilationStage,
      completed: histos.build.ExecutionProducts,
  ): ZIO[GraphCompile.Fed, histos.build.ExecutionError, histos.build.StageProduct] =
    import histos.build.{BuildNode, ExecutionError, StageProduct}
    import histos.plugin.SourceSet
    import histos.zinc.{
      CaptureProduct,
      CaptureRequest,
      CaptureTools,
      CellCapture,
      ProductDigest,
      StageRootError,
      StageRoots,
    }
    val session = execution.session
    val cell    = stage.cell
    val failed = (error: CompileError) => ExecutionError.Test(ModuleTestError.Sources(cell.module, cell.version, error))
    if stage.scope != SourceSet.Test then ZIO.fail(ExecutionError.WrongProduct(BuildNode.Compile(stage)))
    else
      for
        module <- ZIO
          .fromOption(session.graph.projects.collectFirst {
            case module: Module if module.name == cell.module => module
          })
          .orElseFail(ExecutionError.Test(ModuleTestError.Absent(cell.module)))
        main  <- ZIO.fromEither(completed.require(stage.copy(scope = SourceSet.Main)))
        files <- StageRoots
          .of(session.graph)
          .read(session.repo, stage, completed.generatedRoots)
          .mapError(ExecutionError.Roots(_))
        sourceInputs <- ZIO.fromEither(files.compileInputs(session.repo)).mapError(ExecutionError.Roots(_))
        resolution   <- ZIO.service[histos.resolve.ResolutionService]
        lock         <- resolution.current
        tools        <- LockedTools.make(lock, session.graph, resolution)
        selected     <- scopedUpstream(session.graph, module, stage, completed)
          .provideSomeLayer(zio.ZLayer.succeed(tools))
        fetched <- classpath(
          module,
          cell.version,
          cell.platform,
          lock,
          BuildResolvers(session.graph.resolvers),
          histos.load.ApiDep.modules(session.graph),
        ).mapError(ExecutionError.Test(_))
          .provideSomeLayer(zio.ZLayer.succeed(tools))
        library <- standardLibrary(main.compiled)
          .mapError(ExecutionError.Test(_))
          .provideSomeLayer(zio.ZLayer.succeed(tools))
        host <- histos.zinc.ScalacClasspath
          .substituted(session.repo, module, histos.load.ApiDep.modules(session.graph))
          .mapError(error => ExecutionError.Test(ModuleTestError.Compile(error)))
        inherited = (main.classpath ++ selected.flatMap(_.classpath)).distinct
        resolved  = inherited ++ library ++ fetched.paths.filterNot(path =>
          cell.platform != Platform.Js && JvmCompile.isStandardLibrary(path)
        )
        kept =
          if host.isEmpty then resolved
          else resolved.filterNot(path => histos.load.ApiDep.resolvedJar(path) && !host.contains(path))
        basePaths = (kept ++ host).distinct
        paths <- extendRuntime(
          session.coverage.filter(_ => histos.coverage.Cell.measures(cell.version, cell.platform)),
          main.compiled,
          basePaths,
        )
          .mapError(ExecutionError.Test(_))
        identities <- ZIO.foreach(paths)(CellCapture.implementation(_).mapError(failed))
        jars       <- ZIO.filter(paths)(path =>
          ZIO
            .attemptBlocking(Files.isRegularFile(path))
            .mapError(_ => failed(CompileError.Io(IoFailure(path, IoFault.Stat))))
        )
        hashes     <- CompileInputs.jarBytes(jars).mapError(failed)
        signatures <- ZIO.foreach((main :: selected).distinct): product =>
          ZIO
            .attemptBlocking(histos.zinc.UpstreamApis.readAnalysis(product.analysis))
            .mapError(_ => failed(CompileError.Io(IoFailure(product.analysis, IoFault.Read))))
            .flatMap:
              case Some(found)                              => ZIO.succeed(found)
              case None if product.compiled.classFiles == 0 => ZIO.succeed(Nil)
              case None => ZIO.fail(failed(CompileError.Io(IoFailure(product.analysis, IoFault.Read))))
        classes  = testClasses(session.repo, cell.module, cell.version, cell.platform)
        analysis = testAnalysis(session.repo, cell.module, cell.version, cell.platform)
        _      <- execution.report(histos.zinc.CompileFeed.Began(cell.module, cell.version, cell.platform))
        _      <- CellCapture.refreshCompileTime(analysis, classes, identities).mapError(failed)
        result <-
          if files.sources.isEmpty then
            for
              _ <- Workspace.recreate(classes).mapError(error => failed(CompileError.Io(error)))
              _ <- Workspace.recreate(analysis).mapError(error => failed(CompileError.Io(error)))
              _ <- ZIO.fromEither(ApiDigest.writeEmpty(analysis)).mapError(failed)
              _ <- ZIO.fromEither(ProductDigest.write(analysis, classes, session.repo)).mapError(failed)
            yield (
              CompiledCell(
                cell.module,
                cell.version,
                cell.platform,
                classes,
                0,
                None,
                false,
                Some(histos.zinc.SourceCount(0, 0)),
                Some(0),
              ),
              Option.empty[histos.zinc.CompileOutput],
            )
          else
            val compile = CompileRequest(
              CompileToolchain(cell.version, cell.platform, session.jdk, module.flags),
              sourceInputs.map(_.path),
              paths,
              classes,
              analysis,
              hashes,
              (main :: selected).map(_.api) ++ identities,
              permitted = host,
              checkout = session.repo,
              upstream = signatures.flatten,
              sourceInputs = sourceInputs,
            )
            for
              problems <- ZIO.succeed(new java.util.concurrent.ConcurrentLinkedQueue[CompileProblem]())
              output   <- scopedOutput(
                module,
                compile,
                session.native,
                SemanticDb.argv(session.repo, cell.version),
                problem =>
                  val _ = problems.add(problem)
                  (),
              )
                .tapBoth(
                  _ =>
                    ZIO.foreachDiscard(problems.iterator.asScala.toList)(problem =>
                      execution.report(
                        histos.zinc.CompileFeed.Diagnostic(cell.module, cell.version, cell.platform, problem)
                      )
                    ),
                  _ =>
                    ZIO.foreachDiscard(problems.iterator.asScala.toList)(problem =>
                      execution.report(
                        histos.zinc.CompileFeed.Diagnostic(cell.module, cell.version, cell.platform, problem)
                      )
                    ),
                )
                .mapError(failed)
                .provideSomeLayer(zio.ZLayer.succeed(tools))
            yield (
              CompiledCell(
                cell.module,
                cell.version,
                cell.platform,
                classes,
                output.classFiles,
                Some(output.key),
                output.changed,
                output.sources,
                output.classesWritten,
              ),
              Some(output),
            )
        _ <- CellCapture.recordCompileTime(analysis, identities).mapError(failed)
        _ <- AssetSync
          .sync(classes, Some(analysis.resolve(AssetSync.Manifest)), files.resources, files.assets, session.repo)
          .mapError(error => ExecutionError.Roots(StageRootError.Resource(error)))
        (cellProduct, output) = result
        compiled <- output match
          case None         => ZIO.succeed(cellProduct)
          case Some(output) =>
            CellCapture
              .record(
                CaptureRequest(
                  session.repo,
                  stage,
                  files,
                  CaptureProduct(classes, paths),
                  CaptureTools(session.jdk.version, output.runtime, output.compiler, output.bridge),
                )
              )
              .mapError(ExecutionError.Compile(_))
              .map(capture => cellProduct.copy(capture = capture))
        api            <- ZIO.fromEither(ApiDigest.read(analysis)).mapError(failed)
        implementation <- CellCapture.implementation(classes).mapError(failed)
        _              <- execution.report(histos.zinc.CompileFeed.Done(compiled))
      yield StageProduct(stage, compiled, analysis, api, implementation, (classes :: paths).distinct)

  private def scopedUpstream(
      graph: ProjectGraph,
      module: Module,
      stage: histos.CompilationStage,
      completed: histos.build.ExecutionProducts,
  ): ZIO[LockedTools, histos.build.ExecutionError, List[histos.build.StageProduct]] =
    import histos.build.ExecutionError
    import histos.plugin.SourceSet
    val modules = graph.projects.collect { case module: Module => module }
    def grow(
        pending: List[ProjectName],
        seen: Set[ProjectName],
        chain: List[Module],
    ): Either[ExecutionError, List[Module]] = pending match
      case Nil                                 => Right(chain)
      case name :: rest if seen.contains(name) => grow(rest, seen, chain)
      case name :: rest                        =>
        modules.find(_.name == name) match
          case Some(dependency) =>
            grow(dependency.upstream(stage.cell.platform, SourceSet.Test) ++ rest, seen + name, chain :+ dependency)
          case None =>
            Left(
              ExecutionError.Compile(
                GraphCompileError.MissingUpstream(module.name, name, stage.cell.version, stage.cell.platform)
              )
            )
    val chain = grow(module.upstream(stage.cell.platform, SourceSet.Test), Set(module.name), Nil)
    val paths = completed.stages.toList.collect {
      case (upstream, product) if upstream.scope == SourceSet.Test =>
        (upstream.cell.module, upstream.cell.version, upstream.cell.platform) -> product.classes
    }.toMap
    ZIO
      .fromEither(chain)
      .flatMap(found =>
        UpstreamCells
          .read(module, found, stage.cell.version, stage.cell.platform, paths)
          .mapError(ExecutionError.Compile(_))
      )
      .flatMap(cells =>
        ZIO.foreach(cells)(cell =>
          ZIO.fromEither(
            completed.require(
              histos.CompilationStage(histos.Cell(cell.module.name, cell.version, stage.cell.platform), SourceSet.Test)
            )
          )
        )
      )

  private def scopedOutput(
      module: Module,
      request: CompileRequest,
      native: NativeOrigin,
      extra: List[String],
      hear: CompileProblem => Unit,
  ): ZIO[GraphCompile.Fed & LockedTools, CompileError, histos.zinc.CompileOutput] =
    ZIO.serviceWithZIO[LockedTools]: tools =>
      ZIO
        .fromEither(CellPlatform.of(module, request.toolchain.platform, tools.catalog, native))
        .flatMap:
          case CellPlatform.Jvm            => JvmCompile.compilePooled(request, extra, hear, ZIO.unit, _ => ())
          case js: CellPlatform.Js         => ScalaJs.compile(request, js, extra, hear)
          case placed: CellPlatform.Native => NativeCompile.compile(request, placed, extra, hear)

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

  /** The work directory stays empty. `HISTOS_REPO` finds project files. `HISTOS_JAR` is the toolchain jar that launched
    * the suite. `PATH` finds clang. `HOME` finds the Playwright cache. `CI` and `GITHUB_ACTIONS` are copied when they
    * are set. `env` is the machine's.
    */
  private[histos] def suiteEnv(
      repo: Path,
      env: Map[String, String],
  ): Map[String, String] =
    val root = Map("HISTOS_REPO" -> repo.toAbsolutePath.normalize.toString)
    val kept = List("PATH", "HOME", "XDG_CACHE_HOME", "HISTOS_JAR", "CI", "GITHUB_ACTIONS").flatMap: key =>
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
      plugins: Set[ProjectName] = Set.empty,
  ): ZIO[Resolve & LockedTools, ModuleTestError, Linked] =
    for
      fetched <- classpath(
        module,
        cell.version,
        cell.platform,
        lock,
        repositories,
        plugins,
      )
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
        val raw = if drop then libraryPaths.filterNot(JvmCompile.isStandardLibrary) else libraryPaths
        if fetched.needsHost then raw.filterNot(histos.load.ApiDep.resolvedJar) else raw
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

  /** Per-class signatures of one class directory's analysis. The test compile treats them as upstream, so a signature
    * change in this module's main sources reaches the test cell. A missing analysis is the same failure as a missing
    * API digest.
    */
  private def readSignatures(
      module: ProjectName,
      version: ScalaVersion,
      classes: Path,
  ): IO[ModuleTestError, List[histos.zinc.ClassSignature]] =
    val refused = (error: CompileError) => ModuleTestError.Sources(module, version, error)
    GraphCompile.analysisDirOf(classes) match
      case None      => ZIO.fail(refused(CompileError.Io(IoFailure(classes, IoFault.Stat))))
      case Some(dir) =>
        ZIO
          .attemptBlocking(histos.zinc.UpstreamApis.readAnalysis(dir))
          .mapError(_ => refused(CompileError.Io(IoFailure(dir.resolve("inc.zip"), IoFault.Read))))
          .flatMap:
            case Some(found) => ZIO.succeed(found)
            case None        => ZIO.fail(refused(CompileError.Io(IoFailure(dir.resolve("inc.zip"), IoFault.Read))))

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
      hear: CompileProblem => Unit,
      onCompiled: CompileFacts => UIO[Unit] = _ => ZIO.unit,
      coverage: Option[histos.coverage.CoverageCompile] = None,
      onPulse: CompilePulse => Unit = _ => (),
      plugins: Set[ProjectName] = Set.empty,
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
        plugins,
      )
      apis     <- ZIO.foreach(linked.classDirs)(readApi(module.name, cell.version, _))
      upstream <- ZIO.foreach(linked.classDirs)(readSignatures(module.name, cell.version, _))
      bytes    <- CompileInputs
        .jarBytes(linked.permitted)
        .mapError(error => ModuleTestError.Sources(module.name, cell.version, error))
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
        apiBytes = bytes,
        upstream = upstream.flatten,
      )
      held  <- holdMeasured(coverage, linked.classDirs, module.name, cell.version)
      facts <- compileSuite(
        module,
        request,
        native,
        SemanticDb.argv(repo, cell.version),
        hear,
        onPulse,
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
      onPulse: CompilePulse => Unit,
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
            case CellPlatform.Jvm =>
              keep(JvmCompile.compilePooled(request, extra, hear, ZIO.unit, onPulse))
            case js: CellPlatform.Js =>
              keep(ScalaJs.compile(request, js, extra, hear, onPulse))
            case native: CellPlatform.Native =>
              keep(NativeCompile.compile(request, native, extra, hear, onPulse))
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
      plugins: Set[ProjectName] = Set.empty,
  ): ZIO[Resolve & LockedTools, ModuleTestError, Fetched] =
    val pin                   = histos.load.ApiDep.plugin(module, plugins)
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
    held(lock, cell).mapBoth(
      ModuleTestError.Lock(_),
      classpath =>
        val named =
          if pin then classpath.artifacts.filterNot(artifact => histos.load.ApiDep.resolvedName(artifact.name))
          else classpath.artifacts
        val artifacts = named.filterNot(artifact =>
          histos.zinc.ScalacClasspath.toolLibrary(artifact.organization, artifact.name, version, platform)
        )
        Fetched(artifacts.map(_.path), artifacts.map(_.sha256), pin),
    )

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
