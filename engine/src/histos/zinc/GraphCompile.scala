package histos.zinc

import histos.build.{
  BuildDag,
  BuildDagError,
  BuildNode,
  ExecutionError,
  ExecutionProducts,
  ExecutionRequest,
  StageProduct,
}
import histos.plugin.SourceSet
import histos.{Cell, CompilationStage}
import histos.cache.ContentStore
import histos.jdk.JdkRuntime
import histos.jdk.Sha256
import histos.js.ScalaJs
import histos.native.{NativeCompile, NativeOrigin}
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  DependencyLock,
  LockedTools,
  ResolutionService,
  Resolve,
  ToolInput,
}
import histos.schedule.{Histos, Outcome, Parallelism, RemoteCache, StepFailure}
import histos.workspace.{IoFailure, IoFault}
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, NonEmptyChunk, UIO, ZIO, ZLayer}
import zio.stream.ZStream

final case class CompileSession(
    repo: Path,
    graph: ProjectGraph,
    jdk: JdkRuntime,
    native: NativeOrigin,
    lock: histos.resolve.DependencyLock,
    coverage: Option[histos.coverage.CoverageCompile] = None,
)

object GraphCompile:
  /** Resolve, the local CAS, the start counter, the compiler services, the input memo, the resolution service, and the
    * remote cache.
    */
  private[histos] type Fed =
    Resolve & ContentStore & CompilerStarts & Compilers & CompileMemo & ResolutionService & RemoteCache

  /** Main compile leaf. Its upstream products were completed by BuildExecution, not discovered recursively. */
  def stage(
      request: ExecutionRequest,
      stage: CompilationStage,
      completed: ExecutionProducts,
  ): ZIO[Fed, ExecutionError, StageProduct] =
    val session = request.session
    val modules = modulesOf(session.graph)
    val roots   = StageRoots.of(session.graph)
    if stage.scope != SourceSet.Main then ZIO.fail(ExecutionError.WrongProduct(BuildNode.Compile(stage)))
    else
      for
        module <- ZIO
          .fromOption(modules.find(_.name == stage.cell.module))
          .orElseFail(ExecutionError.Compile(GraphCompileError.UnknownCell(stage.cell.module, stage.cell.version)))
        files      <- roots.read(session.repo, stage, completed.generatedRoots).mapError(ExecutionError.Roots(_))
        resolution <- ZIO.service[ResolutionService]
        stamped    <- ZIO.serviceWithZIO[CompileMemo](memo => resolution.current.flatMap(memo.note))
        tools      <- LockedTools.make(stamped.lock, session.graph, resolution)
        plan = CompilePlan(
          session.repo,
          modules,
          stamped.lock,
          session.jdk,
          session.native,
          BuildResolvers(session.graph.resolvers),
          tools,
          stamped.stamp,
          _ => ZIO.unit,
          session.coverage,
          histos.load.ApiDep.modules(session.graph),
          roots,
          completed.generatedRoots,
        )
        done = completed.stages.toList.collect {
          case (upstream, product) if upstream.scope == SourceSet.Main =>
            (upstream.cell.module, upstream.cell.version, upstream.cell.platform) -> product.classes
        }.toMap
        work = CellWork(
          plan,
          module,
          stage.cell.version,
          stage.cell.platform,
          session.repo.resolve(module.baseDirectory).resolve("src"),
          done,
        )
        _      <- request.report(CompileFeed.Began(module.name, stage.cell.version, stage.cell.platform))
        result <- reported(work, files, request.report).mapError(ExecutionError.Compile(_))
        _      <- request.report(
          CompileFeed.Cache(module.name, stage.cell.version, stage.cell.platform, result.note)
        )
        recorded = analysisRoot(plan, module.name, stage.cell.version, stage.cell.platform)
        api <- ZIO
          .fromEither(ApiDigest.read(recorded))
          .mapError(error =>
            ExecutionError.Compile(
              GraphCompileError.Failed(module.name, stage.cell.version, stage.cell.platform, error)
            )
          )
        implementation <- CellCapture
          .implementation(result.cell.classes)
          .mapError(error =>
            ExecutionError.Compile(
              GraphCompileError.Failed(module.name, stage.cell.version, stage.cell.platform, error)
            )
          )
        _ <- request.report(CompileFeed.Done(result.cell))
      yield StageProduct(
        stage,
        result.cell,
        recorded,
        api,
        implementation,
        result.classpath,
      )

  def run(
      session: CompileSession,
      names: List[ProjectName],
  ): ZIO[Resolve & ContentStore & ResolutionService & RemoteCache, GraphCompileError, List[CompiledCell]] = collected(
    console(session, names, None)
  )
  def run(
      session: CompileSession,
      names: List[ProjectName],
      platform: Option[Platform],
  ): ZIO[Resolve & ContentStore & ResolutionService & RemoteCache, GraphCompileError, List[CompiledCell]] = collected(
    console(session, names, platform)
  )

  def run(
      session: CompileSession,
      names: List[ProjectName],
      platform: Option[Platform],
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve & ContentStore & ResolutionService & RemoteCache, GraphCompileError, List[CompiledCell]] = collected(
    console(session, names, platform).tap(report)
  )
  def console(
      session: CompileSession,
      names: List[ProjectName],
      platform: Option[Platform],
  ): ZStream[Resolve & ContentStore & ResolutionService & RemoteCache, GraphCompileError, CompileFeed] =
    fresh(Parallelism.of(session.graph.parallelism).cpu)(
      watch(
        session.repo,
        session.graph,
        choose(session.graph, names),
        None,
        platform,
        session.jdk,
        session.native,
        parallelism = Parallelism.of(session.graph.parallelism),
        inside = _ => ZIO.unit,
      ).provideSomeLayer(CompilerStarts.live)
    )

  /** `hold` runs before that cell compiles. A test spec uses it to keep one cell out of the way. `after` runs once the
    * cell is compiled, before a dependent cell starts. Coverage waits there until this cell's tests finish, so a later
    * test compile cannot drop the measurements those tests wrote.
    */
  private[histos] def held(
      session: CompileSession,
      names: List[ProjectName],
      hold: ProjectName => UIO[Unit],
      after: (ProjectName, ScalaVersion, Platform) => UIO[Unit] = (_, _, _) => ZIO.unit,
  ): ZStream[
    Resolve & ContentStore & Compilers & CompileMemo & ResolutionService & RemoteCache,
    GraphCompileError,
    CompileFeed,
  ] =
    val platform = session.coverage.map(_ => Platform.Jvm)
    val only     = session.coverage.map(_ => histos.coverage.Cell.versions(session.graph))
    watch(
      session.repo,
      session.graph,
      choose(session.graph, names),
      only,
      platform,
      session.jdk,
      session.native,
      hold,
      parallelism = Parallelism.of(session.graph.parallelism),
      inside = _ => ZIO.unit,
      coverage = session.coverage,
      after = after,
    ).provideSomeLayer(CompilerStarts.live)
  def cellFeed(
      session: CompileSession,
      cells: List[(ProjectName, ScalaVersion)],
      platform: Platform,
  ): ZStream[
    Resolve & ContentStore & Compilers & CompileMemo & ResolutionService & RemoteCache,
    GraphCompileError,
    CompileFeed,
  ] =
    val names = cells.map(_._1)
    watch(
      session.repo,
      session.graph,
      choose(session.graph, names),
      Some(cells.map(_._2.value).toSet),
      Some(platform),
      session.jdk,
      session.native,
      parallelism = Parallelism.of(session.graph.parallelism),
      inside = _ => ZIO.unit,
    ).provideSomeLayer(CompilerStarts.live)

  /** Compile every platform a module names, or only `platform` when that is set. Names pull in `dependsOn` upstreams
    * and not dependents. A cell starts when its own upstream cells finish.
    */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
      jdk: JdkRuntime,
      native: NativeOrigin,
      platform: Option[Platform],
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve & ContentStore & RemoteCache, GraphCompileError, List[CompiledCell]] =
    collected(console(repo, graph, names, jdk, native, platform).tap(report))

  /** Cells compile when their upstreams are done. `platform` limits each module to that target. A module that does not
    * build it is skipped.
    */
  def console(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
      jdk: JdkRuntime,
      native: NativeOrigin,
      platform: Option[Platform],
  ): ZStream[Resolve & ContentStore & RemoteCache, GraphCompileError, CompileFeed] =
    fresh[Resolve & ContentStore & CompilerStarts & RemoteCache, CompileFeed](
      Parallelism.of(graph.parallelism).cpu
    )(
      consoleCounted(repo, graph, names, jdk, native, platform, _ => ZIO.unit)
    ).provideSomeLayer(CompilerStarts.live)

  /** The same console, with the caller holding the start counter, the compilers, and the input memo, the way a
    * checkout's server holds them across compiles.
    */
  private[histos] def consoleCounted(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
      jdk: JdkRuntime,
      native: NativeOrigin,
      platform: Option[Platform],
      inside: ProjectName => UIO[Unit],
  ): ZStream[
    Resolve & ContentStore & CompilerStarts & Compilers & CompileMemo & RemoteCache,
    GraphCompileError,
    CompileFeed,
  ] =
    ZStream.unwrapScoped:
      ResolutionService
        .open(repo, graph)
        .mapError(GraphCompileError.Lock(_))
        .map: report =>
          watch(
            repo,
            graph,
            choose(graph, names),
            None,
            platform,
            jdk,
            native,
            parallelism = Parallelism.of(graph.parallelism),
            inside = inside,
          )
            .provideSomeLayer(ZLayer.succeed(report))

  /** The daemon's compile. The caller keeps the compiler services. */
  private[histos] def resident(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
      jdk: JdkRuntime,
      native: NativeOrigin,
      platform: Option[Platform],
  ): ZStream[
    Resolve & ContentStore & Compilers & CompileMemo & ResolutionService & RemoteCache,
    GraphCompileError,
    CompileFeed,
  ] =
    watch(
      repo,
      graph,
      choose(graph, names),
      None,
      platform,
      jdk,
      native,
      parallelism = Parallelism.of(graph.parallelism),
      inside = _ => ZIO.unit,
    )
      .provideSomeLayer(CompilerStarts.live)

  /** Fails after the last event when a cell failed. Test runs keep the lenient stream so passing cells still test.
    */
  def strict[R](
      feeds: ZStream[R, GraphCompileError, CompileFeed]
  ): ZStream[R, GraphCompileError, CompileFeed] =
    ZStream.unwrap:
      zio.Ref
        .make(List.empty[GraphCompileError])
        .map: failed =>
          feeds.tap {
            case CompileFeed.Failed(error) => failed.update(_ :+ error)
            case _                         => ZIO.unit
          } ++ ZStream
            .fromZIO(failed.get.flatMap {
              case Nil          => ZIO.unit
              case head :: rest =>
                ZIO.fail(GraphCompileError.Cells(NonEmptyChunk(head, rest*)))
            })
            .drain

  /** Compile these cells on `platform`, plus their upstream closure, at the requested Scala versions. A class directory
    * from an earlier call does not satisfy `upstreamClasses`. The closure is compiled in this call.
    */
  def cellFeed(
      repo: Path,
      graph: ProjectGraph,
      cells: List[(ProjectName, ScalaVersion)],
      platform: Platform,
      jdk: JdkRuntime,
      native: NativeOrigin,
  ): ZStream[Resolve & ContentStore & RemoteCache, GraphCompileError, CompileFeed] =
    ZStream.unwrapScoped:
      ResolutionService
        .open(repo, graph)
        .mapError(GraphCompileError.Lock(_))
        .map: report =>
          fresh(Parallelism.of(graph.parallelism).cpu)(
            watch(
              repo,
              graph,
              pickCells(graph, cells, platform),
              Some(cells.map(_._2.value).toSet),
              Some(platform),
              jdk,
              native,
              parallelism = Parallelism.of(graph.parallelism),
              inside = _ => ZIO.unit,
            ).provideSomeLayer(CompilerStarts.live ++ ZLayer.succeed(report))
          )

  private def collected[R](
      feeds: ZStream[R, GraphCompileError, CompileFeed]
  ): ZIO[R, GraphCompileError, List[CompiledCell]] =
    feeds.runCollect.flatMap: chunk =>
      val failed = chunk.collect { case CompileFeed.Failed(error) => error }
      val cells  = chunk
        .collect { case CompileFeed.Done(cell) => cell }
        .toList
        .sortBy(cell => (cell.module.value, cell.version.value, cell.platform.token))
      failed.toList match
        case Nil          => ZIO.succeed(cells)
        case head :: rest =>
          ZIO.fail(GraphCompileError.Cells(NonEmptyChunk(head, rest*)))

  /** The running JVM, checked against this Histos version's JDK floor. */
  def jdk(machine: histos.jdk.Machine): IO[GraphCompileError, JdkRuntime] =
    histos.jdk.Toolchain
      .resolve(machine)
      .mapError(GraphCompileError.Runtime(_))

  /** `<module>/src` wins. The repository `src` is only for a graph with one module. The build file stays outside it.
    */
  def sourceBase(
      repo: Path,
      module: Module,
      modules: List[Module],
  ): IO[GraphCompileError, Option[Path]] =
    ZIO
      .attemptBlocking {
        val named  = repo.resolve(module.baseDirectory).resolve("src")
        val shared = repo.resolve("src")
        if Files.isDirectory(named) then Some(named)
        else if modules.length == 1 && Files.isDirectory(shared) then Some(shared)
        else None
      }
      .mapError(_ => GraphCompileError.RootRead(repo))

  private def choose(
      graph: ProjectGraph,
      names: List[ProjectName],
  ): Either[GraphCompileError, List[Module]] =
    val modules = modulesOf(graph)
    histos.select.Selection
      .select(graph, names)
      .left
      .map(GraphCompileError.Target(_))
      .map: selected =>
        mainClosure(modules, selected.modules.map(_.name).toSet)

  private def mainClosure(modules: List[Module], roots: Set[ProjectName]): List[Module] =
    val wanted = modules
      .filter(module => roots.contains(module.name))
      .flatMap { module =>
        module.name :: module.platforms.all.flatMap(platform => Module.chain(module, modules, platform).map(_.name))
      }
      .toSet
    modules.filter(module => wanted.contains(module.name))

  private def pickCells(
      graph: ProjectGraph,
      cells: List[(ProjectName, ScalaVersion)],
      platform: Platform,
  ): Either[GraphCompileError, List[Module]] =
    val modules                                                  = modulesOf(graph)
    val byName                                                   = modules.map(module => module.name -> module).toMap
    def known(name: ProjectName, version: ScalaVersion): Boolean =
      byName.get(name) match
        case Some(module) =>
          module.platforms.all.contains(platform) && module.versions.all.exists(
            _.value == version.value
          )
        case None => false
    cells.collectFirst {
      case (name, version) if !known(name, version) =>
        GraphCompileError.UnknownCell(name, version)
    } match
      case Some(error) => Left(error)
      case None        =>
        Right(mainClosure(modules, cells.map(_._1).toSet))

  private def legacyDagFailure(repo: Path, error: BuildDagError): GraphCompileError = error match
    case BuildDagError.UnknownNode(BuildNode.Compile(stage)) =>
      GraphCompileError.UnknownCell(stage.cell.module, stage.cell.version)
    case BuildDagError.UnknownNode(BuildNode.Link(cell, _)) => GraphCompileError.UnknownCell(cell.module, cell.version)
    case BuildDagError.UnknownNode(BuildNode.Generate(_))   => GraphCompileError.RootRead(repo)
    case BuildDagError.MissingUpstream(stage, module)       =>
      GraphCompileError.MissingUpstream(stage.cell.module, module, stage.cell.version, stage.cell.platform)
    case BuildDagError.MissingCell(_, module, Some(version), _) => GraphCompileError.UnknownCell(module, version)
    case BuildDagError.MissingCell(_, _, None, _)               => GraphCompileError.RootRead(repo)
    case BuildDagError.Declaration(histos.BuildError.Contract(histos.ContractFault.Cycle(path))) =>
      path
        .collectFirst { case histos.BuildNode.Compile(stage) => GraphCompileError.Cycle(stage.cell.module) }
        .getOrElse(GraphCompileError.RootRead(repo))
    case BuildDagError.Declaration(_) => GraphCompileError.RootRead(repo)

  /** The spec holds one cell so an unrelated cell can start. Production does not.
    */
  private[zinc] def gated(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
      jdk: JdkRuntime,
      native: NativeOrigin,
      hold: ProjectName => UIO[Unit],
      inside: ProjectName => UIO[Unit],
  ): ZStream[Resolve & ContentStore & RemoteCache, GraphCompileError, CompileFeed] =
    ZStream.unwrapScoped:
      ResolutionService
        .open(repo, graph)
        .mapError(GraphCompileError.Lock(_))
        .map: report =>
          fresh(64)(
            watch(
              repo,
              graph,
              choose(graph, names),
              None,
              None,
              jdk,
              native,
              hold,
              Parallelism(64, 64, 64),
              inside = inside,
            ).provideSomeLayer(CompilerStarts.live ++ ZLayer.succeed(report))
          )

  /** Compilers that close when the stream does. The daemon does not use this. */
  private def fresh[R, A](width: Int)(
      stream: ZStream[R & Compilers & CompileMemo, GraphCompileError, A]
  ): ZStream[R, GraphCompileError, A] =
    ZStream.unwrapScoped[R](
      (Compilers.live(width) ++ CompileMemo.live).build.map { env =>
        stream.provideSomeLayer(
          ZLayer.succeed(env.get[Compilers]) ++ ZLayer.succeed(env.get[CompileMemo])
        )
      }
    )

  private def watch(
      repo: Path,
      graph: ProjectGraph,
      picked: Either[GraphCompileError, List[Module]],
      only: Option[Set[String]],
      platform: Option[Platform],
      jdk: JdkRuntime,
      native: NativeOrigin,
      hold: ProjectName => UIO[Unit] = _ => ZIO.unit,
      parallelism: Parallelism,
      inside: ProjectName => UIO[Unit],
      coverage: Option[histos.coverage.CoverageCompile] = None,
      after: (ProjectName, ScalaVersion, Platform) => UIO[Unit] = (_, _, _) => ZIO.unit,
  ): ZStream[Fed, GraphCompileError, CompileFeed] =
    ZStream.asyncScoped[Fed, GraphCompileError, CompileFeed](
      emit =>
        val offer    = (feed: CompileFeed) => emit(ZIO.succeed(Chunk.single(feed)))
        val compiled =
          for
            selected <- ZIO.fromEither(picked)
            _        <- ZIO
              .fromEither(
                BuildDag.plan(
                  graph,
                  selected.flatMap { module =>
                    for
                      version <- module.versions.all
                      target  <- module.platforms.all.filter(chosen => platform.forall(_ == chosen))
                    yield BuildNode.Compile(CompilationStage(Cell(module.name, version, target), SourceSet.Main))
                  },
                )
              )
              .mapError(legacyDagFailure(repo, _))
            report  <- ZIO.service[ResolutionService]
            stamped <- ZIO.serviceWithZIO[CompileMemo]: memo =>
              report.current.flatMap(lock => memo.note(lock))
            tools   <- LockedTools.make(stamped.lock, graph, report)
            outcome <-
              val plan = CompilePlan(
                repo,
                modulesOf(graph),
                stamped.lock,
                jdk,
                native,
                BuildResolvers(graph.resolvers),
                tools,
                stamped.stamp,
                inside,
                coverage,
                plugins = histos.load.ApiDep.modules(graph),
                roots = StageRoots.of(graph),
              )
              val goal = ScheduleCells.goal(
                plan,
                selected,
                only,
                platform,
                hold,
                offer,
                after,
              )
              ZIO
                .serviceWithZIO[Histos](_.start(goal))
                .provideSome[Fed & zio.Scope](
                  ZLayer.succeed(parallelism),
                  Histos.live,
                )
                .flatMap(_.outcome)
            _ <- outcome match
              case Outcome.Done(_)          => end(emit, None)
              case Outcome.Failed(failures) =>
                typedFailure(failures) match
                  case Some(error) => end(emit, Some(error))
                  case None        => die(emit)
          yield ()
        compiled
          .foldCauseZIO(
            cause =>
              if cause.isInterrupted then ZIO.failCause(cause)
              else
                cause.failureOption match
                  case Some(error) => end(emit, Some(error))
                  case None        => die(emit)
            ,
            _ => ZIO.unit,
          )
          .forkScoped
      ,
      1024,
    )

  private def end(
      emit: ZIO[Fed, Option[GraphCompileError], Chunk[CompileFeed]] => Unit,
      error: Option[GraphCompileError],
  ): UIO[Unit] =
    val stop = error match
      case Some(found) => ZIO.fail(Some(found))
      case None        => ZIO.fail(None)
    ZIO.attemptBlockingInterrupt(emit(stop)).orDie

  private def die(
      emit: ZIO[Fed, Option[GraphCompileError], Chunk[CompileFeed]] => Unit
  ): UIO[Unit] =
    ZIO
      .attemptBlockingInterrupt(
        emit(
          ZIO.dieMessage(
            "compile finished with a failure that was not a GraphCompileError"
          )
        )
      )
      .orDie

  private def typedFailure(
      failures: NonEmptyChunk[StepFailure[GraphCompileError]]
  ): Option[GraphCompileError] =
    failures.head match
      case StepFailure.Failed(_, cause) => cause.failureOption
      case StepFailure.Skipped(_, _)    => None

  private type CellKey = (ProjectName, ScalaVersion, Platform)

  /** These stay on the feed. The others still fail the compile. */
  private def kept(error: GraphCompileError): Option[GraphCompileError] =
    error match
      case GraphCompileError.NoRoot(_) | GraphCompileError.NoSources(_, _) |
          GraphCompileError.MissingUpstream(_, _, _, _) | GraphCompileError.IncompatibleUpstream(_, _, _, _, _) |
          GraphCompileError.RootRead(_) | GraphCompileError.Output(_) | GraphCompileError.Failed(_, _, _, _) |
          GraphCompileError.Assets(_, _, _, _) =>
        Some(error)
      case _ => None

  private[zinc] def compileCell(
      run: CellRun
  ): ZIO[Fed, GraphCompileError, String] =
    val CellRun(plan, module, version, platform, upstreams, hold, offer, released) = run
    def tell(feed: CompileFeed): UIO[Unit]                                         =
      ZIO.attemptBlockingInterrupt(offer(feed)).orDie
    val hear  = (problem: CompileProblem) => offer(CompileFeed.Diagnostic(module.name, version, platform, problem))
    val pulse = (step: CompilePulse) => offer(CompileFeed.Pulse(module.name, version, platform, step))
    val work  =
      CellWork(plan, module, version, platform, plan.repo.resolve(module.baseDirectory).resolve("src"), upstreams)
    hold(module.name) *>
      (tell(CompileFeed.Began(module.name, version, platform)) *>
        plan.roots
          .read(plan.repo, CompilationStage(Cell(module.name, version, platform), SourceSet.Main), plan.generated)
          .mapError(rootFailure(work, _))
          .flatMap(files => compileOf(work, files, hear, pulse))).foldZIO(
        error =>
          kept(error) match
            case Some(cellError) =>
              tell(CompileFeed.Failed(cellError))
                .as(ScheduleCells.render(upstreams))
            case None => ZIO.fail(error)
        ,
        result =>
          val paths = upstreams.updated(
            (module.name, version, platform),
            result.cell.classes,
          )
          val noted = result.miss match
            case Some(change) =>
              tell(CompileFeed.Miss(module.name, version, platform, change))
            case None => ZIO.unit
          tell(CompileFeed.Cache(module.name, version, platform, result.note)) *> noted *>
            tell(CompileFeed.Done(result.cell)) *> released.as(ScheduleCells.render(paths)),
      )

  private def reported(
      work: CellWork,
      files: StageFiles,
      report: CompileFeed => UIO[Unit],
  ): ZIO[Fed, GraphCompileError, CellResult] =
    val module   = work.module.name
    val version  = work.version
    val platform = work.platform
    ZIO.scoped {
      for
        result <- zio.Promise.make[GraphCompileError, CellResult]
        events = ZStream.asyncScoped[Fed, GraphCompileError, CompileFeed] { emit =>
          val offer = (feed: CompileFeed) => emit(ZIO.succeed(Chunk.single(feed)))
          compileOf(
            work,
            files,
            problem => offer(CompileFeed.Diagnostic(module, version, platform, problem)),
            pulse => offer(CompileFeed.Pulse(module, version, platform, pulse)),
          ).foldCauseZIO(
            cause =>
              result.failCause(cause) *> ZIO
                .attemptBlockingInterrupt(
                  emit(ZIO.failCause(cause.map(error => Some(error))))
                )
                .orDie,
            completed => result.succeed(completed) *> end(emit, None),
          ).forkScoped
        }
        _         <- events.tap(report).runDrain
        completed <- result.await
      yield completed
    }

  private def rootFailure(work: CellWork, error: StageRootError): GraphCompileError =
    val CellWork(plan, module, version, platform, base, _) = work
    error match
      case StageRootError.Source(error)              => GraphCompileError.Failed(module.name, version, platform, error)
      case StageRootError.Resource(error)            => GraphCompileError.Assets(module.name, version, platform, error)
      case StageRootError.ResourceCollision(name, _) =>
        GraphCompileError.Assets(module.name, version, platform, histos.AssetError.Duplicate(name))
      case StageRootError.Io(error) => GraphCompileError.Failed(module.name, version, platform, CompileError.Io(error))
      case StageRootError.Missing(path, _)            => GraphCompileError.RootRead(path)
      case StageRootError.Link(path)                  => GraphCompileError.RootRead(path)
      case StageRootError.Escaped(path)               => GraphCompileError.RootRead(path)
      case StageRootError.MissingGenerated(_)         => GraphCompileError.RootRead(base)
      case StageRootError.DuplicateGenerated(_)       => GraphCompileError.RootRead(base)
      case StageRootError.OutputKindMismatch(_, _, _) => GraphCompileError.RootRead(base)
      case StageRootError.UnknownStage(stage)   => GraphCompileError.UnknownCell(stage.cell.module, stage.cell.version)
      case StageRootError.InvalidPath(raw)      => GraphCompileError.RootRead(plan.repo.resolve(raw))
      case StageRootError.SourceCollision(_, _) => GraphCompileError.RootRead(base)
      case StageRootError.Contract(_)           => GraphCompileError.RootRead(base)

  private final case class CellWork(
      plan: CompilePlan,
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      base: Path,
      upstreams: Map[CellKey, Path],
  )

  private final case class CapturedToolchain(runtime: List[Path], compiler: List[Path], bridge: Path)

  private final case class CellResult(
      cell: CompiledCell,
      miss: Option[KeyMiss],
      closure: Option[List[Path]],
      toolchain: Option[CapturedToolchain],
      classpath: List[Path],
      note: CacheNote,
  )

  /** Lock checksums and source bytes. A hit does not open a compiler. A module that compiles against the host api still
    * hashes that jar. Logical source identities and current bytes make added, deleted, or edited inputs a miss without
    * treating a rematerialized generation as an edit. The key is [[CellTools.recall]]'s, which the compile stores
    * through the same function.
    */
  private def recall(
      work: CellWork,
      sourceInputs: List[CompileSource],
      upstream: List[UpstreamCell],
      apis: List[Sha256],
      classes: Path,
      recorded: Path,
      identity: Option[String],
  ): ZIO[CompileMemo, GraphCompileError, Option[CompileOutput]] =
    val CellWork(plan, module, version, platform, _, _) = work
    val failed = (error: CompileError) => GraphCompileError.Failed(module.name, version, platform, error)
    ZIO.serviceWithZIO[CompileMemo]: memo =>
      memo
        .hashes(
          plan.lock,
          module,
          upstream,
          version,
          platform,
          ClasspathScope.Compile,
          plan.stamp,
          plan.plugins,
        )
        .flatMap:
          case None         => ZIO.none
          case Some(hashes) =>
            CellPlatform.of(module, platform, plan.tools.catalog, plan.native) match
              case Left(_)       => ZIO.none
              case Right(placed) =>
                val carried = (error: GraphCompileError) =>
                  error match
                    case GraphCompileError.RootRead(path) =>
                      GraphCompileError.Failed(
                        module.name,
                        version,
                        platform,
                        CompileError.Io(IoFailure(path, IoFault.Read)),
                      )
                    case GraphCompileError.LockRead(path) =>
                      GraphCompileError.Failed(
                        module.name,
                        version,
                        platform,
                        CompileError.Io(IoFailure(path, IoFault.Read)),
                      )
                    case GraphCompileError.Output(path) =>
                      GraphCompileError.Failed(
                        module.name,
                        version,
                        platform,
                        CompileError.Io(IoFailure(path, IoFault.Write)),
                      )
                    case other => other
                for
                  jars <- ScalacClasspath
                    .substituted(plan.repo, module, plan.plugins)
                    .mapError(carried)
                  bytes <- CompileInputs.jarBytes(jars).mapError(failed)
                  plugin = covered(plan, version, platform).flatMap(_.plugin(version))
                  runtimeHashes <- CompileInputs.jarBytes(plugin.map(_.runtime).toList).mapError(failed)
                  base = CompileRequest(
                    CompileToolchain(version, platform, plan.jdk, module.flags),
                    sourceInputs.map(_.path),
                    Nil,
                    classes,
                    recorded,
                    hashes ++ runtimeHashes,
                    apis,
                    coverage = identity,
                    coverageTools = plugin.fold(List.empty[ToolInput])(_.tools),
                    checkout = plan.repo,
                    apiBytes = bytes,
                    sourceInputs = sourceInputs,
                  )
                  digest <- JvmCompile.sourceDigest(base).mapError(failed)
                  keyed  <- CellTools.recall(plan.tools, placed, base, digest)
                  found  <- keyed match
                    case Some(hit) => CompileReplay.recall(hit.request, hit.key).mapError(failed)
                    case None      => ZIO.none
                yield found

  private def compileOf(
      work: CellWork,
      files: StageFiles,
      hear: CompileProblem => Unit,
      onPulse: CompilePulse => Unit,
  ): ZIO[Fed, GraphCompileError, CellResult] =
    val CellWork(plan, module, version, platform, _, done) = work
    val failed = (error: CompileError) => GraphCompileError.Failed(module.name, version, platform, error)
    val chain  = Module.chain(module, plan.modules, platform)
    for
      read <- upstreams(module, chain, version, platform, done, plan)
      cells = read.map(_._1)
      assembled <- classpathOf(plan, module, cells, version, platform)
      paths = CellCapture.classpath(assembled, cells)
      sourceInputs <- ZIO.fromEither(files.compileInputs(plan.repo)).mapError(rootFailure(work, _))
      sources = sourceInputs.map(_.path)
      compiled <-
        if sources.isEmpty then vacant(plan, module.name, version, platform, paths)
        else
          val classes  = classRoot(plan, module.name, version, platform)
          val analysis = analysisRoot(plan, module.name, version, platform)
          for
            implementations <- ZIO.foreach(cells)(cell => CellCapture.implementation(cell.classes).mapError(failed))
            apis     = read.map(_._2) ++ implementations
            upstream = read.flatMap(_._3)
            planned  <- coveragePlan(plan, module.name, version, platform).mapError(failed)
            known    <- recall(work, sourceInputs, cells, apis, classes, analysis, planned.map(_.identity))
            produced <- known match
              case Some(output) => ZIO.succeed(output)
              case None         =>
                for
                  _     <- CellCapture.refreshCompileTime(analysis, classes, implementations).mapError(failed)
                  bytes <- CompileInputs.jarBytes(assembled.permitted).mapError(failed)
                  request = CompileRequest(
                    CompileToolchain(version, platform, plan.jdk, module.flags),
                    sources,
                    cells.map(_.classes) ++ assembled.jars,
                    classes,
                    analysis,
                    assembled.hashes,
                    apis,
                    permitted = assembled.permitted,
                    coverage = planned.map(_.identity),
                    coverageTools = planned.fold(List.empty[ToolInput])(_.tools),
                    checkout = plan.repo,
                    apiBytes = bytes,
                    upstream = upstream,
                    sourceInputs = sourceInputs,
                  )
                  compiled <- compile(
                    work,
                    request,
                    SemanticDb.argv(plan.repo, version) ++ planned.fold(List.empty[String])(_.argv),
                    hear,
                    plan.admitted(module.name),
                    onPulse,
                  ).mapError(failed)
                yield compiled
            output = produced
            _ <- CellCapture.recordCompileTime(analysis, implementations).mapError(failed)
            closure = module.pluginLibrary.map(_ => paths)
            _ <- forgetMeasurements(plan, version, platform, classes).mapError(failed)
          yield CellResult(
            CompiledCell(
              module.name,
              version,
              platform,
              output.classes,
              output.classFiles,
              Some(output.key),
              output.changed,
              output.sources,
              output.classesWritten,
            ),
            output.miss,
            closure,
            Some(CapturedToolchain(output.runtime, output.compiler, output.bridge)),
            (classes :: paths).distinct,
            output.note,
          )
      _        <- copyResources(plan, module, version, platform, files, compiled.cell.classes)
      recorded <- (compiled.closure, compiled.toolchain) match
        case (None, _)       => ZIO.succeed(compiled.cell)
        case (Some(_), None) =>
          val part = ScalaSeries.of(version) match
            case Some(ScalaSeries.Scala2(13)) => CompilerPart.Compiler
            case _                            => CompilerPart.Scala3Compiler
          ZIO.fail(
            GraphCompileError.Failed(
              module.name,
              version,
              platform,
              CompileError.Toolchain(ToolchainMiss.Missing(version, part)),
            )
          )
        case (Some(paths), Some(tools)) =>
          CellCapture
            .record(
              CaptureRequest(
                plan.repo,
                CompilationStage(Cell(module.name, version, platform), SourceSet.Main),
                files,
                CaptureProduct(compiled.cell.classes, paths),
                CaptureTools(plan.jdk.version, tools.runtime, tools.compiler, tools.bridge),
              )
            )
            .map(capture => compiled.cell.copy(capture = capture))
    yield compiled.copy(cell = recorded)

  /** Resolve once and retain the classpath used by compilation and product capture. */
  private def classpathOf(
      plan: CompilePlan,
      module: Module,
      upstream: List[UpstreamCell],
      version: ScalaVersion,
      platform: Platform,
  ): ZIO[Fed, GraphCompileError, ScalacClasspath] =
    ZIO
      .serviceWithZIO[CompileMemo](
        _.classpath(
          plan.repo,
          module,
          upstream,
          version,
          platform,
          plan.lock,
          plan.repositories,
          ClasspathScope.Compile,
          plan.stamp,
          plan.plugins,
        )
      )
      .provideSomeLayer(ZLayer.succeed(plan.tools))
      .flatMap: assembled =>
        covered(plan, version, platform) match
          case None       => ZIO.succeed(assembled)
          case Some(spec) =>
            val failed = (error: CompileError) => GraphCompileError.Failed(module.name, version, platform, error)
            for
              jars <- ZIO
                .fromEither(
                  histos.coverage.Classpath.tests(assembled.jars, version, spec.plugin(version).map(_.runtime))
                )
                .mapError:
                  case histos.coverage.ClasspathGap.MissingRuntime =>
                    failed(CompileError.Toolchain(ToolchainMiss.Coverage(version)))
              extra = jars.filterNot(assembled.jars.contains)
              hashes <- CompileInputs.jarBytes(spec.plugin(version).map(_.runtime).toList).mapError(failed)
            yield assembled.copy(paths = assembled.paths ++ extra, jars = jars, hashes = assembled.hashes ++ hashes)

  /** Unmanaged resources and generated assets, after compile and after a product restore. The ownership list lives in
    * the analysis directory, so a class-directory wipe does not forget which files this sync owns.
    */
  private def copyResources(
      plan: CompilePlan,
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      files: StageFiles,
      classes: Path,
  ): IO[GraphCompileError, Unit] =
    val refused  = (error: histos.AssetError) => GraphCompileError.Assets(module.name, version, platform, error)
    val manifest = analysisRoot(plan, module.name, version, platform).resolve(histos.pack.AssetSync.Manifest)
    histos.pack.AssetSync
      .sync(classes, Some(manifest), files.resources, files.assets, plan.repo)
      .mapError(refused)

  /** No main sources. The classes directory stays empty so a later test compile has a classpath root.
    */
  private def vacant(
      plan: CompilePlan,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      classpath: List[Path],
  ): IO[GraphCompileError, CellResult] =
    val classes = classRoot(plan, module, version, platform)
    histos.workspace.Workspace
      .recreate(classes)
      .mapError(_ => GraphCompileError.Output(classes)) *>
      histos.workspace.Workspace
        .recreate(analysisRoot(plan, module, version, platform))
        .mapError(_ => GraphCompileError.Output(analysisRoot(plan, module, version, platform))) *>
      ZIO
        .fromEither(ApiDigest.writeEmpty(analysisRoot(plan, module, version, platform)))
        .mapError(error => GraphCompileError.Failed(module, version, platform, error)) *>
      ZIO
        .fromEither(ProductDigest.write(analysisRoot(plan, module, version, platform), classes, plan.repo))
        .mapError(error => GraphCompileError.Failed(module, version, platform, error))
        .as(
          CellResult(
            CompiledCell(
              module,
              version,
              platform,
              classes,
              0,
              key = None,
              changed = false,
              sources = Some(SourceCount(0, 0)),
              classesWritten = Some(0),
            ),
            None,
            None,
            None,
            (classes :: classpath).distinct,
            CacheNote.Vacant,
          )
        )

  private def compile(
      work: CellWork,
      request: CompileRequest,
      extra: List[String],
      hear: CompileProblem => Unit,
      admitted: UIO[Unit],
      onPulse: CompilePulse => Unit,
  ): ZIO[ContentStore & CompilerStarts & Compilers & RemoteCache, CompileError, CompileOutput] =
    val compiled =
      ZIO
        .fromEither(CellPlatform.of(work.module, request.toolchain.platform, work.plan.tools.catalog, work.plan.native))
        .flatMap:
          case CellPlatform.Jvm            => JvmCompile.compilePooled(request, extra, hear, admitted, onPulse)
          case js: CellPlatform.Js         => ScalaJs.compile(request, js, extra, hear, onPulse)
          case native: CellPlatform.Native => NativeCompile.compile(request, native, extra, hear, onPulse)
    compiled.provideSome[ContentStore & CompilerStarts & Compilers & RemoteCache](ZLayer.succeed(work.plan.tools))

  /** The cells [[UpstreamCells.read]] picks from the finished cells, each with the API digest its compile recorded. */
  private def upstreams(
      module: Module,
      chain: List[Module],
      version: ScalaVersion,
      platform: Platform,
      done: Map[CellKey, Path],
      plan: CompilePlan,
  ): IO[GraphCompileError, List[(UpstreamCell, Sha256, List[ClassSignature])]] =
    val failed = (error: CompileError) => GraphCompileError.Failed(module.name, version, platform, error)
    UpstreamCells
      .read(module, chain, version, platform, done)
      .provide(ZLayer.succeed(plan.tools))
      .flatMap: cells =>
        ZIO.foreach(cells): cell =>
          analysisDirOf(cell.classes) match
            case None           => ZIO.fail(failed(CompileError.Io(IoFailure(cell.classes, IoFault.Stat))))
            case Some(recorded) =>
              ZIO
                .fromEither(ApiDigest.read(recorded))
                .mapError(failed)
                .flatMap: digest =>
                  signaturesOf(recorded)
                    .mapError(failed)
                    .map(signatures => (cell, digest, signatures))

  /** The per-class signatures of one upstream analysis. A missing analysis contributed no classes. An analysis that
    * cannot be read is a failed compile: a silent empty table would hide a signature change.
    */
  private def signaturesOf(dir: Path): IO[CompileError, List[ClassSignature]] =
    ZIO
      .attemptBlocking(UpstreamApis.readAnalysis(dir))
      .mapError(_ => CompileError.Io(IoFailure(dir.resolve("inc.zip"), IoFault.Read)))
      .flatMap:
        case Some(found) => ZIO.succeed(found)
        case None        =>
          ZIO
            .fromEither(ApiDigest.read(dir))
            .flatMap: digest =>
              if digest == ApiDigest.empty then ZIO.succeed(Nil)
              else ZIO.fail(CompileError.Io(IoFailure(dir.resolve("inc.zip"), IoFault.Read)))

  /** Classpath root written for one JVM cell. Test compile and the test subprocess read this directory.
    */
  private[histos] def classesDir(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
  ): Path =
    classesDir(repo, module, version, Platform.Jvm)

  private[histos] def classesDir(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    repo
      .resolve(".histos")
      .resolve("classes")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)

  private def analysis(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    repo
      .resolve(".histos")
      .resolve("analysis")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)

  /** Inverse of a classes directory and its analysis directory. `test-classes` maps to `test-analysis`. None for a path
    * that is not one of those products.
    */
  private[histos] def analysisDirOf(classes: Path): Option[Path] =
    val absolute = classes.toAbsolutePath.normalize
    for
      (platform, scalaDir) <- named(absolute)
      (scala, moduleDir)   <- named(scalaDir)
      (module, kindDir)    <- named(moduleDir)
      (kind, above)        <- named(kindDir)
      analysisKind         <- kind match
        case "classes"      => Some("analysis")
        case "test-classes" => Some("test-analysis")
        case _              => None
      (segments, repo) <- rooted(above)
    yield segments
      .foldLeft(repo.resolve(".histos"))((path, segment) => path.resolve(segment))
      .resolve(analysisKind)
      .resolve(module)
      .resolve(scala)
      .resolve(platform)

  /** `.histos` is the product root. `.histos/coverage` is the coverage root. */
  private def rooted(above: Path): Option[(List[String], Path)] =
    named(above) match
      case Some((".histos", repo))       => Some((Nil, repo))
      case Some(("coverage", histosDir)) =>
        named(histosDir) match
          case Some((".histos", repo)) => Some((List("coverage"), repo))
          case _                       => None
      case _ => None

  private def covered(
      plan: CompilePlan,
      version: ScalaVersion,
      platform: Platform,
  ): Option[histos.coverage.CoverageCompile] =
    plan.coverage.filter(_ => histos.coverage.Cell.measures(version, platform))

  private def classRoot(
      plan: CompilePlan,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    covered(plan, version, platform) match
      case Some(_) => histos.coverage.Flags.classesDir(plan.repo, module, version, platform)
      case None    => classesDir(plan.repo, module, version, platform)

  private def analysisRoot(
      plan: CompilePlan,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    covered(plan, version, platform) match
      case Some(_) => histos.coverage.Flags.analysisDir(plan.repo, module, version, platform)
      case None    => analysis(plan.repo, module, version, platform)

  private def coveragePlan(
      plan: CompilePlan,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): IO[CompileError, Option[histos.coverage.Instrumented]] =
    covered(plan, version, platform) match
      case None       => ZIO.succeed(None)
      case Some(spec) =>
        ZIO
          .fromEither(histos.coverage.Cell.instrumented(plan.repo, module, version, platform, spec))
          .mapError:
            case histos.coverage.CellFault.Plugin(found) =>
              CompileError.Toolchain(ToolchainMiss.Coverage(found))
            case histos.coverage.CellFault.Unmeasured(found, where) =>
              if where == Platform.Jvm then CompileError.UnsupportedScala(found)
              else CompileError.UnsupportedPlatform(where)
          .map(Some(_))

  private def forgetMeasurements(
      plan: CompilePlan,
      version: ScalaVersion,
      platform: Platform,
      classes: Path,
  ): IO[CompileError, Unit] =
    covered(plan, version, platform) match
      case None    => ZIO.unit
      case Some(_) =>
        histos.coverage.Measurements.forget(classes).mapError(fault => CompileError.Io(fault))

  private def named(path: Path): Option[(String, Path)] =
    Option(path.getFileName).map(_.toString).zip(Option(path.getParent))

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case module: Module => module }
end GraphCompile
