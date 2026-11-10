package histos.zinc

import histos.cache.ContentStore
import histos.jdk.JdkRuntime
import histos.jdk.Sha256
import histos.js.{JsRequests, ScalaJs}
import histos.native.{NativeCompile, NativeOrigin, NativeToolchain}
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  DependencyLock,
  LockedTools,
  ResolutionService,
  Resolve,
  ResolveError,
}
import histos.select.SelectError
import histos.schedule.{Histos, Outcome, Parallelism, StepFailure}
import histos.workspace.{IoFailure, IoFault}
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

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
  /** Resolve, the local CAS, the start counter, the compiler services, the input memo, and the resolution service. */
  private[zinc] type Fed =
    Resolve & ContentStore & CompilerStarts & Compilers & CompileMemo & ResolutionService

  def run(
      session: CompileSession,
      names: List[ProjectName],
  ): ZIO[Resolve & ContentStore & ResolutionService, GraphCompileError, List[CompiledCell]] = collected(
    console(session, names, None)
  )
  def run(
      session: CompileSession,
      names: List[ProjectName],
      platform: Option[Platform],
  ): ZIO[Resolve & ContentStore & ResolutionService, GraphCompileError, List[CompiledCell]] = collected(
    console(session, names, platform)
  )

  def run(
      session: CompileSession,
      names: List[ProjectName],
      platform: Option[Platform],
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve & ContentStore & ResolutionService, GraphCompileError, List[CompiledCell]] = collected(
    console(session, names, platform).tap(report)
  )
  def console(
      session: CompileSession,
      names: List[ProjectName],
      platform: Option[Platform],
  ): ZStream[Resolve & ContentStore & ResolutionService, GraphCompileError, CompileFeed] =
    fresh(Parallelism.default.cpu)(
      watch(
        session.repo,
        session.graph,
        choose(session.graph, names),
        None,
        platform,
        session.jdk,
        session.native,
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
  ): ZStream[Resolve & ContentStore & Compilers & CompileMemo & ResolutionService, GraphCompileError, CompileFeed] =
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
      coverage = session.coverage,
      after = after,
    ).provideSomeLayer(CompilerStarts.live)
  def cellFeed(
      session: CompileSession,
      cells: List[(ProjectName, ScalaVersion)],
      platform: Platform,
  ): ZStream[Resolve & ContentStore & Compilers & CompileMemo & ResolutionService, GraphCompileError, CompileFeed] =
    val names = cells.map(_._1)
    watch(
      session.repo,
      session.graph,
      choose(session.graph, names),
      Some(cells.map(_._2.value).toSet),
      Some(platform),
      session.jdk,
      session.native,
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
  ): ZIO[Resolve & ContentStore, GraphCompileError, List[CompiledCell]] =
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
  ): ZStream[Resolve & ContentStore, GraphCompileError, CompileFeed] =
    ZStream.unwrapScoped:
      ResolutionService
        .open(repo, graph)
        .mapError(GraphCompileError.Lock(_))
        .map: report =>
          consoleCounted(repo, graph, names, jdk, native, platform).provideSomeLayer(
            CompilerStarts.live ++ ZLayer.succeed(report)
          )

  /** The same console, with the caller holding the start counter. The compilers die with the stream. */
  private[histos] def consoleCounted(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
      jdk: JdkRuntime,
      native: NativeOrigin,
      platform: Option[Platform],
  ): ZStream[Resolve & ContentStore & CompilerStarts, GraphCompileError, CompileFeed] =
    ZStream.unwrapScoped:
      ResolutionService
        .open(repo, graph)
        .mapError(GraphCompileError.Lock(_))
        .map: report =>
          fresh(Parallelism.default.cpu)(
            watch(repo, graph, choose(graph, names), None, platform, jdk, native)
              .provideSomeLayer(ZLayer.succeed(report))
          )

  /** The daemon's compile. The caller keeps the compiler services. */
  private[histos] def resident(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
      jdk: JdkRuntime,
      native: NativeOrigin,
      platform: Option[Platform],
  ): ZStream[
    Resolve & ContentStore & Compilers & CompileMemo & ResolutionService,
    GraphCompileError,
    CompileFeed,
  ] =
    watch(repo, graph, choose(graph, names), None, platform, jdk, native)
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
  ): ZStream[Resolve & ContentStore, GraphCompileError, CompileFeed] =
    ZStream.unwrapScoped:
      ResolutionService
        .open(repo, graph)
        .mapError(GraphCompileError.Lock(_))
        .map: report =>
          fresh(Parallelism.default.cpu)(
            watch(
              repo,
              graph,
              pickCells(graph, cells, platform),
              Some(cells.map(_._2.value).toSet),
              Some(platform),
              jdk,
              native,
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

  /** This process's JVM. Plugin compile and tests run on it. */
  def jdkHere: IO[GraphCompileError, JdkRuntime] =
    histos.jdk.Machine
      .load(Path.of("."))
      .mapError(GraphCompileError.Runtime(_))
      .flatMap(jdk)

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
        Module.closure(modules, selected.modules.map(_.name).toSet)

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
        Right(Module.closure(modules, cells.map(_._1).toSet))

  private def layersOf(
      modules: List[Module]
  ): Either[GraphCompileError, List[List[Module]]] =
    val names = modules.map(_.name).toSet
    def step(
        done: Set[ProjectName],
        rest: List[Module],
        built: List[List[Module]],
    ): Either[GraphCompileError, List[List[Module]]] =
      val (ready, later) = rest.partition: module =>
        module.upstreamNames.forall(dependency => !names.contains(dependency) || done.contains(dependency))
      ready match
        case Nil =>
          later match
            case module :: _ => Left(GraphCompileError.Cycle(module.name))
            case Nil         => Right(built.reverse)
        case _ =>
          step(done ++ ready.map(_.name), later, ready :: built)
    step(Set.empty, modules, Nil)

  /** The spec holds one cell so an unrelated cell can start. Production does not.
    */
  private[zinc] def gated(
      repo: Path,
      graph: ProjectGraph,
      names: List[ProjectName],
      jdk: JdkRuntime,
      native: NativeOrigin,
      hold: ProjectName => UIO[Unit],
      inside: ProjectName => UIO[Unit] = _ => ZIO.unit,
  ): ZStream[Resolve & ContentStore, GraphCompileError, CompileFeed] =
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
      parallelism: Parallelism = Parallelism.default,
      inside: ProjectName => UIO[Unit] = _ => ZIO.unit,
      coverage: Option[histos.coverage.CoverageCompile] = None,
      after: (ProjectName, ScalaVersion, Platform) => UIO[Unit] = (_, _, _) => ZIO.unit,
  ): ZStream[Fed, GraphCompileError, CompileFeed] =
    ZStream.asyncScoped[Fed, GraphCompileError, CompileFeed](
      emit =>
        val offer    = (feed: CompileFeed) => emit(ZIO.succeed(Chunk.single(feed)))
        val compiled =
          for
            selected <- ZIO.fromEither(picked)
            _        <- ZIO.fromEither(layersOf(selected))
            report   <- ZIO.service[ResolutionService]
            stamped  <- ZIO.serviceWithZIO[CompileMemo]: memo =>
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
    val hear = (problem: CompileProblem) => offer(CompileFeed.Diagnostic(module.name, version, platform, problem))
    hold(module.name) *>
      sourceBase(plan.repo, module, plan.modules).flatMap:
        case None =>
          tell(CompileFeed.Failed(GraphCompileError.NoRoot(module.name)))
            .as(ScheduleCells.render(upstreams))
        case Some(base) =>
          tell(CompileFeed.Began(module.name, version, platform)) *>
            compileOf(
              CellWork(plan, module, version, platform, base, upstreams),
              hear,
            ).foldZIO(
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
                noted *> tell(CompileFeed.Done(result.cell)) *> released.as(ScheduleCells.render(paths)),
            )

  private final case class CellWork(
      plan: CompilePlan,
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      base: Path,
      upstreams: Map[CellKey, Path],
  )

  private final case class CellResult(cell: CompiledCell, miss: Option[KeyMiss])

  /** Lock checksums and source bytes. A hit does not fetch jars or open a compiler. A deleted source is not a hit: the
    * path memo drops it, so the key no longer matches.
    */
  private def recall(
      work: CellWork,
      sources: List[Path],
      apis: List[Sha256],
      classes: Path,
      recorded: Path,
      identity: Option[String],
  ): ZIO[CompileMemo, CompileError, Option[CompileOutput]] =
    val CellWork(plan, module, version, platform, _, _) = work
    ZIO.serviceWithZIO[CompileMemo]: memo =>
      memo
        .hashes(plan.lock, plan.modules, module, version, platform, ClasspathScope.Compile, plan.stamp)
        .flatMap:
          case None         => ZIO.none
          case Some(hashes) =>
            val compiler = plan.tools.pinned(CompilerRequests.compiler(version, plan.tools.repositories))
            val tools    = platform match
              case Platform.Js =>
                val revision = module.jsVersion.getOrElse(plan.tools.catalog.js.revision)
                plan.tools
                  .pinned(JsRequests.libraries(version, revision, plan.tools.repositories, plan.tools.catalog))
                  .map(List(_))
              case _ => Some(Nil)
            (compiler, tools) match
              case (Some(compilerInput), Some(compilation)) =>
                memo.digest(sources).flatMap { digest =>
                  val request = CompileRequest(
                    CompileToolchain(version, platform, plan.jdk, module.flags),
                    sources,
                    Nil,
                    classes,
                    recorded,
                    hashes,
                    apis,
                    compilation,
                    coverage = identity,
                  )
                  val key = CompileKey.withTools(
                    CompileKey.of(request, digest),
                    List(compilerInput.fingerprint),
                  )
                  CompileReplay.recall(request, key)
                }
              case _ => ZIO.none

  private def compileOf(
      work: CellWork,
      hear: CompileProblem => Unit,
  ): ZIO[Fed, GraphCompileError, CellResult] =
    val CellWork(plan, module, version, platform, base, done) = work
    val failed = (error: CompileError) => GraphCompileError.Failed(module.name, version, platform, error)
    val chain  = Module.chain(module, plan.modules, platform)
    for
      sources  <- ZIO.serviceWithZIO[CompileMemo](_.sources(base, version, platform)).mapError(failed)
      tests    <- ZIO.serviceWithZIO[CompileMemo](_.tests(base, version, platform)).mapError(failed)
      compiled <-
        if sources.isEmpty && tests.isEmpty then ZIO.fail(GraphCompileError.NoSources(module.name, base))
        else if sources.isEmpty then vacant(plan, module.name, version, platform)
        else
          val classes  = classRoot(plan, module.name, version, platform)
          val analysis = analysisRoot(plan, module.name, version, platform)
          for
            upstream <- upstreams(module, chain, version, platform, done, plan)
            paths = upstream._1
            apis  = upstream._2
            planned  <- coveragePlan(plan, module.name, version, platform).mapError(failed)
            known    <- recall(work, sources, apis, classes, analysis, planned.map(_.identity)).mapError(failed)
            compiled <- known match
              case Some(output) => ZIO.succeed(output)
              case None         =>
                for
                  assembled <- ZIO
                    .serviceWithZIO[CompileMemo](
                      _.classpath(
                        plan.repo,
                        plan.modules,
                        module,
                        version,
                        platform,
                        plan.lock,
                        plan.repositories,
                        ClasspathScope.Compile,
                        plan.stamp,
                      )
                    )
                    .provideSomeLayer(ZLayer.succeed(plan.tools))
                  request = CompileRequest(
                    CompileToolchain(version, platform, plan.jdk, module.flags),
                    sources,
                    paths ++ assembled.jars,
                    classes,
                    analysis,
                    assembled.hashes,
                    apis,
                    permitted = assembled.permitted,
                    coverage = planned.map(_.identity),
                    coverageTools = planned.fold(List.empty[histos.resolve.ToolInput])(_.tools),
                  )
                  compiled <- compile(
                    work,
                    request,
                    SemanticDb.argv(plan.repo, version) ++ planned.fold(List.empty[String])(_.argv),
                    hear,
                    plan.admitted(module.name),
                  ).mapError(failed)
                yield compiled
            _ <- forgetMeasurements(plan, version, platform, classes).mapError(failed)
          yield CellResult(
            CompiledCell(
              module.name,
              version,
              platform,
              compiled.classes,
              compiled.classFiles,
              Some(compiled.key),
              compiled.changed,
              compiled.sources,
              compiled.classesWritten,
            ),
            compiled.miss,
          )
      _ <- copyResources(plan, module, version, platform, base, compiled.cell.classes)
    yield compiled

  /** Unmanaged resources and generated assets, after compile and after a product restore. The ownership list lives in
    * the analysis directory, so a class-directory wipe does not forget which files this sync owns.
    */
  private def copyResources(
      plan: CompilePlan,
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      source: Path,
      classes: Path,
  ): IO[GraphCompileError, Unit] =
    val refused  = (error: histos.AssetError) => GraphCompileError.Assets(module.name, version, platform, error)
    val manifest = analysisRoot(plan, module.name, version, platform).resolve(histos.pack.AssetSync.Manifest)
    histos.pack.Resources
      .filesAt(source.resolve("main").resolve("resources"))
      .mapError(refused)
      .flatMap: unmanaged =>
        histos.pack.AssetSync
          .sync(classes, Some(manifest), unmanaged, module.resources, plan.repo)
          .mapError(refused)

  /** No main sources. The classes directory stays empty so a later test compile has a classpath root.
    */
  private def vacant(
      plan: CompilePlan,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): IO[GraphCompileError, CellResult] =
    val classes = classRoot(plan, module, version, platform)
    histos.workspace.Workspace
      .recreate(classes)
      .mapError(_ => GraphCompileError.Output(classes)) *>
      ZIO
        .fromEither(ApiDigest.writeEmpty(analysisRoot(plan, module, version, platform)))
        .mapError(error => GraphCompileError.Failed(module, version, platform, error)) *>
      ZIO
        .fromEither(ProductDigest.write(analysisRoot(plan, module, version, platform), classes))
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
          )
        )

  private def compile(
      work: CellWork,
      request: CompileRequest,
      extra: List[String],
      hear: CompileProblem => Unit,
      admitted: UIO[Unit],
  ): ZIO[ContentStore & CompilerStarts & Compilers, CompileError, CompileOutput] =
    val compiled = request.toolchain.platform match
      case Platform.Jvm =>
        JvmCompile.compilePooled(request, extra, hear, admitted)
      case Platform.Js =>
        ScalaJs.compile(request, extra, hear, work.module.jsVersion)
      case Platform.Native =>
        NativeCompile.compile(
          request,
          work.module.nativeVersion
            .getOrElse(work.plan.tools.catalog.native.revision),
          work.plan.native,
          extra,
          hear,
        )
    compiled.provideSome[ContentStore & CompilerStarts & Compilers](ZLayer.succeed(work.plan.tools))

  /** An exact cell is the classpath. A 2.13 patch uses the newest declared 2.13 cell. A Scala 3 cell uses the newest
    * TASTy header `dotty.tools.tasty.TastyFormat.isVersionCompatible` accepts. The three compiler numbers come from
    * this cell's `tasty-core`, not from the compiler that built Histos.
    */
  private def upstreams(
      module: Module,
      chain: List[Module],
      version: ScalaVersion,
      platform: Platform,
      done: Map[CellKey, Path],
      plan: CompilePlan,
  ): ZIO[Fed, GraphCompileError, (List[Path], List[Sha256])] =
    val failed  = (error: CompileError) => GraphCompileError.Failed(module.name, version, platform, error)
    val missing = (dependency: Module) =>
      GraphCompileError.MissingUpstream(module.name, dependency.name, version, platform)
    if !UpstreamCells.isScala3(version) || chain.isEmpty then
      foldUpstreams(module, chain, version, platform, done, plan, None, failed, missing)
    else
      ZIO.scoped {
        ZIO
          .acquireRelease(
            Scala3
              .jars(version)
              .provide(ZLayer.succeed(plan.tools))
              .mapError(failed)
              .flatMap: toolchain =>
                ZIO.fromEither(TastyCompat.open(version, toolchain.loaderJars)).mapError(failed)
          )(reader => ZIO.succeed(reader.close()))
          .flatMap: reader =>
            foldUpstreams(module, chain, version, platform, done, plan, Some(reader), failed, missing)
      }

  private def foldUpstreams(
      module: Module,
      chain: List[Module],
      version: ScalaVersion,
      platform: Platform,
      done: Map[CellKey, Path],
      plan: CompilePlan,
      reader: Option[TastyCompat.Reader],
      failed: CompileError => GraphCompileError,
      missing: Module => GraphCompileError,
  ): ZIO[Fed, GraphCompileError, (List[Path], List[Sha256])] =
    ZIO.foldLeft(chain)((List.empty[Path], List.empty[Sha256])): (acc, dependency) =>
      val (paths, apis) = acc
      chosen(module, dependency, version, platform, done, reader, failed, missing).flatMap: picked =>
        done.get((dependency.name, picked, platform)) match
          case None      => ZIO.fail(missing(dependency))
          case Some(dir) =>
            analysisDirOf(dir) match
              case None =>
                ZIO.fail(failed(CompileError.Io(IoFailure(dir, IoFault.Stat))))
              case Some(recorded) =>
                ZIO
                  .fromEither(ApiDigest.read(recorded))
                  .mapError(failed)
                  .map(api => (paths :+ dir, apis :+ api))

  private def chosen(
      module: Module,
      dependency: Module,
      version: ScalaVersion,
      platform: Platform,
      done: Map[CellKey, Path],
      reader: Option[TastyCompat.Reader],
      failed: CompileError => GraphCompileError,
      missing: Module => GraphCompileError,
  ): ZIO[Any, GraphCompileError, ScalaVersion] =
    UpstreamCells.required(version, dependency, platform, done.keySet) match
      case Nil                                                  => ZIO.fail(missing(dependency))
      case (_, exact, _) :: Nil if exact == version             => ZIO.succeed(exact)
      case (_, patch, _) :: Nil if UpstreamCells.is213(version) => ZIO.succeed(patch)
      case many if UpstreamCells.isScala3(version)              =>
        reader match
          case None       => ZIO.fail(failed(CompileError.Toolchain(ToolchainMiss.Unexpected(version, Nil))))
          case Some(open) =>
            readable(module, dependency, version, platform, done, many, open, failed, missing)
      case _ => ZIO.fail(missing(dependency))

  private def readable(
      module: Module,
      dependency: Module,
      version: ScalaVersion,
      platform: Platform,
      done: Map[CellKey, Path],
      cells: List[CellKey],
      reader: TastyCompat.Reader,
      failed: CompileError => GraphCompileError,
      missing: Module => GraphCompileError,
  ): ZIO[Any, GraphCompileError, ScalaVersion] =
    ZIO
      .foreach(cells):
        case (_, upstream, _) =>
          done.get((dependency.name, upstream, platform)) match
            case None      => ZIO.fail(missing(dependency))
            case Some(dir) =>
              TastyCompat.header(dir).mapError(failed).map(header => (upstream, header))
      .flatMap: judged =>
        val present = judged.collect:
          case (upstream, Some(numbers)) => (upstream, numbers)
        if judged.forall(_._2.isEmpty) then ZIO.fail(missing(dependency))
        else
          ZIO
            .foreach(present): (upstream, numbers) =>
              ZIO.fromEither(reader.accepts(numbers)).mapError(failed).map(ok => (upstream, numbers, ok))
            .flatMap: decided =>
              val accepted = decided.collect:
                case (upstream, numbers, true) => (upstream, numbers)
              val ordered = accepted.sortBy((upstream, numbers) => (numbers.minor, UpstreamCells.rank(upstream)))
              ordered.lastOption match
                case Some((upstream, _)) => ZIO.succeed(upstream)
                case None                =>
                  val refused =
                    decided.sortBy((upstream, numbers, _) => (numbers.minor, UpstreamCells.rank(upstream)))
                  refused.lastOption match
                    case Some((upstream, _, _)) =>
                      ZIO.fail(
                        GraphCompileError.IncompatibleUpstream(
                          module.name,
                          dependency.name,
                          version,
                          upstream,
                          platform,
                        )
                      )
                    case None => ZIO.fail(missing(dependency))

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
