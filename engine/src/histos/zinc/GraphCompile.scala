package histos.zinc

import histos.jdk.JdkRuntime
import histos.jdk.Sha256
import histos.js.ScalaJs
import histos.native.{NativeCompile, NativeOrigin, NativeToolchain}
import histos.resolve.LockedTools
import histos.resolve.{BuildResolvers, ClasspathScope, DependencyLock, Resolve, ResolveError}
import histos.select.SelectError
import histos.schedule.{Histos, Outcome, Parallelism, StepFailure}
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, NonEmptyChunk, UIO, ZIO, ZLayer}
import zio.stream.ZStream

final case class CompiledCell(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    classes: Path,
    /** `.class`, `.sjsir`, and `.nir` currently in `classes`, including leftovers.
      */
    classFiles: Int,
    key: Option[Sha256],
    changed: Boolean,
    sources: Option[SourceCount],
    /** Class files this round wrote. Absent when that set is not known. */
    classesWritten: Option[Int],
)

/** One cell's console. Diagnostics stay with that cell. Parallel cells are not interleaved.
  */
enum CompileFeed:
  case Began(module: ProjectName, version: ScalaVersion, platform: Platform)
  case Diagnostic(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      problem: CompileProblem,
  )
  case Done(cell: CompiledCell)
  case Failed(error: GraphCompileError)

enum GraphCompileError:
  case Runtime(error: histos.jdk.ToolchainError)
  case Target(error: SelectError)
  case UnknownCell(module: ProjectName, version: ScalaVersion)
  case NoRoot(module: ProjectName)
  case NoSources(module: ProjectName, root: Path)
  case MissingUpstream(
      module: ProjectName,
      dependency: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  )
  case Cycle(module: ProjectName)
  case Lock(error: ResolveError)
  case LockRead(path: Path)
  case RootRead(path: Path)
  case Output(path: Path)
  case Failed(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      error: CompileError,
  )
  case Cells(errors: NonEmptyChunk[GraphCompileError])

  def message: String = this match
    case Runtime(error)               => error.message
    case Target(error)                => error.message
    case UnknownCell(module, version) =>
      s"'${module.value}' ${version.value} is not a cell on the requested platform"
    case NoRoot(module) =>
      s"'${module.value}' has no source root. Expected ${module.value}/src, or src when it is the only module"
    case NoSources(module, root) =>
      s"'${module.value}' has no main or test sources under $root"
    case MissingUpstream(module, dependency, version, platform) =>
      s"'${module.value}' depends on '${dependency.value}', which has no ${version.value} ${platform.token} classes"
    case Cycle(module)                            => s"'${module.value}' is in a compile cycle"
    case Lock(error)                              => error.message
    case LockRead(path)                           => s"could not read $path"
    case RootRead(path)                           => s"could not read source roots under $path"
    case Output(path)                             => s"could not write $path"
    case Failed(module, version, platform, error) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to compile\n${error.message}"
    case Cells(errors) => errors.map(_.message).mkString("\n")
end GraphCompileError

final case class CompileSession(
    repo: Path,
    graph: ProjectGraph,
    jdk: JdkRuntime,
    native: NativeOrigin,
    lock: histos.resolve.DependencyLock,
)

object GraphCompile:
  def run(
      session: CompileSession,
      names: List[ProjectName],
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] = collected(
    console(session, names, None)
  )
  def run(
      session: CompileSession,
      names: List[ProjectName],
      platform: Option[Platform],
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] = collected(
    console(session, names, platform)
  )

  def run(
      session: CompileSession,
      names: List[ProjectName],
      platform: Option[Platform],
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] = collected(
    console(session, names, platform).tap(report)
  )
  def console(
      session: CompileSession,
      names: List[ProjectName],
      platform: Option[Platform],
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    watch(
      session.repo,
      session.graph,
      choose(session.graph, names),
      None,
      platform,
      session.jdk,
      session.native,
      captured = Some(session.lock),
    )
  def cellFeed(
      session: CompileSession,
      cells: List[(ProjectName, ScalaVersion)],
      platform: Platform,
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    val names = cells.map(_._1)
    watch(
      session.repo,
      session.graph,
      choose(session.graph, names),
      Some(cells.map(_._2.value).toSet),
      Some(platform),
      session.jdk,
      session.native,
      captured = Some(session.lock),
    )

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
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
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
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    watch(repo, graph, choose(graph, names), None, platform, jdk, native)

  /** Fails after the last event when a cell failed. Test runs keep the lenient stream so passing cells still test.
    */
  def strict(
      feeds: ZStream[Resolve, GraphCompileError, CompileFeed]
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
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
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    watch(
      repo,
      graph,
      pickCells(graph, cells, platform),
      Some(cells.map(_._2.value).toSet),
      Some(platform),
      jdk,
      native,
    )

  private def collected(
      feeds: ZStream[Resolve, GraphCompileError, CompileFeed]
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
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
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
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
      captured: Option[DependencyLock] = None,
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    ZStream.asyncScoped[Resolve, GraphCompileError, CompileFeed](
      emit =>
        val offer    = (feed: CompileFeed) => emit(ZIO.succeed(Chunk.single(feed)))
        val compiled =
          for
            selected <- ZIO.fromEither(picked)
            _        <- ZIO.fromEither(layersOf(selected))
            lock     <- captured.fold(readLock(repo))(ZIO.succeed(_))
            tools    <- LockedTools.make(lock, graph)
            outcome  <- ZIO.scoped {
              CompilerPool.make
                .provideSome[zio.Scope](ZLayer.succeed(tools))
                .flatMap: pool =>
                  val plan = CompilePlan(
                    repo,
                    modulesOf(graph),
                    lock,
                    jdk,
                    native,
                    BuildResolvers(graph.resolvers),
                    pool,
                    tools,
                  )
                  val goal = ScheduleCells.goal(
                    plan,
                    selected,
                    only,
                    platform,
                    hold,
                    offer,
                  )
                  ZIO
                    .serviceWithZIO[Histos](_.start(goal))
                    .provideSome[Resolve & zio.Scope](
                      ZLayer.succeed(parallelism),
                      Histos.live,
                    )
                    .flatMap(_.outcome)
            }
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
      emit: ZIO[Resolve, Option[GraphCompileError], Chunk[CompileFeed]] => Unit,
      error: Option[GraphCompileError],
  ): UIO[Unit] =
    val stop = error match
      case Some(found) => ZIO.fail(Some(found))
      case None        => ZIO.fail(None)
    ZIO.attemptBlockingInterrupt(emit(stop)).orDie

  private def die(
      emit: ZIO[Resolve, Option[GraphCompileError], Chunk[CompileFeed]] => Unit
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
          GraphCompileError.MissingUpstream(_, _, _, _) | GraphCompileError.RootRead(_) | GraphCompileError.Output(_) |
          GraphCompileError.Failed(_, _, _, _) =>
        Some(error)
      case _ => None

  private[zinc] def compileCell(
      run: CellRun
  ): ZIO[Resolve, GraphCompileError, String] =
    val CellRun(plan, module, version, platform, upstreams, hold, offer) = run
    def tell(feed: CompileFeed): UIO[Unit]                               =
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
              cell =>
                val paths = upstreams.updated(
                  (module.name, version, platform),
                  cell.classes,
                )
                tell(CompileFeed.Done(cell)).as(ScheduleCells.render(paths)),
            )

  private final case class CellWork(
      plan: CompilePlan,
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      base: Path,
      upstreams: Map[CellKey, Path],
  )

  private def compileOf(
      work: CellWork,
      hear: CompileProblem => Unit,
  ): ZIO[Resolve, GraphCompileError, CompiledCell] =
    val CellWork(plan, module, version, platform, base, done) = work
    val failed = (error: CompileError) => GraphCompileError.Failed(module.name, version, platform, error)
    val chain  = Module.chain(module, plan.modules, platform)
    for
      sources  <- Sources.select(base, version, platform).mapError(failed)
      tests    <- Sources.tests(base, version, platform).mapError(failed)
      compiled <-
        if sources.isEmpty && tests.isEmpty then ZIO.fail(GraphCompileError.NoSources(module.name, base))
        else if sources.isEmpty then vacant(plan.repo, module.name, version, platform)
        else
          for
            upstreamClasses <- ZIO.fromEither(
              upstreamClasses(module, chain, version, platform, done)
            )
            apis <- ZIO
              .foreach(chain): dependency =>
                ZIO.fromEither(
                  ApiDigest.read(analysis(plan.repo, dependency.name, version, platform))
                )
              .mapError(failed)
            assembled <- ScalacClasspath
              .of(
                plan.repo,
                plan.modules,
                module,
                version,
                platform,
                plan.lock,
                plan.repositories,
                ClasspathScope.Compile,
              )
              .provideSome[Resolve](ZLayer.succeed(plan.tools))
            classes = classesDir(plan.repo, module.name, version, platform)
            request = CompileRequest(
              CompileToolchain(version, platform, plan.jdk, module.flags),
              sources,
              upstreamClasses ++ assembled.jars,
              classes,
              analysis(plan.repo, module.name, version, platform),
              assembled.hashes,
              apis,
            )
            compiled <- compile(
              work,
              request,
              SemanticDb.argv(plan.repo, version),
              hear,
            ).mapError(failed)
          yield CompiledCell(
            module.name,
            version,
            platform,
            compiled.classes,
            compiled.classFiles,
            Some(compiled.key),
            compiled.changed,
            compiled.sources,
            compiled.classesWritten,
          )
    yield compiled

  /** No main sources. The classes directory stays empty so a later test compile has a classpath root.
    */
  private def vacant(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): IO[GraphCompileError, CompiledCell] =
    val classes = classesDir(repo, module, version, platform)
    ZIO
      .attemptBlocking {
        if Files.exists(classes) then
          val walk  = Files.walk(classes)
          val paths = try walk.iterator.asScala.toList
          finally walk.close()
          paths.sortBy(path => -path.getNameCount).foreach(Files.deleteIfExists)
        Files.createDirectories(classes)
      }
      .mapError(_ => GraphCompileError.Output(classes)) *>
      ZIO
        .fromEither(ApiDigest.writeEmpty(analysis(repo, module, version, platform)))
        .mapError(error => GraphCompileError.Failed(module, version, platform, error))
        .as(
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
          )
        )

  private def compile(
      work: CellWork,
      request: CompileRequest,
      extra: List[String],
      hear: CompileProblem => Unit,
  ): IO[CompileError, CompileOutput] =
    val compiled = request.toolchain.platform match
      case Platform.Jvm =>
        JvmCompile.compilePooled(work.plan.pool, request, extra, hear)
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
    compiled.provide(ZLayer.succeed(work.plan.tools))

  private def upstreamClasses(
      module: Module,
      chain: List[Module],
      version: ScalaVersion,
      platform: Platform,
      done: Map[CellKey, Path],
  ): Either[GraphCompileError, List[Path]] =
    chain.foldLeft[Either[GraphCompileError, List[Path]]](Right(Nil)): (acc, dependency) =>
      acc.flatMap: paths =>
        done.get((dependency.name, version, platform)) match
          case Some(classes) => Right(paths :+ classes)
          case None          =>
            Left(
              GraphCompileError.MissingUpstream(
                module.name,
                dependency.name,
                version,
                platform,
              )
            )

  private def readLock(repo: Path): IO[GraphCompileError, DependencyLock] =
    Resolve
      .readLock(repo)
      .mapError:
        case ResolveError.Io(failure) =>
          GraphCompileError.LockRead(failure.path)
        case other => GraphCompileError.Lock(other)

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

  /** Inverse of `classesDir` / `analysis`. None for a path that is not a `.histos/classes/<module>/<scala>/<platform>`
    * product.
    */
  private[histos] def analysisDirOf(classes: Path): Option[Path] =
    val absolute = classes.toAbsolutePath.normalize
    for
      (platform, scalaDir) <- named(absolute)
      (scala, moduleDir)   <- named(scalaDir)
      (module, kindDir)    <- named(moduleDir)
      (kind, histos)       <- named(kindDir)
      if kind == "classes"
      (dot, repo) <- named(histos)
      if dot == ".histos"
    yield repo
      .resolve(".histos")
      .resolve("analysis")
      .resolve(module)
      .resolve(scala)
      .resolve(platform)

  private def named(path: Path): Option[(String, Path)] =
    Option(path.getFileName).map(_.toString).zip(Option(path.getParent))

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case module: Module => module }
end GraphCompile
