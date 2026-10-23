package histos.zinc

import histos.jdk.Sha256
import histos.js.ScalaJs
import histos.native.{NativeCompile, NativeOrigin, NativeToolchain}
import histos.resolve.{
  BuildResolvers,
  Classpath,
  ClasspathScope,
  DependencyLock,
  Repositories,
  Resolve,
  ResolveCell,
  ResolveError
}
import histos.workflow.CiError
import histos.schedule.{Histos, Outcome, Parallelism, StepFailure}
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, NonEmptyChunk, System, UIO, ZIO, ZLayer}
import zio.stream.ZStream

private def platformToken(platform: Platform): String =
  platform.token

final case class CompiledCell(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    classes: Path,
    classFiles: Int,
    key: Option[Sha256] = None,
)

/** One cell's console. Diagnostics stay with that cell. Parallel cells are not
  * interleaved.
  */
enum CompileFeed:
  case Began(module: ProjectName, version: ScalaVersion, platform: Platform)
  case Diagnostic(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      problem: CompileProblem
  )
  case Done(cell: CompiledCell)
  case Failed(error: GraphCompileError)

enum GraphCompileError:
  case Target(error: CiError)
  case UnknownCell(module: ProjectName, version: ScalaVersion)
  case NoRoot(module: ProjectName)
  case NoSources(module: ProjectName, root: Path)
  case MissingUpstream(
      module: ProjectName,
      dependency: ProjectName,
      version: ScalaVersion,
      platform: Platform
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
      error: CompileError
  )
  case Cells(first: GraphCompileError, rest: List[GraphCompileError])
  case Jdk(home: Path)

  def message: String = this match
    case Target(error)                => error.message
    case UnknownCell(module, version) =>
      s"'${module.value}' ${version.value} is not a cell on the requested platform"
    case NoRoot(module) =>
      s"'${module.value}' has no source root. Expected ${module.value}/src, or src when it is the only module"
    case NoSources(module, root) =>
      s"'${module.value}' has no main or test sources under $root"
    case MissingUpstream(module, dependency, version, platform) =>
      s"'${module.value}' depends on '${dependency.value}', which has no ${version.value} ${platformToken(platform)} classes"
    case Cycle(module)  => s"'${module.value}' is in a compile cycle"
    case Lock(error)    => error.message
    case LockRead(path) => s"could not read $path"
    case RootRead(path) => s"could not read source roots under $path"
    case Output(path)   => s"could not write $path"
    case Failed(module, version, platform, error) =>
      s"'${module.value}' ${version.value} ${platformToken(platform)} failed to compile\n${error.message}"
    case Cells(first, rest) => (first :: rest).map(_.message).mkString("\n")
    case Jdk(home) => s"could not read the JDK release file under $home"

object GraphCompile:
  /** Compile every platform a module names, or only `platform` when that is set.
    * Names pull in `dependsOn` upstreams and not dependents. A cell starts when
    * its own upstream cells finish.
    */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
      native: NativeOrigin,
      platform: Option[Platform] = None,
      report: CompileFeed => UIO[Unit] = _ => ZIO.unit
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
    collected(console(repo, graph, names, jdk, native, platform).tap(report))

  /** Cells compile when their upstreams are done. `platform` limits each module
    * to that target. A module that does not build it is skipped.
    */
  def console(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
      native: NativeOrigin,
      platform: Option[Platform] = None
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    watch(repo, graph, choose(graph, names), None, platform, jdk, native)

  /** Fails after the last event when a cell failed. Test runs keep the lenient stream so passing cells still test. */
  def strict(
      feeds: ZStream[Resolve, GraphCompileError, CompileFeed]
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    ZStream.unwrap:
      zio.Ref.make(List.empty[GraphCompileError]).map: failed =>
        feeds.tap {
          case CompileFeed.Failed(error) => failed.update(_ :+ error)
          case _                         => ZIO.unit
        } ++ ZStream
          .fromZIO(failed.get.flatMap {
            case Nil          => ZIO.unit
            case head :: rest => ZIO.fail(GraphCompileError.Cells(head, rest))
          })
          .drain

  /** Compile these cells on `platform`, plus their upstream closure, at the
    * requested Scala versions. A class directory from an earlier call does not
    * satisfy `upstreamClasses`. The closure is compiled in this call.
    */
  def cellFeed(
      repo: Path,
      graph: ProjectGraph,
      cells: List[(ProjectName, ScalaVersion)],
      platform: Platform,
      jdk: ResolvedJdk,
      native: NativeOrigin
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    watch(
      repo,
      graph,
      pickCells(graph, cells, platform),
      Some(cells.map(_._2.value).toSet),
      Some(platform),
      jdk,
      native
    )

  private def collected(
      feeds: ZStream[Resolve, GraphCompileError, CompileFeed]
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
    feeds.runCollect.flatMap: chunk =>
      val failed = chunk.collect { case CompileFeed.Failed(error) => error }
      val cells = chunk
        .collect { case CompileFeed.Done(cell) => cell }
        .toList
        .sortBy(cell =>
          (cell.module.value, cell.version.value, platformToken(cell.platform))
        )
      failed.toList match
        case Nil          => ZIO.succeed(cells)
        case head :: rest => ZIO.fail(GraphCompileError.Cells(head, rest))

  /** The JDK this fiber's `zio.System` is running. The release file is the version. */
  def jdkHere: IO[GraphCompileError, ResolvedJdk] =
    System.property("java.home").mapError(_ => GraphCompileError.Jdk(Path.of("."))).flatMap:
      case Some(raw) if raw.nonEmpty => fromHome(Path.of(raw))
      case _                         => ZIO.fail(GraphCompileError.Jdk(Path.of(".")))

  private def fromHome(home: Path): IO[GraphCompileError, ResolvedJdk] =
    ZIO.attemptBlocking(releaseText(home)).mapError(_ => GraphCompileError.Jdk(home)).flatMap: text =>
      histos.jdk.JdkVersion.fromRelease(text) match
        case Right(version) => ZIO.succeed(ResolvedJdk(home, version))
        case Left(_)        => ZIO.fail(GraphCompileError.Jdk(home))

  private def releaseText(home: Path): String =
    val file = home.resolve("release")
    if Files.isRegularFile(file) then Files.readString(file) else ""

  /** `<module>/src` wins. The repository `src` is only for a graph with one
    * module. The build file stays outside it.
    */
  def sourceBase(
      repo: Path,
      module: Module,
      modules: List[Module]
  ): IO[GraphCompileError, Option[Path]] =
    ZIO
      .attemptBlocking {
        val named = repo.resolve(module.baseDirectory).resolve("src")
        val shared = repo.resolve("src")
        if Files.isDirectory(named) then Some(named)
        else if modules.length == 1 && Files.isDirectory(shared) then
          Some(shared)
        else None
      }
      .mapError(_ => GraphCompileError.RootRead(repo))

  private def choose(
      graph: ProjectGraph,
      names: List[String]
  ): Either[GraphCompileError, List[Module]] =
    val modules = modulesOf(graph)
    histos.select.Selection
      .select(graph, names)
      .left
      .map(GraphCompileError.Target(_))
      .map: selected =>
        upstream(modules, selected.modules.map(_.name).toSet)

  private def pickCells(
      graph: ProjectGraph,
      cells: List[(ProjectName, ScalaVersion)],
      platform: Platform
  ): Either[GraphCompileError, List[Module]] =
    val modules = modulesOf(graph)
    val byName = modules.map(module => module.name -> module).toMap
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
      case None        => Right(upstream(modules, cells.map(_._1).toSet))

  private def upstream(
      modules: List[Module],
      roots: Set[ProjectName]
  ): List[Module] =
    val byName = modules.map(module => module.name -> module).toMap
    def grow(have: Set[ProjectName]): Set[ProjectName] =
      val extra =
        have.flatMap(name => byName.get(name).toList.flatMap(_.upstreamNames))
      if extra.subsetOf(have) then have else grow(have ++ extra)
    val needed = grow(roots)
    modules.filter(module => needed.contains(module.name))

  private def layersOf(
      modules: List[Module]
  ): Either[GraphCompileError, List[List[Module]]] =
    val names = modules.map(_.name).toSet
    def step(
        done: Set[ProjectName],
        rest: List[Module],
        built: List[List[Module]]
    ): Either[GraphCompileError, List[List[Module]]] =
      val (ready, later) = rest.partition: module =>
        module.upstreamNames.forall(dependency =>
          !names.contains(dependency) || done.contains(dependency)
        )
      ready match
        case Nil =>
          later match
            case module :: _ => Left(GraphCompileError.Cycle(module.name))
            case Nil         => Right(built.reverse)
        case _ =>
          step(done ++ ready.map(_.name), later, ready :: built)
    step(Set.empty, modules, Nil)

  /** The spec holds one cell so an unrelated cell can start. Production does not. */
  private[zinc] def gated(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
      native: NativeOrigin,
      hold: ProjectName => UIO[Unit],
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    watch(repo, graph, choose(graph, names), None, None, jdk, native, hold, Parallelism(64, 64, 64))

  private def watch(
      repo: Path,
      graph: ProjectGraph,
      picked: Either[GraphCompileError, List[Module]],
      only: Option[Set[String]],
      platform: Option[Platform],
      jdk: ResolvedJdk,
      native: NativeOrigin,
      hold: ProjectName => UIO[Unit] = _ => ZIO.unit,
      parallelism: Parallelism = Parallelism.default,
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    ZStream.asyncScoped[Resolve, GraphCompileError, CompileFeed](
      emit =>
        val offer = (feed: CompileFeed) => emit(ZIO.succeed(Chunk.single(feed)))
        val compiled =
          for
            selected <- ZIO.fromEither(picked)
            _ <- ZIO.fromEither(layersOf(selected))
            lock <- readLock(repo)
            outcome <- ZIO.scoped {
              CompilerPool.make.flatMap: pool =>
                val plan = CompilePlan(repo, modulesOf(graph), lock, jdk, native, BuildResolvers(graph.resolvers), pool)
                val goal = ScheduleCells.goal(plan, selected, only, platform, hold, offer)
                ZIO
                  .serviceWithZIO[Histos](_.start(goal))
                  .provideSome[Resolve & zio.Scope](ZLayer.succeed(parallelism), Histos.live)
                  .flatMap(_.outcome)
            }
            _ <- outcome match
              case Outcome.Done(_) => end(emit, None)
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
      .attemptBlockingInterrupt(emit(ZIO.dieMessage("compile finished with a failure that was not a GraphCompileError")))
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
          GraphCompileError.MissingUpstream(_, _, _, _) |
          GraphCompileError.RootRead(_) | GraphCompileError.Output(_) |
          GraphCompileError.Failed(_, _, _, _) =>
        Some(error)
      case _ => None

  private[zinc] def compileCell(run: CellRun): ZIO[Resolve, GraphCompileError, String] =
    val CellRun(plan, module, version, platform, upstreams, hold, offer) = run
    def tell(feed: CompileFeed): UIO[Unit] =
      ZIO.attemptBlockingInterrupt(offer(feed)).orDie
    val hear = (problem: CompileProblem) =>
      offer(CompileFeed.Diagnostic(module.name, version, platform, problem))
    hold(module.name) *>
      sourceBase(plan.repo, module, plan.modules).flatMap:
        case None =>
          tell(CompileFeed.Failed(GraphCompileError.NoRoot(module.name))).as(ScheduleCells.render(upstreams))
        case Some(base) =>
          tell(CompileFeed.Began(module.name, version, platform)) *>
            compileOf(CellWork(plan, module, version, platform, base, upstreams), hear).foldZIO(
              error =>
                kept(error) match
                  case Some(cellError) => tell(CompileFeed.Failed(cellError)).as(ScheduleCells.render(upstreams))
                  case None            => ZIO.fail(error)
              ,
              cell =>
                val paths = upstreams.updated((module.name, version, platform), cell.classes)
                tell(CompileFeed.Done(cell)).as(ScheduleCells.render(paths))
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
    val chain = Module.chain(module, plan.modules, platform)
    for
      sources <- Sources.select(base, version, platform).mapError(failed)
      tests <- Sources.tests(base, version, platform).mapError(failed)
      compiled <-
        if sources.isEmpty && tests.isEmpty then ZIO.fail(GraphCompileError.NoSources(module.name, base))
        else if sources.isEmpty then vacant(plan.repo, module.name, version, platform)
        else
          for
            upstreamClasses <- ZIO.fromEither(upstreamClasses(module, chain, version, platform, done))
            fetched <- ZIO
              .foreach(chain :+ module)(item => classpathOf(item, version, platform, plan.lock, plan.repositories))
              .map(merge)
            libraries = if platform == Platform.Jvm then withoutStdlib(fetched) else fetched
            classes = classesDir(plan.repo, module.name, version, platform)
            request = CompileRequest(
              CompileToolchain(version, platform, plan.jdk, module.flags),
              sources,
              upstreamClasses ++ libraries.paths,
              classes,
              analysis(plan.repo, module.name, version, platform),
              libraries.hashes,
            )
            compiled <- compile(module, request, plan.native, plan.pool, SemanticDb.argv(plan.repo, version), hear).mapError(failed)
          yield CompiledCell(module.name, version, platform, compiled.classes, compiled.classFiles, Some(compiled.key))
    yield compiled

  /** No main sources. The classes directory stays empty so a later test compile
    * has a classpath root.
    */
  private def vacant(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform
  ): IO[GraphCompileError, CompiledCell] =
    val classes = classesDir(repo, module, version, platform)
    ZIO
      .attemptBlocking {
        if Files.exists(classes) then
          val walk = Files.walk(classes)
          val paths = try walk.iterator.asScala.toList
          finally walk.close()
          paths.sortBy(path => -path.getNameCount).foreach(Files.deleteIfExists)
        Files.createDirectories(classes)
        CompiledCell(module, version, platform, classes, 0)
      }
      .mapError(_ => GraphCompileError.Output(classes))

  private def compile(
      module: Module,
      request: CompileRequest,
      native: NativeOrigin,
      pool: CompilerPool,
      extra: List[String],
      hear: CompileProblem => Unit
  ): IO[CompileError, CompileOutput] =
    request.toolchain.platform match
      case Platform.Jvm    => JvmCompile.compilePooled(pool, request, extra, hear)
      case Platform.Js     => ScalaJs.compile(request, extra, hear)
      case Platform.Native =>
        val revision =
          module.nativeVersion.getOrElse(NativeToolchain.ReleaseRevision)
        NativeCompile.compile(request, revision, native, extra, hear)

  private def upstreamClasses(
      module: Module,
      chain: List[Module],
      version: ScalaVersion,
      platform: Platform,
      done: Map[CellKey, Path]
  ): Either[GraphCompileError, List[Path]] =
    chain.foldLeft[Either[GraphCompileError, List[Path]]](Right(Nil)):
      (acc, dependency) =>
        acc.flatMap: paths =>
          done.get((dependency.name, version, platform)) match
            case Some(classes) => Right(paths :+ classes)
            case None          =>
              Left(
                GraphCompileError.MissingUpstream(
                  module.name,
                  dependency.name,
                  version,
                  platform
                )
              )

  /** JVM compile prepends `JvmCompile.runtime`. The lock's copy of that library
    * has to stay off.
    */
  private def withoutStdlib(fetched: Fetched): Fetched =
    val kept = fetched.paths
      .zip(fetched.hashes)
      .filterNot((path, _) => JvmCompile.isStandardLibrary(path))
    Fetched(kept.map(_._1), kept.map(_._2))

  private def merge(fetched: List[Fetched]): Fetched =
    val paired = fetched.flatMap(item => item.paths.zip(item.hashes))
    val (paths, hashes, _) =
      paired.foldLeft((List.empty[Path], List.empty[Sha256], Set.empty[Path])):
        case ((paths, hashes, seen), (path, _)) if seen.contains(path) =>
          (paths, hashes, seen)
        case ((paths, hashes, seen), (path, hash)) =>
          (paths :+ path, hashes :+ hash, seen + path)
    Fetched(paths, hashes)

  private def classpathOf(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories
  ): ZIO[Resolve, GraphCompileError, Fetched] =
    val cell = ResolveCell(
      module.name,
      Dep.select(module.dependencies, platform),
      version,
      platform,
      repositories,
      ClasspathScope.Compile
    )
    Resolve.materialize(cell, lock).mapBoth(GraphCompileError.Lock(_), fetched)

  private final case class Fetched(paths: List[Path], hashes: List[Sha256])

  private def fetched(classpath: Classpath): Fetched =
    Fetched(classpath.artifacts.map(_.path), classpath.artifacts.map(_.sha256))

  private def readLock(repo: Path): IO[GraphCompileError, DependencyLock] =
    Resolve.readLock(repo).mapError:
      case ResolveError.Io(failure) => GraphCompileError.LockRead(failure.path)
      case other                    => GraphCompileError.Lock(other)

  /** Classpath root written for one JVM cell. Test compile and the test
    * subprocess read this directory.
    */
  private[histos] def classesDir(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion
  ): Path =
    classesDir(repo, module, version, Platform.Jvm)

  private[histos] def classesDir(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform
  ): Path =
    repo
      .resolve(".histos")
      .resolve("classes")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platformToken(platform))

  private def analysis(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform
  ): Path =
    repo
      .resolve(".histos")
      .resolve("analysis")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platformToken(platform))

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case module: Module => module }
