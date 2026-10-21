package histos.zinc

import histos.jdk.Sha256
import histos.js.ScalaJs
import histos.native.{NativeCompile, NativeToolchain}
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
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}
import java.util.concurrent.LinkedBlockingQueue

import scala.jdk.CollectionConverters.*
import java.util.concurrent.atomic.AtomicBoolean

import zio.{Chunk, IO, UIO, ZIO}
import zio.stream.ZStream

private def platformToken(platform: Platform): String =
  platform.token

final case class CompiledCell(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    classes: Path,
    classFiles: Int
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
  /** Compile every platform a module names, or only `platform` when that is
    * set. Names pull in `dependsOn` upstreams and not dependents. Independent
    * modules in one layer compile together.
    */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
      platform: Option[Platform] = None,
      report: CompileFeed => UIO[Unit] = _ => ZIO.unit
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
    collected(console(repo, graph, names, jdk, platform).tap(report))

  /** One cell at a time, in dependency order, then module name, then Scala
    * version. A layer still compiles in parallel. Its lines are not
    * interleaved. `platform` limits each module to that target. A module that
    * does not build it is skipped.
    */
  def console(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
      platform: Option[Platform] = None
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    watch(repo, graph, choose(graph, names), None, platform, jdk)

  /** Compile these JVM cells, plus `dependsOn` at the same Scala version. A
    * module's other Scala versions stay untouched.
    */
  def runCells(
      repo: Path,
      graph: ProjectGraph,
      cells: List[(ProjectName, ScalaVersion)],
      jdk: ResolvedJdk
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
    collected(
      watch(
        repo,
        graph,
        chooseCells(graph, cells),
        Some(cells.map(_._2.value).toSet),
        None,
        jdk
      )
    )

  /** Compile these cells on `platform`, plus their upstream closure, at the
    * requested Scala versions. A class directory from an earlier call does not
    * satisfy `upstreamClasses`. The closure is compiled in this call.
    */
  def cellFeed(
      repo: Path,
      graph: ProjectGraph,
      cells: List[(ProjectName, ScalaVersion)],
      platform: Platform,
      jdk: ResolvedJdk
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    watch(
      repo,
      graph,
      pickCells(graph, cells, platform),
      Some(cells.map(_._2.value).toSet),
      Some(platform),
      jdk
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

  def jdkHere: IO[GraphCompileError, ResolvedJdk] =
    val home = Option(java.lang.System.getProperty("java.home"))
      .filter(_.nonEmpty)
      .map(Path.of(_))
    home match
      case None       => ZIO.fail(GraphCompileError.Jdk(Path.of(".")))
      case Some(path) =>
        ZIO
          .attemptBlocking(Files.readString(path.resolve("release")))
          .mapBoth(
            _ => GraphCompileError.Jdk(path),
            text => histos.jdk.JdkVersion.fromRelease(text)
          )
          .flatMap:
            case Right(version) => ZIO.succeed(ResolvedJdk(path, version))
            case Left(_)        => ZIO.fail(GraphCompileError.Jdk(path))

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

  private def chooseCells(
      graph: ProjectGraph,
      cells: List[(ProjectName, ScalaVersion)]
  ): Either[GraphCompileError, List[Module]] =
    val modules = modulesOf(graph)
    val byName = modules.map(module => module.name -> module).toMap
    def known(name: ProjectName, version: ScalaVersion): Boolean =
      byName.get(name) match
        case Some(module) =>
          module.platforms.all.contains(Platform.Jvm) && module.versions.all
            .exists(_.value == version.value)
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

  private def watch(
      repo: Path,
      graph: ProjectGraph,
      picked: Either[GraphCompileError, List[Module]],
      only: Option[Set[String]],
      platform: Option[Platform],
      jdk: ResolvedJdk
  ): ZStream[Resolve, GraphCompileError, CompileFeed] =
    ZStream.unwrapScoped {
      for
        selected <- ZIO.fromEither(picked)
        layers <- ZIO.fromEither(layersOf(selected))
        lock <- readLock(repo)
        notes = slots(selected, only, platform)
        result <- zio.Promise.make[GraphCompileError, Unit]
        _ <- compileLayers(
          repo,
          modulesOf(graph),
          layers,
          lock,
          jdk,
          BuildResolvers(graph.resolvers),
          only,
          platform,
          notes
        )
          .foldZIO(
            error => ZIO.succeed(closeAll(notes)) *> result.fail(error),
            _ => ZIO.succeed(closeAll(notes)) *> result.succeed(())
          )
          .ensuring(ZIO.succeed(closeAll(notes)))
          .forkScoped
      yield drains(layers, only, platform, notes) ++ ZStream
        .fromZIO(result.await)
        .drain
    }

  private type CellKey = (ProjectName, ScalaVersion, Platform)

  private def slots(
      modules: List[Module],
      only: Option[Set[String]],
      platform: Option[Platform]
  ): Map[CellKey, CellNotes] =
    modules
      .flatMap(module =>
        cellsOf(module, only, platform).map(key => key -> CellNotes(key))
      )
      .toMap

  private def cellsOf(
      module: Module,
      only: Option[Set[String]],
      platform: Option[Platform]
  ): List[CellKey] =
    wanted(module, only).flatMap(version =>
      platforms(module, platform).map(chosen => (module.name, version, chosen))
    )

  private def platforms(
      module: Module,
      keep: Option[Platform]
  ): List[Platform] =
    val built = List(Platform.Jvm, Platform.Js, Platform.Native).filter(
      module.platforms.all.contains
    )
    keep match
      case None         => built
      case Some(chosen) => built.filter(_ == chosen)

  private def wanted(
      module: Module,
      only: Option[Set[String]]
  ): List[ScalaVersion] =
    val versions = only match
      case None       => module.versions.all
      case Some(keep) =>
        module.versions.all.filter(version => keep.contains(version.value))
    versions.sortBy(_.value)

  private def closeAll(notes: Map[CellKey, CellNotes]): Unit =
    notes.values.foreach(_.finish(Note.Stop))

  private def drains(
      layers: List[List[Module]],
      only: Option[Set[String]],
      platform: Option[Platform],
      notes: Map[CellKey, CellNotes]
  ): ZStream[Any, GraphCompileError, CompileFeed] =
    val streams = layers.flatMap: layer =>
      layer
        .sortBy(_.name.value)
        .flatMap: module =>
          cellsOf(module, only, platform).flatMap: key =>
            notes.get(key).map(drain).toList
    ZStream.concatAll(Chunk.fromIterable(streams))

  private def drain(
      slot: CellNotes
  ): ZStream[Any, GraphCompileError, CompileFeed] =
    ZStream
      .fromZIO(ZIO.attemptBlockingInterrupt(slot.take()).orDie)
      .flatMap:
        case Note.Feed(feed)    => ZStream.succeed(feed) ++ drain(slot)
        case Note.Stop          => ZStream.empty
        case Note.Broken(error) => ZStream.fail(error)

  private enum Note:
    case Feed(feed: CompileFeed)
    case Stop
    case Broken(error: GraphCompileError)

  private final class CellNotes(key: CellKey):
    private val notes = new LinkedBlockingQueue[Note]()
    private val closed = new AtomicBoolean(false)

    def began(): Unit =
      notes.put(Note.Feed(CompileFeed.Began(key._1, key._2, key._3)))

    def hear(problem: CompileProblem): Unit =
      notes.put(
        Note.Feed(CompileFeed.Diagnostic(key._1, key._2, key._3, problem))
      )

    def succeed(cell: CompiledCell): Unit =
      notes.put(Note.Feed(CompileFeed.Done(cell)))
      finish(Note.Stop)

    /** A cell failed. Siblings keep running. The stream records the failure and
      * this slot stops.
      */
    def failed(error: GraphCompileError): Unit =
      notes.put(Note.Feed(CompileFeed.Failed(error)))
      finish(Note.Stop)

    def fail(error: GraphCompileError): Unit =
      finish(Note.Broken(error))

    def finish(note: Note): Unit =
      if closed.compareAndSet(false, true) then notes.put(note)

    def take(): Note =
      notes.take()

  private def compileLayers(
      repo: Path,
      all: List[Module],
      layers: List[List[Module]],
      lock: DependencyLock,
      jdk: ResolvedJdk,
      repositories: Repositories,
      only: Option[Set[String]],
      platform: Option[Platform],
      notes: Map[CellKey, CellNotes]
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
    ZIO
      .foldLeft(layers)((Map.empty[CellKey, Path], List.empty[CompiledCell])):
        (state, layer) =>
          val (done, acc) = state
          ZIO
            .foreachPar(layer)(module =>
              compileModule(
                repo,
                all,
                module,
                done,
                lock,
                jdk,
                repositories,
                only,
                platform,
                notes
              )
            )
            .map: batches =>
              val cells = batches.flatten
              val paths = cells
                .map(cell =>
                  (cell.module, cell.version, cell.platform) -> cell.classes
                )
                .toMap
              (done ++ paths, acc ++ cells)
      .map(_._2)

  private def compileModule(
      repo: Path,
      all: List[Module],
      module: Module,
      done: Map[CellKey, Path],
      lock: DependencyLock,
      jdk: ResolvedJdk,
      repositories: Repositories,
      only: Option[Set[String]],
      platform: Option[Platform],
      notes: Map[CellKey, CellNotes]
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
    val runs = cellsOf(module, only, platform).flatMap(key =>
      notes.get(key).map(key -> _)
    )
    val builds = platform.forall(module.platforms.all.contains)
    if !builds then ZIO.succeed(Nil)
    else
      sourceBase(repo, module, all).flatMap:
        case None =>
          ZIO
            .foreachDiscard(runs)((_, slot) =>
              ZIO.succeed(slot.failed(GraphCompileError.NoRoot(module.name)))
            )
            .as(Nil)
        case Some(base) =>
          ZIO
            .foreachPar(runs):
              case ((_, version, chosen), slot) =>
                ZIO.succeed(slot.began()) *>
                  compileVersion(
                    repo,
                    all,
                    module,
                    version,
                    chosen,
                    base,
                    done,
                    lock,
                    jdk,
                    repositories,
                    slot.hear
                  ).foldZIO(
                    error =>
                      kept(error) match
                        case Some(cellError) =>
                          ZIO.succeed(slot.failed(cellError)).as(None)
                        case None =>
                          ZIO.succeed(slot.fail(error)) *> ZIO.fail(error)
                    ,
                    cell => ZIO.succeed(slot.succeed(cell)).as(Some(cell))
                  )
            .map(_.flatten.toList)

  /** These stay on the feed. The others still fail the compile. */
  private def kept(error: GraphCompileError): Option[GraphCompileError] =
    error match
      case GraphCompileError.NoRoot(_) | GraphCompileError.NoSources(_, _) |
          GraphCompileError.MissingUpstream(_, _, _, _) |
          GraphCompileError.RootRead(_) | GraphCompileError.Output(_) |
          GraphCompileError.Failed(_, _, _, _) =>
        Some(error)
      case _ => None

  private def compileVersion(
      repo: Path,
      modules: List[Module],
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      base: Path,
      done: Map[CellKey, Path],
      lock: DependencyLock,
      jdk: ResolvedJdk,
      repositories: Repositories,
      hear: CompileProblem => Unit
  ): ZIO[Resolve, GraphCompileError, CompiledCell] =
    val failed = (error: CompileError) =>
      GraphCompileError.Failed(module.name, version, platform, error)
    val chain = Module.chain(module, modules, platform)
    for
      sources <- Sources.select(base, version, platform).mapError(failed)
      tests <- Sources.tests(base, version, platform).mapError(failed)
      compiled <-
        if sources.isEmpty && tests.isEmpty then
          ZIO.fail(GraphCompileError.NoSources(module.name, base))
        else if sources.isEmpty then
          vacant(repo, module.name, version, platform)
        else
          for
            upstreamClasses <- ZIO.fromEither(
              upstreamClasses(module, chain, version, platform, done)
            )
            fetched <- ZIO
              .foreach(chain :+ module)(item =>
                classpathOf(item, version, platform, lock, repositories)
              )
              .map(merge)
            kept =
              if platform == Platform.Jvm then withoutStdlib(fetched)
              else fetched
            classes = classesDir(repo, module.name, version, platform)
            request = CompileRequest(
              CompileToolchain(version, platform, jdk, module.flags),
              sources,
              upstreamClasses ++ kept.paths,
              classes,
              analysis(repo, module.name, version, platform),
              kept.hashes
            )
            compiled <- compile(
              module,
              request,
              SemanticDb.argv(repo, version),
              hear
            ).mapError(failed)
          yield CompiledCell(
            module.name,
            version,
            platform,
            compiled.classes,
            compiled.classFiles
          )
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
      extra: List[String],
      hear: CompileProblem => Unit
  ): IO[CompileError, CompileOutput] =
    request.toolchain.platform match
      case Platform.Jvm    => JvmCompile.compile(request, extra, hear)
      case Platform.Js     => ScalaJs.compile(request, extra, hear)
      case Platform.Native =>
        val revision =
          module.nativeVersion.getOrElse(NativeToolchain.ReleaseRevision)
        NativeCompile.compile(request, revision, extra, hear)

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
    val path = repo.resolve(".histos").resolve("dependencies.lock")
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(path) then Right(Files.readString(path))
        else Left(GraphCompileError.Lock(ResolveError.NoLock(path)))
      }
      .mapError(_ => GraphCompileError.LockRead(path))
      .flatMap(ZIO.fromEither)
      .flatMap: text =>
        ZIO.fromEither(
          DependencyLock
            .parse(text)
            .left
            .map(error => GraphCompileError.Lock(ResolveError.Lock(error)))
        )

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
