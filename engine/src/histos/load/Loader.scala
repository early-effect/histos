package histos.load

import histos.jdk.Sha256Digest
import histos.plugin.{ApiDigest, PluginCoordinate, PluginLoadError}
import histos.resolve.{DependencyLock, Resolve}
import histos.Resolution
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.CompileProblem
import histos.HistosBuild

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, Scope, ZIO}

/** Diagnostics from compiling `histos.scala`. Positions are 0-based dotty positions.
  */
final case class BuildNotes(failed: Boolean, problems: List[CompileProblem])

trait Loader:
  def loadLocked(
      repo: Path,
      apiJar: Path,
      lock: DependencyLock,
  ): ZIO[Scope, LoadError, LoadedBuild]
  def load(repo: Path, apiJar: Path): ZIO[Scope, LoadError, LoadedBuild]

  /** Compile `histos.scala` against `classpath` and return the notes. `classpath` is the list the load compiled with.
    */
  def reportBuild(repo: Path, classpath: List[Path]): IO[LoadError, BuildNotes]
end Loader

object Loader:
  val live: zio.ZLayer[Resolve, Nothing, Loader] =
    zio.ZLayer.fromZIO(zio.ZIO.serviceWith[Resolve](LoaderLive(_)))

/** Compiles `histos.scala`, reads the graph, compiles in-repo plugin modules, then ship pins. A locked graph fetches
  * those pins. A solve asks `Resolve`. A catalog load of a distribution jar checks its compiler payload. A classes
  * directory does not. `histos.wiring.scala` is not compiled. `loadLocked` solves even when the graph is locked:
  * `resolve update` writes the cells afterwards.
  */
private final class LoaderLive(resolve: Resolve) extends Loader:
  def reportBuild(repo: Path, classpath: List[Path]): IO[LoadError, BuildNotes] =
    ZIO.scoped:
      for
        text     <- readBuild(repo)
        out      <- BuildFile.tempDir("histos-classes")
        source   <- BuildFile.writeTemp(text)
        reporter <- BuildFile.drive(List(source), classpath, out)
      yield BuildFile.notesOf(reporter, repo.resolve("histos.scala"), source)

  def load(repo: Path, apiJar: Path): ZIO[Scope, LoadError, LoadedBuild] =
    catalogued(repo, apiJar, None)

  def loadLocked(
      repo: Path,
      apiJar: Path,
      lock: DependencyLock,
  ): ZIO[Scope, LoadError, LoadedBuild] =
    ZIO.succeed(lock) *> catalogued(repo, apiJar, Some(histos.lock.LockMode.Update))

  /** A catalog uses the first-load protocol. Update still solves: the override is `Update`, and a lock file does not
    * choose it. No catalog keeps the graph's own resolution.
    */
  private def catalogued(
      repo: Path,
      apiJar: Path,
      mode: Option[histos.lock.LockMode],
  ): ZIO[Scope, LoadError, LoadedBuild] =
    val catalog = repo.resolve("histos.plugins.scala")
    ZIO.ifZIO(exists(catalog))(
      payloadOf.flatMap: payload =>
        BootstrapLoad
          .prepare(resolve, BootstrapLoad.Prepare(repo, apiJar, payload = payload, lockMode = mode))
          .provideSomeLayer[Scope](BuildCache.ephemeral),
      open(repo, apiJar, if mode.isDefined then ShipSource.Solve else ShipSource.Follow),
    )

  private enum ShipSource:
    case Follow
    case Solve

  private def open(repo: Path, apiJar: Path, source: ShipSource): ZIO[Scope, LoadError, LoadedBuild] =
    for
      text     <- readBuild(repo)
      launcher <- digestOf(apiJar)
      boundary <- Boundary.jars(apiJar)
      leases   <- GenerationLeases.make
      _        <- ZIO.addFinalizer(leases.retire)
      loaded   <- inGeneration(repo, source, text, boundary, launcher, leases).onError(_ => leases.discard)
    yield loaded

  /** Wiring loader and the class directories it reads. A short loader that only reads the graph stays in its own scope.
    * The script compiles before plugin outputs exist, so its classpath is the boundary.
    */
  private def inGeneration(
      repo: Path,
      source: ShipSource,
      text: String,
      boundary: List[Path],
      launcher: ApiDigest,
      leases: GenerationLeases,
  ): ZIO[Any, LoadError, LoadedBuild] =
    leases.use:
      for
        out        <- BuildFile.tempDir("histos-classes")
        sourceFile <- BuildFile.writeTemp(text)
        _          <- BuildFile.compileLabeled(List(sourceFile -> "histos.scala"), boundary, out)
        graph      <- readGraph(out)
        modules    <- PluginModules.compile(repo, graph, boundary, resolve)
        ships      <- shipsOf(repo, graph, launcher, source)
        plugins = modules ++ ships
        opened  <- ZIO.acquireRelease(BuildFile.open(out, plugins))(BuildFile.close)
        threads <- PluginThreads.make(opened.build)
        builds  <- BuildFile.entries(out, opened.build)
        build   <- BuildFile.select(builds)
        runtime <- PluginRuntime.loaded(
          repo,
          graph,
          opened.build,
          threads,
          Nil,
          launcher,
          leases,
          Nil,
        )
      yield LoadedBuild(graph, runtime, build, buildFileClasspath = boundary)

  /** No published pin reads no lock and calls no resolver. */
  private def shipsOf(
      repo: Path,
      graph: histos.ProjectGraph,
      launcher: ApiDigest,
      source: ShipSource,
  ): IO[LoadError, List[Path]] =
    val published = graph.coordinates.exists:
      case PluginCoordinate.Published(_, _) => true
      case PluginCoordinate.Module(_)       => false
    if !published then ZIO.succeed(Nil)
    else
      source match
        case ShipSource.Solve =>
          PluginShips.jars(graph, resolve, launcher, None)
        case ShipSource.Follow =>
          graph.resolution match
            case Resolution.Solve =>
              PluginShips.jars(graph, resolve, launcher, None)
            case Resolution.Lock =>
              Resolve
                .readLock(repo)
                .mapError(LoadError.Resolve(_))
                .flatMap: lock =>
                  PluginShips.jars(graph, resolve, launcher, Some(lock))

  /** The graph is read before plugin modules compile. This loader closes with the read. */
  private def readGraph(out: Path): IO[LoadError, histos.ProjectGraph] =
    ZIO.scoped:
      for
        opened  <- ZIO.acquireRelease(BuildFile.open(out, Nil))(BuildFile.close)
        threads <- PluginThreads.make(opened.build)
        builds  <- BuildFile.entries(out, opened.build)
        build   <- BuildFile.select(builds)
        graph   <- graphOf(opened.build, threads, build)
      yield graph

  /** A distribution jar is the compiler payload. A classes directory, and any other code source, is not. */
  private def payloadOf: IO[LoadError, Option[Path]] =
    Boundary.codeSource(classOf[histos.HistosBuild]) match
      case None       => ZIO.succeed(None)
      case Some(path) =>
        BuildClasspath.distribution(path).map(found => Option.when(found)(path))

  private def readBuild(repo: Path): IO[LoadError, String] =
    val source = repo.resolve("histos.scala")
    for
      _    <- ZIO.unlessZIO(exists(source))(ZIO.fail(LoadError.MissingFile(source)))
      text <- ZIO
        .attemptBlocking(Files.readString(source))
        .mapError(BuildFile.failed(source, IoFault.Read))
    yield text

  private def graphOf(
      loader: BuildLoader,
      threads: PluginThreads,
      build: HistosBuild,
  ): IO[LoadError, histos.ProjectGraph] =
    threads
      .run(ZIO.fromEither(build.graph).mapError(LoadError.BrokenGraph(_)))
      .either
      .flatMap: outcome =>
        loader.takeRefused match
          case Some(refusal) =>
            ZIO.fail(LoadError.Plugin(PluginLoadError.BuildLeak(refusal.binary)))
          case None => ZIO.fromEither(outcome)

  private def digestOf(apiJar: Path): IO[LoadError, ApiDigest] =
    ZIO
      .attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(apiJar))))
      .mapError(BuildFile.failed(apiJar, IoFault.Read))
      .flatMap: sha =>
        ApiDigest.from(sha.value) match
          case Right(digest) => ZIO.succeed(digest)
          case Left(_)       =>
            ZIO.fail(LoadError.Io(IoFailure(apiJar, IoFault.Stat)))

  private def exists(path: Path): IO[LoadError, Boolean] =
    ZIO
      .attemptBlocking(Files.isRegularFile(path))
      .mapError(BuildFile.failed(path, IoFault.Stat))
end LoaderLive
