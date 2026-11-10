package histos.load

import histos.jdk.Sha256Digest
import histos.plugin.{
  ApiDigest,
  PluginCoordinate,
  PluginLinked,
  PluginLoadError,
  Plugins,
  RegisteredPlugin,
  WiringBuilt,
}
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
  def buildClasspath(apiJar: Path): IO[LoadError, List[Path]]
  def reportBuild(repo: Path, apiJar: Path): IO[LoadError, BuildNotes]

object Loader:
  val live: zio.ZLayer[Resolve, Nothing, Loader] =
    zio.ZLayer.fromZIO(zio.ZIO.serviceWith[Resolve](LoaderLive(_)))

/** Compiles `histos.scala`, reads the graph, compiles in-repo plugin modules, then ship pins. A locked graph fetches
  * those pins. A solve asks `Resolve`. `histos.wiring.scala` is compiled against those classes. `loadLocked` solves
  * even when the graph is locked: `resolve update` writes the cells afterwards.
  */
private final class LoaderLive(resolve: Resolve) extends Loader:
  def buildClasspath(apiJar: Path): IO[LoadError, List[Path]] =
    Boundary.jars(apiJar)

  def reportBuild(repo: Path, apiJar: Path): IO[LoadError, BuildNotes] =
    ZIO.scoped:
      for
        text     <- readBuild(repo)
        boundary <- Boundary.jars(apiJar)
        out      <- BuildFile.tempDir("histos-classes")
        source   <- BuildFile.writeTemp(text)
        reporter <- BuildFile.drive(List(source), boundary, out)
      yield BuildFile.notesOf(reporter, repo.resolve("histos.scala"), source)

  def load(repo: Path, apiJar: Path): ZIO[Scope, LoadError, LoadedBuild] =
    open(repo, apiJar, ShipSource.Follow)

  def loadLocked(
      repo: Path,
      apiJar: Path,
      lock: DependencyLock,
  ): ZIO[Scope, LoadError, LoadedBuild] =
    ZIO.succeed(lock) *> open(repo, apiJar, ShipSource.Solve)

  private enum ShipSource:
    case Follow
    case Solve

  private def open(repo: Path, apiJar: Path, source: ShipSource): ZIO[Scope, LoadError, LoadedBuild] =
    for
      text       <- readBuild(repo)
      launcher   <- digestOf(apiJar)
      boundary   <- Boundary.jars(apiJar)
      out        <- BuildFile.tempDir("histos-classes")
      sourceFile <- BuildFile.writeTemp(text)
      _          <- BuildFile.compile(sourceFile, boundary, out)
      graph      <- readGraph(out)
      modules    <- PluginModules.compile(repo, graph, boundary)
      ships      <- shipsOf(repo, graph, launcher, source)
      plugins = modules ++ ships
      wired   <- compileWiring(repo, boundary ++ plugins, out)
      opened  <- ZIO.acquireRelease(BuildFile.open(out, plugins))(BuildFile.close)
      threads <- PluginThreads.make(opened.build)
      builds  <- BuildFile.entries(out, opened.build)
      build   <- BuildFile.select(builds)
      loaded  <- assemble(wired, opened.build, threads, graph, launcher)
      runtime <- PluginRuntime.loaded(repo, loaded.graph, opened.build, threads, loaded.links, launcher)
    yield LoadedBuild(loaded.graph, runtime, build)

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

  /** The graph decides the wiring classpath, so it is read before that compile. The loader closes with the graph. */
  private def readGraph(out: Path): IO[LoadError, histos.ProjectGraph] =
    ZIO.scoped:
      for
        opened  <- ZIO.acquireRelease(BuildFile.open(out, Nil))(BuildFile.close)
        threads <- PluginThreads.make(opened.build)
        builds  <- BuildFile.entries(out, opened.build)
        build   <- BuildFile.select(builds)
        graph   <- graphOf(opened.build, threads, build)
      yield graph

  /** The wiring sees the api and the plugin class directories. It does not see the engine. The generated object calls
    * `HistosWiring.layer`.
    */
  private def compileWiring(
      repo: Path,
      classpath: List[Path],
      out: Path,
  ): ZIO[Scope, LoadError, Boolean] =
    val wiring = repo.resolve("histos.wiring.scala")
    ZIO.ifZIO(exists(wiring))(
      for
        generated <- BuildFile.writeTemp(WiringSource.generated)
        _         <- BuildFile.compileLabeled(
          List(wiring -> "histos.wiring.scala", generated -> WiringSource.label),
          classpath,
          out,
        )
      yield true,
      ZIO.succeed(false),
    )

  private final case class Assembled(graph: histos.ProjectGraph, links: List[PluginLink])

  private def assemble(
      wired: Boolean,
      loader: BuildLoader,
      threads: PluginThreads,
      graph: histos.ProjectGraph,
      launcher: ApiDigest,
  ): IO[LoadError, Assembled] =
    if !wired then ZIO.succeed(Assembled(graph, Nil))
    else
      for
        built      <- readBuilt(loader, threads)
        registered <- ZIO.foreach(built.values.toList): plugin =>
          ZIO
            .attempt(RegisteredPlugin(plugin.id, plugin.contribute)(plugin))
            .mapError(thrown => LoadError.Plugin(PluginLoadError.Construction(thrown.getClass.getName)))
      yield Assembled(graph.withPlugins(registered), linksOf(registered, graph, launcher))

  private def readBuilt(loader: ClassLoader, threads: PluginThreads): IO[LoadError, Plugins] =
    threads.run:
      ZIO.scoped:
        loadWiring(loader).flatMap: built =>
          built.build.mapError(error => LoadError.Plugin(PluginLoadError.Construction(error.toString)))

  private def loadWiring(loader: ClassLoader): IO[LoadError, WiringBuilt] =
    ZIO
      .attemptBlocking:
        val cls = loader.loadClass("WiringBuilt$")
        cls.getField("MODULE$").get(null)
      .mapError(thrown => LoadError.Plugin(PluginLoadError.Construction(thrown.getClass.getName)))
      .flatMap:
        case null =>
          ZIO.fail(LoadError.Plugin(PluginLoadError.Construction("WiringBuilt")))
        case built: WiringBuilt =>
          ZIO.succeed(built)
        case other =>
          ZIO.fail(LoadError.Plugin(PluginLoadError.Construction(other.getClass.getName)))

  /** A module coordinate was compiled here. A ship pin arrived as a jar. Both at once is not split per value yet. */
  private def linksOf(
      registered: List[RegisteredPlugin],
      graph: histos.ProjectGraph,
      launcher: ApiDigest,
  ): List[PluginLink] =
    val linked =
      if publishedOnly(graph) then PluginLinked.FromBinary(launcher)
      else PluginLinked.FromSource(launcher, launcher)
    registered.map(plugin => PluginLink(plugin.id, linked, Nil))

  private def publishedOnly(graph: histos.ProjectGraph): Boolean =
    val published = graph.coordinates.exists:
      case histos.plugin.PluginCoordinate.Published(_, _) => true
      case histos.plugin.PluginCoordinate.Module(_)       => false
    val modules = graph.coordinates.exists:
      case histos.plugin.PluginCoordinate.Module(_)       => true
      case histos.plugin.PluginCoordinate.Published(_, _) => false
    published && !modules

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
