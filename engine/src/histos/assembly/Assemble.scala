package histos.assembly

import histos.jdk.JdkRuntime
import histos.resolve.LockedTools
import histos.zinc.JvmCompile
import histos.load.BuildClasspath
import histos.native.NativeOrigin
import histos.pack.{Assembly, AssemblyError, AssemblyFile, Assembled, ClassJar, JarError, MainClass}
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  DependencyLock,
  ResolutionService,
  Repositories,
  RepositoryIo,
  Resolve,
  ResolveCell,
  ResolveError,
}
import histos.schedule.CacheConfig
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileFeed,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  UpstreamCell,
  UpstreamCells,
}
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, Scope, UIO, ZIO, ZLayer}

object Assemble:
  val ApiEntry: String = BuildClasspath.Entry
  private val Program  = MainClass.trusted("histos.cli.Main")

  /** Compile `app` and the modules it depends on, and write the one jar the launcher execs.
    */
  def run(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      native: NativeOrigin,
      destination: Path,
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve & histos.cache.ContentStore & ResolutionService, AssembleError, Assembled] =
    ZIO.scoped:
      for
        process <- CacheConfig.processEnv
        remote  <- CacheConfig
          .open(process)
          .mapError(error => AssembleError.Toolchain(CompileError.Remote(error)))
        app      <- module(graph, "app")
        api      <- module(graph, "api")
        ordered  <- ZIO.fromEither(closure(app, index(graph)))
        resolved <- ZIO.service[ResolutionService]
        lock     <- resolved.current
        _        <- ToolchainStamp.write(repo)
        compiled <- GraphCompile
          .run(
            histos.zinc.CompileSession(repo, graph, jdk, native, lock),
            List(app.name),
            None,
            report,
          )
          .provideSomeLayer(ZLayer.succeed(remote))
          .mapError(AssembleError.Compile(_))
        version = app.versions.first
        tools   = zio.ZLayer.fromZIO(LockedTools.make(lock, graph, resolved))
        upstream <- UpstreamCells
          .read(
            app,
            Module.chain(app, graph.projects.collect { case found: Module => found }, Platform.Jvm),
            version,
            Platform.Jvm,
            UpstreamCells.compiled(compiled),
          )
          .mapError(AssembleError.Compile(_))
          .provideSome[Resolve](tools)
        cells <- ZIO.fromEither(placed(app, ordered, upstream, compiled, version))
        classes = cells.map(_.classes)
        jars    <- artifacts(cells, BuildResolvers(graph.resolvers))
        library <- JvmCompile
          .runtime(version)
          .mapError(error => AssembleError.Toolchain(error))
          .provideSome[Resolve](tools)
        apiClasses <- ZIO.fromEither(apiDirectory(api.name, cells, version))
        assembled  <- pack(
          classes,
          library ++ jars.filterNot(library.contains),
          apiClasses,
          destination,
        )
      yield assembled

  private def module(
      graph: ProjectGraph,
      name: String,
  ): IO[AssembleError, Module] =
    ZIO
      .fromEither(
        ProjectName
          .from(name)
          .left
          .map(error => AssembleError.NoModule(error.message))
      )
      .flatMap: project =>
        graph.get(project) match
          case Some(found: Module) => ZIO.succeed(found)
          case _                   => ZIO.fail(AssembleError.NoModule(name))

  private def index(graph: ProjectGraph): Map[ProjectName, Module] =
    graph.projects.collect { case found: Module => found.name -> found }.toMap

  private def closure(
      module: Module,
      modules: Map[ProjectName, Module],
  ): Either[AssembleError, List[Module]] =
    module
      .upstream(Platform.Jvm)
      .foldLeft[Either[AssembleError, List[Module]]](Right(Nil)): (acc, name) =>
        acc.flatMap: seen =>
          modules.get(name) match
            case None       => Left(AssembleError.NoModule(name.value))
            case Some(next) =>
              closure(next, modules).map(deeper => seen ++ deeper)
      .map(dependencies => (module :: dependencies).distinctBy(_.name))

  /** `app`'s own cell, then each module it depends on at the cell `upstream` read, in `ordered` order. */
  private def placed(
      app: Module,
      ordered: List[Module],
      upstream: List[UpstreamCell],
      compiled: List[CompiledCell],
      version: ScalaVersion,
  ): Either[AssembleError, List[UpstreamCell]] =
    ordered.foldLeft[Either[AssembleError, List[UpstreamCell]]](Right(Nil)): (acc, module) =>
      acc.flatMap: cells =>
        val found =
          if module.name == app.name then
            compiled
              .find(cell => cell.module == app.name && cell.version == version && cell.platform == Platform.Jvm)
              .map(cell => UpstreamCell(app, version, cell.classes))
          else upstream.find(_.module.name == module.name)
        found match
          case Some(cell) => Right(cells :+ cell)
          case None       => Left(AssembleError.NoClasses(module.name, version))

  private def apiDirectory(
      api: ProjectName,
      cells: List[UpstreamCell],
      version: ScalaVersion,
  ): Either[AssembleError, Path] =
    cells.find(_.module.name == api) match
      case Some(cell) => Right(cell.classes)
      case None       => Left(AssembleError.NoClasses(api, version))

  /** Each cell's runtime jars, at that cell's own version. */
  private def artifacts(
      cells: List[UpstreamCell],
      repositories: Repositories,
  ): ZIO[Resolve & ResolutionService, AssembleError, List[Path]] =
    ZIO.foldLeft(cells)(List.empty[Path]): (acc, placed) =>
      val cell = ResolveCell(
        placed.module.name,
        Dep.runtime(placed.module.dependencies, Platform.Jvm),
        placed.version,
        Platform.Jvm,
        repositories,
        ClasspathScope.Compile,
      )
      ZIO
        .serviceWithZIO[ResolutionService](_.module(cell))
        .mapBoth(
          AssembleError.Resolve(_),
          classpath =>
            val fresh = classpath.artifacts.map(_.path).filterNot(acc.contains)
            acc ++ fresh,
        )

  private def readLock(repo: Path): IO[AssembleError, DependencyLock] =
    Resolve
      .readLock(repo)
      .mapError:
        case ResolveError.Io(failure) => AssembleError.Io(failure.path)
        case other                    => AssembleError.Resolve(other)

  private def pack(
      classes: List[Path],
      jars: List[Path],
      apiClasses: Path,
      destination: Path,
  ): ZIO[Any, AssembleError, Assembled] =
    ZIO.scoped:
      for
        dir <- temp
        jar = dir.resolve("histos-api.jar")
        _         <- ClassJar.write(apiClasses, jar).mapError(AssembleError.Jar(_))
        assembled <- Assembly
          .write(
            classes,
            jars,
            Program,
            destination,
            List(AssemblyFile(ApiEntry, jar)),
          )
          .mapError(AssembleError.Pack(_))
      yield assembled

  private def temp: ZIO[Scope, AssembleError, Path] =
    Workspace
      .work("histos-api")
      .mapError(failure => AssembleError.Io(failure.path))
end Assemble
