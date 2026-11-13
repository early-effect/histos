package histos.assembly

import histos.jdk.JdkRuntime
import histos.load.BuildClasspath
import histos.native.NativeOrigin
import histos.pack.{Assembly, AssemblyFile, Assembled, ClassJar, MainClass as BinaryName}
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  LockedTools,
  ResolutionService,
  Repositories,
  Resolve,
  ResolveCell,
}
import histos.schedule.CacheConfig
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileFeed,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  JvmCompile,
  UpstreamCell,
  UpstreamCells,
}
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, Publication, ScalaVersion, ToolchainIdentity}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.json.*
import zio.{IO, Scope, UIO, ZIO, ZLayer}

object Assemble:
  val ApiEntry: String = BuildClasspath.Entry

  /** Compile `name` and the modules it depends on, and write that assembly. A launcher also embeds the ship's library
    * members and, when this checkout has a commit, `META-INF/histos/toolchain.json`.
    */
  def run(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      native: NativeOrigin,
      name: ProjectName,
      destination: Path,
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve & histos.cache.ContentStore & ResolutionService, AssembleError, Assembled] =
    ZIO.scoped:
      for
        target  <- ZIO.fromEither(require(graph, name))
        main    <- ZIO.fromEither(mainOf(target))
        libs    <- ZIO.fromEither(libraries(graph, target.name))
        process <- CacheConfig.processEnv
        remote  <- CacheConfig
          .open(process)
          .mapError(error => AssembleError.Toolchain(CompileError.Remote(error)))
        ordered  <- ZIO.fromEither(closure(target, index(graph)))
        resolved <- ZIO.service[ResolutionService]
        lock     <- resolved.current
        compiled <- GraphCompile
          .run(
            histos.zinc.CompileSession(repo, graph, jdk, native, lock),
            List(target.name),
            None,
            report,
          )
          .provideSomeLayer(ZLayer.succeed(remote))
          .mapError(AssembleError.Compile(_))
        version = target.versions.first
        tools   = zio.ZLayer.fromZIO(LockedTools.make(lock, graph, resolved))
        upstream <- UpstreamCells
          .read(
            target,
            Module.chain(target, graph.projects.collect { case found: Module => found }, Platform.Jvm),
            version,
            Platform.Jvm,
            UpstreamCells.compiled(compiled),
          )
          .mapError(AssembleError.Compile(_))
          .provideSome[Resolve](tools)
        cells <- ZIO.fromEither(placed(target, ordered, upstream, compiled, version))
        classes = cells.map(_.classes)
        jars    <- artifacts(cells, BuildResolvers(graph.resolvers))
        runtime <- JvmCompile
          .runtime(version)
          .mapError(error => AssembleError.Toolchain(error))
          .provideSome[Resolve](tools)
        dirs      <- ZIO.fromEither(directories(libs, cells))
        extra     <- embedded(repo, dirs, isLauncher(graph, target.name))
        assembled <- pack(
          classes,
          runtime ++ jars.filterNot(runtime.contains),
          main,
          extra,
          destination,
        )
      yield assembled

  private def require(graph: ProjectGraph, name: ProjectName): Either[AssembleError, Module] =
    graph.get(name) match
      case Some(found: Module) => Right(found)
      case _                   => Left(AssembleError.NoModule(name.value))

  private def mainOf(module: Module): Either[AssembleError, BinaryName] =
    module.publication match
      case Publication.Assembly(main) => Right(BinaryName.trusted(main.value))
      case _                          => Left(AssembleError.NotAssembly(module.name))

  /** Library members of the ship that names `launcher`. Empty when `launcher` is not a launcher. */
  private def libraries(graph: ProjectGraph, launcher: ProjectName): Either[AssembleError, List[Module]] =
    graph.catalog.ships.find(_.toolchain.exists(_.launcher == launcher)) match
      case None       => Right(Nil)
      case Some(ship) =>
        ship.members.toList
          .filterNot(_ == launcher)
          .foldLeft[Either[AssembleError, List[Module]]](Right(Nil)): (acc, name) =>
            acc.flatMap: seen =>
              graph.get(name) match
                case Some(module: Module) =>
                  module.publication match
                    case Publication.Library => Right(seen :+ module)
                    case _                   => Right(seen)
                case _ => Left(AssembleError.NoModule(name.value))

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

  /** The assembly's own cell, then each module it depends on, in `ordered` order. */
  private def placed(
      root: Module,
      ordered: List[Module],
      upstream: List[UpstreamCell],
      compiled: List[CompiledCell],
      version: ScalaVersion,
  ): Either[AssembleError, List[UpstreamCell]] =
    ordered.foldLeft[Either[AssembleError, List[UpstreamCell]]](Right(Nil)): (acc, module) =>
      acc.flatMap: cells =>
        val found =
          if module.name == root.name then
            compiled
              .find(cell => cell.module == root.name && cell.version == version && cell.platform == Platform.Jvm)
              .map(cell => UpstreamCell(root, version, cell.classes))
          else upstream.find(_.module.name == module.name)
        found match
          case Some(cell) => Right(cells :+ cell)
          case None       => Left(AssembleError.NoClasses(module.name, version))

  /** Each library member's own cell. The launcher's Scala version is not the member's. */
  private def directories(libs: List[Module], cells: List[UpstreamCell]): Either[AssembleError, List[Path]] =
    libs.foldLeft[Either[AssembleError, List[Path]]](Right(Nil)): (acc, module) =>
      acc.flatMap: seen =>
        cells.find(_.module.name == module.name) match
          case Some(cell) => Right(seen :+ cell.classes)
          case None       => Left(AssembleError.NoClasses(module.name, module.versions.first))

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

  /** The bootstrap jar of library classes. A launcher also carries the identity resource when git names a commit. */
  private def embedded(
      repo: Path,
      dirs: List[Path],
      launcher: Boolean,
  ): ZIO[Scope, AssembleError, List[AssemblyFile]] =
    for
      nested <- nested(dirs)
      ident  <- if launcher then identityFile(repo) else ZIO.succeed(None)
    yield nested.toList ++ ident.toList

  private def isLauncher(graph: ProjectGraph, name: ProjectName): Boolean =
    graph.catalog.ships.exists(_.toolchain.exists(_.launcher == name))

  private def nested(dirs: List[Path]): ZIO[Scope, AssembleError, Option[AssemblyFile]] =
    dirs match
      case Nil        => ZIO.succeed(None)
      case one :: Nil =>
        for
          dir <- temp("histos-api")
          jar = dir.resolve("libraries.jar")
          written <- ClassJar.write(one, jar).mapError(AssembleError.Jar(_))
        yield Some(AssemblyFile(ApiEntry, written.path))
      case many =>
        for
          dir <- temp("histos-api")
          merged = dir.resolve("classes")
          _ <- ZIO.foreachDiscard(many)(copyTree(_, merged))
          jar = dir.resolve("libraries.jar")
          written <- ClassJar.write(merged, jar).mapError(AssembleError.Jar(_))
        yield Some(AssemblyFile(ApiEntry, written.path))

  private def identityFile(repo: Path): ZIO[Scope, AssembleError, Option[AssemblyFile]] =
    ToolchainIdentity
      .discover(repo)
      .flatMap:
        case None           => ZIO.succeed(None)
        case Some(identity) =>
          for
            dir <- temp("histos-identity")
            path = dir.resolve("toolchain.json")
            _ <- ZIO
              .attemptBlocking(Files.writeString(path, identity.toJson, StandardCharsets.UTF_8))
              .mapError(_ => AssembleError.Io(path))
          yield Some(AssemblyFile(ToolchainIdentity.entry, path))

  private def copyTree(from: Path, into: Path): IO[AssembleError, Unit] =
    ZIO
      .attemptBlocking:
        val walk = Files.walk(from)
        try
          walk.forEach: path =>
            val dest = into.resolve(from.relativize(path))
            if Files.isDirectory(path) then Files.createDirectories(dest)
            else
              Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
              Files.copy(path, dest, StandardCopyOption.REPLACE_EXISTING)
        finally walk.close()
      .mapError(_ => AssembleError.Io(from))
      .unit

  private def pack(
      classes: List[Path],
      jars: List[Path],
      main: BinaryName,
      extra: List[AssemblyFile],
      destination: Path,
  ): IO[AssembleError, Assembled] =
    Assembly
      .write(classes, jars, main, destination, extra)
      .mapError(AssembleError.Pack(_))

  private def temp(name: String): ZIO[Scope, AssembleError, Path] =
    Workspace
      .work(name)
      .mapError(failure => AssembleError.Io(failure.path))
end Assemble
