package histos.assembly

import histos.load.BuildClasspath
import histos.pack.{Assembly, AssemblyError, AssemblyFile, Assembled, ClassJar, JarError, MainClass}
import histos.resolve.{BuildResolvers, ClasspathScope, DependencyLock, Repositories, RepositoryIo, Resolve, ResolveCell, ResolveError}
import histos.zinc.{CompiledCell, GraphCompile, GraphCompileError, ResolvedJdk}
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, Scope, ZIO}

enum AssembleError:
  case Compile(error: GraphCompileError)
  case Resolve(error: ResolveError)
  case Jar(error: JarError)
  case Pack(error: AssemblyError)
  case NoModule(name: String)
  case NoClasses(module: ProjectName, version: ScalaVersion)
  case Io(path: Path)

  def message: String = this match
    case Compile(error)             => error.message
    case Resolve(error)             => error.message
    case Jar(error)                 => error.message
    case Pack(error)                => error.message
    case NoModule(name)             => s"'$name' is not a module"
    case NoClasses(module, version) => s"'${module.value}' has no ${version.value} classes"
    case Io(path)                   => s"could not write $path"

object Assemble:
  val ApiEntry: String = BuildClasspath.Entry
  private val Program = MainClass.trusted("histos.cli.Main")

  /** Compile `app` and the modules it depends on, and write the one jar the launcher execs. */
  def run(repo: Path, graph: ProjectGraph, jdk: ResolvedJdk, destination: Path): ZIO[Resolve, AssembleError, Assembled] =
    for
      app <- module(graph, "app")
      api <- module(graph, "api")
      ordered <- ZIO.fromEither(closure(app, index(graph)))
      compiled <- GraphCompile.run(repo, graph, List(app.name.value), jdk).mapError(AssembleError.Compile(_))
      classes <- ZIO.fromEither(directories(ordered, compiled, app.versions.first))
      lock <- readLock(repo)
      jars <- artifacts(ordered, app.versions.first, BuildResolvers(graph.resolvers), lock)
      library <- standardLibrary(app.versions.first)
      apiClasses <- ZIO.fromEither(apiDirectory(api.name, compiled, app.versions.first))
      assembled <- pack(classes, library ++ jars.filterNot(library.contains), apiClasses, destination)
    yield assembled

  private def module(graph: ProjectGraph, name: String): IO[AssembleError, Module] =
    ZIO.fromEither(ProjectName.from(name).left.map(error => AssembleError.NoModule(error.message))).flatMap: project =>
      graph.get(project) match
        case Some(found: Module) => ZIO.succeed(found)
        case _                    => ZIO.fail(AssembleError.NoModule(name))

  private def index(graph: ProjectGraph): Map[ProjectName, Module] =
    graph.projects.collect { case found: Module => found.name -> found }.toMap

  private def closure(module: Module, modules: Map[ProjectName, Module]): Either[AssembleError, List[Module]] =
    module.upstream(Platform.Jvm).foldLeft[Either[AssembleError, List[Module]]](Right(Nil)): (acc, name) =>
      acc.flatMap: seen =>
        modules.get(name) match
          case None       => Left(AssembleError.NoModule(name.value))
          case Some(next) => closure(next, modules).map(deeper => seen ++ deeper)
    .map(dependencies => (module :: dependencies).distinctBy(_.name))

  private def directories(
      ordered: List[Module],
      compiled: List[CompiledCell],
      version: ScalaVersion,
  ): Either[AssembleError, List[Path]] =
    val cells = compiled.filter(_.version == version)
    ordered.foldLeft[Either[AssembleError, List[Path]]](Right(Nil)): (acc, module) =>
      acc.flatMap: paths =>
        cells.find(_.module == module.name) match
          case Some(cell) => Right(paths :+ cell.classes)
          case None       => Left(AssembleError.NoClasses(module.name, version))

  private def apiDirectory(api: ProjectName, compiled: List[CompiledCell], version: ScalaVersion): Either[AssembleError, Path] =
    compiled.find(cell => cell.module == api && cell.version == version) match
      case Some(cell) => Right(cell.classes)
      case None       => Left(AssembleError.NoClasses(api, version))

  private def artifacts(
      ordered: List[Module],
      version: ScalaVersion,
      repositories: Repositories,
      lock: DependencyLock,
  ): ZIO[Resolve, AssembleError, List[Path]] =
    ZIO.foldLeft(ordered)(List.empty[Path]): (acc, module) =>
      val cell = ResolveCell(module.name, Dep.select(module.dependencies, Platform.Jvm), version, Platform.Jvm, repositories, ClasspathScope.Compile)
      Resolve.materialize(cell, lock).mapBoth(
        AssembleError.Resolve(_),
        classpath =>
          val fresh = classpath.artifacts.map(_.path).filterNot(acc.contains)
          acc ++ fresh,
      )

  /**
   * The program and `scala3-compiler` share one class loader in this jar.
   * The compiler calls methods that exist on the Scala version the module names.
   * A lock can still carry an older `scala-library`, so that version goes in first and wins.
   */
  private def standardLibrary(version: ScalaVersion): IO[AssembleError, List[Path]] =
    ZIO.attemptBlockingInterrupt(fetchLibrary(version)).mapError(_ =>
      AssembleError.Resolve(ResolveError.Repository(RepositoryIo.Fetch))
    ).flatMap(ZIO.fromEither)

  private def fetchLibrary(version: ScalaVersion): Either[AssembleError, List[Path]] =
    val dependency = coursier.Dependency(
      coursier.Module(coursier.Organization("org.scala-lang"), coursier.ModuleName("scala-library")),
      coursier.version.VersionConstraint(version.value),
    )
    val files = coursier.cache.FileCache()
    val got = coursier.Fetch(files)(using coursier.util.Task.sync)
      .noMirrors
      .withGradleModuleSupport(false)
      .addDependencies(dependency)
      .eitherResult()
    got match
      case Left(_) => Left(AssembleError.Resolve(ResolveError.Repository(RepositoryIo.Fetch)))
      case Right(result) =>
        val paths = result.files.map(_.toPath).toList
        if paths.isEmpty then Left(AssembleError.Resolve(ResolveError.NotFound("org.scala-lang", "scala-library", version.value)))
        else Right(paths.sortBy(path => if libraryJar(path) then 0 else 1))

  private def libraryJar(path: Path): Boolean =
    Option(path.getFileName).exists(_.toString.startsWith("scala-library-"))

  private def readLock(repo: Path): IO[AssembleError, DependencyLock] =
    val path = Resolve.lockPath(repo)
    ZIO.attemptBlocking {
      if Files.isRegularFile(path) then Right(Files.readString(path))
      else Left(AssembleError.Resolve(ResolveError.NoLock(path)))
    }.mapError(_ => AssembleError.Io(path)).flatMap(ZIO.fromEither).flatMap: text =>
      ZIO.fromEither(DependencyLock.parse(text).left.map(error => AssembleError.Resolve(ResolveError.Lock(error))))

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
          _ <- ClassJar.write(apiClasses, jar).mapError(AssembleError.Jar(_))
          assembled <- Assembly
            .write(classes, jars, Program, destination, List(AssemblyFile(ApiEntry, jar)))
            .mapError(AssembleError.Pack(_))
        yield assembled

  private def temp: ZIO[Scope, AssembleError, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-api")).mapError(_ => AssembleError.Io(Path.of("histos-api")))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore
