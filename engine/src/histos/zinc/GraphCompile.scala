package histos.zinc

import histos.jdk.Sha256
import histos.resolve.{BuildResolvers, Classpath, ClasspathScope, DependencyLock, Repositories, Resolve, ResolveCell, ResolveError}
import histos.workflow.{CiError, TestTarget, Workflow}
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

final case class CompiledCell(module: ProjectName, version: ScalaVersion, classes: Path, classFiles: Int)

enum GraphCompileError:
  case Target(error: CiError)
  case NoJvm(module: ProjectName)
  case NoRoot(module: ProjectName)
  case NoSources(module: ProjectName, root: Path)
  case MissingUpstream(module: ProjectName, dependency: ProjectName, version: ScalaVersion)
  case Cycle(module: ProjectName)
  case Lock(error: ResolveError)
  case LockRead(path: Path)
  case RootRead(path: Path)
  case Failed(module: ProjectName, version: ScalaVersion, error: CompileError)
  case Jdk(home: Path)

  def message: String = this match
    case Target(error) => error.message
    case NoJvm(module) => s"'${module.value}' does not target the JVM"
    case NoRoot(module) =>
      s"'${module.value}' has no source root. Expected ${module.value}/src, or src when it is the only module"
    case NoSources(module, root) => s"'${module.value}' has no Scala sources under $root"
    case MissingUpstream(module, dependency, version) =>
      s"'${module.value}' depends on '${dependency.value}', which has no ${version.value} classes"
    case Cycle(module) => s"'${module.value}' is in a compile cycle"
    case Lock(error) => error.message
    case LockRead(path) => s"could not read $path"
    case RootRead(path) => s"could not read source roots under $path"
    case Failed(module, version, error) => s"'${module.value}' ${version.value} failed to compile\n${error.message}"
    case Jdk(home) => s"could not read the JDK release file under $home"

object GraphCompile:
  /** Compile the JVM cells. Names pull in `dependsOn` upstreams and not dependents. Independent modules in one layer compile together. */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
    for
      selected <- ZIO.fromEither(choose(graph, names))
      layers   <- ZIO.fromEither(layersOf(selected))
      lock     <- readLock(repo)
      compiled <- compileLayers(repo, modulesOf(graph), layers, lock, jdk, BuildResolvers(graph.resolvers))
    yield compiled.sortBy(cell => (cell.module.value, cell.version.value))

  def jdkHere: IO[GraphCompileError, ResolvedJdk] =
    val home = Option(java.lang.System.getProperty("java.home")).filter(_.nonEmpty).map(Path.of(_))
    home match
      case None => ZIO.fail(GraphCompileError.Jdk(Path.of(".")))
      case Some(path) =>
        ZIO.attemptBlocking(Files.readString(path.resolve("release"))).mapBoth(
          _ => GraphCompileError.Jdk(path),
          text => histos.jdk.JdkVersion.fromRelease(text),
        ).flatMap:
          case Right(version) => ZIO.succeed(ResolvedJdk(path, version))
          case Left(_)        => ZIO.fail(GraphCompileError.Jdk(path))

  /** `<module>/src` wins. The repository `src` is only for a graph with one module. The build file stays outside it. */
  def sourceBase(repo: Path, module: Module, modules: List[Module]): IO[GraphCompileError, Option[Path]] =
    ZIO.attemptBlocking {
      val named = repo.resolve(module.name.value).resolve("src")
      val shared = repo.resolve("src")
      if Files.isDirectory(named) then Some(named)
      else if modules.length == 1 && Files.isDirectory(shared) then Some(shared)
      else None
    }.mapError(_ => GraphCompileError.RootRead(repo))

  private def choose(graph: ProjectGraph, names: List[String]): Either[GraphCompileError, List[Module]] =
    val modules = modulesOf(graph)
    if names.isEmpty then Right(modules)
    else
      Workflow.target(graph, names).left.map(GraphCompileError.Target(_)).map: target =>
        val roots = target match
          case TestTarget.Entire                 => modules.map(_.name).toSet
          case TestTarget.Named(first, rest)     => (first :: rest).toSet
        upstream(modules, roots)

  private def upstream(modules: List[Module], roots: Set[ProjectName]): List[Module] =
    val byName = modules.map(module => module.name -> module).toMap
    def grow(have: Set[ProjectName]): Set[ProjectName] =
      val extra = have.flatMap(name => byName.get(name).toList.flatMap(_.dependsOn))
      if extra.subsetOf(have) then have else grow(have ++ extra)
    val needed = grow(roots)
    modules.filter(module => needed.contains(module.name))

  private def layersOf(modules: List[Module]): Either[GraphCompileError, List[List[Module]]] =
    val names = modules.map(_.name).toSet
    def step(done: Set[ProjectName], rest: List[Module], built: List[List[Module]]): Either[GraphCompileError, List[List[Module]]] =
      val (ready, later) = rest.partition: module =>
        module.dependsOn.forall(dependency => !names.contains(dependency) || done.contains(dependency))
      ready match
        case Nil =>
          later match
            case module :: _ => Left(GraphCompileError.Cycle(module.name))
            case Nil         => Right(built.reverse)
        case _ =>
          step(done ++ ready.map(_.name), later, ready :: built)
    step(Set.empty, modules, Nil)

  private def compileLayers(
      repo: Path,
      all: List[Module],
      layers: List[List[Module]],
      lock: DependencyLock,
      jdk: ResolvedJdk,
      repositories: Repositories,
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
    ZIO.foldLeft(layers)((Map.empty[(ProjectName, ScalaVersion), Path], List.empty[CompiledCell])): (state, layer) =>
      val (done, acc) = state
      ZIO.foreachPar(layer)(module => compileModule(repo, all, module, done, lock, jdk, repositories)).map: batches =>
        val cells = batches.flatten
        val paths = cells.map(cell => (cell.module, cell.version) -> cell.classes).toMap
        (done ++ paths, acc ++ cells)
    .map(_._2)

  private def compileModule(
      repo: Path,
      all: List[Module],
      module: Module,
      done: Map[(ProjectName, ScalaVersion), Path],
      lock: DependencyLock,
      jdk: ResolvedJdk,
      repositories: Repositories,
  ): ZIO[Resolve, GraphCompileError, List[CompiledCell]] =
    if !module.platforms.all.contains(Platform.Jvm) then ZIO.fail(GraphCompileError.NoJvm(module.name))
    else
      sourceBase(repo, module, all).flatMap:
        case None => ZIO.fail(GraphCompileError.NoRoot(module.name))
        case Some(base) =>
          ZIO.foreachPar(module.versions.all)(version => compileVersion(repo, module, version, base, done, lock, jdk, repositories))

  private def compileVersion(
      repo: Path,
      module: Module,
      version: ScalaVersion,
      base: Path,
      done: Map[(ProjectName, ScalaVersion), Path],
      lock: DependencyLock,
      jdk: ResolvedJdk,
      repositories: Repositories,
  ): ZIO[Resolve, GraphCompileError, CompiledCell] =
    for
      sources <- Sources.select(base, version, Platform.Jvm).mapError(error =>
                   GraphCompileError.Failed(module.name, version, error)
                 )
      _ <- ZIO.when(sources.isEmpty)(ZIO.fail(GraphCompileError.NoSources(module.name, base)))
      upstreamClasses <- ZIO.fromEither(upstreamClasses(module, version, done))
      fetched <- classpathOf(module, version, lock, repositories)
      classes = classesDir(repo, module.name, version)
      compiled <- JvmCompile.compile(CompileRequest(
                    CompileToolchain(version, Platform.Jvm, jdk, ScalacFlags.empty),
                    sources,
                    upstreamClasses ++ fetched.paths,
                    classes,
                    analysis(repo, module.name, version),
                    fetched.hashes,
                  )).mapError(error => GraphCompileError.Failed(module.name, version, error))
    yield CompiledCell(module.name, version, compiled.classes, compiled.classFiles)

  private def upstreamClasses(
      module: Module,
      version: ScalaVersion,
      done: Map[(ProjectName, ScalaVersion), Path],
  ): Either[GraphCompileError, List[Path]] =
    module.dependsOn.foldLeft[Either[GraphCompileError, List[Path]]](Right(Nil)): (acc, dependency) =>
      acc.flatMap: paths =>
        done.get((dependency, version)) match
          case Some(classes) => Right(paths :+ classes)
          case None          => Left(GraphCompileError.MissingUpstream(module.name, dependency, version))

  private def classpathOf(
      module: Module,
      version: ScalaVersion,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, GraphCompileError, Fetched] =
    val cell = ResolveCell(module.name, module.dependencies, version, Platform.Jvm, repositories, ClasspathScope.Compile)
    Resolve.materialize(cell, lock).mapBoth(GraphCompileError.Lock(_), fetched)

  private final case class Fetched(paths: List[Path], hashes: List[Sha256])

  private def fetched(classpath: Classpath): Fetched =
    Fetched(classpath.artifacts.map(_.path), classpath.artifacts.map(_.sha256))

  private def readLock(repo: Path): IO[GraphCompileError, DependencyLock] =
    val path = repo.resolve(".histos").resolve("dependencies.lock")
    ZIO.attemptBlocking {
      if Files.isRegularFile(path) then Right(Files.readString(path))
      else Left(GraphCompileError.Lock(ResolveError.NoLock(path)))
    }.mapError(_ => GraphCompileError.LockRead(path)).flatMap(ZIO.fromEither).flatMap: text =>
      ZIO.fromEither(DependencyLock.parse(text).left.map(error => GraphCompileError.Lock(ResolveError.Lock(error))))

  /** Classpath root written for one JVM cell. Test compile and the test subprocess read this directory. */
  private[histos] def classesDir(repo: Path, module: ProjectName, version: ScalaVersion): Path =
    repo.resolve(".histos").resolve("classes").resolve(module.value).resolve(version.value).resolve("jvm")

  private def analysis(repo: Path, module: ProjectName, version: ScalaVersion): Path =
    repo.resolve(".histos").resolve("analysis").resolve(module.value).resolve(version.value).resolve("jvm")

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case module: Module => module }
