package histos.testrun

import histos.jdk.Sha256
import histos.resolve.{BuildResolvers, ClasspathScope, DependencyLock, Repositories, Resolve, ResolveCell, ResolveError}
import histos.workflow.{CiError, TestTarget, Workflow}
import histos.zinc.{
  CompileError,
  CompileRequest,
  CompileToolchain,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  JvmCompile,
  ResolvedJdk,
  ScalacFlags,
  Sources,
}
import histos.pack.{MainClass, MainClassError}
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion, TestMain, TestProvider}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, ZIO}
import zio.stream.ZStream

final case class TestedCell(module: ProjectName, version: ScalaVersion, status: TestStatus)

/** What one cell did. `NoTests` means there was nothing to run. It is not a passing suite. */
enum TestStatus:
  case NoTests
  case Passed(out: String, err: String)
  case Failed(exit: Int, out: String, err: String)

  def label: String = this match
    case NoTests            => "no tests"
    case Passed(_, _)       => "passed"
    case Failed(exit, _, _) => s"failed $exit"

/** What the test console is doing. Suites are not merged: one tree finishes, then the next starts. */
enum TestFeed:
  case Begin(module: ProjectName, version: ScalaVersion)
  case Out(text: String)
  case Err(text: String)
  case Done(cell: TestedCell)

enum ModuleTestError:
  case Target(error: CiError)
  case Compile(error: GraphCompileError)
  case Lock(error: ResolveError)
  case LockRead(path: Path)
  case Sources(module: ProjectName, version: ScalaVersion, error: CompileError)
  case MissingSources(module: ProjectName, main: TestMain)
  case MissingProvider(module: ProjectName, provider: TestProvider)
  case ProviderDependency(module: ProjectName, provider: TestProvider)
  case NoneDiscovered(module: ProjectName, provider: TestProvider)
  case Mixed(module: ProjectName)
  case Process(module: ProjectName, version: ScalaVersion, error: TestError)
  case Main(error: MainClassError)
  case Absent(module: ProjectName)
  case MissingUpstream(module: ProjectName, dependency: ProjectName, version: ScalaVersion)
  case Work(path: Path)
  case Root(path: Path)
  case Runner(path: Path)

  def message: String = this match
    case Target(error)  => error.message
    case Compile(error) => error.message
    case Lock(error)    => error.message
    case LockRead(path) => s"could not read $path"
    case Sources(module, version, error) =>
      s"'${module.value}' ${version.value} failed to compile tests\n${error.message}"
    case MissingSources(module, main) =>
      s"'${module.value}' names ${main.value} and has no test sources"
    case MissingProvider(module, provider) =>
      s"'${module.value}' runs ${provider.framework.value} and has no test sources"
    case ProviderDependency(module, provider) =>
      val library = provider.library
      s"'${module.value}' runs ${provider.framework.value} and testDependencies do not include ${library.organization}::${library.name}"
    case NoneDiscovered(module, provider) =>
      s"'${module.value}' ran ${provider.framework.value} and it found no tests"
    case Mixed(module) =>
      s"'${module.value}' names a test main and a test provider"
    case Process(module, version, error) =>
      s"'${module.value}' ${version.value} did not start\n${error.message}"
    case Main(error) => error.message
    case Absent(module) =>
      s"'${module.value}' was compiled and is not a module in the graph"
    case MissingUpstream(module, dependency, version) =>
      s"'${module.value}' depends on '${dependency.value}', which has no ${version.value} classes"
    case Work(path) => s"could not prepare the test work directory $path"
    case Root(path) => s"could not read test roots under $path"
    case Runner(path) => s"could not write the test provider runner at $path"

object ModuleTest:
  /**
   * Compile each requested module, then run its test provider in one subprocess per Scala version.
   * `TestProvider.Zio` is the provider when the module names neither a provider nor a main.
   * Upstream modules are compiled and not run. The engine does not load the provider.
   */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
  ): ZIO[Resolve, ModuleTestError, List[TestedCell]] =
    console(repo, graph, names, jdk).collect { case TestFeed.Done(cell) => cell }.runCollect.map(_.toList)

  /**
   * Test compiles stay parallel. The console is `concatAll`: one suite's lines, then the next,
   * in module name order. Merging the trees would make them unreadable.
   */
  def console(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
  ): ZStream[Resolve, ModuleTestError, TestFeed] =
    val repositories = BuildResolvers(graph.resolvers)
    ZStream.unwrap {
      for
        wanted <- ZIO.fromEither(requested(graph, names).left.map(ModuleTestError.Target(_)))
        compiled <- GraphCompile.run(repo, graph, names, jdk).mapError(ModuleTestError.Compile(_))
        lock <- readLock(repo)
        cells = compiled.filter(cell => wanted.contains(cell.module)).sortBy(cell => (cell.module.value, cell.version.value))
        streams <- ZIO.foreachPar(cells)(cell => prepare(repo, graph, cell, jdk, lock, repositories))
      yield ZStream.concatAll(Chunk.fromIterable(streams))
    }

  private def requested(graph: ProjectGraph, names: List[String]): Either[CiError, Set[ProjectName]] =
    val modules = graph.projects.collect { case module: Module => module.name }.toSet
    if names.isEmpty then Right(modules)
    else
      Workflow.target(graph, names).map:
        case TestTarget.Entire             => modules
        case TestTarget.Named(first, rest) => (first :: rest).toSet

  private def prepare(
      repo: Path,
      graph: ProjectGraph,
      cell: CompiledCell,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    val modules = graph.projects.collect { case module: Module => module }
    graph.get(cell.module) match
        case Some(module: Module) =>
          testBase(repo, module, modules).flatMap:
            case None =>
              module.test match
                case None       => ZIO.succeed(finished(cell, TestStatus.NoTests))
                case Some(main) => ZIO.fail(ModuleTestError.MissingSources(cell.module, main))
            case Some(root) =>
              Sources
                .select(root, cell.version, Platform.Jvm)
                .mapError(error => ModuleTestError.Sources(cell.module, cell.version, error))
                .flatMap: sources =>
                  (sources, module.test, module.provider) match
                    case (_, Some(_), Some(_)) => ZIO.fail(ModuleTestError.Mixed(cell.module))
                    case (Nil, None, None)     => ZIO.succeed(finished(cell, TestStatus.NoTests))
                    case (Nil, Some(main), None) =>
                      ZIO.fail(ModuleTestError.MissingSources(cell.module, main))
                    case (Nil, None, Some(provider)) =>
                      ZIO.fail(ModuleTestError.MissingProvider(cell.module, provider))
                    case (_, Some(main), None) =>
                      execute(repo, modules, module, cell, sources, main, jdk, lock, repositories)
                    case (_, None, provider) =>
                      provide(repo, modules, module, cell, sources, provider.getOrElse(TestProvider.Zio), jdk, lock, repositories)
        case _ =>
          ZIO.fail(ModuleTestError.Absent(cell.module))

  private def finished(cell: CompiledCell, status: TestStatus): ZStream[Any, Nothing, TestFeed] =
    ZStream.succeed(TestFeed.Done(TestedCell(cell.module, cell.version, status)))

  private def execute(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    val classes = testClasses(repo, module.name, cell.version)
    val analysis = testAnalysis(repo, module.name, cell.version)
    for
      binary <- ZIO.fromEither(MainClass.from(main.value).left.map(ModuleTestError.Main(_)))
      linked <- runtimeClasspath(repo, modules, module, cell, lock, repositories)
      _ <- JvmCompile
        .compile(CompileRequest(
          CompileToolchain(cell.version, Platform.Jvm, jdk, ScalacFlags.empty),
          sources,
          linked.paths,
          classes,
          analysis,
          linked.hashes,
        ))
        .mapError(error => ModuleTestError.Sources(cell.module, cell.version, error))
      work <- fresh(testWork(repo, module.name, cell.version))
    yield session(module.name, cell.version, None, request(repo, jdk, classes, linked.paths, binary, work))

  private def provide(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      provider: TestProvider,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    val classes = testClasses(repo, module.name, cell.version)
    val analysis = testAnalysis(repo, module.name, cell.version)
    val runner = providerSource(repo, module.name, cell.version)
    for
      _ <- ZIO.unless(satisfied(module, provider))(ZIO.fail(ModuleTestError.ProviderDependency(module.name, provider)))
      linked <- runtimeClasspath(repo, modules, module, cell, lock, repositories)
      _ <- writeRunner(runner, ProviderRunner.source(provider.framework.value, classes))
      _ <- JvmCompile
        .compile(CompileRequest(
          CompileToolchain(cell.version, Platform.Jvm, jdk, ScalacFlags.empty),
          sources :+ runner,
          linked.paths,
          classes,
          analysis,
          linked.hashes,
        ))
        .mapError(error => ModuleTestError.Sources(cell.module, cell.version, error))
      work <- fresh(testWork(repo, module.name, cell.version))
    yield session(module.name, cell.version, Some(provider), request(repo, jdk, classes, linked.paths, ProviderRunner.Main, work))

  private def request(
      repo: Path,
      jdk: ResolvedJdk,
      classes: Path,
      classpath: List[Path],
      main: MainClass,
      work: Path,
  ): TestRequest =
    TestRequest(
      Platform.Jvm,
      jdk.home.resolve("bin").resolve("java"),
      TestClasspath(classes, classpath),
      main,
      work,
      suiteEnv(repo),
    )

  private final case class Transcript(out: String, err: String)

  /** Header, then lines as the process writes them, then the cell. `NoneFound` fails the stream. */
  private def session(
      module: ProjectName,
      version: ScalaVersion,
      provider: Option[TestProvider],
      request: TestRequest,
  ): ZStream[Any, ModuleTestError, TestFeed] =
    ZStream.succeed(TestFeed.Begin(module, version)) ++
      TestProcess.events(request).mapError(error => ModuleTestError.Process(module, version, error)).mapAccumZIO(Transcript("", "")) {
        (acc, event) => hear(module, version, provider, acc, event)
      }

  private def hear(
      module: ProjectName,
      version: ScalaVersion,
      provider: Option[TestProvider],
      acc: Transcript,
      event: TestEvent,
  ): ZIO[Any, ModuleTestError, (Transcript, TestFeed)] =
    event match
      case TestEvent.Out(text) =>
        val next = acc.copy(out = join(acc.out, text))
        ZIO.succeed((next, TestFeed.Out(text)))
      case TestEvent.Err(text) =>
        val next = acc.copy(err = join(acc.err, text))
        ZIO.succeed((next, TestFeed.Err(text)))
      case TestEvent.Exited(code) =>
        provider match
          case Some(found) if code.value == ProviderRunner.NoneFound =>
            ZIO.fail(ModuleTestError.NoneDiscovered(module, found))
          case _ =>
            val status =
              if code.value == 0 then TestStatus.Passed(acc.out, acc.err)
              else TestStatus.Failed(code.value, acc.out, acc.err)
            ZIO.succeed((acc, TestFeed.Done(TestedCell(module, version, status))))

  private def join(have: String, line: String): String =
    s"$have$line\n"

  private def satisfied(module: Module, provider: TestProvider): Boolean =
    val library = provider.library
    library.organization.nonEmpty && library.name.nonEmpty &&
      module.testDependencies.exists(dep => dep.organization == library.organization && dep.name == library.name)

  private def providerSource(repo: Path, module: ProjectName, version: ScalaVersion): Path =
    repo.resolve(".histos").resolve("provider-src").resolve(module.value).resolve(version.value).resolve("subprocess").resolve("Provider.scala")

  private def writeRunner(path: Path, source: String): IO[ModuleTestError, Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, source)
      ()
    }.mapError(_ => ModuleTestError.Runner(path))

  /** The work directory stays empty. `HISTOS_REPO` finds project files. `PATH` finds a toolchain. */
  private def suiteEnv(repo: Path): Map[String, String] =
    val root = Map("HISTOS_REPO" -> repo.toAbsolutePath.normalize.toString)
    sys.env.get("PATH").filter(_.nonEmpty) match
      case Some(path) => root + ("PATH" -> path)
      case None       => root

  /** `<module>/test` wins. The repository `test` is only for a graph with one module. */
  private def testBase(repo: Path, module: Module, modules: List[Module]): IO[ModuleTestError, Option[Path]] =
    ZIO.attemptBlocking {
      val named = repo.resolve(module.name.value).resolve("test")
      val shared = repo.resolve("test")
      if Files.isDirectory(named) then Some(named)
      else if modules.length == 1 && Files.isDirectory(shared) then Some(shared)
      else None
    }.mapError(_ => ModuleTestError.Root(repo))

  private def upstreamClasses(
      repo: Path,
      module: Module,
      version: ScalaVersion,
  ): IO[ModuleTestError, List[Path]] =
    ZIO.foreach(module.dependsOn): dependency =>
      val path = GraphCompile.classesDir(repo, dependency, version)
      ZIO.attemptBlocking(Files.isDirectory(path)).mapError(_ => ModuleTestError.Root(path)).flatMap: found =>
        if found then ZIO.succeed(path)
        else ZIO.fail(ModuleTestError.MissingUpstream(module.name, dependency, version))

  private final case class Fetched(paths: List[Path], hashes: List[Sha256])

  /** Compiler library first, then locked jars. The first jar on the classpath defines `scala.*`. */
  private final case class Linked(paths: List[Path], hashes: List[Sha256])

  private def runtimeClasspath(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ModuleTestError, Linked] =
    for
      fetched <- classpath(module, cell.version, lock, repositories).mapError(ModuleTestError.Lock(_))
      upstream <- upstreamClasses(repo, module, cell.version)
      libraries <- dependencyJars(module, modules, cell.version, lock, repositories)
      library <- JvmCompile.runtime(cell.version).mapError(error => ModuleTestError.Sources(cell.module, cell.version, error))
      jars = (fetched.paths ++ libraries).distinct.filterNot(library.toSet.contains)
    yield Linked(cell.classes :: (upstream ++ library ++ jars), fetched.hashes)

  /** Compile jars of `dependsOn`, not their tests. The child runs those classes. */
  private def dependencyJars(
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ModuleTestError, List[Path]] =
    val byName = modules.map(item => item.name -> item).toMap
    def grow(have: Set[ProjectName]): Set[ProjectName] =
      val extra = have.flatMap(name => byName.get(name).toList.flatMap(_.dependsOn))
      if extra.subsetOf(have) then have else grow(have ++ extra)
    val found = grow(module.dependsOn.toSet).toList.flatMap(byName.get)
    ZIO.foreach(found)(compileArtifacts(_, version, lock, repositories)).map(_.flatten.distinct)

  private def compileArtifacts(
      module: Module,
      version: ScalaVersion,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ModuleTestError, List[Path]] =
    val cell = ResolveCell(module.name, module.dependencies, version, Platform.Jvm, repositories, ClasspathScope.Compile)
    Resolve.materialize(cell, lock).mapBoth(ModuleTestError.Lock(_), _.artifacts.map(_.path))

  private def classpath(
      module: Module,
      version: ScalaVersion,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ResolveError, Fetched] =
    val (dependencies, scope) =
      if module.testDependencies.isEmpty then (module.dependencies, ClasspathScope.Compile)
      else (module.dependencies ++ module.testDependencies, ClasspathScope.Test)
    val cell = ResolveCell(module.name, dependencies, version, Platform.Jvm, repositories, scope)
    Resolve.materialize(cell, lock).map(fetched => Fetched(fetched.artifacts.map(_.path), fetched.artifacts.map(_.sha256)))

  private def readLock(repo: Path): IO[ModuleTestError, DependencyLock] =
    val path = repo.resolve(".histos").resolve("dependencies.lock")
    ZIO.attemptBlocking {
      if Files.isRegularFile(path) then Right(Files.readString(path))
      else Left(ModuleTestError.Lock(ResolveError.NoLock(path)))
    }.mapError(_ => ModuleTestError.LockRead(path)).flatMap(ZIO.fromEither).flatMap: text =>
      ZIO.fromEither(DependencyLock.parse(text).left.map(error => ModuleTestError.Lock(ResolveError.Lock(error))))

  private def fresh(work: Path): IO[ModuleTestError, Path] =
    ZIO.attemptBlocking {
      if Files.exists(work) then
        val walk = Files.walk(work)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      Files.createDirectories(work)
      work
    }.mapError(_ => ModuleTestError.Work(work))

  private def testClasses(repo: Path, module: ProjectName, version: ScalaVersion): Path =
    repo.resolve(".histos").resolve("test-classes").resolve(module.value).resolve(version.value).resolve("jvm")

  private def testAnalysis(repo: Path, module: ProjectName, version: ScalaVersion): Path =
    repo.resolve(".histos").resolve("test-analysis").resolve(module.value).resolve(version.value).resolve("jvm")

  private def testWork(repo: Path, module: ProjectName, version: ScalaVersion): Path =
    repo.resolve(".histos").resolve("test-work").resolve(module.value).resolve(version.value).resolve("jvm")
