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
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion, TestMain}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

final case class TestedCell(module: ProjectName, version: ScalaVersion, status: TestStatus)

/** What one cell did. `NoTests` means there was nothing to run. It is not a passing suite. */
enum TestStatus:
  case NoTests
  case Passed
  case Failed(exit: Int, out: String, err: String)

  def label: String = this match
    case NoTests            => "no tests"
    case Passed             => "passed"
    case Failed(exit, _, _) => s"failed $exit"

enum ModuleTestError:
  case Target(error: CiError)
  case Compile(error: GraphCompileError)
  case Lock(error: ResolveError)
  case LockRead(path: Path)
  case Sources(module: ProjectName, version: ScalaVersion, error: CompileError)
  case MissingMain(module: ProjectName)
  case MissingSources(module: ProjectName, main: TestMain)
  case Process(module: ProjectName, version: ScalaVersion, error: TestError)
  case Main(error: MainClassError)
  case Absent(module: ProjectName)
  case MissingUpstream(module: ProjectName, dependency: ProjectName, version: ScalaVersion)
  case Work(path: Path)
  case Root(path: Path)

  def message: String = this match
    case Target(error)  => error.message
    case Compile(error) => error.message
    case Lock(error)    => error.message
    case LockRead(path) => s"could not read $path"
    case Sources(module, version, error) =>
      s"'${module.value}' ${version.value} failed to compile tests\n${error.message}"
    case MissingMain(module) =>
      s"'${module.value}' has test sources and does not name a main. Add .test(TestMain(\"demo.Check\"))"
    case MissingSources(module, main) =>
      s"'${module.value}' names ${main.value} and has no test sources"
    case Process(module, version, error) =>
      s"'${module.value}' ${version.value} did not start\n${error.message}"
    case Main(error) => error.message
    case Absent(module) =>
      s"'${module.value}' was compiled and is not a module in the graph"
    case MissingUpstream(module, dependency, version) =>
      s"'${module.value}' depends on '${dependency.value}', which has no ${version.value} classes"
    case Work(path) => s"could not prepare the test work directory $path"
    case Root(path) => s"could not read test roots under $path"

object ModuleTest:
  /**
   * Compile each requested module, then run its named main in one subprocess per Scala version.
   * Upstream modules are compiled and not run. There is no test framework.
   */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
  ): ZIO[Resolve, ModuleTestError, List[TestedCell]] =
    val repositories = BuildResolvers(graph.resolvers)
    for
      wanted <- ZIO.fromEither(requested(graph, names).left.map(ModuleTestError.Target(_)))
      compiled <- GraphCompile.run(repo, graph, names, jdk).mapError(ModuleTestError.Compile(_))
      lock <- readLock(repo)
      cells = compiled.filter(cell => wanted.contains(cell.module))
      tested <- ZIO.foreachPar(cells)(cell => one(repo, graph, cell, jdk, lock, repositories))
    yield tested.sortBy(cell => (cell.module.value, cell.version.value))

  private def requested(graph: ProjectGraph, names: List[String]): Either[CiError, Set[ProjectName]] =
    val modules = graph.projects.collect { case module: Module => module.name }.toSet
    if names.isEmpty then Right(modules)
    else
      Workflow.target(graph, names).map:
        case TestTarget.Entire             => modules
        case TestTarget.Named(first, rest) => (first :: rest).toSet

  private def one(
      repo: Path,
      graph: ProjectGraph,
      cell: CompiledCell,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ModuleTestError, TestedCell] =
    val modules = graph.projects.collect { case module: Module => module }
    graph.get(cell.module) match
      case Some(module: Module) =>
        testBase(repo, module, modules).flatMap:
          case None =>
            module.test match
              case None       => ZIO.succeed(TestedCell(cell.module, cell.version, TestStatus.NoTests))
              case Some(main) => ZIO.fail(ModuleTestError.MissingSources(cell.module, main))
          case Some(root) =>
            Sources
              .select(root, cell.version, Platform.Jvm)
              .mapError(error => ModuleTestError.Sources(cell.module, cell.version, error))
              .flatMap: sources =>
                (sources, module.test) match
                  case (Nil, None)       => ZIO.succeed(TestedCell(cell.module, cell.version, TestStatus.NoTests))
                  case (Nil, Some(main)) => ZIO.fail(ModuleTestError.MissingSources(cell.module, main))
                  case (_, None)         => ZIO.fail(ModuleTestError.MissingMain(cell.module))
                  case (_, Some(main))   => execute(repo, module, cell, sources, main, jdk, lock, repositories)
      case _ =>
        ZIO.fail(ModuleTestError.Absent(cell.module))

  private def execute(
      repo: Path,
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ModuleTestError, TestedCell] =
    val classes = testClasses(repo, module.name, cell.version)
    val analysis = testAnalysis(repo, module.name, cell.version)
    for
      binary <- ZIO.fromEither(MainClass.from(main.value).left.map(ModuleTestError.Main(_)))
      fetched <- classpath(module, cell.version, lock, repositories).mapError(ModuleTestError.Lock(_))
      upstream <- upstreamClasses(repo, module, cell.version)
      compileAgainst = cell.classes :: (upstream ++ fetched.paths)
      _ <- JvmCompile
        .compile(CompileRequest(
          CompileToolchain(cell.version, Platform.Jvm, jdk, ScalacFlags.empty),
          sources,
          compileAgainst,
          classes,
          analysis,
          fetched.hashes,
        ))
        .mapError(error => ModuleTestError.Sources(cell.module, cell.version, error))
      work <- fresh(testWork(repo, module.name, cell.version))
      outcome <- TestProcess
        .run(TestRequest(
          Platform.Jvm,
          jdk.home.resolve("bin").resolve("java"),
          TestClasspath(classes, compileAgainst),
          binary,
          work,
        ))
        .mapError(error => ModuleTestError.Process(cell.module, cell.version, error))
    yield TestedCell(cell.module, cell.version, status(outcome))

  private def status(outcome: TestOutcome): TestStatus =
    if outcome.exit.value == 0 then TestStatus.Passed
    else TestStatus.Failed(outcome.exit.value, outcome.out.text, outcome.err.text)

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
