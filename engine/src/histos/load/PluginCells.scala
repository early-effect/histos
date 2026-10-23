package histos.load

import histos.plugin.{
  CellClasspath,
  DeclaredTask,
  PluginDetail,
  PluginRunError,
  TaskDep
}
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  DependencyLock,
  Repositories,
  Resolve,
  ResolveCell
}
import histos.native.NativeOrigin
import histos.testrun.ModuleTest
import histos.zinc.{
  CompileFeed,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  JvmCompile
}
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** Compiles every `TaskDep.Compile` cell before the plugin task is hashed, then
  * reads the classpath that compile wrote.
  */
private[load] object PluginCells:
  final case class Cell(
      module: Module,
      version: ScalaVersion,
      platform: Platform
  )

  def id(cell: Cell): String =
    s"compile:${cell.module.name.value}:${cell.version.value}:${cell.platform.token}"

  def selected(
      graph: ProjectGraph,
      task: DeclaredTask
  ): Either[PluginRunError, List[Cell]] =
    task.dependsOn.foldLeft[Either[PluginRunError, List[Cell]]](Right(Nil)):
      case (Left(error), _)                                       => Left(error)
      case (Right(acc), TaskDep.Same(_))                          => Right(acc)
      case (Right(acc), TaskDep.Compile(name, version, platform)) =>
        named(graph, name, version, platform).map(acc ++ _)

  def named(
      graph: ProjectGraph,
      name: ProjectName,
      version: Option[ScalaVersion],
      platform: Option[Platform]
  ): Either[PluginRunError, List[Cell]] =
    module(graph, name) match
      case None => Left(failed(s"module ${name.value} is not in the graph"))
      case Some(found) =>
        cells(found, version, platform) match
          case Nil =>
            Left(failed(s"'${name.value}' does not build the requested cell"))
          case some => Right(some)

  def classpath(
      repo: Path,
      graph: ProjectGraph,
      task: DeclaredTask
  ): ZIO[Resolve, PluginRunError, List[CellClasspath]] =
    ZIO
      .fromEither(selected(graph, task))
      .flatMap: found =>
        ZIO.foreach(found.distinct)(paths(repo, graph, _))

  private[load] def compile(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell,
      jdk: histos.zinc.ResolvedJdk
  ): ZIO[Resolve, PluginFault, Unit] =
    for
      _ <- GraphCompile
        .cellFeed(
          repo,
          graph,
          List(cell.module.name -> cell.version),
          cell.platform,
          jdk,
          NativeOrigin.Central,
        )
        .runCollect
        .mapError(error => PluginFault.Compiled(error))
        .flatMap: chunk =>
          chunk.toList.collect { case CompileFeed.Failed(error) => error } match
            case Nil          => ZIO.unit
            case error :: _ => ZIO.fail(PluginFault.Compiled(error))
      sources <- testSources(repo, graph, cell).mapError(error => PluginFault.Task(error))
      _ <- ZIO.unless(sources.isEmpty)(
        compileTests(repo, graph, cell, sources, jdk).mapError(error => PluginFault.Task(error))
      )
    yield ()

  private def compileTests(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell,
      sources: List[Path],
      jdk: histos.zinc.ResolvedJdk
  ): ZIO[Resolve, PluginRunError, Unit] =
    val modules = modulesOf(graph)
    val compiled = CompiledCell(
      cell.module.name,
      cell.version,
      cell.platform,
      classes(repo, cell),
      0
    )
    for
      lock <- readLock(repo)
      _ <- ModuleTest
        .compiledTests(
          repo,
          modules,
          cell.module,
          compiled,
          sources,
          jdk,
          NativeOrigin.Central,
          lock,
          BuildResolvers(graph.resolvers)
        )
        .mapError(error => failed(line(error.message)))
    yield ()

  private def paths(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell
  ): ZIO[Resolve, PluginRunError, CellClasspath] =
    val modules = modulesOf(graph)
    val repositories = BuildResolvers(graph.resolvers)
    for
      lock <- readLock(repo)
      compile <- compileClasspath(repo, cell, modules, lock, repositories)
      sources <- testSources(repo, graph, cell)
      test <-
        if sources.isEmpty then ZIO.succeed(compile)
        else
          testJars(cell, lock, repositories)
            .map(extra => testClasses(repo, cell) :: (compile ++ extra))
    yield CellClasspath(
      cell.module.name,
      cell.version,
      cell.platform,
      compile,
      test
    )

  private def compileClasspath(
      repo: Path,
      cell: Cell,
      modules: List[Module],
      lock: DependencyLock,
      repositories: Repositories
  ): ZIO[Resolve, PluginRunError, List[Path]] =
    val upstream = Module
      .chain(cell.module, modules, cell.platform)
      .map(dependency => classes(repo, dependency.name, cell))
    for
      fetched <- materialize(
        cell,
        lock,
        repositories,
        ClasspathScope.Compile,
        cell.module.dependencies
      )
      library <- standardLibrary(cell)
      jars = fetched.filterNot(path =>
        cell.platform == Platform.Jvm && JvmCompile.isStandardLibrary(path)
      )
    yield classes(repo, cell) :: (upstream ++ library ++ jars)

  private def testJars(
      cell: Cell,
      lock: DependencyLock,
      repositories: Repositories
  ): ZIO[Resolve, PluginRunError, List[Path]] =
    val compileDeps = Dep.select(cell.module.dependencies, cell.platform)
    val testDeps = Dep.select(cell.module.testDependencies, cell.platform)
    if testDeps.isEmpty then ZIO.succeed(Nil)
    else
      for
        compile <- materialize(
          cell,
          lock,
          repositories,
          ClasspathScope.Compile,
          cell.module.dependencies
        )
        both <- materialize(
          cell,
          lock,
          repositories,
          ClasspathScope.Test,
          compileDeps ++ testDeps
        )
      yield both.filterNot(compile.toSet.contains)

  private def materialize(
      cell: Cell,
      lock: DependencyLock,
      repositories: Repositories,
      scope: ClasspathScope,
      dependencies: List[Dep]
  ): ZIO[Resolve, PluginRunError, List[Path]] =
    val request = ResolveCell(
      cell.module.name,
      Dep.select(dependencies, cell.platform),
      cell.version,
      cell.platform,
      repositories,
      scope
    )
    Resolve
      .materialize(request, lock)
      .mapBoth(error => failed(line(error.message)), _.artifacts.map(_.path))

  private def testSources(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell
  ): IO[PluginRunError, List[Path]] =
    ModuleTest
      .testSources(
        repo,
        cell.module,
        modulesOf(graph),
        cell.version,
        cell.platform
      )
      .mapError(error => failed(line(error.message)))

  private def standardLibrary(cell: Cell): IO[PluginRunError, List[Path]] =
    cell.platform match
      case Platform.Jvm =>
        JvmCompile
          .runtime(cell.version)
          .mapError(error => failed(line(error.message)))
      case Platform.Js | Platform.Native => ZIO.succeed(Nil)

  private def cells(
      module: Module,
      version: Option[ScalaVersion],
      platform: Option[Platform]
  ): List[Cell] =
    val versions = version.fold(module.versions.all)(one =>
      if module.versions.all.contains(one) then List(one) else Nil
    )
    val platforms = platform.fold(module.platforms.all)(one =>
      if module.platforms.all.contains(one) then List(one) else Nil
    )
    versions
      .flatMap(item => platforms.map(target => Cell(module, item, target)))
      .sortBy(cell => (cell.version.value, cell.platform.token))

  private def readLock(repo: Path): IO[PluginRunError, DependencyLock] =
    val path = Resolve.lockPath(repo)
    ZIO
      .attempt:
        if Files.isRegularFile(path) then Right(Files.readString(path))
        else Left(failed(s"no dependency lock at $path"))
      .mapError(_ => failed(s"could not read $path"))
      .flatMap(ZIO.fromEither)
      .flatMap: text =>
        ZIO.fromEither(
          DependencyLock
            .parse(text)
            .left
            .map(error => failed(line(error.message)))
        )

  private def classes(repo: Path, cell: Cell): Path =
    classes(repo, cell.module.name, cell)

  private def classes(repo: Path, name: ProjectName, cell: Cell): Path =
    GraphCompile.classesDir(repo, name, cell.version, cell.platform)

  private def testClasses(repo: Path, cell: Cell): Path =
    ModuleTest.testClasses(repo, cell.module.name, cell.version, cell.platform)

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect:
      case found: Module => found

  private def module(graph: ProjectGraph, name: ProjectName): Option[Module] =
    modulesOf(graph).find(_.name == name)

  private def failed(message: String): PluginRunError =
    PluginRunError.Failed(PluginDetail.trusted(message))

  private def line(message: String): String =
    val text = message.replace('\n', ' ').trim
    if text.isEmpty then "compile failed" else text
end PluginCells
