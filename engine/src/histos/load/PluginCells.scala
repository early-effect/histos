package histos.load

import histos.jdk.{JdkRuntime, Sha256, Sha256Digest}
import histos.js.{JsLink, JsLinkRequest}
import histos.plugin.{CellClasspath, DeclaredTask, LinkedScript, PluginDetail, PluginRunError, TaskDep}
import histos.program.Program
import histos.workspace.Workspace
import histos.resolve.LockedTools
import histos.resolve.{BuildResolvers, ClasspathScope, DependencyLock, Repositories, Resolve, ResolveCell, ResolveError}
import histos.native.NativeOrigin
import histos.testrun.{ModuleTestError, TestCompile}
import histos.zinc.{CompileFeed, CompiledCell, GraphCompile, JvmCompile}
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ZIO, ZLayer}
import zio.stream.ZStream

/** Compiles every `TaskDep.Compile` cell before the plugin task is hashed, then reads the classpath that compile wrote.
  */
private[load] object PluginCells:
  final case class Cell(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
  )

  def id(cell: Cell): String =
    s"compile:${cell.module.name.value}:${cell.version.value}:${cell.platform.token}"

  def selected(
      graph: ProjectGraph,
      task: DeclaredTask,
  ): Either[PluginRunError, List[Cell]] =
    task.dependsOn.foldLeft[Either[PluginRunError, List[Cell]]](Right(Nil)):
      case (Left(error), _)                                       => Left(error)
      case (Right(acc), TaskDep.Same(_))                          => Right(acc)
      case (Right(acc), TaskDep.Compile(name, version, platform)) =>
        named(graph, name, version, platform).map(acc ++ _)
      case (Right(acc), TaskDep.Link(_, _)) => Right(acc)

  def links(
      graph: ProjectGraph,
      task: DeclaredTask,
  ): Either[PluginRunError, List[Cell]] =
    task.dependsOn.foldLeft[Either[PluginRunError, List[Cell]]](Right(Nil)):
      case (Left(error), _)                          => Left(error)
      case (Right(acc), TaskDep.Link(name, version)) =>
        named(graph, name, version, Some(Platform.Js)).map(acc ++ _)
      case (Right(acc), TaskDep.Compile(_, _, _)) => Right(acc)
      case (Right(acc), TaskDep.Same(_))          => Right(acc)

  def named(
      graph: ProjectGraph,
      name: ProjectName,
      version: Option[ScalaVersion],
      platform: Option[Platform],
  ): Either[PluginRunError, List[Cell]] =
    module(graph, name) match
      case None        => Left(failed(s"module ${name.value} is not in the graph"))
      case Some(found) =>
        cells(found, version, platform) match
          case Nil =>
            Left(failed(s"'${name.value}' does not build the requested cell"))
          case some => Right(some)

  def classpath(
      repo: Path,
      graph: ProjectGraph,
      task: DeclaredTask,
      lock: IO[PluginFault, DependencyLock],
  ): ZIO[Resolve, PluginFault, List[CellClasspath]] =
    ZIO
      .fromEither(selected(graph, task).left.map(PluginFault.Task(_)))
      .flatMap: found =>
        if found.isEmpty then ZIO.succeed(Nil)
        else lock.flatMap(captured => ZIO.foreach(found.distinct)(paths(repo, graph, _, captured)))

  /** Compile the cell, link it, and remember the script path. The returned text is the script digest. */
  def link(
      session: histos.zinc.CompileSession,
      cell: Cell,
  ): ZIO[Resolve & histos.resolve.ResolutionService, PluginFault, String] =
    val histos.zinc.CompileSession(repo, graph, jdk, _, lock, _) = session
    val asked                                                    =
      cell.module.application match
        case Some(main) =>
          histos.pack.MainClass.from(main.value) match
            case Left(error)   => Left(failed(error.message))
            case Right(binary) => Right((binary, true))
        case None if cell.platform == Platform.Js =>
          Right((histos.pack.MainClass.trusted("histos.Exports"), false))
        case None =>
          Left(failed(s"'${cell.module.name.value}' has no main class to link"))
    asked match
      case Left(error)                              => ZIO.fail(PluginFault.Task(error))
      case Right(_) if cell.platform != Platform.Js =>
        ZIO.fail(PluginFault.Task(failed(s"'${cell.module.name.value}' is not a JavaScript module")))
      case Right((binary, initialize)) =>
        val modules      = modulesOf(graph)
        val repositories = BuildResolvers(graph.resolvers)
        val compiled     = histos.zinc.CompiledCell(
          cell.module.name,
          cell.version,
          cell.platform,
          classes(repo, cell),
          0,
          None,
          false,
          None,
          None,
        )
        val work = Program.workDir(repo, cell.module.name, cell.version, cell.platform)
        for
          service <- ZIO.service[histos.resolve.ResolutionService]
          tools = ZLayer.fromZIO(LockedTools.make(lock, graph, service))
          paths <- Program
            .classpath(repo, modules, cell.module, compiled, lock, repositories)
            .provideSome[Resolve](tools)
            .mapError(error => PluginFault.Task(failed(error.message)))
          _      <- Workspace.recreate(work).mapError(error => PluginFault.Task(failed(error.message)))
          linked <- JsLink
            .link(
              JsLinkRequest(
                cell.version,
                binary,
                paths,
                work,
                jdk,
                cell.module.jsModule,
                revision = cell.module.jsVersion,
                initialize = initialize,
              )
            )
            .provideSome[Resolve](tools)
            .mapError(error => PluginFault.Task(failed(error.message)))
          _ <- Workspace
            .writeString(scriptRecord(repo, cell.module.name, cell.version), linked.script.toString)
            .mapError(error => PluginFault.Task(failed(error.message)))
        yield linked.key.value

  def scripts(
      repo: Path,
      graph: ProjectGraph,
      task: DeclaredTask,
  ): IO[PluginRunError, List[LinkedScript]] =
    ZIO
      .fromEither(links(graph, task))
      .flatMap: found =>
        ZIO.foreach(found.distinct): cell =>
          Workspace
            .readString(scriptRecord(repo, cell.module.name, cell.version))
            .mapBoth(
              _ => failed(s"link of ${cell.module.name.value} ${cell.version.value} was not written"),
              text => LinkedScript(cell.module.name, cell.version, Path.of(text.trim)),
            )

  def scriptDigest(
      repo: Path,
      graph: ProjectGraph,
      name: ProjectName,
      version: Option[ScalaVersion],
  ): IO[PluginRunError, Sha256] =
    ZIO
      .fromEither(named(graph, name, version, Some(Platform.Js)))
      .flatMap: found =>
        ZIO
          .foreach(found): cell =>
            Workspace
              .readString(scriptRecord(repo, cell.module.name, cell.version))
              .mapBoth(
                _ => failed(s"link of ${cell.module.name.value} ${cell.version.value} was not written"),
                identity,
              )
              .flatMap: text =>
                val script = Path.of(text.trim)
                ZIO
                  .attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(script))))
                  .orElseFail(failed(s"could not read ${script}"))
          .map: hashes =>
            Sha256Digest.of(
              Chunk.fromArray(hashes.map(_.value).mkString("\n").getBytes(java.nio.charset.StandardCharsets.UTF_8))
            )

  private[load] def scriptRecord(repo: Path, module: ProjectName, version: ScalaVersion): Path =
    repo.resolve(".histos").resolve("link").resolve(module.value).resolve(version.value).resolve("script")

  private[load] def compile(
      session: histos.zinc.CompileSession,
      cell: Cell,
  ): ZIO[Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService, PluginFault, Unit] =
    val histos.zinc.CompileSession(repo, graph, jdk, origin, lock, _) = session
    ZIO.scoped:
      (histos.zinc.Compilers.live ++ histos.zinc.CompileMemo.live).build.flatMap { env =>
        val compilers = zio.ZLayer.succeed(env.get[histos.zinc.Compilers])
        val memo      = zio.ZLayer.succeed(env.get[histos.zinc.CompileMemo])
        for
          chunk <- GraphCompile
            .cellFeed(
              session,
              List(cell.module.name -> cell.version),
              cell.platform,
            )
            .provideSomeLayer(compilers ++ memo)
            .runCollect
            .mapError(error => PluginFault.Compiled(error))
          _ <- chunk.toList.collect { case CompileFeed.Failed(error) => error } match
            case Nil        => ZIO.unit
            case error :: _ => ZIO.fail(PluginFault.Compiled(error))
          sources <- testSources(repo, graph, cell).mapError(PluginFault.Tested(_))
          _       <- ZIO.unless(sources.isEmpty)(
            compileTests(repo, graph, cell, sources, jdk, origin, lock)
              .provideSomeLayer[Resolve & histos.cache.ContentStore](compilers)
              .mapError(PluginFault.Tested(_))
          )
        yield ()
      }

  private def compileTests(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell,
      sources: List[Path],
      jdk: JdkRuntime,
      origin: NativeOrigin,
      lock: DependencyLock,
  ): ZIO[Resolve & histos.cache.ContentStore & histos.zinc.Compilers, ModuleTestError, Unit] =
    val modules  = modulesOf(graph)
    val compiled = CompiledCell(
      cell.module.name,
      cell.version,
      cell.platform,
      classes(repo, cell),
      0,
      key = None,
      changed = true,
      sources = None,
      classesWritten = None,
    )
    for _ <- TestCompile
        .compiledTests(
          repo,
          modules,
          cell.module,
          compiled,
          sources,
          jdk,
          origin,
          lock,
          BuildResolvers(graph.resolvers),
        )
        .provideSome[Resolve & histos.cache.ContentStore & histos.zinc.Compilers](
          zio.ZLayer.fromZIO(LockedTools.make(lock, graph))
        )
    yield ()

  private def paths(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell,
      lock: DependencyLock,
  ): ZIO[Resolve, PluginFault, CellClasspath] =
    val modules      = modulesOf(graph)
    val repositories = BuildResolvers(graph.resolvers)
    for
      compile <- compileClasspath(repo, cell, modules, lock, repositories)
        .provideSome[Resolve](
          zio.ZLayer.fromZIO(LockedTools.make(lock, graph))
        )
      sources <- testSources(repo, graph, cell).mapError(PluginFault.Tested(_))
      test    <-
        if sources.isEmpty then ZIO.succeed(compile)
        else
          testJars(cell, lock, repositories)
            .map(extra => testClasses(repo, cell) :: (compile ++ extra))
    yield CellClasspath(
      cell.module.name,
      cell.version,
      cell.platform,
      compile,
      test,
    )

  private def compileClasspath(
      repo: Path,
      cell: Cell,
      modules: List[Module],
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve & LockedTools, PluginFault, List[Path]] =
    val upstream = Module
      .chain(cell.module, modules, cell.platform)
      .map(dependency => classes(repo, dependency.name, cell))
    for
      fetched <- materialize(
        cell,
        lock,
        repositories,
        ClasspathScope.Compile,
        cell.module.dependencies,
      )
      library    <- standardLibrary(cell).mapError(PluginFault.Tested(_))
      transitive <- ZIO.foreach(Module.chain(cell.module, modules, cell.platform)): dependency =>
        histos.zinc.UpstreamCells.chosen(cell.version, dependency, cell.platform, modules) match
          case None =>
            ZIO.fail(
              PluginFault.Tested(
                ModuleTestError.MissingUpstream(cell.module.name, dependency.name, cell.version)
              )
            )
          case Some(chosen) =>
            materialize(
              Cell(dependency, chosen, cell.platform),
              lock,
              repositories,
              ClasspathScope.Compile,
              dependency.dependencies,
            )
      jars =
        (fetched ++ transitive.flatten.distinct.filterNot(fetched.toSet.contains))
          .filterNot(path => cell.platform == Platform.Jvm && JvmCompile.isStandardLibrary(path))
    yield classes(repo, cell) :: (upstream ++ library ++ jars)

  private def testJars(
      cell: Cell,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, PluginFault, List[Path]] =
    val compileDeps = Dep.select(cell.module.dependencies, cell.platform)
    val testDeps    = Dep.select(cell.module.testDependencies, cell.platform)
    if testDeps.isEmpty then ZIO.succeed(Nil)
    else
      for
        compile <- materialize(
          cell,
          lock,
          repositories,
          ClasspathScope.Compile,
          cell.module.dependencies,
        )
        both <- materialize(
          cell,
          lock,
          repositories,
          ClasspathScope.Test,
          compileDeps ++ testDeps,
        )
      yield both.filterNot(compile.toSet.contains)

  private def materialize(
      cell: Cell,
      lock: DependencyLock,
      repositories: Repositories,
      scope: ClasspathScope,
      dependencies: List[Dep],
  ): ZIO[Resolve, PluginFault, List[Path]] =
    val request = ResolveCell(
      cell.module.name,
      Dep.select(dependencies, cell.platform),
      cell.version,
      cell.platform,
      repositories,
      scope,
    )
    Resolve
      .materialize(request, lock)
      .mapBoth(PluginFault.Resolved(_), _.artifacts.map(_.path))

  private def testSources(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell,
  ): IO[ModuleTestError, List[Path]] =
    TestCompile
      .testSources(
        repo,
        cell.module,
        modulesOf(graph),
        cell.version,
        cell.platform,
      )

  private def standardLibrary(
      cell: Cell
  ): ZIO[LockedTools, ModuleTestError, List[Path]] =
    cell.platform match
      case Platform.Jvm =>
        JvmCompile
          .runtime(cell.version)
          .mapError(error => ModuleTestError.Sources(cell.module.name, cell.version, error))
      case Platform.Js | Platform.Native => ZIO.succeed(Nil)

  private def cells(
      module: Module,
      version: Option[ScalaVersion],
      platform: Option[Platform],
  ): List[Cell] =
    val versions =
      version.fold(module.versions.all)(one => if module.versions.all.contains(one) then List(one) else Nil)
    val platforms =
      platform.fold(module.platforms.all)(one => if module.platforms.all.contains(one) then List(one) else Nil)
    versions
      .flatMap(item => platforms.map(target => Cell(module, item, target)))
      .sortBy(cell => (cell.version.value, cell.platform.token))

  private def classes(repo: Path, cell: Cell): Path =
    classes(repo, cell.module.name, cell)

  private def classes(repo: Path, name: ProjectName, cell: Cell): Path =
    GraphCompile.classesDir(repo, name, cell.version, cell.platform)

  private def testClasses(repo: Path, cell: Cell): Path =
    TestCompile.testClasses(repo, cell.module.name, cell.version, cell.platform)

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect:
      case found: Module => found

  private def module(graph: ProjectGraph, name: ProjectName): Option[Module] =
    modulesOf(graph).find(_.name == name)

  private def failed(message: String): PluginRunError =
    PluginRunError.Failed(PluginDetail.trusted(message))

end PluginCells
