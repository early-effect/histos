package histos.load

import histos.cache.{ContentStore, LinkerStarts}
import histos.jdk.{JdkRuntime, Sha256, Sha256Digest}
import histos.js.{JsLink, JsLinkRequest}
import histos.cli.CompileConsole
import histos.plugin.{
  CellClasspath,
  DeclaredTask,
  InvocationPluginEvent,
  LinkedScript,
  PluginDetail,
  PluginEvents,
  PluginRunError,
  TaskDep,
  SourceSet,
}
import histos.program.Program
import histos.workspace.Workspace
import histos.resolve.LockedTools
import histos.resolve.{BuildResolvers, ClasspathScope, DependencyLock, Repositories, Resolve, ResolveCell, ResolveError}
import histos.native.NativeOrigin
import histos.testrun.{ModuleTestError, TestCompile}
import histos.zinc.{CompileFeed, CompiledCell, GraphCompile, JvmCompile, ScalacClasspath, UpstreamCells}
import histos.{Dep, JsOptimize, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, UIO, ZIO, ZLayer}
import zio.stream.ZStream

/** Compiles requested Main or Test stages before hashing a plugin task, then reads their completed classpaths.
  */
private[load] object PluginCells:
  final case class Cell(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      scope: SourceSet,
  )

  final case class Link(cell: Cell, optimize: JsOptimize)

  def id(cell: Cell): String =
    s"compile:${cell.module.name.value}:${cell.version.value}:${cell.platform.token}:${scopeToken(cell.scope)}"

  def scopeToken(scope: SourceSet): String = scope match
    case SourceSet.Main => "main"
    case SourceSet.Test => "test"

  def selected(
      graph: ProjectGraph,
      task: DeclaredTask,
  ): Either[PluginRunError, List[Cell]] =
    task.dependsOn
      .foldLeft[Either[PluginRunError, List[Cell]]](Right(Nil)):
        case (Left(error), _)                                       => Left(error)
        case (Right(acc), TaskDep.Compile(name, version, platform)) =>
          named(graph, name, version, platform, SourceSet.Test).map(acc ++ _)
        case (Right(acc), TaskDep.CompileScoped(name, version, platform, scope)) =>
          named(graph, name, version, platform, scope).map(acc ++ _)
        case (Right(acc), TaskDep.Link(_, _, _))             => Right(acc)
        case (Right(acc), TaskDep.Same(_))                   => Right(acc)
        case (Right(acc), TaskDep.Generate(_))               => Right(acc)
        case (Right(acc), TaskDep.Emit(_))                   => Right(acc)
        case (Right(acc), TaskDep.EmitScoped(_, _, _, _, _)) => Right(acc)
      .map: found =>
        found.distinct.filterNot: cell =>
          cell.scope == SourceSet.Main && found.exists(other => sameCell(cell, other) && other.scope == SourceSet.Test)

  def links(
      graph: ProjectGraph,
      task: DeclaredTask,
  ): Either[PluginRunError, List[Link]] =
    task.dependsOn.foldLeft[Either[PluginRunError, List[Link]]](Right(Nil)):
      case (Left(error), _)                                    => Left(error)
      case (Right(acc), TaskDep.Link(name, version, optimize)) =>
        named(graph, name, version, Some(Platform.Js), SourceSet.Main).map: found =>
          acc ++ found.map(cell =>
            Link(cell, JsOptimize.effective(graph.tools.js.optimize, cell.module.jsOptimize, optimize))
          )
      case (Right(acc), TaskDep.Compile(_, _, _))          => Right(acc)
      case (Right(acc), TaskDep.CompileScoped(_, _, _, _)) => Right(acc)
      case (Right(acc), TaskDep.Same(_))                   => Right(acc)
      case (Right(acc), TaskDep.Generate(_))               => Right(acc)
      case (Right(acc), TaskDep.Emit(_))                   => Right(acc)
      case (Right(acc), TaskDep.EmitScoped(_, _, _, _, _)) => Right(acc)

  private def sameCell(left: Cell, right: Cell): Boolean =
    left.module.name == right.module.name && left.version == right.version && left.platform == right.platform

  def named(
      graph: ProjectGraph,
      name: ProjectName,
      version: Option[ScalaVersion],
      platform: Option[Platform],
      scope: SourceSet,
  ): Either[PluginRunError, List[Cell]] =
    module(graph, name) match
      case None        => Left(failed(s"module ${name.value} is not in the graph"))
      case Some(found) =>
        cells(found, version, platform, scope) match
          case Nil =>
            Left(failed(s"'${name.value}' does not build the requested cell"))
          case some => Right(some)

  def classpath(
      repo: Path,
      graph: ProjectGraph,
      task: DeclaredTask,
      lock: IO[PluginFault, DependencyLock],
  ): ZIO[Resolve & histos.resolve.ResolutionService, PluginFault, List[CellClasspath]] =
    ZIO
      .fromEither(selected(graph, task).left.map(PluginFault.Task(_)))
      .flatMap: found =>
        if found.isEmpty then ZIO.succeed(Nil)
        else lock.flatMap(captured => ZIO.foreach(found.distinct)(paths(repo, graph, _, captured)))

  /** Link a completed Main cell and remember its effective-mode script path. The returned text is the link key. */
  def link(
      session: histos.zinc.CompileSession,
      request: Link,
  ): ZIO[Resolve & histos.resolve.ResolutionService & ContentStore & histos.schedule.RemoteCache, PluginFault, String] =
    val Link(cell, optimize)                                     = request
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
        val work = Program.workDir(repo, cell.module.name, cell.version, cell.platform).resolve(optimize.token)
        for
          service <- ZIO.service[histos.resolve.ResolutionService]
          tools = ZLayer.fromZIO(LockedTools.make(lock, graph, service))
          paths <- Program
            .classpath(
              repo,
              modules,
              cell.module,
              compiled,
              lock,
              repositories,
            )
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
                optimize = optimize,
              )
            )
            .provideSome[Resolve & ContentStore & histos.schedule.RemoteCache](tools ++ LinkerStarts.live)
            .mapError(error => PluginFault.Task(failed(error.message)))
          _ <- Workspace
            .writeString(scriptRecord(repo, cell.module.name, cell.version, optimize), linked.script.toString)
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
        ZIO.foreach(found.distinct): request =>
          val Link(cell, optimize) = request
          scriptPath(repo, request).map(path => LinkedScript(cell.module.name, cell.version, path, optimize))

  def scriptDigest(repo: Path, request: Link): IO[PluginRunError, Sha256] =
    scriptPath(repo, request).flatMap: script =>
      ZIO
        .attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(script))))
        .orElseFail(failed(s"could not read $script"))

  private def scriptPath(repo: Path, request: Link): IO[PluginRunError, Path] =
    val Link(cell, optimize) = request
    Workspace
      .readString(scriptRecord(repo, cell.module.name, cell.version, optimize))
      .mapError(_ => failed(s"link of ${cell.module.name.value} ${cell.version.value} was not written"))
      .flatMap(text => ZIO.attempt(Path.of(text.trim)).orElseFail(failed("invalid linked script path")))

  private[load] def scriptRecord(repo: Path, module: ProjectName, version: ScalaVersion, optimize: JsOptimize): Path =
    repo
      .resolve(".histos")
      .resolve("link")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(s"script-${optimize.token}")

  private[load] def compile(
      session: histos.zinc.CompileSession,
      cell: Cell,
      events: PluginEvents.Invocation,
  ): ZIO[
    Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService & histos.schedule.RemoteCache,
    PluginFault,
    Unit,
  ] =
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
            .tap(feed => report(repo, events, feed))
            .provideSomeLayer[
              Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService & histos.schedule.RemoteCache
            ](compilers ++ memo)
            .runCollect
            .mapError(error => PluginFault.Compiled(error))
          _ <- chunk.toList.collect { case CompileFeed.Failed(error) => error } match
            case Nil        => ZIO.unit
            case error :: _ => ZIO.fail(PluginFault.Compiled(error))
          _ <- cell.scope match
            case SourceSet.Main => ZIO.unit
            case SourceSet.Test =>
              for
                sources <- testSources(repo, graph, cell).mapError(PluginFault.Tested(_))
                _       <- ZIO.unless(sources.isEmpty)(
                  compileTests(repo, graph, cell, sources, jdk, origin, lock)
                    .provideSomeLayer[
                      Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService &
                        histos.schedule.RemoteCache
                    ](compilers)
                    .mapError(PluginFault.Tested(_))
                )
              yield ()
        yield ()
      }

  /** Each compile line is an invocation event, so the console shows it while the cell is still compiling. A closed
    * handle does not fail the compile.
    */
  private def report(repo: Path, events: PluginEvents.Invocation, feed: CompileFeed): UIO[Unit] =
    ZIO.foreachDiscard(CompileConsole.lines(repo, feed)): line =>
      val text = line.text.filterNot(_.isControl)
      if text.isEmpty then ZIO.unit
      else
        PluginDetail.from(text) match
          case Left(_)       => ZIO.unit
          case Right(detail) =>
            val event = line match
              case _: CompileConsole.Line.Err => InvocationPluginEvent.Err(detail)
              case _: CompileConsole.Line.Out => InvocationPluginEvent.Message(detail)
            events.emit(event).orElseSucceed(())

  private def compileTests(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell,
      sources: List[Path],
      jdk: JdkRuntime,
      origin: NativeOrigin,
      lock: DependencyLock,
  ): ZIO[
    Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService & histos.zinc.Compilers &
      histos.schedule.RemoteCache,
    ModuleTestError,
    Unit,
  ] =
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
    for
      service <- ZIO.service[histos.resolve.ResolutionService]
      _       <- TestCompile
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
          _ => (),
          plugins = ApiDep.modules(graph),
        )
        .provideSome[
          Resolve & histos.cache.ContentStore & histos.zinc.Compilers & histos.schedule.RemoteCache
        ](
          zio.ZLayer.fromZIO(LockedTools.make(lock, graph, service))
        )
    yield ()

  private def paths(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell,
      lock: DependencyLock,
  ): ZIO[Resolve & histos.resolve.ResolutionService, PluginFault, CellClasspath] =
    pathsOf(repo, graph, cell, lock, ApiDep.plugin(cell.module, graph))

  private def pathsOf(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell,
      lock: DependencyLock,
      pin: Boolean,
  ): ZIO[Resolve & histos.resolve.ResolutionService, PluginFault, CellClasspath] =
    val modules      = modulesOf(graph)
    val repositories = BuildResolvers(graph.resolvers)
    for
      service <- ZIO.service[histos.resolve.ResolutionService]
      compile <- compileClasspath(repo, cell, modules, repositories, pin)
        .provideSome[Resolve & histos.resolve.ResolutionService](
          zio.ZLayer.fromZIO(LockedTools.make(lock, graph, service))
        )
      test <- cell.scope match
        case SourceSet.Main => ZIO.succeed(Nil)
        case SourceSet.Test =>
          testSources(repo, graph, cell)
            .mapError(PluginFault.Tested(_))
            .flatMap: sources =>
              if sources.isEmpty then ZIO.succeed(compile)
              else
                testJars(cell, repositories)
                  .map: extra =>
                    val kept = if pin then extra.filterNot(ApiDep.resolvedJar) else extra
                    testClasses(repo, cell) :: (compile ++ kept)
    yield CellClasspath(
      cell.module.name,
      cell.version,
      cell.platform,
      compile,
      test,
      cell.scope,
    )

  private def compileClasspath(
      repo: Path,
      cell: Cell,
      modules: List[Module],
      repositories: Repositories,
      pin: Boolean,
  ): ZIO[Resolve & histos.resolve.ResolutionService & LockedTools, PluginFault, List[Path]] =
    for
      read <- UpstreamCells
        .read(
          cell.module,
          Module.chain(cell.module, modules, cell.platform),
          cell.version,
          cell.platform,
          UpstreamCells.declared(repo, modules, cell.platform),
        )
        .mapError(PluginFault.Compiled(_))
      upstream = read.map(_.classes)
      fetched <- materialize(
        cell,
        repositories,
        ClasspathScope.Compile,
        cell.module.dependencies,
      )
      library    <- standardLibrary(cell).mapError(PluginFault.Tested(_))
      transitive <- ZIO.foreach(read): found =>
        materialize(
          Cell(found.module, found.version, cell.platform, SourceSet.Main),
          repositories,
          ClasspathScope.Compile,
          found.module.dependencies,
        )
      jars =
        (fetched ++ transitive.flatten.distinct.filterNot(fetched.toSet.contains))
          .filterNot(path => cell.platform == Platform.Jvm && JvmCompile.isStandardLibrary(path))
      kept = if pin then jars.filterNot(ApiDep.resolvedJar) else jars
      host <-
        if pin then ScalacClasspath.hostApi(repo).mapBoth(PluginFault.Compiled(_), path => List(path))
        else ZIO.succeed(Nil)
    yield classes(repo, cell) :: (upstream ++ library ++ kept ++ host)

  private def testJars(
      cell: Cell,
      repositories: Repositories,
  ): ZIO[Resolve & histos.resolve.ResolutionService, PluginFault, List[Path]] =
    val compileDeps = Dep.select(cell.module.dependencies, cell.platform)
    val testDeps    = Dep.select(cell.module.testDependencies, cell.platform)
    if testDeps.isEmpty then ZIO.succeed(Nil)
    else
      for
        compile <- materialize(
          cell,
          repositories,
          ClasspathScope.Compile,
          cell.module.dependencies,
        )
        both <- materialize(
          cell,
          repositories,
          ClasspathScope.Test,
          compileDeps ++ testDeps,
        )
      yield both.filterNot(compile.toSet.contains)

  private def materialize(
      cell: Cell,
      repositories: Repositories,
      scope: ClasspathScope,
      dependencies: List[Dep],
  ): ZIO[Resolve & histos.resolve.ResolutionService, PluginFault, List[Path]] =
    val request = ResolveCell(
      cell.module.name,
      Dep.select(dependencies, cell.platform),
      cell.version,
      cell.platform,
      repositories,
      scope,
    )
    ZIO
      .serviceWithZIO[histos.resolve.ResolutionService](_.module(request))
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
      scope: SourceSet,
  ): List[Cell] =
    val versions =
      version.fold(module.versions.all)(one => if module.versions.all.contains(one) then List(one) else Nil)
    val platforms =
      platform.fold(module.platforms.all)(one => if module.platforms.all.contains(one) then List(one) else Nil)
    versions
      .flatMap(item => platforms.map(target => Cell(module, item, target, scope)))
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
