package histos.program

import histos.cache.{ContentStore, LinkerStarts}
import histos.doctor.Suggest
import histos.jdk.JdkRuntime
import histos.resolve.LockedTools
import histos.js.{JsLink, JsLinkRequest, JsRun, JsRunRequest}
import histos.native.{NativeLink, NativeLinkRequest, NativeStack, NativeToolchain}
import histos.node.NodeMiss
import histos.pack.{MainClass as Binary, MainClassError}
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  DependencyLock,
  Repositories,
  ResolutionService,
  Resolve,
  ResolveCell,
  ResolveError,
}
import histos.process.{ChildClasspath, ChildEvent, ChildProcess, ChildRequest, RunRelay}
import histos.server.RunLaunch
import histos.toolchain.Runtimes
import histos.workspace.Workspace
import histos.zinc.{CompileFeed, GraphCompile, GraphCompileError, JvmCompile, UpstreamCells}
import histos.schedule.{Histos, Outcome, Parallelism, RemoteCache, StepFailure}
import histos.step.{Input, InputName, InputValue, Need, Phase, Step, StepId, StepInfo}
import histos.{Aggregate, ClangPin, Dep, MainClass, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path, StandardCopyOption}
import java.nio.file.attribute.PosixFilePermissions

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Queue, Ref, UIO, ZIO}
import zio.stream.{Take, ZStream}

/** Run the class a module names, on one platform. The main classpath only. No test classes and no classpath scan. A
  * module that builds more than one platform does not compile and does not start until `choice` names one of them.
  */
object Program:
  def console(
      repo: Path,
      graph: ProjectGraph,
      name: String,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      choice: Option[histos.PlatformSelection],
      launch: RunLaunch,
      args: List[String],
      relay: RunRelay,
      report: CompileFeed => UIO[Unit],
      afterCompile: UIO[Unit],
      mainOverride: Option[histos.MainClass] = None,
      plugins: Option[histos.load.PluginRuntime] = None,
  ): ZStream[Resolve & histos.cache.ContentStore & ResolutionService & RemoteCache, ProgramError, ProgramFeed] =
    ZStream.unwrapScoped:
      for
        module <- ZIO.fromEither(locate(graph, name))
        main   <- ZIO.fromEither(
          mainOverride.orElse(module.application).toRight(ProgramError.NoMain(module.name))
        )
        binary <- ZIO.fromEither(
          Binary.from(main.value).left.map(ProgramError.Main(_))
        )
        platform <- ZIO.fromEither(histos.RunPlatform.resolve(module, choice)).mapError(ProgramError.Selection(_))
        lock     <- ZIO.serviceWithZIO[ResolutionService](_.current)
        runtime  <- histos.build.BuildRuntime.open(plugins)
        roots = module.versions.all.map(version =>
          histos.build.BuildNode.Compile(
            histos.CompilationStage(histos.Cell(module.name, version, platform), histos.plugin.SourceSet.Main)
          )
        )
        products <- runtime
          .ensure(
            histos.build.ExecutionRequest(
              histos.zinc.CompileSession(repo, graph, jdk, runtimes.native, lock),
              roots,
              report,
            )
          )
          .mapError(error => ProgramError.Scoped(histos.testrun.TestFailures(error)))
        _ <- afterCompile
        cells = products.stages.toList
          .collect {
            case (stage, product)
                if stage.scope == histos.plugin.SourceSet.Main && stage.cell.module == module.name && stage.cell.platform == platform =>
              product.compiled
          }
          .sortBy(_.version.value)
        continue <- Ref.make(true)
        store    <- ZIO.service[ContentStore]
      yield ZStream
        .fromIterable(cells)
        .flatMap: cell =>
          ZStream.unwrap:
            continue.get.map: keep =>
              if keep then
                ZStream
                  .unwrap {
                    resourcesFor(
                      repo,
                      graph,
                      histos.CompilationStage(
                        histos.Cell(cell.module, cell.version, cell.platform),
                        histos.plugin.SourceSet.Main,
                      ),
                      products,
                    )
                      .map(resources =>
                        live(
                          CellRun(
                            repo,
                            graph,
                            module,
                            binary,
                            cell,
                            lock,
                            jdk,
                            runtimes,
                            store,
                            launch,
                            args,
                            relay,
                            resources,
                          )
                        )
                      )
                  }
                  .tap:
                    case ProgramFeed.Exited(_, _, _, code) if code != 0 =>
                      continue.set(false)
                    case _ => ZIO.unit
              else ZStream.empty

  /** Link the native client and copy the binary to [[ClientBinary.destination]]. Does not start it. */
  def install(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve & histos.cache.ContentStore & ResolutionService & RemoteCache, ProgramError, Path] =
    link(repo, graph, ClientBinary.moduleName, ClientBinary.destination(repo), jdk, runtimes, report)

  /** Compile `moduleName` to NIR and zip that classpath. Does not call clang. */
  def exportKit(
      repo: Path,
      graph: ProjectGraph,
      moduleName: String,
      destination: Path,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve & histos.cache.ContentStore & ResolutionService & RemoteCache, ProgramError, Path] =
    for
      ready <- prepare(repo, graph, moduleName, jdk, runtimes, report)
      paths <- kitClasspath(repo, graph, ready)
      major <- nativeMajor(ready.module, ready.cell.version)
      native = ready.module.nativeVersion.getOrElse(ready.tools.catalog.native.revision)
      kept   = NativeLink.withoutStandardLibraries(paths)
      written <- ClientKit
        .write(destination, ClientKit(ready.binary, major, ready.cell.version, native, kept))
        .mapError(error => ProgramError.Kit(error.message))
    yield written

  /** Link `moduleName` and copy the binary to `destination`. Does not start it. */
  def link(
      repo: Path,
      graph: ProjectGraph,
      moduleName: String,
      destination: Path,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve & histos.cache.ContentStore & ResolutionService & RemoteCache, ProgramError, Path] =
    prepare(repo, graph, moduleName, jdk, runtimes, report).flatMap: ready =>
      linkClient(
        repo,
        graph,
        ready.module,
        ready.cell,
        ready.lock,
        jdk,
        ready.binary,
        runtimes,
        ready.store,
        destination,
      ).provideSomeLayer[Resolve & RemoteCache](zio.ZLayer.succeed(ready.tools))

  private final case class Prepared(
      module: Module,
      cell: histos.zinc.CompiledCell,
      binary: Binary,
      lock: DependencyLock,
      tools: LockedTools,
      store: ContentStore,
  )

  private def prepare(
      repo: Path,
      graph: ProjectGraph,
      moduleName: String,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve & histos.cache.ContentStore & ResolutionService & RemoteCache, ProgramError, Prepared] =
    for
      module   <- ZIO.fromEither(locate(graph, moduleName))
      main     <- ZIO.fromEither(module.application.toRight(ProgramError.NoMain(module.name)))
      binary   <- ZIO.fromEither(Binary.from(main.value).left.map(ProgramError.Main(_)))
      _        <- ZIO.fromEither(pick(module, Some(Platform.Native)))
      service  <- ZIO.service[ResolutionService]
      lock     <- service.current
      tools    <- LockedTools.make(lock, graph, service)
      store    <- ZIO.service[ContentStore]
      compiled <- GraphCompile
        .run(
          histos.zinc.CompileSession(repo, graph, jdk, runtimes.native, lock),
          List(module.name),
          Some(Platform.Native),
          report,
        )
        .mapError(ProgramError.Compile(_))
      cell <- compiled.filter(cell => cell.module == module.name && cell.platform == Platform.Native) match
        case cell :: Nil => ZIO.succeed(cell)
        case _           =>
          module.versions.all match
            case version :: _ =>
              ZIO.fail(
                ProgramError.Link(
                  module.name,
                  version,
                  Platform.Native,
                  "the client did not compile to one native cell",
                )
              )
            case Nil => ZIO.fail(ProgramError.Absent(moduleName))
    yield Prepared(module, cell, binary, lock, tools, store)

  private def kitClasspath(
      repo: Path,
      graph: ProjectGraph,
      ready: Prepared,
  ): ZIO[Resolve, ProgramError, List[Path]] =
    val modules      = graph.projects.collect { case item: Module => item }
    val repositories = BuildResolvers(graph.resolvers)
    classpath(repo, modules, ready.module, ready.cell, ready.lock, repositories)
      .provideSomeLayer[Resolve](zio.ZLayer.succeed(ready.tools))

  private def linkClient(
      repo: Path,
      graph: ProjectGraph,
      module: Module,
      cell: histos.zinc.CompiledCell,
      lock: DependencyLock,
      jdk: JdkRuntime,
      binary: Binary,
      runtimes: Runtimes,
      store: ContentStore,
      destination: Path,
  ): ZIO[Resolve & LockedTools & RemoteCache, ProgramError, Path] =
    val modules      = graph.projects.collect { case item: Module => item }
    val repositories = BuildResolvers(graph.resolvers)
    for
      view  <- ZIO.service[LockedTools]
      paths <- classpath(
        repo,
        modules,
        module,
        cell,
        lock,
        repositories,
      )
      linkDir   <- fresh(workDir(repo, cell))
      major     <- nativeMajor(module, cell.version)
      toolchain <- ZIO
        .fromEither(
          NativeToolchain.of(
            module.nativeVersion.getOrElse(view.catalog.native.revision),
            runtimes.native,
          )
        )
        .mapError(error =>
          ProgramError.Tool(
            cell.module,
            cell.version,
            histos.toolchain.PlatformToolError.NativeOrigin(error),
          )
        )
      program <- cachedLink(
        store,
        NativeLink.linkClasspath(
          NativeLinkRequest(
            major,
            toolchain,
            runtimes.env,
            linkDir,
            jdk,
            binary,
            cell.version,
            libraries = module.libraries,
          ),
          paths,
        ),
      ).mapError(error =>
        ProgramError.Tool(
          cell.module,
          cell.version,
          histos.toolchain.PlatformToolError.Native(error),
        )
      )
      placed <- place(program, destination)
    yield placed

  private def place(binary: Path, dest: Path): IO[ProgramError, Path] =
    Option(dest.getParent) match
      case None         => ZIO.fail(ProgramError.Work(dest))
      case Some(parent) =>
        ZIO
          .attemptBlocking:
            Files.createDirectories(parent)
            Files.copy(binary, dest, StandardCopyOption.REPLACE_EXISTING)
            Files.setPosixFilePermissions(dest, PosixFilePermissions.fromString("rwxr-xr-x"))
            dest
          .mapError(_ => ProgramError.Work(dest))

  /** One cell's feeds as the program produces them. The run is a step, so it stays scheduled, and it hands each feed to
    * the stream as it happens. The run belongs to the stream's scope: ending the stream stops the program.
    */
  private def live(run: CellRun): ZStream[Resolve & ResolutionService & RemoteCache, ProgramError, ProgramFeed] =
    ZStream.unwrapScoped:
      for
        queue <- Queue.unbounded[Take[ProgramError, ProgramFeed]]
        _     <- runCell(run, feed => queue.offer(Take.single(feed)).unit)
          .foldCauseZIO(cause => queue.offer(Take.failCause(cause)), _ => queue.offer(Take.end))
          .forkScoped
      yield ZStream.fromQueue(queue).flattenTake

  private def runCell(
      run: CellRun,
      emit: ProgramFeed => UIO[Unit],
  ): ZIO[Resolve & ResolutionService & RemoteCache, ProgramError, Unit] =
    (for
      report   <- ZIO.service[ResolutionService]
      fresh    <- report.current
      tools    <- LockedTools.make(fresh, run.graph, report)
      selected <- tools
        .application(
          run.module,
          run.cell.version,
          run.cell.platform,
          run.runtimes.native,
        )
        .mapError(ProgramError.Lock(_))
      inputs <- run.runtimes
        .inputs(run.cell.platform)
        .mapError(error => ProgramError.NoNode(run.cell.module, run.cell.version, error))
    yield (tools, selected, inputs)).flatMap: (tools, selected, inputs) =>
      val info = StepInfo(
        StepId
          .Cell(
            run.cell.module,
            run.cell.version,
            run.cell.platform,
            Phase.Run,
          ),
        inputs ++ Chunk(
          Input(
            InputName("jdk"),
            InputValue.Text(Runtimes.identityToken(run.jdk.identity)),
          ),
          Input(
            InputName("tools"),
            InputValue.Text(
              selected
                .map(_.fingerprint.value)
                .mkString("\n")
            ),
          ),
        ),
        Set(Need.Process),
      )
      execute(
        Step.local[Unit](info)(
          one(run)
            .runForeach(emit)
            .provideSome[Resolve & RemoteCache](
              zio.ZLayer.succeed(tools)
            )
        )
      )

  private def execute[A](
      goal: Step[Resolve & RemoteCache, ProgramError, A]
  ): ZIO[Resolve & RemoteCache, ProgramError, A] =
    ZIO.scoped:
      ZIO
        .serviceWithZIO[Histos](_.start(goal))
        .flatMap(_.outcome)
        .provideSome[Resolve & RemoteCache & zio.Scope](Parallelism.live >>> Histos.live)
        .flatMap:
          case Outcome.Done(value)      => ZIO.succeed(value)
          case Outcome.Failed(failures) =>
            failures.head match
              case StepFailure.Failed(_, cause) => ZIO.failCause(cause)
              case StepFailure.Skipped(_, _)    =>
                ZIO.dieMessage("run step was skipped")

  private def locate(
      graph: ProjectGraph,
      name: String,
  ): Either[ProgramError, Module] =
    val near = Suggest.runs(name, graph.projects.map(_.name.value))
    ProjectName.from(name) match
      case Left(_)      => Left(ProgramError.Absent(name, near))
      case Right(found) =>
        graph.get(found) match
          case Some(module: Module) => Right(module)
          case Some(_: Aggregate)   => Left(ProgramError.Aggregate(found))
          case None                 => Left(ProgramError.Absent(name, near))

  /** One platform, or a refusal. Nothing is compiled and nothing is started on the refusal.
    */
  private def pick(
      module: Module,
      choice: Option[Platform],
  ): Either[ProgramError, Platform] =
    val built = module.platforms.all
    choice match
      case Some(platform) if built.contains(platform) => Right(platform)
      case Some(platform)                             =>
        Left(ProgramError.Unbuilt(module.name, platform, built))
      case None =>
        built match
          case only :: Nil => Right(only)
          case many        => Left(ProgramError.Choose(module.name, many))

  private def resourcesFor(
      repo: Path,
      graph: ProjectGraph,
      stage: histos.CompilationStage,
      products: histos.build.ExecutionProducts,
  ): IO[ProgramError, List[histos.ResourceName]] =
    if stage.cell.platform != Platform.Native then ZIO.succeed(Nil)
    else
      for
        plan <- ZIO
          .fromEither(histos.build.BuildDag.plan(graph, List(histos.build.BuildNode.Compile(stage))))
          .mapError(error =>
            ProgramError.Scoped(histos.testrun.ModuleTestError.Scoped(histos.testrun.TestFailures.graph(error)))
          )
        files <- ZIO.foreach(plan.nodes.collect {
          case histos.build.BuildNode.Compile(stage)
              if stage.cell.platform == Platform.Native && stage.scope == histos.plugin.SourceSet.Main =>
            stage
        })(stage =>
          histos.zinc.StageRoots
            .of(graph)
            .read(repo, stage, products.generatedRoots)
            .mapError(error =>
              ProgramError.Scoped(histos.testrun.ModuleTestError.Scoped(histos.testrun.TestFailures.root(error)))
            )
        )
      yield files.flatMap(file => file.resources.map(_._1) ++ file.assets.map(_.name)).distinct

  private final case class CellRun(
      repo: Path,
      graph: ProjectGraph,
      module: Module,
      binary: Binary,
      cell: histos.zinc.CompiledCell,
      lock: DependencyLock,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      store: ContentStore,
      launch: RunLaunch,
      args: List[String],
      relay: RunRelay,
      resources: List[histos.ResourceName],
  )

  private def one(
      run: CellRun
  ): ZStream[Resolve & LockedTools & RemoteCache, ProgramError, ProgramFeed] =
    val CellRun(repo, graph, module, binary, cell, lock, jdk, runtimes, store, launch, args, relay, resources) = run
    val serverEnv    = runtimes.env
    val programEnv   = launch.programEnv(serverEnv)
    val modules      = graph.projects.collect { case item: Module => item }
    val repositories = BuildResolvers(graph.resolvers)
    ZStream.unwrap:
      launchDirectory(repo, launch).map: cwd =>
        val began = ZStream.succeed(
          ProgramFeed.Begin(cell.module, cell.version, cell.platform)
        )
        val ran =
          cell.platform match
            case Platform.Jvm =>
              ZStream.unwrap:
                for
                  paths <- classpath(
                    repo,
                    modules,
                    module,
                    cell,
                    lock,
                    repositories,
                  )
                  work <- fresh(workDir(repo, cell))
                yield feeds(
                  cell,
                  ChildProcess.events(
                    ChildRequest(
                      Platform.Jvm,
                      jdk.home.resolve("bin").resolve("java"),
                      ChildClasspath(cell.classes, paths.drop(1)),
                      binary,
                      work,
                      programEnv,
                      directory = cwd,
                      arguments = args,
                      jvmOptions = module.jvmOptions,
                    ),
                    relay,
                  ),
                )
            case Platform.Js =>
              ZStream.unwrap:
                for
                  view <- ZIO.service[LockedTools]
                  mode    = histos.JsOptimize.effective(view.catalog.js.optimize, module.jsOptimize, None)
                  linkDir = workDir(repo, cell).resolve(mode.token)
                  node  <- runtimes.node.mapError(miss => ProgramError.NoNode(cell.module, cell.version, miss))
                  paths <- classpath(
                    repo,
                    modules,
                    module,
                    cell,
                    lock,
                    repositories,
                  )
                  _      <- fresh(linkDir)
                  linked <- cachedLink(
                    store,
                    JsLink.link(
                      JsLinkRequest(
                        cell.version,
                        binary,
                        paths,
                        linkDir,
                        jdk,
                        module.jsModule,
                        revision = module.jsVersion,
                        arguments = args,
                        optimize = mode,
                      )
                    ),
                  ).mapError(error =>
                    ProgramError.Tool(
                      cell.module,
                      cell.version,
                      histos.toolchain.PlatformToolError.JsLink(error),
                    )
                  )
                  asked <- JsRun
                    .start(
                      JsRunRequest(
                        module.jsEnv,
                        module.jsModule,
                        linked.script,
                        linkDir,
                        jdk,
                        programEnv,
                        node,
                        directory = cwd,
                      )
                    )
                    .mapError(error =>
                      ProgramError.Tool(
                        cell.module,
                        cell.version,
                        histos.toolchain.PlatformToolError.JsRun(error),
                      )
                    )
                yield feeds(cell, ChildProcess.events(asked, relay))
            case Platform.Native =>
              ZStream.unwrap:
                val linkDir = workDir(repo, cell)
                for
                  view  <- ZIO.service[LockedTools]
                  major <- nativeMajor(module, cell.version)
                  paths <- classpath(
                    repo,
                    modules,
                    module,
                    cell,
                    lock,
                    repositories,
                  )
                  _         <- fresh(linkDir)
                  toolchain <- ZIO
                    .fromEither(
                      NativeToolchain.of(
                        module.nativeVersion
                          .getOrElse(view.catalog.native.revision),
                        runtimes.native,
                      )
                    )
                    .mapError(error =>
                      ProgramError.Tool(
                        cell.module,
                        cell.version,
                        histos.toolchain.PlatformToolError.NativeOrigin(error),
                      )
                    )
                  program <- cachedLink(
                    store,
                    NativeLink.linkClasspath(
                      NativeLinkRequest(
                        major,
                        toolchain,
                        serverEnv,
                        linkDir,
                        jdk,
                        binary,
                        cell.version,
                        libraries = module.libraries,
                        resources = resources,
                      ),
                      paths,
                    ),
                  ).mapError(error =>
                    ProgramError.Tool(
                      cell.module,
                      cell.version,
                      histos.toolchain.PlatformToolError.Native(error),
                    )
                  )
                yield feeds(
                  cell,
                  ChildProcess.command(
                    program.toString :: args,
                    cwd.getOrElse(linkDir),
                    nativeEnvironment(programEnv, module),
                    relay,
                  ),
                )
        began ++ ran

  /** A shell directory may sit outside the repo. A tool directory must stay inside it. */
  private def launchDirectory(repo: Path, launch: RunLaunch): IO[ProgramError, Option[Path]] =
    launch match
      case RunLaunch.Shell(_, directory) =>
        RunDirectory.existing(directory).mapBoth(ProgramError.Directory(_), Some(_))
      case RunLaunch.Tool(_, None) =>
        ZIO.succeed(None)
      case RunLaunch.Tool(_, Some(directory)) =>
        RunDirectory.accept(repo, directory).mapBoth(ProgramError.Directory(_), Some(_))

  private def cachedLink[R, E, A](
      store: ContentStore,
      effect: ZIO[R & ContentStore & LinkerStarts, E, A],
  ): ZIO[R, E, A] =
    effect.provideSomeLayer(zio.ZLayer.succeed(store) ++ LinkerStarts.live)

  /** Lines from the child's pipes. */
  private def feeds(
      cell: histos.zinc.CompiledCell,
      ran: ZStream[Any, histos.process.ChildError, ChildEvent],
  ): ZStream[Any, ProgramError, ProgramFeed] =
    ran
      .mapError(error => ProgramError.Process(cell.module, cell.version, error.message))
      .map:
        case ChildEvent.Out(text)    => ProgramFeed.Out(text)
        case ChildEvent.Err(text)    => ProgramFeed.Err(text)
        case ChildEvent.Bytes(data)  => ProgramFeed.Bytes(data)
        case ChildEvent.Exited(code) =>
          ProgramFeed.Exited(
            cell.module,
            cell.version,
            cell.platform,
            code.value,
          )

  /** A Native program's environment. The module's [[histos.ThreadStack]] wins over the program environment. */
  private[histos] def nativeEnvironment(env: Map[String, String], module: Module): Map[String, String] =
    env ++ NativeStack.environment(module.threadStack)

  /** Main classes, each upstream cell's classes, the JVM library on JVM, then this cell's lock jars and each upstream
    * cell's. The upstream cells are the ones [[UpstreamCells.read]] picks. A JVM run takes runtime dependencies, and a
    * link takes compile dependencies.
    */
  private[histos] def classpath(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: histos.zinc.CompiledCell,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve & LockedTools, ProgramError, List[Path]] =
    val jars = (item: Module, version: ScalaVersion) =>
      val dependencies = cell.platform match
        case Platform.Jvm                  => Dep.runtime(item.dependencies, cell.platform)
        case Platform.Js | Platform.Native => Dep.select(item.dependencies, cell.platform)
      val asked = ResolveCell(item.name, dependencies, version, cell.platform, repositories, ClasspathScope.Compile)
      ZIO
        .serviceWithZIO[LockedTools]: tools =>
          val view = if tools.service.isDefined then tools else tools.copy(lock = lock)
          view.module(asked)
        .mapBoth(ProgramError.Lock(_), _.artifacts.map(_.path))
    for
      cells <- UpstreamCells
        .read(
          module,
          Module.chain(module, modules, cell.platform),
          cell.version,
          cell.platform,
          UpstreamCells.declared(repo, modules, cell.platform),
        )
        .mapError(ProgramError.Compile(_))
      upstream <- ZIO.foreach(cells): read =>
        ZIO
          .attemptBlocking(Files.isDirectory(read.classes))
          .mapError(_ => ProgramError.Root(read.classes))
          .flatMap: found =>
            if found then ZIO.succeed(read.classes)
            else ZIO.fail(ProgramError.MissingUpstream(module.name, read.module.name, read.version))
      own     <- jars(module, cell.version)
      theirs  <- ZIO.foreach(cells)(read => jars(read.module, read.version))
      library <- cell.platform match
        case Platform.Jvm =>
          JvmCompile
            .runtime(cell.version)
            .mapError(ProgramError.Library(_))
        case Platform.Js | Platform.Native => ZIO.succeed(Nil)
      paths = (own ++ theirs.flatten).distinct
      jars  =
        cell.platform match
          case Platform.Jvm                  => paths.filterNot(JvmCompile.isStandardLibrary)
          case Platform.Js | Platform.Native =>
            paths.filterNot(library.toSet.contains)
    yield cell.classes :: (upstream ++ library ++ jars)

  private def nativeMajor(
      module: Module,
      version: ScalaVersion,
  ): IO[ProgramError, histos.ClangMajor] =
    module.clang match
      case ClangPin.Major(major) => ZIO.succeed(major)
      case ClangPin.Unused       =>
        ZIO.fail(ProgramError.NoClang(module.name, version))

  private def readLock(repo: Path): IO[ProgramError, DependencyLock] =
    Resolve
      .readLock(repo)
      .mapError:
        case ResolveError.Io(failure) => ProgramError.LockRead(failure.path)
        case other                    => ProgramError.Lock(other)

  private def fresh(work: Path): IO[ProgramError, Path] =
    Workspace.recreate(work).mapError(_ => ProgramError.Work(work))

  private def workDir(repo: Path, cell: histos.zinc.CompiledCell): Path =
    workDir(repo, cell.module, cell.version, cell.platform)

  private[histos] def workDir(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    repo
      .resolve(".histos")
      .resolve("run")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)
end Program

enum ProgramFeed:
  case Begin(module: ProjectName, version: ScalaVersion, platform: Platform)
  case Out(text: String)
  case Err(text: String)
  case Bytes(data: Chunk[Byte])
  case Exited(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      code: Int,
  )
