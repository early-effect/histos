package histos.program

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
import histos.process.{ChildClasspath, ChildEvent, ChildProcess, ChildRequest}
import histos.toolchain.Runtimes
import histos.workspace.Workspace
import histos.zinc.{CompileFeed, GraphCompile, GraphCompileError, JvmCompile}
import histos.schedule.{Histos, Outcome, Parallelism, StepFailure}
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
      choice: Option[Platform],
      report: CompileFeed => UIO[Unit],
      afterCompile: UIO[Unit],
  ): ZStream[Resolve & histos.cache.ContentStore & ResolutionService, ProgramError, ProgramFeed] =
    ZStream.unwrap:
      for
        module <- ZIO.fromEither(locate(graph, name))
        main   <- ZIO.fromEither(
          module.application.toRight(ProgramError.NoMain(module.name))
        )
        binary <- ZIO.fromEither(
          Binary.from(main.value).left.map(ProgramError.Main(_))
        )
        platform <- ZIO.fromEither(pick(module, choice))
        lock     <- ZIO.serviceWithZIO[ResolutionService](_.current)
        compiled <- GraphCompile
          .run(
            histos.zinc.CompileSession(repo, graph, jdk, runtimes.native, lock),
            List(module.name),
            Some(platform),
            report,
          )
          .mapError(ProgramError.Compile(_))
        _ <- afterCompile
        cells = compiled
          .filter(cell => cell.module == module.name && cell.platform == platform)
          .sortBy(_.version.value)
        continue <- Ref.make(true)
      yield ZStream
        .fromIterable(cells)
        .flatMap: cell =>
          ZStream.unwrap:
            continue.get.map: keep =>
              if keep then
                live(CellRun(repo, graph, module, binary, cell, lock, jdk, runtimes))
                  .tap:
                    case ProgramFeed.Exited(_, _, _, code) if code != 0 =>
                      continue.set(false)
                    case _ => ZIO.unit
              else ZStream.empty

  /** Link the native client and copy the binary to [[ClientBinary.destination]]. Does not start it.
    */
  def install(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      report: CompileFeed => UIO[Unit],
  ): ZIO[Resolve & histos.cache.ContentStore & ResolutionService, ProgramError, Path] =
    for
      module   <- ZIO.fromEither(locate(graph, ClientBinary.moduleName))
      main     <- ZIO.fromEither(module.application.toRight(ProgramError.NoMain(module.name)))
      binary   <- ZIO.fromEither(Binary.from(main.value).left.map(ProgramError.Main(_)))
      _        <- ZIO.fromEither(pick(module, Some(Platform.Native)))
      service  <- ZIO.service[ResolutionService]
      lock     <- service.current
      tools    <- LockedTools.make(lock, graph, service)
      compiled <- GraphCompile
        .run(
          histos.zinc.CompileSession(repo, graph, jdk, runtimes.native, lock),
          List(module.name),
          Some(Platform.Native),
          report,
        )
        .mapError(ProgramError.Compile(_))
      cells = compiled.filter(cell => cell.module == module.name && cell.platform == Platform.Native)
      linked <- cells match
        case cell :: Nil =>
          linkClient(repo, graph, module, cell, lock, jdk, binary, runtimes)
            .provideSomeLayer[Resolve](zio.ZLayer.succeed(tools))
        case _ =>
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
            case Nil => ZIO.fail(ProgramError.Absent(ClientBinary.moduleName))
    yield linked

  private def linkClient(
      repo: Path,
      graph: ProjectGraph,
      module: Module,
      cell: histos.zinc.CompiledCell,
      lock: DependencyLock,
      jdk: JdkRuntime,
      binary: Binary,
      runtimes: Runtimes,
  ): ZIO[Resolve & LockedTools, ProgramError, Path] =
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
        Dep.select(module.dependencies, cell.platform),
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
      program <- NativeLink
        .linkClasspath(
          NativeLinkRequest(
            major,
            toolchain,
            runtimes.env,
            linkDir,
            jdk,
            binary,
            cell.version,
          ),
          paths,
        )
        .mapError(error =>
          ProgramError.Tool(
            cell.module,
            cell.version,
            histos.toolchain.PlatformToolError.Native(error),
          )
        )
      placed <- place(program, ClientBinary.destination(repo))
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
  private def live(run: CellRun): ZStream[Resolve & ResolutionService, ProgramError, ProgramFeed] =
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
  ): ZIO[Resolve & ResolutionService, ProgramError, Unit] =
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
            .provideSome[Resolve](
              zio.ZLayer.succeed(tools)
            )
        )
      )

  private def execute[A](
      goal: Step[Resolve, ProgramError, A]
  ): ZIO[Resolve, ProgramError, A] =
    ZIO.scoped:
      ZIO
        .serviceWithZIO[Histos](_.start(goal))
        .flatMap(_.outcome)
        .provideSome[Resolve & zio.Scope](Parallelism.live >>> Histos.live)
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

  private final case class CellRun(
      repo: Path,
      graph: ProjectGraph,
      module: Module,
      binary: Binary,
      cell: histos.zinc.CompiledCell,
      lock: DependencyLock,
      jdk: JdkRuntime,
      runtimes: Runtimes,
  )

  private def one(
      run: CellRun
  ): ZStream[Resolve & LockedTools, ProgramError, ProgramFeed] =
    val CellRun(repo, graph, module, binary, cell, lock, jdk, runtimes) = run
    val env                                                             = runtimes.env
    val modules      = graph.projects.collect { case item: Module => item }
    val repositories = BuildResolvers(graph.resolvers)
    val began        = ZStream.succeed(
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
                Dep.runtime(module.dependencies, cell.platform),
              )
              work <- fresh(workDir(repo, cell))
            yield events(
              cell,
              ChildRequest(
                Platform.Jvm,
                jdk.home.resolve("bin").resolve("java"),
                ChildClasspath(cell.classes, paths.drop(1)),
                binary,
                work,
                env,
              ),
            )
        case Platform.Js =>
          ZStream.unwrap:
            val linkDir = workDir(repo, cell)
            for
              node  <- runtimes.node.mapError(miss => ProgramError.NoNode(cell.module, cell.version, miss))
              paths <- classpath(
                repo,
                modules,
                module,
                cell,
                lock,
                repositories,
                Dep.select(module.dependencies, cell.platform),
              )
              _      <- fresh(linkDir)
              linked <- JsLink
                .link(
                  JsLinkRequest(
                    cell.version,
                    binary,
                    paths,
                    linkDir,
                    jdk,
                    module.jsModule,
                    revision = module.jsVersion,
                  )
                )
                .mapError(error =>
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
                    env,
                    node,
                  )
                )
                .mapError(error =>
                  ProgramError.Tool(
                    cell.module,
                    cell.version,
                    histos.toolchain.PlatformToolError.JsRun(error),
                  )
                )
            yield events(cell, asked)
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
                Dep.select(module.dependencies, cell.platform),
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
              program <- NativeLink
                .linkClasspath(
                  NativeLinkRequest(
                    major,
                    toolchain,
                    env,
                    linkDir,
                    jdk,
                    binary,
                    cell.version,
                  ),
                  paths,
                )
                .mapError(error =>
                  ProgramError.Tool(
                    cell.module,
                    cell.version,
                    histos.toolchain.PlatformToolError.Native(error),
                  )
                )
              work <- fresh(linkDir.resolve("exec"))
            yield command(cell, List(program.toString), work, nativeEnvironment(env, module))
    began ++ ran

  private def events(
      cell: histos.zinc.CompiledCell,
      request: ChildRequest,
  ): ZStream[Any, ProgramError, ProgramFeed] =
    ChildProcess
      .events(request)
      .mapError(error => ProgramError.Process(cell.module, cell.version, error.message))
      .map:
        case ChildEvent.Out(text)    => ProgramFeed.Out(text)
        case ChildEvent.Err(text)    => ProgramFeed.Err(text)
        case ChildEvent.Exited(code) =>
          ProgramFeed.Exited(
            cell.module,
            cell.version,
            cell.platform,
            code.value,
          )

  /** A Native program's environment. The module's [[histos.ThreadStack]] wins over one the server inherited. */
  private[histos] def nativeEnvironment(env: Map[String, String], module: Module): Map[String, String] =
    env ++ NativeStack.environment(module.threadStack)

  private def command(
      cell: histos.zinc.CompiledCell,
      argv: List[String],
      work: Path,
      env: Map[String, String],
  ): ZStream[Any, ProgramError, ProgramFeed] =
    ChildProcess
      .command(argv, work, env)
      .mapError(error => ProgramError.Process(cell.module, cell.version, error.message))
      .map:
        case ChildEvent.Out(text)    => ProgramFeed.Out(text)
        case ChildEvent.Err(text)    => ProgramFeed.Err(text)
        case ChildEvent.Exited(code) =>
          ProgramFeed.Exited(
            cell.module,
            cell.version,
            cell.platform,
            code.value,
          )

  /** Main classes, upstream main classes, the JVM library on JVM, then compile jars.
    */
  private[histos] def classpath(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: histos.zinc.CompiledCell,
      lock: DependencyLock,
      repositories: Repositories,
      dependencies: List[Dep],
  ): ZIO[Resolve & LockedTools, ProgramError, List[Path]] =
    val resolveCell = ResolveCell(
      module.name,
      dependencies,
      cell.version,
      cell.platform,
      repositories,
      ClasspathScope.Compile,
    )
    for
      fetched <- ZIO
        .serviceWithZIO[LockedTools]: tools =>
          val view = if tools.service.isDefined then tools else tools.copy(lock = lock)
          view.module(resolveCell)
        .mapError(ProgramError.Lock(_))
      upstream <- ZIO.foreach(Module.chain(module, modules, cell.platform)): dependency =>
        val path = GraphCompile.classesDir(
          repo,
          dependency.name,
          cell.version,
          cell.platform,
        )
        ZIO
          .attemptBlocking(Files.isDirectory(path))
          .mapError(_ => ProgramError.Root(path))
          .flatMap: found =>
            if found then ZIO.succeed(path)
            else
              ZIO.fail(
                ProgramError
                  .MissingUpstream(module.name, dependency.name, cell.version)
              )
      library <- cell.platform match
        case Platform.Jvm =>
          JvmCompile
            .runtime(cell.version)
            .mapError(ProgramError.Library(_))
        case Platform.Js | Platform.Native => ZIO.succeed(Nil)
      paths = fetched.artifacts.map(_.path).distinct
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
  case Exited(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      code: Int,
  )
