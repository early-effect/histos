package histos.bsp

import histos.jdk.JdkRuntime
import zio.stream.ZStream

import histos.load.{BuildNotes, LoadError}
import histos.resolve.LockedTools
import histos.toolchain.BuildCompiler
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  DependencyLock,
  LockedArtifact,
  Repository,
  Resolve,
  ResolveCell,
  ResolveError,
}
import histos.program.{EntryPoints, Program, ProgramError}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.testrun.{ModuleTest, SuiteNames, TestCompile}
import histos.toolchain.Runtimes
import histos.TestProvider
import histos.zinc.{
  CompileError,
  CompileProblem,
  CompileSeverity,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  JvmCompile,
  PositionBase,

  ScalaSeries,
  SemanticDb,
}
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import zio.json.*
import zio.{Chunk, IO, Ref, UIO, ZIO}

/** BSP 2.2 for Metals. JVM targets, a build-file target, and diagnostics. Test running stays out.
  */
object Bsp:
  val BspVersion: String    = "2.2.0"
  val ServerName: String    = "histos"
  val ServerVersion: String = "0.0.1-SNAPSHOT"

  enum Answer:
    case Reply(line: String)
    case Quiet
    case Stop(code: Int)

  private enum Phase:
    case New, Ready, Shutdown

  /** One BSP method. The wire token stays the protocol string. */
  private[bsp] enum Method:
    case Exit, Initialize, Initialized, Shutdown, Targets, Sources, ScalacOptions, DependencySources, Compile,
      MainClasses, TestClasses, RunEnvironment, TestEnvironment

    def wire: String = this match
      case Exit              => "build/exit"
      case Initialize        => "build/initialize"
      case Initialized       => "build/initialized"
      case Shutdown          => "build/shutdown"
      case Targets           => "workspace/buildTargets"
      case Sources           => "buildTarget/sources"
      case ScalacOptions     => "buildTarget/scalacOptions"
      case DependencySources => "buildTarget/dependencySources"
      case Compile           => "buildTarget/compile"
      case MainClasses       => "buildTarget/scalaMainClasses"
      case TestClasses       => "buildTarget/scalaTestClasses"
      case RunEnvironment    => "buildTarget/jvmRunEnvironment"
      case TestEnvironment   => "buildTarget/jvmTestEnvironment"
  end Method

  private[bsp] object Method:
    val all: List[Method] =
      List(
        Method.Exit,
        Method.Initialize,
        Method.Initialized,
        Method.Shutdown,
        Method.Targets,
        Method.Sources,
        Method.ScalacOptions,
        Method.DependencySources,
        Method.Compile,
        Method.MainClasses,
        Method.TestClasses,
        Method.RunEnvironment,
        Method.TestEnvironment,
      )

    def from(raw: String): Option[Method] =
      all.find(_.wire == raw)
  end Method

  private[bsp] enum ParamFault:
    case Missing, Rejected

    def message: String = this match
      case Missing  => Rpc.ParamsDecode.Missing.message
      case Rejected => Rpc.ParamsDecode.Rejected.message
  end ParamFault

  private[bsp] enum BspError:
    case Machine(error: histos.jdk.ToolchainError)
    case Parse
    case NotRequest
    case NoMethod
    case NotInitialized
    case ShutDown
    case UnknownMethod(method: String)
    case Params(fault: ParamFault)
    case UnknownTarget
    case Lock(error: ResolveError)
    case LockRead(path: Path)
    case Resolve(error: ResolveError)
    case Toolchain(error: CompileError)
    case Sources(error: GraphCompileError)
    case Tests(error: histos.testrun.ModuleTestError)
    case Run(error: ProgramError)
    case Build(error: LoadError)
    case NotTest
    case NotMain
    case Platforms(text: String)
    case Mains(failure: IoFailure)
    case Io(failure: IoFailure)

    def code: Int = this match
      case Parse                                         => -32700
      case NotRequest | NoMethod | ShutDown              => -32600
      case UnknownMethod(_)                              => -32601
      case Params(_) | UnknownTarget | NotTest | NotMain => -32602
      case NotInitialized                                => -32002
      case Machine(_) | Lock(_) | LockRead(_) | Resolve(_) | Toolchain(_) | Sources(_) | Tests(_) | Run(_) | Build(_) |
          Platforms(_) | Mains(_) | Io(_) =>
        -32603

    def message: String = this match
      case Machine(error)        => error.message
      case Parse                 => "parse error"
      case NotRequest            => "bsp requires a json-rpc object"
      case NoMethod              => "bsp requires a method"
      case NotInitialized        => "bsp is not initialized"
      case ShutDown              => "bsp has shut down"
      case UnknownMethod(method) => s"bsp does not handle $method"
      case Params(fault)         => fault.message
      case UnknownTarget         => "unknown build target"
      case Lock(error)           => error.message
      case LockRead(path)        => s"could not read $path"
      case Resolve(error)        => error.message
      case Toolchain(error)      => error.message
      case Sources(error)        => error.message
      case Tests(error)          => error.message
      case Run(error)            => error.message
      case Build(error)          => error.message
      case NotTest               => "jvmTestEnvironment needs a test target"
      case NotMain               => "jvmRunEnvironment needs a main target"
      case Platforms(text)       => text
      case Mains(failure)        => failure.message
      case Io(failure)           => failure.message
  end BspError

  /** A directory the request needs. A file in the way is a fault, not a defect. */
  private[bsp] def ensureDir(path: Path): IO[BspError, Path] =
    Workspace.createDirectories(path).mapError(BspError.Io(_))

  /** The compiler calls `hear` on its own thread. The chunk is still there when the effect fails. */
  private[bsp] def problems[R, E, A](
      use: (CompileProblem => Unit) => ZIO[R, E, A]
  ): ZIO[R, Nothing, (Either[E, A], Chunk[CompileProblem])] =
    ZIO.succeed(new ConcurrentLinkedQueue[CompileProblem]).flatMap { queue =>
      def hear(problem: CompileProblem): Unit =
        queue.add(problem)
        ()
      use(hear).either.map { result =>
        (result, Chunk.fromIterable(queue.asScala))
      }
    }

  def open(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      buildClasspath: List[Path] = Nil,
      notify: String => UIO[Unit] = _ => ZIO.unit,
      build: IO[LoadError, BuildNotes] = ZIO.succeed(BuildNotes(false, Nil)),
      gate: ZIO[Resolve, Nothing, Int] => ZIO[Resolve, Nothing, Int] = effect => effect,
      shared: Option[Ref[ProjectGraph]] = None,
  ): IO[ResolveError, Session] =
    for
      buildScala <- BuildCompiler.version
      phase      <- Ref.make[Phase](Phase.New)
      current    <- shared match
        case Some(ref) => ZIO.succeed(ref)
        case None      => Ref.make(graph)
      request  <- Ref.make(graph)
      captured <- Ref.make(Option.empty[DependencyLock])
    yield Session.start(
      buildScala,
      repo,
      jdk,
      runtimes,
      buildClasspath,
      notify,
      build,
      phase,
      gate,
      current,
      request,
      captured,
    )

  object Session:
    private[Bsp] def start(
        buildScala: ScalaVersion,
        repo: Path,
        jdk: JdkRuntime,
        runtimes: Runtimes,
        buildClasspath: List[Path],
        notify: String => UIO[Unit],
        build: IO[LoadError, BuildNotes],
        phase: Ref[Phase],
        gate: ZIO[Resolve, Nothing, Int] => ZIO[Resolve, Nothing, Int],
        current: Ref[ProjectGraph],
        request: Ref[ProjectGraph],
        captured: Ref[Option[DependencyLock]],
    ): Session =
      new Session(
        buildScala,
        repo,
        jdk,
        runtimes,
        buildClasspath,
        notify,
        build,
        phase,
        gate,
        current,
        request,
        captured,
      )

  final class Session private (
      BuildScala: ScalaVersion,
      repo: Path,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      buildClasspath: List[Path],
      notify: String => UIO[Unit],
      build: IO[LoadError, BuildNotes],
      phase: Ref[Phase],
      gate: ZIO[Resolve, Nothing, Int] => ZIO[Resolve, Nothing, Int],
      current: Ref[ProjectGraph],
      request: Ref[ProjectGraph],
      captured: Ref[Option[DependencyLock]],
  ):
    /** One request sees the graph it started with. A later swap does not rewrite it. The JDK and Node are the ones
      * captured when the session opened.
      */
    def answer(payload: String): ZIO[Resolve, Nothing, Answer] =
      current.get.flatMap(graph => answer(graph, payload))

    def answer(
        seen: ProjectGraph,
        payload: String,
    ): ZIO[Resolve, Nothing, Answer] =
      for
        requested <- Ref.make(seen)
        lock      <- Ref.make(Option.empty[DependencyLock])
        session = Session.start(
          BuildScala,
          repo,
          jdk,
          runtimes,
          buildClasspath,
          notify,
          build,
          phase,
          gate,
          current,
          requested,
          lock,
        )
        answer <- session.read(payload)
      yield answer

    /** Kinds are BSP's created (1), changed (2), and deleted (3). */
    def changed(
        previous: ProjectGraph,
        next: ProjectGraph,
    ): ZIO[Resolve, Nothing, Unit] =
      targetUris(previous).either.zip(targetUris(next).either).flatMap {
        case (Right(before), Right(after)) =>
          notify(
            Rpc.note(
              "buildTarget/didChange",
              Rpc.DidChange(Bsp.events(before, after)),
            )
          )
        case _ => ZIO.unit
      }

    private def read(payload: String): ZIO[Resolve, Nothing, Answer] =
      payload.fromJson[zio.json.ast.Json] match
        case Left(_) =>
          ZIO.succeed(Answer.Reply(failure(Rpc.RpcId.Absent, BspError.Parse)))
        case Right(json) =>
          json.as[Rpc.Incoming] match
            case Left(_) =>
              ZIO.succeed(
                Answer.Reply(failure(Rpc.RpcId.Absent, BspError.NotRequest))
              )
            case Right(incoming) =>
              val id = Rpc.idOf(incoming)
              incoming.method match
                case None =>
                  ZIO.succeed(Answer.Reply(failure(id, BspError.NoMethod)))
                case Some(name) =>
                  phase.get.flatMap(current => dispatch(current, id, name, incoming, incoming.id.isEmpty))

    private def dispatch(
        current: Phase,
        id: Rpc.RpcId,
        name: String,
        incoming: Rpc.Incoming,
        notification: Boolean,
    ): ZIO[Resolve, Nothing, Answer] =
      val asked = Method.from(name)
      (current, asked, notification) match
        case (_, Some(Method.Exit), _) =>
          ZIO.succeed(Answer.Stop(if current == Phase.Shutdown then 0 else 1))
        case (Phase.New, Some(Method.Initialize), false) =>
          phase.set(Phase.Ready).as(Answer.Reply(Rpc.ok(id, initialize)))
        case (Phase.New, _, true) =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.New, _, _) =>
          ZIO.succeed(Answer.Reply(failure(id, BspError.NotInitialized)))
        case (Phase.Shutdown, _, true) =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.Shutdown, _, _) =>
          ZIO.succeed(Answer.Reply(failure(id, BspError.ShutDown)))
        case (Phase.Ready, Some(Method.Initialized), _) =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.Ready, Some(Method.Shutdown), false) =>
          phase.set(Phase.Shutdown).as(Answer.Reply(Rpc.ok(id, Rpc.NullResult())))
        case (Phase.Ready, Some(Method.Targets), false) =>
          reply(id, targets)
        case (Phase.Ready, Some(Method.Sources), false) =>
          sources(id, incoming)
        case (Phase.Ready, Some(Method.ScalacOptions), false) =>
          options(id, incoming)
        case (Phase.Ready, Some(Method.DependencySources), false) =>
          dependencySources(id, incoming)
        case (Phase.Ready, Some(Method.Compile), false) =>
          compile(id, incoming)
        case (Phase.Ready, Some(Method.MainClasses), false) =>
          mainClasses(id, incoming)
        case (Phase.Ready, Some(Method.TestClasses), false) =>
          testClasses(id, incoming)
        case (Phase.Ready, Some(Method.RunEnvironment), false) =>
          runEnvironment(id, incoming)
        case (Phase.Ready, Some(Method.TestEnvironment), false) =>
          testEnvironment(id, incoming)
        case (Phase.Ready, _, true) =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.Ready, _, false) =>
          ZIO.succeed(Answer.Reply(failure(id, BspError.UnknownMethod(name))))

    private def sources(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming,
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found => ZIO.foreach(found)(sourceItem).map(Rpc.SourcesResult(_))),
      )

    private def options(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming,
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found => ZIO.foreach(found)(scalac).map(Rpc.ScalacOptionsResult(_))),
      )

    private def dependencySources(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming,
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found => ZIO.foreach(found)(sourceJars).map(Rpc.DependencySourcesResult(_))),
      )

    private def compile(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming,
    ): ZIO[Resolve, Nothing, Answer] =
      ZIO
        .fromEither(Rpc.compileParams(incoming.params).left.map(fault))
        .foldZIO(
          error => ZIO.succeed(Answer.Reply(failure(id, error))),
          params =>
            request.get.flatMap { graph =>
              val found = params.targets.map(_.uri)
              val ids   = found.flatMap(uri => identify(uri, graph))
              if ids.length != found.length then ZIO.succeed(Answer.Reply(failure(id, BspError.UnknownTarget)))
              else
                gate(runCompile(ids)).map(status =>
                  Answer.Reply(
                    Rpc.ok(id, Rpc.CompileResult(status, params.originId))
                  )
                )
            },
        )

    private def runCompile(ids: List[Id]): ZIO[Resolve, Nothing, Int] =
      val mains  = ids.collect { case id: Id.Main => id }
      val tests  = ids.collect { case id: Id.Test => id }
      val builds = ids.collect { case Id.Build => Id.Build }
      for
        mainStatus <- ZIO
          .foreach(mains.groupBy(_.platform).toList) { case (platform, group) =>
            compileMains(platform, group)
          }
          .map(worst)
        testStatus  <- ZIO.foreach(tests)(compileTest).map(worst)
        buildStatus <- ZIO.foreach(builds)(_ => compileBuild).map(worst)
      yield worst(List(mainStatus, testStatus, buildStatus))

    private def worst(codes: List[Int]): Int =
      if codes.contains(2) then 2 else 1

    private def compileMains(
        platform: Platform,
        group: List[Id.Main],
    ): ZIO[Resolve, Nothing, Int] =
      val cells = group.map(id => id.module.name -> id.version)
      request.get.flatMap { graph =>
        Ref.make(false).flatMap { failed =>
          val ran =
            ZIO.foreachDiscard(group)(id => clear(targetUri(id))) *>
              ZStream
                .unwrap(
                  readLock.map(lock =>
                    GraphCompile.cellFeed(
                      histos.zinc.CompileSession(
                        repo,
                        graph,
                        jdk,
                        runtimes.native,
                        lock,
                      ),
                      cells,
                      platform,
                    )
                  )
                )
                .mapError {
                  case error: GraphCompileError => BspError.Sources(error)
                  case error: BspError          => error
                }
                .foreach {
                  case histos.zinc.CompileFeed.Diagnostic(
                        module,
                        version,
                        chosen,
                        problem,
                      ) =>
                    publish(
                      s"${directoryUri(repo)}${module.value}?scala=${version.value}&platform=${chosen.token}&scope=main",
                      problem,
                    )
                  case histos.zinc.CompileFeed.Began(_, _, _) => ZIO.unit
                  case histos.zinc.CompileFeed.Done(_)        => ZIO.unit
                  case histos.zinc.CompileFeed.Failed(_)      => failed.set(true)
                } *> failed.get
          ran.either.flatMap {
            case Left(error)  => statusOf(Left(error))
            case Right(true)  => ZIO.succeed(2)
            case Right(false) => ZIO.succeed(1)
          }
        }
      }

    private def compileTest(id: Id.Test): ZIO[Resolve, Nothing, Int] =
      request.get.flatMap { graph =>
        val modules = modulesOf(graph)
        val uri     = targetUri(id)
        val cell    = CompiledCell(
          id.module.name,
          id.version,
          id.platform,
          GraphCompile
            .classesDir(repo, id.module.name, id.version, id.platform),
          0,
          key = None,
          changed = true,
          sources = None,
          classesWritten = None,
        )
        problems { hear =>
          withTools(
            clear(uri) *>
              (for
                lock    <- readLock
                sources <- TestCompile
                  .testSources(repo, id.module, modules, id.version, id.platform)
                  .mapError(BspError.Tests(_))
                _ <- TestCompile
                  .compiledTests(
                    repo,
                    modules,
                    id.module,
                    cell,
                    sources,
                    jdk,
                    runtimes.native,
                    lock,
                    BuildResolvers(graph.resolvers),
                    hear,
                  )
                  .mapError(BspError.Tests(_))
              yield ())
          )
        }.flatMap { (result, found) =>
          ZIO.foreachDiscard(found)(problem => publish(uri, problem)) *> statusOf(result)
        }
      }

    private def compileBuild: ZIO[Resolve, Nothing, Int] =
      val uri = buildUri
      (clear(uri) *> build.mapError(BspError.Build(_))).either.flatMap {
        case Left(error)  => log(CompileSeverity.Error, error.message).as(2)
        case Right(notes) =>
          ZIO
            .foreachDiscard(notes.problems)(problem => publish(uri, problem))
            .as(if notes.failed then 2 else 1)
      }

    private def statusOf(result: Either[BspError, Unit]): UIO[Int] =
      result match
        case Left(error) => log(CompileSeverity.Error, error.message).as(2)
        case Right(_)    => ZIO.succeed(1)

    private def uris(incoming: Rpc.Incoming): IO[BspError, List[String]] =
      ZIO.fromEither(Rpc.targets(incoming.params).left.map(fault))

    private def fault(decoded: Rpc.ParamsDecode): BspError =
      decoded match
        case Rpc.ParamsDecode.Missing  => BspError.Params(ParamFault.Missing)
        case Rpc.ParamsDecode.Rejected => BspError.Params(ParamFault.Rejected)

    private def reply[A: JsonCodec](
        id: Rpc.RpcId,
        body: ZIO[Resolve, BspError, A],
    ): ZIO[Resolve, Nothing, Answer] =
      body.fold(
        error => Answer.Reply(failure(id, error)),
        value => Answer.Reply(Rpc.ok(id, value)),
      )

    private def scalac(
        uri: String
    ): ZIO[Resolve, BspError, Rpc.ScalacOptionsItem] =
      parsed(uri) {
        case None           => ZIO.fail(BspError.UnknownTarget)
        case Some(Id.Build) =>
          ZIO.succeed(
            Rpc.ScalacOptionsItem(
              Rpc.TargetId(uri),
              SemanticDb.argv(repo, BuildScala),
              buildClasspath.map(fileUri),
              directoryUri(repo),
            )
          )
        case Some(id: Id.Main) =>
          classpath(id.module, id.version, id.platform).map: paths =>
            Rpc.ScalacOptionsItem(
              Rpc.TargetId(uri),
              id.module.flags.argv ++ SemanticDb.argv(repo, id.version),
              paths.map(fileUri),
              fileUri(
                GraphCompile.classesDir(
                  repo,
                  id.module.name,
                  id.version,
                  id.platform,
                )
              ),
            )
        case Some(id: Id.Test) =>
          testClasspath(id).map: paths =>
            Rpc.ScalacOptionsItem(
              Rpc.TargetId(uri),
              id.module.flags.argv ++ SemanticDb.argv(repo, id.version),
              paths.map(fileUri),
              fileUri(
                TestCompile.testClasses(
                  repo,
                  id.module.name,
                  id.version,
                  id.platform,
                )
              ),
            )

      }
    private def withTools[A](
        effect: ZIO[Resolve & LockedTools, BspError, A]
    ): ZIO[Resolve, BspError, A] =
      request.get.flatMap(graph =>
        readLock.flatMap(lock =>
          effect.provideSome[Resolve](
            zio.ZLayer.fromZIO(LockedTools.make(lock, graph))
          )
        )
      )

    private def classpath(
        module: Module,
        version: ScalaVersion,
        platform: Platform,
    ): ZIO[Resolve, BspError, List[Path]] =
      request.get.flatMap { graph =>
        val cell = ResolveCell(
          module.name,
          Dep.select(module.dependencies, platform),
          version,
          platform,
          BuildResolvers(graph.resolvers),
          ClasspathScope.Compile,
        )
        val upstream = module
          .upstream(platform)
          .map(dependency => GraphCompile.classesDir(repo, dependency, version, platform))
        for
          lock <- readLock
          jars <- Resolve
            .materialize(cell, lock)
            .mapBoth(BspError.Resolve(_), _.artifacts.map(_.path))
          library <- withTools(
            JvmCompile.runtime(version).mapError(BspError.Toolchain(_))
          )
        yield (library ++ dropStdlib(jars) ++ upstream).distinct
      }

    /** The `runtimeClasspath` order, naming directories that are not there yet. Import runs before compile.
      */
    private def testClasspath(id: Id.Test): ZIO[Resolve, BspError, List[Path]] =
      request.get.flatMap { graph =>
        val modules = modulesOf(graph)
        val main    =
          GraphCompile.classesDir(repo, id.module.name, id.version, id.platform)
        val upstream = Module
          .chain(id.module, modules, id.platform)
          .map(dependency =>
            GraphCompile
              .classesDir(repo, dependency.name, id.version, id.platform)
          )
        val dependencies =
          Dep.select(id.module.dependencies, id.platform) ++ Dep.select(
            id.module.testDependencies,
            id.platform,
          )
        val scope =
          if Dep.select(id.module.testDependencies, id.platform).isEmpty then ClasspathScope.Compile
          else ClasspathScope.Test
        val cell = ResolveCell(
          id.module.name,
          dependencies,
          id.version,
          id.platform,
          BuildResolvers(graph.resolvers),
          scope,
        )
        for
          lock <- readLock
          jars <- Resolve
            .materialize(cell, lock)
            .mapBoth(BspError.Resolve(_), _.artifacts.map(_.path))
          library <- withTools(
            JvmCompile.runtime(id.version).mapError(BspError.Toolchain(_))
          )
        yield main :: (upstream ++ library ++ dropStdlib(jars))
      }

    private def sourceJars(
        uri: String
    ): ZIO[Resolve, BspError, Rpc.DependencySourcesItem] =
      parsed(uri) {
        case None     => ZIO.fail(BspError.UnknownTarget)
        case Some(id) =>
          jarsOf(id)
            .flatMap(paths => ZIO.foreach(paths)(oneSource).map(_.flatten))
            .map: sources =>
              Rpc.DependencySourcesItem(Rpc.TargetId(uri), sources.map(fileUri))

      }
    private def jarsOf(id: Id): ZIO[Resolve, BspError, List[SourceAsk]] =
      id match
        case Id.Build =>
          ZIO.succeed(buildClasspath.flatMap(runtimeAsk(BuildScala, _)))
        case Id.Main(module, version, platform) =>
          lockedAsks(module, version, platform, test = false)
        case Id.Test(module, version, platform) =>
          lockedAsks(module, version, platform, test = true)

    private def lockedAsks(
        module: Module,
        version: ScalaVersion,
        platform: Platform,
        test: Boolean,
    ): ZIO[Resolve, BspError, List[SourceAsk]] =
      for
        lock    <- readLock
        library <- withTools(
          JvmCompile.runtime(version).mapError(BspError.Toolchain(_))
        )
      yield
        val scope =
          if test && Dep.select(module.testDependencies, platform).nonEmpty then ClasspathScope.Test
          else ClasspathScope.Compile
        val locked = lock.cells
          .collectFirst {
            case cell
                if cell.module == module.name && cell.scalaVersion == version && cell.platform == platform && cell.scope == scope =>
              cell.artifacts
          }
          .getOrElse(Nil)
        locked.map(ask) ++ library.flatMap(runtimeAsk(version, _))

    private def ask(artifact: LockedArtifact): SourceAsk =
      SourceAsk(
        artifact.organization,
        artifact.name,
        artifact.revision,
        artifact.repository,
      )

    private def runtimeAsk(
        version: ScalaVersion,
        path: Path,
    ): Option[SourceAsk] =
      Option(path.getFileName).map(_.toString) match
        case Some(name) if name.startsWith("scala3-library") =>
          Some(
            SourceAsk(
              "org.scala-lang",
              "scala3-library_3",
              version.value,
              Repository.Central,
            )
          )
        case Some(name) if name.startsWith("scala-library") =>
          Some(
            SourceAsk(
              "org.scala-lang",
              "scala-library",
              version.value,
              Repository.Central,
            )
          )
        case _ => None

    private def oneSource(ask: SourceAsk): ZIO[Resolve, Nothing, Option[Path]] =
      ZIO
        .serviceWithZIO[Resolve](
          _.sources(ask.organization, ask.name, ask.revision, ask.repository)
        )
        .either
        .map:
          case Right(artifact) => Some(artifact.path)
          case Left(_)         => None

    private def readLock: IO[BspError, DependencyLock] =
      captured.get.flatMap:
        case Some(lock) => ZIO.succeed(lock)
        case None       =>
          Resolve
            .readLock(repo)
            .mapError {
              case ResolveError.Io(failure) => BspError.LockRead(failure.path)
              case other                    => BspError.Lock(other)
            }
            .tap(lock => captured.set(Some(lock)))

    private def mainClasses(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming,
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found => ZIO.foreach(found)(mainClassItem).map(Rpc.ScalaMainClassesResult(_))),
      )

    private def testClasses(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming,
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found => ZIO.foreach(found)(testClassItem).map(Rpc.ScalaTestClassesResult(_))),
      )

    private def runEnvironment(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming,
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found => ZIO.foreach(found)(runItem).map(Rpc.JvmEnvironmentResult(_))),
      )

    private def testEnvironment(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming,
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found => ZIO.foreach(found)(testItem).map(Rpc.JvmEnvironmentResult(_))),
      )

    private def mainClassItem(
        uri: String
    ): ZIO[Resolve, BspError, Rpc.ScalaMainClassesItem] =
      parsed(uri) {
        case Some(id: Id.Main) =>
          val named   = id.module.application.toList.map(_.value)
          val classes = GraphCompile.classesDir(
            repo,
            id.module.name,
            id.version,
            id.platform,
          )
          EntryPoints
            .mains(classes)
            .mapError(failure => BspError.Mains(failure))
            .map: found =>
              val names = (named ++ found).distinct.sorted
              Rpc.ScalaMainClassesItem(
                Rpc.TargetId(uri),
                names.map(name => Rpc.ScalaMainClass(name, Nil, Nil)),
              )
        case Some(_) =>
          ZIO.succeed(Rpc.ScalaMainClassesItem(Rpc.TargetId(uri), Nil))
        case None => ZIO.fail(BspError.UnknownTarget)

      }
    private def testClassItem(
        uri: String
    ): ZIO[Resolve, BspError, Rpc.ScalaTestClassesItem] =
      parsed(uri) {
        case Some(id: Id.Test) =>
          suites(id).map: found =>
            Rpc.ScalaTestClassesItem(
              Rpc.TargetId(uri),
              found.names,
              found.framework,
            )
        case Some(_) =>
          ZIO.succeed(Rpc.ScalaTestClassesItem(Rpc.TargetId(uri), Nil, None))
        case None => ZIO.fail(BspError.UnknownTarget)

      }
    private def runItem(
        uri: String
    ): ZIO[Resolve, BspError, Rpc.JvmEnvironmentItem] =
      parsed(uri) {
        case Some(id: Id.Main) =>
          val classes = GraphCompile.classesDir(
            repo,
            id.module.name,
            id.version,
            id.platform,
          )
          val cell = CompiledCell(
            id.module.name,
            id.version,
            id.platform,
            classes,
            0,
            key = None,
            changed = true,
            sources = None,
            classesWritten = None,
          )
          val work =
            Program.workDir(repo, id.module.name, id.version, id.platform)
          for
            lock    <- readLock
            modules <- graphModules
            seen    <- request.get
            paths   <- withTools(
              Program
                .classpath(
                  repo,
                  modules,
                  id.module,
                  cell,
                  lock,
                  BuildResolvers(seen.resolvers),
                )
                .mapError(BspError.Run(_))
            )
            _ <- ensureDir(work)
          yield Rpc.JvmEnvironmentItem(
            Rpc.TargetId(uri),
            paths.map(fileUri),
            Nil,
            work.toAbsolutePath.normalize.toString,
            TestCompile.suiteEnv(repo, runtimes.env),
          )
        case Some(_) => ZIO.fail(BspError.NotMain)
        case None    => ZIO.fail(BspError.UnknownTarget)

      }
    private def testItem(
        uri: String
    ): ZIO[Resolve, BspError, Rpc.JvmEnvironmentItem] =
      parsed(uri) {
        case Some(id: Id.Test) =>
          val work =
            TestCompile.testWork(repo, id.module.name, id.version, id.platform)
          val classes = TestCompile.testClasses(
            repo,
            id.module.name,
            id.version,
            id.platform,
          )
          val cell = CompiledCell(
            id.module.name,
            id.version,
            id.platform,
            GraphCompile
              .classesDir(repo, id.module.name, id.version, id.platform),
            0,
            key = None,
            changed = true,
            sources = None,
            classesWritten = None,
          )
          for
            _ <- ZIO
              .fromEither(TestCompile.launchProvider(id.module))
              .mapError(BspError.Tests(_))
            lock    <- readLock
            modules <- graphModules
            seen    <- request.get
            linked  <- withTools(
              TestCompile
                .runtimeClasspath(
                  repo,
                  modules,
                  id.module,
                  cell,
                  lock,
                  BuildResolvers(seen.resolvers),
                )
                .mapError(BspError.Tests(_))
            )
            _ <- ensureDir(work)
          yield Rpc.JvmEnvironmentItem(
            Rpc.TargetId(uri),
            (classes :: linked.paths).map(fileUri),
            Nil,
            work.toAbsolutePath.normalize.toString,
            TestCompile.suiteEnv(repo, runtimes.env),
          )
        case Some(_) => ZIO.fail(BspError.NotTest)
        case None    => ZIO.fail(BspError.UnknownTarget)
      }

    private final case class Suites(
        names: List[String],
        framework: Option[String],
    )

    /** Compiled suites when the classes directory exists. An uncompiled target is an empty list, not an error.
      */
    private def suites(id: Id.Test): ZIO[Resolve, BspError, Suites] =
      val classes =
        TestCompile.testClasses(repo, id.module.name, id.version, id.platform)
      id.module.test match
        case Some(main) => ZIO.succeed(Suites(List(main.value), None))
        case None       =>
          TestCompile.launchProvider(id.module) match
            case Left(_)               => ZIO.succeed(Suites(Nil, None))
            case Right(None)           => ZIO.succeed(Suites(Nil, None))
            case Right(Some(provider)) =>
              val cell = CompiledCell(
                id.module.name,
                id.version,
                id.platform,
                GraphCompile
                  .classesDir(repo, id.module.name, id.version, id.platform),
                0,
                key = None,
                changed = true,
                sources = None,
                classesWritten = None,
              )
              readLock.flatMap { lock =>
                graphModules.flatMap { modules =>
                  request.get.flatMap { seen =>
                    withTools(
                      TestCompile
                        .runtimeClasspath(
                          repo,
                          modules,
                          id.module,
                          cell,
                          lock,
                          BuildResolvers(seen.resolvers),
                        )
                        .mapError(BspError.Tests(_))
                    ).either
                      .flatMap {
                        case Left(error)   => ZIO.fail(error)
                        case Right(linked) =>
                          SuiteNames
                            .discover(
                              classes,
                              linked.paths,
                              provider.framework.value,
                            )
                            .either
                            .map {
                              case Right(SuiteNames.Found.Suites(names)) =>
                                Suites(names, Some(frameworkName(provider)))
                              case _ =>
                                Suites(Nil, Some(frameworkName(provider)))
                            }
                      }
                  }
                }
              }

    private def frameworkName(provider: TestProvider): String =
      if provider.framework.value == TestProvider.Zio.framework.value then "ZIO Test"
      else provider.framework.value

    private def sourceItem(uri: String): IO[BspError, Rpc.SourcesItem] =
      parsed(uri) {
        case None     => ZIO.fail(BspError.UnknownTarget)
        case Some(id) =>
          filesOf(id).map: paths =>
            Rpc.SourcesItem(
              Rpc.TargetId(uri),
              paths.map(path => Rpc.SourceItem(fileUri(path), 1, false)),
            )

      }
    private def filesOf(id: Id): IO[BspError, List[Path]] =
      id match
        case Id.Build =>
          val file = repo.resolve("histos.scala")
          ZIO
            .attemptBlocking(Files.isRegularFile(file))
            .mapError(_ => BspError.Io(IoFailure(file, IoFault.Stat)))
            .map(present => if present then List(file) else Nil)
        case Id.Main(module, version, platform) =>
          graphModules.flatMap { modules =>
            GraphCompile
              .sourceBase(repo, module, modules)
              .mapError(BspError.Sources(_))
              .flatMap {
                case None       => ZIO.succeed(Nil)
                case Some(root) =>
                  histos.zinc.Sources
                    .select(root, version, platform)
                    .mapError(BspError.Toolchain(_))
              }
          }
        case Id.Test(module, version, platform) =>
          graphModules.flatMap: modules =>
            TestCompile
              .testSources(repo, module, modules, version, platform)
              .mapError(BspError.Tests(_))

    private def targets: ZIO[Resolve, BspError, Rpc.BuildTargetsResult] =
      request.get.flatMap(graph => targetList(graph))

    private def targetUris(
        graph: ProjectGraph
    ): ZIO[Resolve, BspError, List[String]] =
      targetList(graph).map(_.targets.map(_.id.uri))

    private def targetList(
        graph: ProjectGraph
    ): ZIO[Resolve, BspError, Rpc.BuildTargetsResult] =
      for
        extra <- allowed
        cells    = cellsOf(graph, extra)
        versions = (cells.map(_.version) :+ BuildScala).distinct
        result <- listed(graph, cells, versions)
      yield result

    /** JVM is always published. `js` and `native` are added only when the allow-list names them.
      */
    private def allowed: IO[BspError, Set[Platform]] =
      val path = repo.resolve(".histos").resolve("bsp-platforms")
      ZIO
        .attemptBlocking(Files.exists(path))
        .mapError(_ => BspError.Platforms(s"could not read $path"))
        .flatMap:
          case false => ZIO.succeed(Set.empty)
          case true  =>
            ZIO
              .attemptBlocking(Files.readString(path))
              .mapError(_ => BspError.Platforms(s"could not read $path"))
              .flatMap: text =>
                ZIO
                  .fromEither(parsePlatforms(text))
                  .mapError(BspError.Platforms(_))

    private def parsePlatforms(text: String): Either[String, Set[Platform]] =
      val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
      lines.foldLeft[Either[String, Set[Platform]]](Right(Set.empty)): (acc, token) =>
        acc.flatMap: have =>
          token match
            case "js"     => Right(have + Platform.Js)
            case "native" => Right(have + Platform.Native)
            case other    => Left(s"'$other' is not a bsp platform")

    private def listed(
        graph: ProjectGraph,
        cells: List[Cell],
        versions: List[ScalaVersion],
    ): ZIO[Resolve, BspError, Rpc.BuildTargetsResult] =
      for
        jars <- ZIO
          .foreach(versions)(version =>
            withTools(
              JvmCompile.scalaJars(version).mapError(BspError.Toolchain(_))
            ).map(version.value -> _)
          )
          .mapBoth(
            identity,
            _.toMap,
          )
        tests <- ZIO.foreach(cells)(cell =>
          TestCompile
            .testSources(
              repo,
              cell.module,
              modulesOf(graph),
              cell.version,
              cell.platform,
            )
            .mapBoth(
              BspError.Tests(_),
              sources => Option.when(sources.nonEmpty)(cell),
            )
        )
      yield
        val mains  = cells.map(cell => shown(graph, Id.Main(cell.module, cell.version, cell.platform), jars))
        val suites = tests.flatten.map(cell => shown(graph, Id.Test(cell.module, cell.version, cell.platform), jars))
        val file   = shown(graph, Id.Build, jars)
        Rpc.BuildTargetsResult(mains ++ suites ++ List(file))

    private def shown(
        graph: ProjectGraph,
        id: Id,
        jars: Map[String, List[Path]],
    ): Rpc.BuildTarget =
      val (name, base, tags, version, platform, dependencies, canTest, canRun) =
        id match
          case Id.Main(module, version, platform) =>
            (
              module.name.value,
              directoryUri(repo.resolve(module.baseDirectory)),
              List("library"),
              version,
              platformCode(platform),
              dependencyUris(graph, module, version, platform),
              false,
              true,
            )
          case Id.Test(module, version, platform) =>
            (
              s"${module.name.value}-test",
              directoryUri(repo.resolve(module.baseDirectory)),
              List("test"),
              version,
              platformCode(platform),
              List(targetUri(Id.Main(module, version, platform))),
              true,
              false,
            )
          case Id.Build =>
            (
              "histos.scala",
              directoryUri(repo),
              List("library"),
              BuildScala,
              1,
              Nil,
              false,
              false,
            )
      Rpc.BuildTarget(
        Rpc.TargetId(targetUri(id)),
        name,
        base,
        tags,
        List("scala"),
        dependencies.map(Rpc.TargetId(_)),
        Rpc.TargetCapabilities(
          canCompile = true,
          canTest = canTest,
          canRun = canRun,
        ),
        "scala",
        Rpc.ScalaBuildTarget(
          "org.scala-lang",
          version.value,
          binary(version),
          platform,
          jars.getOrElse(version.value, Nil).map(fileUri),
        ),
      )

    private def cellsOf(graph: ProjectGraph, extra: Set[Platform]): List[Cell] =
      val wanted = Set[Platform](Platform.Jvm) ++ extra
      val order  = List(Platform.Jvm, Platform.Js, Platform.Native)
      graph.projects
        .collect { case module: Module => module }
        .flatMap: module =>
          module.versions.all.flatMap: version =>
            order
              .filter(platform =>
                wanted.contains(platform) && module.platforms.all
                  .contains(platform)
              )
              .map: platform =>
                Cell(module, version, platform)
        .sortBy(cell => (cell.module.name.value, cell.version.value, cell.platform.token))

    private def dependencyUris(
        graph: ProjectGraph,
        module: Module,
        version: ScalaVersion,
        platform: Platform,
    ): List[String] =
      module
        .upstream(platform)
        .flatMap: name =>
          graph.get(name) match
            case Some(dependency: Module)
                if dependency.platforms.all.contains(
                  platform
                ) && dependency.versions.all.exists(_.value == version.value) =>
              Some(targetUri(Id.Main(dependency, version, platform)))
            case _ => None

    private def parsed[R, E, A](uri: String)(
        use: Option[Id] => ZIO[R, E, A]
    ): ZIO[R, E, A] =
      request.get.flatMap(graph => use(identify(uri, graph)))

    private def identify(uri: String, graph: ProjectGraph): Option[Id] =
      if uri == buildUri then Some(Id.Build)
      else
        val prefix = directoryUri(repo)
        if !uri.startsWith(prefix) then None
        else
          val rest  = uri.drop(prefix.length)
          val name  = rest.takeWhile(_ != '?')
          val query = rest
            .drop(name.length)
            .drop(1)
            .split("&")
            .toList
            .flatMap: part =>
              part.split("=", 2) match
                case Array(key, value) => Some(key -> value)
                case _                 => None
            .toMap
          val found = for
            version <- query
              .get("scala")
              .flatMap(raw => ScalaVersion.from(raw).toOption)
            platform <- query
              .get("platform")
              .flatMap(raw => Platform.from(raw).toOption)
            scope  <- query.get("scope")
            module <- modulesOf(graph).find(_.name.value == name)
          yield (module, version, platform, scope)
          found match
            case Some(module, version, platform, "main") =>
              Some(Id.Main(module, version, platform))
            case Some(module, version, platform, "test") =>
              Some(Id.Test(module, version, platform))
            case _ => None

    private def modulesOf(graph: ProjectGraph): List[Module] =
      graph.projects.collect { case module: Module => module }

    private def graphModules: UIO[List[Module]] =
      request.get.map(modulesOf)

    private def targetUri(id: Id): String =
      id match
        case Id.Main(module, version, platform) =>
          s"${directoryUri(repo)}${module.name.value}?scala=${version.value}&platform=${platform.token}&scope=main"
        case Id.Test(module, version, platform) =>
          s"${directoryUri(repo)}${module.name.value}?scala=${version.value}&platform=${platform.token}&scope=test"
        case Id.Build => buildUri

    private def buildUri: String =
      s"${directoryUri(repo)}?build=histos.scala"

    private def initialize: Rpc.InitializeResult =
      Rpc.InitializeResult(
        ServerName,
        ServerVersion,
        BspVersion,
        Rpc.Capabilities(
          Rpc.CompileProvider(List("scala")),
          dependencySourcesProvider = true,
          jvmRunEnvironmentProvider = true,
          jvmTestEnvironmentProvider = true,
          buildTargetChangedProvider = true,
        ),
      )

    /** Metals rejects a target id as a text document. Reset each source file.
      */
    private def clear(target: String): IO[BspError, Unit] =
      parsed(target) {
        case None     => ZIO.unit
        case Some(id) =>
          filesOf(id).flatMap: files =>
            ZIO.foreachDiscard(files): file =>
              notify(
                Rpc.note(
                  "build/publishDiagnostics",
                  Rpc.PublishDiagnostics(
                    Rpc.TextDocument(fileUri(file)),
                    Rpc.TargetId(target),
                    Nil,
                    true,
                  ),
                )
              )

      }

    /** The compiler library is `JvmCompile.runtime`. A locked copy of either standard library is a second version.
      */
    private def dropStdlib(jars: List[Path]): List[Path] =
      jars.filterNot(JvmCompile.isStandardLibrary)

    private def publish(target: String, problem: CompileProblem): UIO[Unit] =
      Ranges.of(problem) match
        case None        => log(problem.severity, problem.message)
        case Some(range) =>
          val file = problem.path.map(fileUri).getOrElse(target)
          notify(
            Rpc.note(
              "build/publishDiagnostics",
              Rpc.PublishDiagnostics(
                Rpc.TextDocument(file),
                Rpc.TargetId(target),
                List(
                  Rpc.Diagnostic(
                    range,
                    severityCode(problem.severity),
                    ServerName,
                    problem.message,
                  )
                ),
                false,
              ),
            )
          )

    private def log(severity: CompileSeverity, message: String): UIO[Unit] =
      notify(
        Rpc.note(
          "build/logMessage",
          Rpc.LogMessage(severityCode(severity), message),
        )
      )

    private def severityCode(severity: CompileSeverity): Int =
      severity match
        case CompileSeverity.Error => 1
        case CompileSeverity.Warn  => 2
        case CompileSeverity.Info  => 3

    private def failure(id: Rpc.RpcId, error: BspError): String =
      Rpc.fail(id, error.code, error.message)
  end Session

  private enum Id:
    case Main(module: Module, version: ScalaVersion, platform: Platform)
    case Test(module: Module, version: ScalaVersion, platform: Platform)
    case Build

  private final case class Cell(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
  )

  private final case class SourceAsk(
      organization: String,
      name: String,
      revision: String,
      repository: Repository,
  )

  private object Ranges:
    def of(problem: CompileProblem): Option[Rpc.Range] =
      (problem.path, problem.startLine, problem.startColumn) match
        case (Some(_), Some(line), Some(column)) =>
          val shift = problem.base match
            case PositionBase.Zinc  => 1
            case PositionBase.Dotty => 0
          val startLine   = math.max(0, line - shift)
          val startColumn = math.max(0, column - shift)
          val endLine0    =
            problem.endLine.fold(startLine)(value => math.max(0, value - shift))
          val endColumn0 = problem.endColumn.fold(startColumn)(value => math.max(0, value - shift))
          val samePoint  = endLine0 == startLine && endColumn0 <= startColumn
          val endColumn  = if samePoint then startColumn + 1 else endColumn0
          Some(
            Rpc.Range(
              Rpc.Position(startLine, startColumn),
              Rpc.Position(endLine0, endColumn),
            )
          )
        case _ => None

  /** Created is 1, changed is 2, deleted is 3. An id in both lists is changed.
    */
  def events(
      before: List[String],
      after: List[String],
  ): List[Rpc.BuildTargetEvent] =
    val previous = before.toSet
    val next     = after.toSet
    val created  = (next -- previous).toList.sorted.map(event(_, 1))
    val changed  = previous.intersect(next).toList.sorted.map(event(_, 2))
    val deleted  = (previous -- next).toList.sorted.map(event(_, 3))
    created ++ changed ++ deleted

  private def event(uri: String, kind: Int): Rpc.BuildTargetEvent =
    Rpc.BuildTargetEvent(Rpc.TargetId(uri), kind)

  private def fileUri(path: Path): String =
    path.toAbsolutePath.normalize.toUri.toString

  private def directoryUri(path: Path): String =
    val text = fileUri(path)
    if text.endsWith("/") then text else s"$text/"

  private def binary(version: ScalaVersion): String =
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala3)        => "3"
      case Some(ScalaSeries.Scala2(minor)) => s"2.$minor"
      case None                            => version.value

  private def platformCode(platform: Platform): Int =
    platform match
      case Platform.Jvm    => 1
      case Platform.Js     => 2
      case Platform.Native => 3
end Bsp
