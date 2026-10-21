package histos.bsp

import histos.load.{BuildNotes, LoadError}
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  DependencyLock,
  LockedArtifact,
  Repository,
  Resolve,
  ResolveCell,
  ResolveError
}
import histos.program.{EntryPoints, Program, ProgramError}
import histos.testrun.{ModuleTest, SuiteNames}
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
  ResolvedJdk,
  ScalaSeries,
  SemanticDb
}
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.json.*
import zio.{IO, Ref, UIO, ZIO}

/** BSP 2.2 for Metals. JVM targets, a build-file target, and diagnostics. Test
  * running stays out.
  */
object Bsp:
  val BspVersion: String = "2.2.0"
  val ServerName: String = "histos"
  val ServerVersion: String = "0.0.1-SNAPSHOT"
  private val BuildScala: ScalaVersion = ScalaVersion.trusted("3.10.0")

  enum Answer:
    case Reply(line: String)
    case Quiet
    case Stop(code: Int)

  private enum Phase:
    case New, Ready, Shutdown

  private enum ParamFault:
    case Missing, Rejected

    def message: String = this match
      case Missing  => "bsp params are required"
      case Rejected => "bsp params were rejected"

  private enum BspError:
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

    def code: Int = this match
      case Parse                                         => -32700
      case NotRequest | NoMethod | ShutDown              => -32600
      case UnknownMethod(_)                              => -32601
      case Params(_) | UnknownTarget | NotTest | NotMain => -32602
      case NotInitialized                                => -32002
      case Lock(_) | LockRead(_) | Resolve(_) | Toolchain(_) | Sources(_) |
          Tests(_) | Run(_) | Build(_) | Platforms(_) =>
        -32603

    def message: String = this match
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

  def open(
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      buildClasspath: List[Path] = Nil,
      notify: String => UIO[Unit] = _ => ZIO.unit,
      build: IO[LoadError, BuildNotes] = ZIO.succeed(BuildNotes(false, Nil)),
      gate: ZIO[Resolve, Nothing, Int] => ZIO[Resolve, Nothing, Int] = effect =>
        effect,
      shared: Option[Ref[ProjectGraph]] = None
  ): UIO[Session] =
    for
      phase <- Ref.make[Phase](Phase.New)
      current <- shared match
        case Some(ref) => ZIO.succeed(ref)
        case None      => Ref.make(graph)
      request <- Ref.make(graph)
    yield Session.start(
      repo,
      jdk,
      buildClasspath,
      notify,
      build,
      phase,
      gate,
      current,
      request
    )

  object Session:
    private[Bsp] def start(
        repo: Path,
        jdk: ResolvedJdk,
        buildClasspath: List[Path],
        notify: String => UIO[Unit],
        build: IO[LoadError, BuildNotes],
        phase: Ref[Phase],
        gate: ZIO[Resolve, Nothing, Int] => ZIO[Resolve, Nothing, Int],
        current: Ref[ProjectGraph],
        request: Ref[ProjectGraph]
    ): Session =
      new Session(
        repo,
        jdk,
        buildClasspath,
        notify,
        build,
        phase,
        gate,
        current,
        request
      )

  final class Session private (
      repo: Path,
      jdk: ResolvedJdk,
      buildClasspath: List[Path],
      notify: String => UIO[Unit],
      build: IO[LoadError, BuildNotes],
      phase: Ref[Phase],
      gate: ZIO[Resolve, Nothing, Int] => ZIO[Resolve, Nothing, Int],
      current: Ref[ProjectGraph],
      request: Ref[ProjectGraph]
  ):
    /** One request sees the graph it started with. A later swap does not
      * rewrite it.
      */
    def answer(payload: String): ZIO[Resolve, Nothing, Answer] =
      current.get.flatMap(graph => answer(graph, payload))

    def answer(
        seen: ProjectGraph,
        payload: String
    ): ZIO[Resolve, Nothing, Answer] =
      request.set(seen) *> read(payload)

    /** Kinds are BSP's created (1), changed (2), and deleted (3). */
    def changed(
        previous: ProjectGraph,
        next: ProjectGraph
    ): ZIO[Resolve, Nothing, Unit] =
      targetUris(previous).either.zip(targetUris(next).either).flatMap {
        case (Right(before), Right(after)) =>
          notify(
            Rpc.note(
              "buildTarget/didChange",
              Rpc.DidChange(Bsp.events(before, after))
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
                  phase.get.flatMap(current =>
                    dispatch(current, id, name, incoming, incoming.id.isEmpty)
                  )

    private def dispatch(
        current: Phase,
        id: Rpc.RpcId,
        name: String,
        incoming: Rpc.Incoming,
        notification: Boolean
    ): ZIO[Resolve, Nothing, Answer] =
      (current, name) match
        case (_, "build/exit") =>
          ZIO.succeed(Answer.Stop(if current == Phase.Shutdown then 0 else 1))
        case (Phase.New, "build/initialize") if !notification =>
          phase.set(Phase.Ready).as(Answer.Reply(Rpc.ok(id, initialize)))
        case (Phase.New, _) if notification =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.New, _) =>
          ZIO.succeed(Answer.Reply(failure(id, BspError.NotInitialized)))
        case (Phase.Shutdown, _) if notification =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.Shutdown, _) =>
          ZIO.succeed(Answer.Reply(failure(id, BspError.ShutDown)))
        case (Phase.Ready, "build/initialized") =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.Ready, "build/shutdown") if !notification =>
          phase
            .set(Phase.Shutdown)
            .as(Answer.Reply(Rpc.ok(id, Rpc.NullResult())))
        case (Phase.Ready, "workspace/buildTargets") if !notification =>
          reply(id, targets)
        case (Phase.Ready, "buildTarget/sources") if !notification =>
          sources(id, incoming)
        case (Phase.Ready, "buildTarget/scalacOptions") if !notification =>
          options(id, incoming)
        case (Phase.Ready, "buildTarget/dependencySources") if !notification =>
          dependencySources(id, incoming)
        case (Phase.Ready, "buildTarget/compile") if !notification =>
          compile(id, incoming)
        case (Phase.Ready, "buildTarget/scalaMainClasses") if !notification =>
          mainClasses(id, incoming)
        case (Phase.Ready, "buildTarget/scalaTestClasses") if !notification =>
          testClasses(id, incoming)
        case (Phase.Ready, "buildTarget/jvmRunEnvironment") if !notification =>
          runEnvironment(id, incoming)
        case (Phase.Ready, "buildTarget/jvmTestEnvironment") if !notification =>
          testEnvironment(id, incoming)
        case (Phase.Ready, _) if notification =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.Ready, other) =>
          ZIO.succeed(Answer.Reply(failure(id, BspError.UnknownMethod(other))))

    private def sources(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found =>
          ZIO.foreach(found)(sourceItem).map(Rpc.SourcesResult(_))
        )
      )

    private def options(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found =>
          ZIO.foreach(found)(scalac).map(Rpc.ScalacOptionsResult(_))
        )
      )

    private def dependencySources(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found =>
          ZIO.foreach(found)(sourceJars).map(Rpc.DependencySourcesResult(_))
        )
      )

    private def compile(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming
    ): ZIO[Resolve, Nothing, Answer] =
      ZIO
        .fromEither(Rpc.compileParams(incoming.params).left.map(fault))
        .foldZIO(
          error => ZIO.succeed(Answer.Reply(failure(id, error))),
          params =>
            request.get.flatMap { graph =>
              val found = params.targets.map(_.uri)
              val ids = found.flatMap(uri => identify(uri, graph))
              if ids.length != found.length then
                ZIO.succeed(Answer.Reply(failure(id, BspError.UnknownTarget)))
              else
                gate(runCompile(ids)).map(status =>
                  Answer.Reply(
                    Rpc.ok(id, Rpc.CompileResult(status, params.originId))
                  )
                )
            }
        )

    private def runCompile(ids: List[Id]): ZIO[Resolve, Nothing, Int] =
      val mains = ids.collect { case id: Id.Main => id }
      val tests = ids.collect { case id: Id.Test => id }
      val builds = ids.collect { case Id.Build => Id.Build }
      for
        mainStatus <- ZIO
          .foreach(mains.groupBy(_.platform).toList) { case (platform, group) =>
            compileMains(platform, group)
          }
          .map(worst)
        testStatus <- ZIO.foreach(tests)(compileTest).map(worst)
        buildStatus <- ZIO.foreach(builds)(_ => compileBuild).map(worst)
      yield worst(List(mainStatus, testStatus, buildStatus))

    private def worst(codes: List[Int]): Int =
      if codes.contains(2) then 2 else 1

    private def compileMains(
        platform: Platform,
        group: List[Id.Main]
    ): ZIO[Resolve, Nothing, Int] =
      val cells = group.map(id => id.module.name -> id.version)
      request.get.flatMap { graph =>
        Ref.make(false).flatMap { failed =>
          val ran =
            ZIO.foreachDiscard(group)(id => clear(targetUri(id))) *>
              GraphCompile
                .cellFeed(repo, graph, cells, platform, jdk)
                .mapError(BspError.Sources(_))
                .foreach {
                  case histos.zinc.CompileFeed.Diagnostic(
                        module,
                        version,
                        chosen,
                        problem
                      ) =>
                    publish(
                      s"${directoryUri(repo)}${module.value}?scala=${version.value}&platform=${chosen.token}&scope=main",
                      problem
                    )
                  case histos.zinc.CompileFeed.Began(_, _, _) => ZIO.unit
                  case histos.zinc.CompileFeed.Done(_)        => ZIO.unit
                  case histos.zinc.CompileFeed.Failed(_) => failed.set(true)
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
        val uri = targetUri(id)
        val heard =
          new java.util.concurrent.ConcurrentLinkedQueue[CompileProblem]()
        val cell = CompiledCell(
          id.module.name,
          id.version,
          id.platform,
          GraphCompile
            .classesDir(repo, id.module.name, id.version, id.platform),
          0
        )
        val ran =
          clear(uri) *>
            (for
              lock <- readLock
              sources <- ModuleTest
                .testSources(repo, id.module, modules, id.version, id.platform)
                .mapError(BspError.Tests(_))
              _ <- ModuleTest
                .compiledTests(
                  repo,
                  modules,
                  id.module,
                  cell,
                  sources,
                  jdk,
                  lock,
                  BuildResolvers(graph.resolvers),
                  problem => heard.add(problem)
                )
                .mapError(BspError.Tests(_))
            yield ())
        ran.either.flatMap { result =>
          ZIO.foreachDiscard(heard.asScala.toList)(problem =>
            publish(uri, problem)
          ) *> statusOf(result)
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
        body: ZIO[Resolve, BspError, A]
    ): ZIO[Resolve, Nothing, Answer] =
      body.fold(
        error => Answer.Reply(failure(id, error)),
        value => Answer.Reply(Rpc.ok(id, value))
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
              directoryUri(repo)
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
                  id.platform
                )
              )
            )
        case Some(id: Id.Test) =>
          testClasspath(id).map: paths =>
            Rpc.ScalacOptionsItem(
              Rpc.TargetId(uri),
              id.module.flags.argv ++ SemanticDb.argv(repo, id.version),
              paths.map(fileUri),
              fileUri(
                ModuleTest.testClasses(
                  repo,
                  id.module.name,
                  id.version,
                  id.platform
                )
              )
            )

      }
    private def classpath(
        module: Module,
        version: ScalaVersion,
        platform: Platform
    ): ZIO[Resolve, BspError, List[Path]] =
      request.get.flatMap { graph =>
        val cell = ResolveCell(
          module.name,
          Dep.select(module.dependencies, platform),
          version,
          platform,
          BuildResolvers(graph.resolvers),
          ClasspathScope.Compile
        )
        val upstream = module
          .upstream(platform)
          .map(dependency =>
            GraphCompile.classesDir(repo, dependency, version, platform)
          )
        for
          lock <- readLock
          jars <- Resolve
            .materialize(cell, lock)
            .mapBoth(BspError.Resolve(_), _.artifacts.map(_.path))
          library <- JvmCompile.runtime(version).mapError(BspError.Toolchain(_))
        yield (library ++ dropStdlib(jars) ++ upstream).distinct
      }

    /** The `runtimeClasspath` order, naming directories that are not there yet.
      * Import runs before compile.
      */
    private def testClasspath(id: Id.Test): ZIO[Resolve, BspError, List[Path]] =
      request.get.flatMap { graph =>
        val modules = modulesOf(graph)
        val main =
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
            id.platform
          )
        val scope =
          if Dep.select(id.module.testDependencies, id.platform).isEmpty then
            ClasspathScope.Compile
          else ClasspathScope.Test
        val cell = ResolveCell(
          id.module.name,
          dependencies,
          id.version,
          id.platform,
          BuildResolvers(graph.resolvers),
          scope
        )
        for
          lock <- readLock
          jars <- Resolve
            .materialize(cell, lock)
            .mapBoth(BspError.Resolve(_), _.artifacts.map(_.path))
          library <- JvmCompile
            .runtime(id.version)
            .mapError(BspError.Toolchain(_))
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
        test: Boolean
    ): ZIO[Resolve, BspError, List[SourceAsk]] =
      for
        lock <- readLock
        library <- JvmCompile.runtime(version).mapError(BspError.Toolchain(_))
      yield
        val scope =
          if test && Dep.select(module.testDependencies, platform).nonEmpty then
            ClasspathScope.Test
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
        artifact.repository
      )

    private def runtimeAsk(
        version: ScalaVersion,
        path: Path
    ): Option[SourceAsk] =
      Option(path.getFileName).map(_.toString) match
        case Some(name) if name.startsWith("scala3-library") =>
          Some(
            SourceAsk(
              "org.scala-lang",
              "scala3-library_3",
              version.value,
              Repository.Central
            )
          )
        case Some(name) if name.startsWith("scala-library") =>
          Some(
            SourceAsk(
              "org.scala-lang",
              "scala-library",
              version.value,
              Repository.Central
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
      val path = repo.resolve(".histos").resolve("dependencies.lock")
      ZIO
        .attemptBlocking {
          if Files.isRegularFile(path) then Right(Files.readString(path))
          else Left(BspError.Lock(ResolveError.NoLock(path)))
        }
        .mapError(_ => BspError.LockRead(path))
        .flatMap(ZIO.fromEither)
        .flatMap: text =>
          ZIO.fromEither(
            DependencyLock
              .parse(text)
              .left
              .map(error => BspError.Lock(ResolveError.Lock(error)))
          )

    private def mainClasses(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found =>
          ZIO.foreach(found)(mainClassItem).map(Rpc.ScalaMainClassesResult(_))
        )
      )

    private def testClasses(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found =>
          ZIO.foreach(found)(testClassItem).map(Rpc.ScalaTestClassesResult(_))
        )
      )

    private def runEnvironment(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found =>
          ZIO.foreach(found)(runItem).map(Rpc.JvmEnvironmentResult(_))
        )
      )

    private def testEnvironment(
        id: Rpc.RpcId,
        incoming: Rpc.Incoming
    ): ZIO[Resolve, Nothing, Answer] =
      reply(
        id,
        uris(incoming).flatMap(found =>
          ZIO.foreach(found)(testItem).map(Rpc.JvmEnvironmentResult(_))
        )
      )

    private def mainClassItem(
        uri: String
    ): ZIO[Resolve, BspError, Rpc.ScalaMainClassesItem] =
      parsed(uri) {
        case Some(id: Id.Main) =>
          val named = id.module.application.toList.map(_.value)
          val classes = GraphCompile.classesDir(
            repo,
            id.module.name,
            id.version,
            id.platform
          )
          EntryPoints
            .mains(classes)
            .map: found =>
              val names = (named ++ found).distinct.sorted
              Rpc.ScalaMainClassesItem(
                Rpc.TargetId(uri),
                names.map(name => Rpc.ScalaMainClass(name, Nil, Nil))
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
              found.framework
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
            id.platform
          )
          val cell =
            CompiledCell(id.module.name, id.version, id.platform, classes, 0)
          val work =
            Program.workDir(repo, id.module.name, id.version, id.platform)
          for
            lock <- readLock
            modules <- graphModules
            seen <- request.get
            paths <- Program
              .classpath(
                repo,
                modules,
                id.module,
                cell,
                lock,
                BuildResolvers(seen.resolvers)
              )
              .mapError(BspError.Run(_))
            _ <- ZIO.attemptBlocking(Files.createDirectories(work)).orDie
          yield Rpc.JvmEnvironmentItem(
            Rpc.TargetId(uri),
            paths.map(fileUri),
            Nil,
            work.toAbsolutePath.normalize.toString,
            ModuleTest.suiteEnv(repo)
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
            ModuleTest.testWork(repo, id.module.name, id.version, id.platform)
          val classes = ModuleTest.testClasses(
            repo,
            id.module.name,
            id.version,
            id.platform
          )
          val cell = CompiledCell(
            id.module.name,
            id.version,
            id.platform,
            GraphCompile
              .classesDir(repo, id.module.name, id.version, id.platform),
            0
          )
          for
            _ <- ZIO
              .fromEither(ModuleTest.launchProvider(id.module))
              .mapError(BspError.Tests(_))
            lock <- readLock
            modules <- graphModules
            seen <- request.get
            linked <- ModuleTest
              .runtimeClasspath(
                repo,
                modules,
                id.module,
                cell,
                lock,
                BuildResolvers(seen.resolvers)
              )
              .mapError(BspError.Tests(_))
            _ <- ZIO.attemptBlocking(Files.createDirectories(work)).orDie
          yield Rpc.JvmEnvironmentItem(
            Rpc.TargetId(uri),
            (classes :: linked.paths).map(fileUri),
            Nil,
            work.toAbsolutePath.normalize.toString,
            ModuleTest.suiteEnv(repo)
          )
        case Some(_) => ZIO.fail(BspError.NotTest)
        case None    => ZIO.fail(BspError.UnknownTarget)
      }

    private final case class Suites(
        names: List[String],
        framework: Option[String]
    )

    /** Compiled suites when the classes directory exists. An uncompiled target
      * is an empty list, not an error.
      */
    private def suites(id: Id.Test): ZIO[Resolve, BspError, Suites] =
      val classes =
        ModuleTest.testClasses(repo, id.module.name, id.version, id.platform)
      id.module.test match
        case Some(main) => ZIO.succeed(Suites(List(main.value), None))
        case None       =>
          ModuleTest.launchProvider(id.module) match
            case Left(_)               => ZIO.succeed(Suites(Nil, None))
            case Right(None)           => ZIO.succeed(Suites(Nil, None))
            case Right(Some(provider)) =>
              val cell = CompiledCell(
                id.module.name,
                id.version,
                id.platform,
                GraphCompile
                  .classesDir(repo, id.module.name, id.version, id.platform),
                0
              )
              readLock.flatMap { lock =>
                graphModules.flatMap { modules =>
                  request.get.flatMap { seen =>
                    ModuleTest
                      .runtimeClasspath(
                        repo,
                        modules,
                        id.module,
                        cell,
                        lock,
                        BuildResolvers(seen.resolvers)
                      )
                      .either
                      .flatMap {
                        case Left(_)       => ZIO.succeed(Suites(Nil, None))
                        case Right(linked) =>
                          SuiteNames
                            .discover(
                              classes,
                              linked.paths,
                              provider.framework.value
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
      if provider.framework.value == TestProvider.Zio.framework.value then
        "ZIO Test"
      else provider.framework.value

    private def sourceItem(uri: String): IO[BspError, Rpc.SourcesItem] =
      parsed(uri) {
        case None     => ZIO.fail(BspError.UnknownTarget)
        case Some(id) =>
          filesOf(id).map: paths =>
            Rpc.SourcesItem(
              Rpc.TargetId(uri),
              paths.map(path => Rpc.SourceItem(fileUri(path), 1, false))
            )

      }
    private def filesOf(id: Id): IO[BspError, List[Path]] =
      id match
        case Id.Build =>
          val file = repo.resolve("histos.scala")
          ZIO
            .attemptBlocking(Files.isRegularFile(file))
            .orDie
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
            ModuleTest
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
        cells = cellsOf(graph, extra)
        versions = (cells.map(_.version) :+ BuildScala).distinct
        result <- listed(graph, cells, versions)
      yield result

    /** JVM is always published. `js` and `native` are added only when the
      * allow-list names them.
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
      lines.foldLeft[Either[String, Set[Platform]]](Right(Set.empty)):
        (acc, token) =>
          acc.flatMap: have =>
            token match
              case "js"     => Right(have + Platform.Js)
              case "native" => Right(have + Platform.Native)
              case other    => Left(s"'$other' is not a bsp platform")

    private def listed(
        graph: ProjectGraph,
        cells: List[Cell],
        versions: List[ScalaVersion]
    ): ZIO[Resolve, BspError, Rpc.BuildTargetsResult] =
      for
        jars <- ZIO
          .foreach(versions)(version =>
            JvmCompile.scalaJars(version).map(version.value -> _)
          )
          .mapBoth(
            BspError.Toolchain(_),
            _.toMap
          )
        tests <- ZIO.foreach(cells)(cell =>
          ModuleTest
            .testSources(
              repo,
              cell.module,
              modulesOf(graph),
              cell.version,
              cell.platform
            )
            .mapBoth(
              BspError.Tests(_),
              sources => Option.when(sources.nonEmpty)(cell)
            )
        )
      yield
        val mains = cells.map(cell =>
          shown(graph, Id.Main(cell.module, cell.version, cell.platform), jars)
        )
        val suites = tests.flatten.map(cell =>
          shown(graph, Id.Test(cell.module, cell.version, cell.platform), jars)
        )
        val file = shown(graph, Id.Build, jars)
        Rpc.BuildTargetsResult(mains ++ suites ++ List(file))

    private def shown(
        graph: ProjectGraph,
        id: Id,
        jars: Map[String, List[Path]]
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
              true
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
              false
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
              false
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
          canRun = canRun
        ),
        "scala",
        Rpc.ScalaBuildTarget(
          "org.scala-lang",
          version.value,
          binary(version),
          platform,
          jars.getOrElse(version.value, Nil).map(fileUri)
        )
      )

    private def cellsOf(graph: ProjectGraph, extra: Set[Platform]): List[Cell] =
      val wanted = Set[Platform](Platform.Jvm) ++ extra
      val order = List(Platform.Jvm, Platform.Js, Platform.Native)
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
        .sortBy(cell =>
          (cell.module.name.value, cell.version.value, cell.platform.token)
        )

    private def dependencyUris(
        graph: ProjectGraph,
        module: Module,
        version: ScalaVersion,
        platform: Platform
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
          val rest = uri.drop(prefix.length)
          val name = rest.takeWhile(_ != '?')
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
            scope <- query.get("scope")
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
          buildTargetChangedProvider = true
        )
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
                    true
                  )
                )
              )

      }

    /** The compiler library is `JvmCompile.runtime`. A locked copy of either
      * standard library is a second version.
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
                    "histos",
                    problem.message
                  )
                ),
                false
              )
            )
          )

    private def log(severity: CompileSeverity, message: String): UIO[Unit] =
      notify(
        Rpc.note(
          "build/logMessage",
          Rpc.LogMessage(severityCode(severity), message)
        )
      )

    private def severityCode(severity: CompileSeverity): Int =
      severity match
        case CompileSeverity.Error => 1
        case CompileSeverity.Warn  => 2
        case CompileSeverity.Info  => 3

    private def failure(id: Rpc.RpcId, error: BspError): String =
      Rpc.fail(id, error.code, error.message)

  private enum Id:
    case Main(module: Module, version: ScalaVersion, platform: Platform)
    case Test(module: Module, version: ScalaVersion, platform: Platform)
    case Build

  private final case class Cell(
      module: Module,
      version: ScalaVersion,
      platform: Platform
  )

  private final case class SourceAsk(
      organization: String,
      name: String,
      revision: String,
      repository: Repository
  )

  private object Ranges:
    def of(problem: CompileProblem): Option[Rpc.Range] =
      (problem.path, problem.startLine, problem.startColumn) match
        case (Some(_), Some(line), Some(column)) =>
          val shift = problem.base match
            case PositionBase.Zinc  => 1
            case PositionBase.Dotty => 0
          val startLine = math.max(0, line - shift)
          val startColumn = math.max(0, column - shift)
          val endLine0 =
            problem.endLine.fold(startLine)(value => math.max(0, value - shift))
          val endColumn0 = problem.endColumn.fold(startColumn)(value =>
            math.max(0, value - shift)
          )
          val samePoint = endLine0 == startLine && endColumn0 <= startColumn
          val endColumn = if samePoint then startColumn + 1 else endColumn0
          Some(
            Rpc.Range(
              Rpc.Position(startLine, startColumn),
              Rpc.Position(endLine0, endColumn)
            )
          )
        case _ => None

  /** Created is 1, changed is 2, deleted is 3. An id in both lists is changed.
    */
  def events(
      before: List[String],
      after: List[String]
  ): List[Rpc.BuildTargetEvent] =
    val previous = before.toSet
    val next = after.toSet
    val created = (next -- previous).toList.sorted.map(event(_, 1))
    val changed = previous.intersect(next).toList.sorted.map(event(_, 2))
    val deleted = (previous -- next).toList.sorted.map(event(_, 3))
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
