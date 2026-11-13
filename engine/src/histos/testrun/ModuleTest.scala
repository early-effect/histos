package histos.testrun

import histos.jdk.JdkRuntime
import histos.resolve.LockedTools
import histos.jdk.Sha256
import histos.js.{JsLink, JsLinkError, JsLinkRequest, JsSuite, JsSuiteRequest, ScalaJs}
import histos.native.{
  NativeCompile,
  NativeLink,
  NativeLinkRequest,
  NativeOrigin,
  NativeSuite,
  NativeSuiteRequest,
  NativeToolchain,
}
import histos.node.{NodeBinary, NodeMiss}
import histos.toolchain.Runtimes
import histos.resolve.{BuildResolvers, ClasspathScope, DependencyLock, Repositories, Resolve, ResolveCell, ResolveError}
import histos.schedule.{CacheConfig, Parallelism, RemoteCache}
import histos.select.SelectError
import histos.process.{ChildClasspath, ChildError, ChildEvent, ChildProcess, ChildRequest}
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileProblem,
  CompileRequest,
  CompileToolchain,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  JvmCompile,

  ScalacFlags,
  SemanticDb,
  Sources,
}
import histos.pack.{MainClass, MainClassError}
import histos.{ClangPin, Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion, TestMain, TestProvider}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Fiber, IO, Promise, Queue, Ref, Semaphore, UIO, ZIO, ZLayer}
import zio.stream.{Take, ZStream}

object ModuleTest:
  /** Compile each requested module, then run its test provider once per Scala version and platform. `TestProvider.Zio`
    * is the provider when the module names neither a provider nor a main. Upstream modules are compiled and not run.
    * The engine does not load the provider.
    */
  def run(asked: TestRun): ZIO[Resolve & histos.cache.ContentStore, ModuleTestError, List[TestReport]] =
    console(asked)
      .runFold(Reports(Map.empty, Chunk.empty))(report)
      .map(_.done.toList)

  private final case class Reports(
      heard: Map[(ProjectName, ScalaVersion, Platform), Transcript],
      done: Chunk[TestReport],
  )

  /** Lines stay with the cell that wrote them. Two cells may be on the stream at once.
    */
  private def report(have: Reports, feed: TestFeed): Reports =
    val key = cellKey(feed)
    feed match
      case TestFeed.Begin(_, _, _) =>
        have.copy(heard = have.heard.updated(key, Transcript.empty))
      case TestFeed.Out(_, _, _, text) =>
        val prior = have.heard.getOrElse(key, Transcript.empty)
        have.copy(heard = have.heard.updated(key, prior.withOut(text)))
      case TestFeed.Err(_, _, _, text) =>
        val prior = have.heard.getOrElse(key, Transcript.empty)
        have.copy(heard = have.heard.updated(key, prior.withErr(text)))
      case TestFeed.Done(cell) =>
        val heard = have.heard.getOrElse(key, Transcript.empty)
        Reports(have.heard.removed(key), have.done :+ TestReport(cell, heard))
      case TestFeed.Cached(_, _, _, _) => have

  private def cellKey(feed: TestFeed): (ProjectName, ScalaVersion, Platform) =
    feed match
      case TestFeed.Begin(module, version, platform)     => (module, version, platform)
      case TestFeed.Out(module, version, platform, _)    => (module, version, platform)
      case TestFeed.Err(module, version, platform, _)    => (module, version, platform)
      case TestFeed.Done(cell)                           => (cell.module, cell.version, cell.platform)
      case TestFeed.Cached(module, version, platform, _) => (module, version, platform)

  /** A module's tests start when that module's compile finishes.
    */
  def console(asked: TestRun): ZStream[Resolve & histos.cache.ContentStore, ModuleTestError, TestFeed] =
    session(asked).collect { case SessionFeed.Test(feed) => feed }

  /** Compile and test together. `hold` pauses one compile so a spec can see another cell test.
    */
  private[histos] def session(
      asked: TestRun,
      hold: ProjectName => UIO[Unit] = _ => ZIO.unit,
  ): ZStream[Resolve & histos.cache.ContentStore, ModuleTestError, SessionFeed] =
    ZStream.unwrapScoped:
      for
        process <- CacheConfig.processEnv
        remote  <- CacheConfig.open(process).mapError(ModuleTestError.Remote(_))
        report  <- histos.resolve.ResolutionService
          .open(asked.repo, asked.graph)
          .mapError(ModuleTestError.Lock(_))
        env <- (histos.zinc.Compilers.live ++ histos.zinc.CompileMemo.live).build
      yield sessionOn(asked, hold).provideSomeLayer(
        ZLayer.succeed(env.get[histos.zinc.Compilers]) ++
          ZLayer.succeed(env.get[histos.zinc.CompileMemo]) ++
          ZLayer.succeed(report) ++
          ZLayer.succeed(remote)
      )

  /** The daemon's test. The caller keeps the compiler services. */
  private[histos] def sessionOn(
      asked: TestRun,
      hold: ProjectName => UIO[Unit] = _ => ZIO.unit,
  ): ZStream[
    Resolve & histos.cache.ContentStore & histos.zinc.Compilers & histos.zinc.CompileMemo &
      histos.resolve.ResolutionService & RemoteCache,
    ModuleTestError,
    SessionFeed,
  ] =
    ZStream.unwrap:
      ZIO
        .serviceWithZIO[histos.resolve.ResolutionService](_.current)
        .map(lock => arriving(asked, lock, hold))

  def suites(
      asked: TestRun,
      compiled: List[CompiledCell],
  ): ZStream[
    Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService & RemoteCache,
    ModuleTestError,
    TestFeed,
  ] =
    ZStream.unwrapScoped:
      for
        compilers <- histos.zinc.Compilers.live.build
        pool = zio.ZLayer.succeed(compilers.get[histos.zinc.Compilers])
        report <- ZIO.service[histos.resolve.ResolutionService]
        lock   <- report.current
      yield ZStream
        .fromZIO(ProviderInstall.make.zip(LockedTools.make(lock, asked.graph, report)))
        .flatMap: (install, tools) =>
          runCells(asked, compiled, lock).provideSomeLayer[Resolve & histos.cache.ContentStore & RemoteCache](
            zio.ZLayer.succeed(install) ++ zio.ZLayer.succeed(tools) ++ pool
          )

  private[histos] def suitesLocked(
      asked: TestRun,
      compiled: List[CompiledCell],
      lock: DependencyLock,
  ): ZStream[Resolve & histos.cache.ContentStore & RemoteCache, ModuleTestError, TestFeed] =
    ZStream.unwrapScoped:
      histos.zinc.Compilers.live.build.map: compilers =>
        val pool = zio.ZLayer.succeed(compilers.get[histos.zinc.Compilers])
        ZStream
          .fromZIO(ProviderInstall.make.zip(LockedTools.make(lock, asked.graph)))
          .flatMap: (install, tools) =>
            runCells(asked, compiled, lock).provideSomeLayer[Resolve & histos.cache.ContentStore & RemoteCache](
              zio.ZLayer.succeed(install) ++ zio.ZLayer.succeed(tools) ++ pool
            )

  private def runCells(
      asked: TestRun,
      compiled: List[CompiledCell],
      lock: DependencyLock,
  ): ZStream[
    Resolve & ProviderInstall & LockedTools & histos.cache.ContentStore & histos.zinc.Compilers &
      histos.schedule.RemoteCache,
    ModuleTestError,
    TestFeed,
  ] =
    val TestRun(_, graph, names, _, _, _, patterns, _, _) = asked
    ZStream.unwrap:
      for
        wanted <- ZIO.fromEither(
          requested(graph, names).left.map(ModuleTestError.Target(_))
        )
        matched <- Ref.make(false)
        cells = compiled
          .filter(cell => wanted.contains(cell.module))
          .sortBy: cell =>
            (cell.module.value, cell.version.value, platformRank(cell.platform))
      yield
        val gate = unmatched(patterns, matched)
        if cells.isEmpty then gate
        else
          ZStream
            .fromIterable(cells)
            .flatMapPar(Parallelism.default.process)(cell => cellStream(asked, cell, lock, matched)) ++ gate

  /** Compile feeds, and a test cell as soon as its own compile is done. At most `process` tests run at once.
    */
  private def arriving(
      asked: TestRun,
      lock: DependencyLock,
      hold: ProjectName => UIO[Unit],
  ): ZStream[
    Resolve & histos.cache.ContentStore & histos.zinc.Compilers & histos.zinc.CompileMemo &
      histos.resolve.ResolutionService & RemoteCache,
    ModuleTestError,
    SessionFeed,
  ] =
    val TestRun(_, graph, names, _, _, _, _, _, _) = asked
    ZStream.unwrapScoped:
      for
        install <- ProviderInstall.make
        report  <- ZIO.service[histos.resolve.ResolutionService]
        tools   <- LockedTools.make(lock, graph, report)
        wanted  <- ZIO.fromEither(requested(graph, names).left.map(ModuleTestError.Target(_)))
        matched <- Ref.make(false)
        fibers  <- Ref.make(Chunk.empty[Fiber[Nothing, Unit]])
        gate    <- Semaphore.make(Parallelism.default.process)
        queue   <- Queue.unbounded[Take[ModuleTestError, SessionFeed]]
        scope   <- ZIO.scope
        _       <- compileThenTest(asked, lock, hold, wanted, matched, fibers, gate, queue, scope)
          .provideSomeLayer[
            Resolve & histos.cache.ContentStore & histos.zinc.Compilers & histos.zinc.CompileMemo &
              histos.resolve.ResolutionService & RemoteCache
          ](
            zio.ZLayer.succeed(install) ++ zio.ZLayer.succeed(tools)
          )
          .forkScoped
      yield takes(queue)

  private def compileThenTest(
      asked: TestRun,
      lock: DependencyLock,
      hold: ProjectName => UIO[Unit],
      wanted: Set[ProjectName],
      matched: Ref[Boolean],
      fibers: Ref[Chunk[Fiber[Nothing, Unit]]],
      gate: Semaphore,
      queue: Queue[Take[ModuleTestError, SessionFeed]],
      scope: zio.Scope,
  ): ZIO[
    Resolve & histos.cache.ContentStore & histos.zinc.Compilers & histos.zinc.CompileMemo & ProviderInstall &
      LockedTools & histos.resolve.ResolutionService & RemoteCache,
    Nothing,
    Unit,
  ] =
    Ref
      .make(Map.empty[(ProjectName, ScalaVersion, Platform), Promise[Nothing, Unit]])
      .flatMap: gates =>
        def cellGate(name: ProjectName, version: ScalaVersion, platform: Platform): UIO[Promise[Nothing, Unit]] =
          Promise
            .make[Nothing, Unit]
            .flatMap: fresh =>
              gates.modify: map =>
                val key = (name, version, platform)
                map.get(key) match
                  case Some(found) => (found, map)
                  case None        => (fresh, map.updated(key, fresh))
        val session = histos.zinc.CompileSession(
          asked.repo,
          asked.graph,
          asked.jdk,
          asked.runtimes.native,
          lock,
          asked.coverage,
        )
        val after = (name: ProjectName, version: ScalaVersion, platform: Platform) =>
          asked.coverage match
            case None    => ZIO.unit
            case Some(_) =>
              if wanted.contains(name) then cellGate(name, version, platform).flatMap(_.await)
              else ZIO.unit
        GraphCompile
          .held(session, asked.names, hold, after)
          .mapError(ModuleTestError.Compile(_))
          .runForeach { feed =>
            queue.offer(Take.single(SessionFeed.Compile(feed))).unit *>
              (feed match
                case histos.zinc.CompileFeed.Done(cell) if wanted.contains(cell.module) =>
                  val ran =
                    gate.withPermit(
                      cellStream(asked, cell, lock, matched)
                        .runForeach(item => queue.offer(Take.single(SessionFeed.Test(item))).unit)
                        .foldCauseZIO(
                          cause => queue.offer(Take.failCause(cause)).unit,
                          _ => ZIO.unit,
                        )
                    )
                  val finished =
                    asked.coverage match
                      case None    => ran
                      case Some(_) =>
                        ran.ensuring(
                          cellGate(cell.module, cell.version, cell.platform).flatMap(_.succeed(())).unit
                        )
                  finished.forkIn(scope).flatMap(fiber => fibers.update(_ :+ fiber))
                case _ => ZIO.unit)
          }
      .foldCauseZIO(
        cause => queue.offer(Take.failCause(cause)).unit,
        _ =>
          fibers.get.flatMap(started => ZIO.foreachDiscard(started)(_.join)) *>
            matched.get.flatMap { hit =>
              if asked.patterns.nonEmpty && !hit then
                queue.offer(Take.fail(ModuleTestError.NoneMatched(asked.patterns))).unit
              else queue.offer(Take.end).unit
            },
      )

  private def cellStream(
      asked: TestRun,
      cell: CompiledCell,
      lock: DependencyLock,
      matched: Ref[Boolean],
  ): ZStream[
    Resolve & ProviderInstall & LockedTools & histos.cache.ContentStore & histos.zinc.Compilers &
      histos.schedule.RemoteCache,
    ModuleTestError,
    TestFeed,
  ] =
    val TestRun(repo, graph, _, jdk, runtimes, mode, patterns, onTestCompile, coverage) = asked
    ZStream.unwrap(
      prepare(
        repo,
        graph,
        cell,
        jdk,
        runtimes,
        lock,
        BuildResolvers(graph.resolvers),
        patterns,
        matched,
        mode,
        onTestCompile,
        coverage,
      ).map(stream => watch(matched, stream))
    )

  private def takes[A](queue: Queue[Take[ModuleTestError, A]]): ZStream[Any, ModuleTestError, A] =
    ZStream.repeatZIOChunkOption(
      queue.take.flatMap: taken =>
        taken.foldZIO(
          ZIO.fail(None),
          cause => ZIO.failCause(cause.map(Some(_))),
          feeds => if feeds.isEmpty then ZIO.succeed(Chunk.empty) else ZIO.succeed(feeds),
        )
    )

  private def unmatched(
      patterns: List[SuitePattern],
      matched: Ref[Boolean],
  ): ZStream[Any, ModuleTestError, Nothing] =
    if patterns.isEmpty then ZStream.empty
    else
      ZStream.unwrap(
        matched.get.map: hit =>
          if hit then ZStream.empty
          else ZStream.fail(ModuleTestError.NoneMatched(patterns))
      )

  private def requested(
      graph: ProjectGraph,
      names: List[ProjectName],
  ): Either[SelectError, Set[ProjectName]] =
    histos.select.Selection
      .select(graph, names)
      .map(selected => selected.modules.map(_.name).toSet)

  private def prepare(
      repo: Path,
      graph: ProjectGraph,
      cell: CompiledCell,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      matched: Ref[Boolean],
      mode: TestMode,
      onTestCompile: (
          ProjectName,
          ScalaVersion,
          Platform,
          histos.zinc.CompileFacts,
      ) => UIO[Unit],
      coverage: Option[histos.coverage.CoverageCompile],
  ): ZIO[
    Resolve & ProviderInstall & LockedTools & histos.cache.ContentStore & histos.zinc.Compilers &
      histos.schedule.RemoteCache,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed],
  ] =
    val modules = graph.projects.collect { case module: Module => module }
    graph.get(cell.module) match
      case Some(module: Module) =>
        TestCompile
          .testSources(repo, module, modules, cell.version, cell.platform)
          .flatMap: sources =>
            val chosen = (sources, module.test, module.provider) match
              case (_, Some(_), Some(_)) =>
                ZIO.fail(ModuleTestError.Mixed(cell.module))
              case (Nil, None, None) =>
                if patterns.isEmpty then ZIO.succeed(SuiteLaunch.finished(cell, TestStatus.NoTests))
                else ZIO.succeed(ZStream.empty)
              case (Nil, Some(main), None) =>
                ZIO.fail(ModuleTestError.MissingSources(cell.module, main))
              case (Nil, None, Some(provider)) =>
                ZIO.fail(ModuleTestError.MissingProvider(cell.module, provider))
              case (_, Some(main), None) =>
                if !SuiteMatch.allows(patterns, main.value) then ZIO.succeed(ZStream.empty)
                else
                  val note = (facts: histos.zinc.CompileFacts) =>
                    onTestCompile(
                      cell.module,
                      cell.version,
                      cell.platform,
                      facts,
                    )
                  cell.platform match
                    case Platform.Jvm =>
                      SuiteLaunch.execute(
                        repo,
                        modules,
                        module,
                        cell,
                        sources,
                        main,
                        jdk,
                        runtimes,
                        lock,
                        repositories,
                        mode,
                        note,
                        coverage,
                      )
                    case Platform.Js =>
                      SuiteLaunch.runJs(
                        repo,
                        modules,
                        module,
                        cell,
                        sources,
                        main,
                        jdk,
                        runtimes,
                        lock,
                        repositories,
                        mode,
                        note,
                      )
                    case Platform.Native =>
                      SuiteLaunch.runNative(
                        repo,
                        modules,
                        module,
                        cell,
                        sources,
                        main,
                        jdk,
                        runtimes,
                        lock,
                        repositories,
                        mode,
                        note,
                      )
              case (_, None, provider) =>
                val framework = provider.getOrElse(TestProvider.Zio)
                val note      = (facts: histos.zinc.CompileFacts) =>
                  onTestCompile(
                    cell.module,
                    cell.version,
                    cell.platform,
                    facts,
                  )
                cell.platform match
                  case Platform.Jvm =>
                    SuiteLaunch.provide(
                      repo,
                      modules,
                      module,
                      cell,
                      sources,
                      framework,
                      jdk,
                      runtimes,
                      lock,
                      repositories,
                      patterns,
                      mode,
                      note,
                      coverage,
                    )
                  case Platform.Js =>
                    SuiteLaunch.runJsProvider(
                      repo,
                      modules,
                      module,
                      cell,
                      sources,
                      framework,
                      jdk,
                      runtimes,
                      lock,
                      repositories,
                      patterns,
                      mode,
                      note,
                    )
                  case Platform.Native =>
                    SuiteLaunch.runNativeProvider(
                      repo,
                      modules,
                      module,
                      cell,
                      sources,
                      framework,
                      jdk,
                      runtimes,
                      lock,
                      repositories,
                      patterns,
                      mode,
                      note,
                    )
            val executes = sources.nonEmpty && (module.test match
              case Some(main) => SuiteMatch.allows(patterns, main.value)
              case None       =>
                SuiteLaunch.satisfied(
                  module,
                  module.provider.getOrElse(TestProvider.Zio),
                ))
            val context = if executes then TestContext.make(module, cell, jdk, runtimes)
            else ZIO.succeed(TestContext.empty)
            context
              .flatMap(value =>
                chosen.provideSome[
                  Resolve & ProviderInstall & LockedTools & histos.cache.ContentStore & histos.zinc.Compilers &
                    histos.schedule.RemoteCache
                ](
                  zio.ZLayer.succeed(value)
                )
              )
              .map(watch(matched, _))
      case _ =>
        ZIO.fail(ModuleTestError.Absent(cell.module))

  /** A passed or failed suite is a match. `unmatched` and `no tests` are not.
    */
  private def watch(
      matched: Ref[Boolean],
      stream: ZStream[Any, ModuleTestError, TestFeed],
  ): ZStream[Any, ModuleTestError, TestFeed] =
    stream.tap:
      case TestFeed.Cached(_, _, _, _) =>
        matched.set(true)
      case TestFeed.Done(
            TestedCell(
              _,
              _,
              _,
              TestStatus.Passed | TestStatus.Failed(_),
            )
          ) =>
        matched.set(true)
      case _ =>
        ZIO.unit

  private def platformRank(platform: Platform): Int =
    platform match
      case Platform.Jvm    => 0
      case Platform.Js     => 1
      case Platform.Native => 2
end ModuleTest
