package histos.testrun

import histos.build.{BuildExecution, BuildNode, ExecutionError, ExecutionLeaves, ExecutionProducts, ExecutionRequest}
import histos.cache.{ContentStore, LinkerStarts}
import histos.plugin.SourceSet
import histos.resolve.{LockedTools, ResolutionService, Resolve}
import histos.schedule.{CacheConfig, Histos, Parallelism, RemoteCache}
import histos.zinc.{CompileMemo, CompileSession, CompilerStarts, Compilers, GraphCompile}
import histos.{Cell, ProjectName}

import zio.{Chunk, Queue, Ref, Scope, UIO, ZIO, ZLayer}
import zio.stream.{Take, ZStream}

object ModuleTest:
  type Error                  = ModuleTestError | ExecutionError | TestSelectionError
  private[histos] type RunEnv = LockedTools & ContentStore & LinkerStarts & RemoteCache

  def run(asked: TestRun): ZIO[Resolve & ContentStore, ModuleTestError, List[TestReport]] =
    ZIO
      .fromEither(TestCells.plan(asked.graph, asked.selection, asked.cellPolicy))
      .mapError(error => ModuleTestError.Scoped(ScopedTestFailure.Selection(error)))
      .flatMap: plan =>
        console(asked)
          .runFold(Reports(Map.empty, Chunk.empty))((have, feed) => report(plan, have, feed))
          .map(_.done.toList)

  private final case class Reports(heard: Map[Cell, Transcript], done: Chunk[TestReport])

  private def report(plan: TestCellPlan, have: Reports, feed: TestFeed): Reports =
    val cell = feed match
      case TestFeed.Begin(module, version, platform)     => Cell(module, version, platform)
      case TestFeed.Out(module, version, platform, _)    => Cell(module, version, platform)
      case TestFeed.Err(module, version, platform, _)    => Cell(module, version, platform)
      case TestFeed.Done(done)                           => Cell(done.module, done.version, done.platform)
      case TestFeed.Cached(module, version, platform, _) => Cell(module, version, platform)
    feed match
      case TestFeed.Begin(_, _, _)     => have.copy(heard = have.heard.updated(cell, Transcript.empty))
      case TestFeed.Out(_, _, _, text) =>
        have.copy(heard = have.heard.updated(cell, have.heard.getOrElse(cell, Transcript.empty).withOut(text)))
      case TestFeed.Err(_, _, _, text) =>
        have.copy(heard = have.heard.updated(cell, have.heard.getOrElse(cell, Transcript.empty).withErr(text)))
      case TestFeed.Done(done) =>
        val owner = plan.executionGroups
          .collectFirst {
            case TestExecutionGroup.NativeAggregate(owner, members) if members.contains(cell) => owner
          }
          .getOrElse(cell)
        have.copy(done = have.done :+ TestReport(done, have.heard.getOrElse(owner, Transcript.empty)))
      case TestFeed.Cached(_, _, _, _) => have

  def console(asked: TestRun): ZStream[Resolve & ContentStore, ModuleTestError, TestFeed] =
    session(asked).collect { case SessionFeed.Test(feed) => feed }

  private[histos] def session(asked: TestRun, hold: ProjectName => UIO[Unit] = _ => ZIO.unit): ZStream[
    Resolve & ContentStore,
    ModuleTestError,
    SessionFeed,
  ] =
    ZStream.unwrapScoped:
      for
        process    <- CacheConfig.processEnv
        remote     <- CacheConfig.open(process).mapError(ModuleTestError.Remote(_))
        resolution <- ResolutionService.open(asked.repo, asked.graph).mapError(ModuleTestError.Lock(_))
        services   <- (Compilers.live ++ CompileMemo.live).build
      yield sessionOn(asked, hold).provideSomeLayer[Resolve & ContentStore](
        ZLayer.succeed(resolution) ++ ZLayer.succeed(remote) ++
          ZLayer.succeed(services.get[Compilers]) ++ ZLayer.succeed(services.get[CompileMemo])
      )

  /** Compile-only graph adapter. Generated/link leaves are supplied by the integrator through sessionWith. */
  private[histos] def sessionOn(asked: TestRun, hold: ProjectName => UIO[Unit] = _ => ZIO.unit): ZStream[
    Resolve & ContentStore & Compilers & CompileMemo & ResolutionService & RemoteCache,
    ModuleTestError,
    SessionFeed,
  ] =
    ZStream.unwrapScoped:
      for
        scheduler  <- Histos.live.build.provideSomeLayer[Scope](ZLayer.succeed(Parallelism.default)).map(_.get[Histos])
        resolution <- ZIO.service[ResolutionService]
        lock       <- resolution.current
        tools      <- LockedTools.make(lock, asked.graph, resolution)
        leaves = ExecutionLeaves[GraphCompile.Fed, ExecutionError](
          (request, stage, completed) => hold(stage.cell.module) *> GraphCompile.stage(request, stage, completed),
          TestCompile.stage,
          (_, task, _) => ZIO.fail(ExecutionError.Dependency(BuildNode.Generate(task))),
          (_, cell, mode, _) => ZIO.fail(ExecutionError.Dependency(BuildNode.Link(cell, mode))),
        )
      yield sessionWith(new BuildExecution(leaves, scheduler), asked)
        .provideSomeLayer[
          Resolve & ContentStore & Compilers & CompileMemo & ResolutionService & RemoteCache
        ](ZLayer.succeed(tools) ++ CompilerStarts.live ++ LinkerStarts.live)
        .mapError(TestFailures(_))

  /** Shared executor keeps generated read leases alive through every requested child. */
  private[histos] def sessionWith[R, E](execution: BuildExecution[R, E], asked: TestRun): ZStream[
    R & RunEnv & ResolutionService,
    E | Error,
    SessionFeed,
  ] =
    ZStream.unwrapScoped:
      for
        plan  <- ZIO.fromEither(TestCells.plan(asked.graph, asked.selection, asked.cellPolicy))
        lock  <- ZIO.serviceWithZIO[ResolutionService](_.current)
        queue <- ZIO.acquireRelease(Queue.unbounded[Take[E | Error, SessionFeed]])(_.shutdown)
        session = CompileSession(asked.repo, asked.graph, asked.jdk, asked.runtimes.native, lock, asked.coverage)
        request = ExecutionRequest(
          session,
          plan.compileStageRoots.map(BuildNode.Compile(_)),
          feed => queue.offer(Take.single(SessionFeed.Compile(feed))).unit,
        )
        work = execution
          .ensure(request)
          .flatMap: products =>
            execute(asked, plan, products).runForeach(feed => queue.offer(Take.single(SessionFeed.Test(feed))).unit)
        _ <- work
          .foldCauseZIO(cause => queue.offer(Take.failCause(cause)).unit, _ => queue.offer(Take.end).unit)
          .forkScoped
      yield ZStream.fromQueue(queue).flattenTake

  /** Execution consumes actual completed Test products and never recursively compiles a suite. */
  private[histos] def execute(asked: TestRun, plan: TestCellPlan, completed: ExecutionProducts): ZStream[
    RunEnv,
    ModuleTestError | ExecutionError,
    TestFeed,
  ] =
    ZStream.unwrap:
      for
        install <- ProviderInstall.make
        matched <- Ref.make(false)
      yield
        val streams = ZStream
          .fromIterable(plan.executionGroups)
          .flatMapPar(Parallelism.default.process): group =>
            ZStream
              .unwrap(SuiteLaunch.run(asked, group, completed))
              .tap:
                case TestFeed.Cached(_, _, _, _) => matched.set(true)
                case TestFeed.Done(done)         =>
                  done.status match
                    case TestStatus.Passed | TestStatus.Failed(_)  => matched.set(true)
                    case TestStatus.NoTests | TestStatus.Unmatched => ZIO.unit
                case TestFeed.Begin(_, _, _) | TestFeed.Out(_, _, _, _) | TestFeed.Err(_, _, _, _) => ZIO.unit
        val gate = ZStream
          .fromZIO(matched.get.flatMap: hit =>
            if asked.patterns.nonEmpty && !hit then ZIO.fail(ModuleTestError.NoneMatched(asked.patterns))
            else ZIO.unit)
          .drain
        val compiled = ZIO.foreachDiscard(plan.requestedCells): cell =>
          ZIO
            .fromEither(completed.require(histos.CompilationStage(cell, SourceSet.Test)))
            .flatMap(product =>
              asked.onTestCompile(
                cell.module,
                cell.version,
                cell.platform,
                histos.zinc
                  .CompileFacts(product.compiled.changed, product.compiled.sources, product.compiled.classesWritten),
              )
            )
        (ZStream.fromZIO(compiled).drain ++ streams ++ gate).provideSomeLayer[RunEnv](ZLayer.succeed(install))
end ModuleTest
