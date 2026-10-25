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
import histos.schedule.{Histos, Outcome, Parallelism, StepFailure}
import histos.step.{Input, InputName, InputValue, Need, Phase, Step, StepId, StepInfo}
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

import zio.{Chunk, IO, Queue, Ref, UIO, ZIO}
import zio.stream.{Take, ZStream}

object ModuleTest:
  /** Compile each requested module, then run its test provider once per Scala version and platform. `TestProvider.Zio`
    * is the provider when the module names neither a provider nor a main. Upstream modules are compiled and not run.
    * The engine does not load the provider.
    */
  def run(asked: TestRun): ZIO[Resolve, ModuleTestError, List[TestReport]] =
    console(asked)
      .runFold(Reports(Transcript.empty, Chunk.empty))(report)
      .map(_.done.toList)

  private final case class Reports(heard: Transcript, done: Chunk[TestReport])

  /** Cells do not interleave, so the lines since the last cell belong to the next `Done`.
    */
  private def report(have: Reports, feed: TestFeed): Reports =
    feed match
      case TestFeed.Begin(_, _, _) => have.copy(heard = Transcript.empty)
      case TestFeed.Out(text)      => have.copy(heard = have.heard.withOut(text))
      case TestFeed.Err(text)      => have.copy(heard = have.heard.withErr(text))
      case TestFeed.Done(cell)     =>
        Reports(Transcript.empty, have.done :+ TestReport(cell, have.heard))
      case TestFeed.Cached(_, _, _, _) => have

  /** Test compiles stay parallel. Cells run in parallel. The console emits one cell, then the next, so the trees do not
    * interleave. The speaking cell is printed as it writes. A later cell keeps only its own lines.
    */
  def console(asked: TestRun): ZStream[Resolve, ModuleTestError, TestFeed] =
    ZStream.unwrap:
      TestCompile
        .readLock(asked.repo)
        .map: lock =>
          ZStream
            .fromZIO(
              GraphCompile
                .run(
                  histos.zinc.CompileSession(
                    asked.repo,
                    asked.graph,
                    asked.jdk,
                    asked.runtimes.native,
                    lock,
                  ),
                  asked.names,
                )
                .mapError(ModuleTestError.Compile(_))
            )
            .flatMap(compiled => suitesLocked(asked, compiled, lock))

  def suites(
      asked: TestRun,
      compiled: List[CompiledCell],
  ): ZStream[Resolve, ModuleTestError, TestFeed] =
    ZStream.unwrap(
      TestCompile
        .readLock(asked.repo)
        .map(lock => suitesLocked(asked, compiled, lock))
    )

  private[histos] def suitesLocked(
      asked: TestRun,
      compiled: List[CompiledCell],
      lock: DependencyLock,
  ): ZStream[Resolve, ModuleTestError, TestFeed] =
    ZStream
      .fromZIO(ProviderInstall.make.zip(LockedTools.make(lock, asked.graph)))
      .flatMap: (install, tools) =>
        runCells(asked, compiled, lock).provideSomeLayer[Resolve](
          zio.ZLayer.succeed(install) ++ zio.ZLayer.succeed(tools)
        )

  private def runCells(
      asked: TestRun,
      compiled: List[CompiledCell],
      lock: DependencyLock,
  ): ZStream[
    Resolve & ProviderInstall & LockedTools,
    ModuleTestError,
    TestFeed,
  ] =
    val TestRun(_, graph, names, jdk, runtimes, patterns, _, _) = asked
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
        tools <- ZIO.service[LockedTools]
      yield
        val gate = unmatched(patterns, matched)
        if cells.isEmpty then gate
        else
          ZStream.unwrapScoped:
            for
              queues <- ZIO.foreach(cells)(_ => cellQueue)
              steps = cells
                .zip(queues)
                .map: (cell, queue) =>
                  val module = graph.projects.collectFirst {
                    case found: Module if found.name == cell.module => found
                  }
                  val inputs = Chunk(
                    Input(
                      InputName("jdk"),
                      InputValue.Text(Runtimes.identityToken(jdk.identity)),
                    ),
                    Input(
                      InputName("tools"),
                      InputValue.Text(
                        module.toList
                          .flatMap(found =>
                            tools.declaredCompilation(
                              found,
                              cell.version,
                              cell.platform,
                              runtimes.native,
                            )
                          )
                          .mkString("\n")
                      ),
                    ),
                  )
                  testStep(asked, cell, lock, matched, inputs, queue)
              fiber <- execute(combine(steps)).forkScoped
            yield inOrder(queues) ++
              ZStream.fromZIO(fiber.join).drain ++
              gate

  /** One queue per cell. Unbounded because a later cell that blocked on a full queue would keep its process permit, and
    * that permit can stop the speaking cell from starting.
    */
  private def cellQueue: UIO[Queue[Take[ModuleTestError, TestFeed]]] =
    Queue.unbounded

  /** Feeds in cell order. The queue being read is emitted as it arrives. */
  private[testrun] def inOrder(
      queues: List[Queue[Take[ModuleTestError, TestFeed]]]
  ): ZStream[Any, ModuleTestError, TestFeed] =
    ZStream.unwrap:
      Ref
        .make(queues)
        .map: state =>
          ZStream.repeatZIOChunkOption:
            def read(
                left: List[Queue[Take[ModuleTestError, TestFeed]]]
            ): ZIO[Any, Option[ModuleTestError], Chunk[TestFeed]] =
              left match
                case Nil           => ZIO.fail(None)
                case queue :: more =>
                  queue.take.flatMap: taken =>
                    taken.foldZIO(
                      state.set(more) *> read(more),
                      cause => ZIO.failCause(cause.map(Some(_))),
                      feeds =>
                        if feeds.isEmpty then read(left)
                        else ZIO.succeed(feeds),
                    )
            state.get.flatMap(read)

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

  private def testStep(
      asked: TestRun,
      cell: CompiledCell,
      lock: DependencyLock,
      matched: Ref[Boolean],
      inputs: Chunk[Input],
      queue: Queue[Take[ModuleTestError, TestFeed]],
  ): Step[Resolve & ProviderInstall & LockedTools, ModuleTestError, Unit] =
    val TestRun(repo, graph, _, jdk, runtimes, patterns, quick, onTestCompile) =
      asked
    val repositories = BuildResolvers(graph.resolvers)
    val info         = StepInfo(
      StepId.Cell(cell.module, cell.version, cell.platform, Phase.Test),
      inputs,
      Set(Need.Process),
    )
    Step.local[Unit](info):
      prepare(
        repo,
        graph,
        cell,
        jdk,
        runtimes,
        lock,
        repositories,
        patterns,
        matched,
        quick,
        onTestCompile,
      ).flatMap(stream => publish(queue, watch(matched, stream)))
        .exit
        .flatMap: exit =>
          exit.foldExit(
            cause =>
              queue.offer(Take.failCause(cause)) *>
                ZIO.when(cause.isInterrupted)(ZIO.failCause(cause)).unit,
            _ => ZIO.unit,
          )

  /** The step stays successful. A cell failure is a failed take, so lines already written stay written and the drain
    * fails when it reaches them.
    */
  private def publish(
      queue: Queue[Take[ModuleTestError, TestFeed]],
      stream: ZStream[Any, ModuleTestError, TestFeed],
  ): UIO[Unit] =
    Ref
      .make(false)
      .flatMap: closed =>
        stream
          .runForeach(feed => queue.offer(Take.single(feed)).unit)
          .exit
          .flatMap: exit =>
            queue.offer(exit.foldExit(Take.failCause, _ => Take.end)) *>
              closed.set(true)
          .ensuring(
            closed.get
              .flatMap(done => ZIO.unless(done)(queue.offer(Take.end)).unit)
          )

  private def combine(
      steps: List[
        Step[Resolve & ProviderInstall & LockedTools, ModuleTestError, Unit]
      ]
  ): Step[Resolve & ProviderInstall & LockedTools, ModuleTestError, Unit] =
    steps match
      case Nil =>
        Step.local[Unit](
          StepInfo(StepId.Work("test"), Chunk.empty, Set(Need.Process))
        )(ZIO.unit)
      case head :: tail =>
        tail.foldLeft(head): (acc, step) =>
          acc.zip(step).map((_, _) => ())

  private def execute[A](
      goal: Step[Resolve & ProviderInstall & LockedTools, ModuleTestError, A]
  ): ZIO[Resolve & ProviderInstall & LockedTools, ModuleTestError, A] =
    ZIO.scoped:
      ZIO
        .serviceWithZIO[Histos](_.start(goal))
        .flatMap(_.outcome)
        .provideSome[Resolve & ProviderInstall & LockedTools & zio.Scope](
          Parallelism.live >>> Histos.live
        )
        .flatMap:
          case Outcome.Done(value)      => ZIO.succeed(value)
          case Outcome.Failed(failures) =>
            failures.head match
              case StepFailure.Failed(_, cause) => ZIO.failCause(cause)
              case StepFailure.Skipped(_, _)    =>
                ZIO.dieMessage("test step was skipped")

  private def requested(
      graph: ProjectGraph,
      names: List[String],
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
      quick: Boolean,
      onTestCompile: (
          ProjectName,
          ScalaVersion,
          Platform,
          histos.zinc.CompileFacts,
      ) => UIO[Unit],
  ): ZIO[
    Resolve & ProviderInstall & LockedTools,
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
                if !SuitePattern.allows(patterns, main.value) then ZIO.succeed(ZStream.empty)
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
                        quick,
                        note,
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
                        quick,
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
                        quick,
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
                      quick,
                      note,
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
                      quick,
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
                      quick,
                      note,
                    )
            val executes = sources.nonEmpty && (module.test match
              case Some(main) => SuitePattern.allows(patterns, main.value)
              case None       =>
                SuiteLaunch.satisfied(
                  module,
                  module.provider.getOrElse(TestProvider.Zio),
                ))
            val context = if executes then TestContext.make(module, cell, jdk, runtimes)
            else ZIO.succeed(TestContext.empty)
            context
              .flatMap(value =>
                chosen.provideSome[Resolve & ProviderInstall & LockedTools](
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
