package histos.build

import histos.cache.{ContentStore, LinkerStarts}
import histos.js.{JsLinkRequest, JsProducts}
import histos.load.{PluginFault, PluginRuntime, TaskInvocation}
import histos.plugin.{PluginArgs, PluginEvents, SourceSet}
import histos.resolve.{LockedTools, ResolutionService, Resolve}
import histos.schedule.{ActionCache, Histos, Parallelism, RemoteCache}
import histos.testrun.{ModuleTest, ModuleTestError, SessionFeed, TestCompile, TestFailures, TestRun}
import histos.zinc.{CompileMemo, CompilerStarts, Compilers, GraphCompile}
import histos.{CompilationStage, Module}
import zio.{Scope, ZEnvironment, ZIO, ZLayer}
import zio.stream.ZStream

/** One command's compiler services and injectable leaves. Caller scope owns all generated read leases. */
final class BuildRuntime private (
    execution: BuildExecution[BuildRuntime.Env, ExecutionError],
    services: ZEnvironment[BuildRuntime.Env],
):
  def ensure(request: ExecutionRequest): ZIO[Scope, ExecutionError, ExecutionProducts] =
    execution.ensure(request).provideSomeEnvironment[Scope](scope => services ++ scope)

  def tests(asked: TestRun): ZStream[Any, ModuleTestError, SessionFeed] =
    ZStream.unwrap:
      for
        lock  <- services.get[ResolutionService].current
        tools <- LockedTools.make(lock, asked.graph, services.get[ResolutionService]).provideEnvironment(services)
      yield ModuleTest
        .sessionWith(execution, asked)
        .provideEnvironment(services.add(tools))
        .mapError(TestFailures(_))
end BuildRuntime

object BuildRuntime:
  private[build] type Env = GraphCompile.Fed & ActionCache & LinkerStarts

  def compileRoots(
      graph: histos.ProjectGraph,
      names: List[histos.ProjectName],
      platforms: histos.PlatformSelection,
      scala: histos.ScalaSelection = histos.ScalaSelection.All,
  ): Either[ModuleTestError, List[BuildNode]] =
    val selected = if names.isEmpty then Right(graph.projects.collect { case module: Module => module })
    else histos.select.Selection.select(graph, names).left.map(ModuleTestError.Target(_)).map(_.modules)
    selected.flatMap { modules =>
      modules.foldLeft[Either[ModuleTestError, List[BuildNode]]](Right(Nil)) { (acc, module) =>
        acc.flatMap { nodes =>
          val chosen = if names.isEmpty then Right(platforms.selected(module.platforms))
          else
            platforms
              .validate(module.name, module.platforms)
              .left
              .map(error =>
                ModuleTestError.Scoped(
                  histos.testrun.ScopedTestFailure.Selection(histos.testrun.TestSelectionError.Selection(error))
                )
              )
          val versions =
            if names.isEmpty then Right(scala.filter(module.versions.all))
            else
              scala
                .require(module.name, module.versions.all)
                .left
                .map(error =>
                  ModuleTestError.Scoped(
                    histos.testrun.ScopedTestFailure.Selection(histos.testrun.TestSelectionError.Selection(error))
                  )
                )
          chosen.flatMap(values =>
            versions.map(picked =>
              nodes ++ (for
                version  <- picked
                platform <- values
              yield BuildNode.Compile(CompilationStage(histos.Cell(module.name, version, platform), SourceSet.Main)))
            )
          )
        }
      }
    }

  def open(
      plugins: Option[PluginRuntime],
      invocation: Option[TaskInvocation] = None,
      cache: Option[ActionCache] = None,
      parallelism: Parallelism = Parallelism.default,
  ): ZIO[Resolve & ContentStore & ResolutionService & RemoteCache & Scope, Nothing, BuildRuntime] =
    for
      resolve    <- ZIO.service[Resolve]
      store      <- ZIO.service[ContentStore]
      resolution <- ZIO.service[ResolutionService]
      remote     <- ZIO.service[RemoteCache]
      compilers  <- (Compilers.live(
        parallelism.cpu
      ) ++ CompileMemo.live ++ CompilerStarts.live ++ LinkerStarts.live).build
      scheduler <- (ZLayer.succeed(parallelism) >>> Histos.live).build
      services = ZEnvironment(
        resolve,
        store,
        resolution,
        remote,
        cache.getOrElse(ContentStore.actions(store)),
      ) ++ compilers
      leaves = ExecutionLeaves[Env, ExecutionError](
        GraphCompile.stage,
        TestCompile.stage,
        (request, task, completed) =>
          plugins match
            case None          => ZIO.fail(ExecutionError.Plugin(PluginFault.MissingProduct(task)))
            case Some(runtime) =>
              val passed = invocation
                .filter(_.id == task)
                .getOrElse(
                  TaskInvocation(task, PluginArgs.empty, PluginRuntime.standIn(task), PluginEvents.closedInvocation)
                )
              runtime.generate(request, task, completed, passed),
        (request, cell, mode, completed) =>
          for
            product <- ZIO.fromEither(completed.require(CompilationStage(cell, SourceSet.Main)))
            module  <- ZIO
              .fromOption(request.session.graph.projects.collectFirst {
                case module: Module if module.name == cell.module => module
              })
              .orElseFail(ExecutionError.MissingProduct(BuildNode.Compile(CompilationStage(cell, SourceSet.Main))))
            resolution <- ZIO.service[ResolutionService]
            lock       <- resolution.current
            tools      <- LockedTools.make(lock, request.session.graph, resolution)
            main = module.application.fold(histos.pack.MainClass.trusted("histos.Exports"))(value =>
              histos.pack.MainClass.trusted(value.value)
            )
            linked <- JsProducts
              .link(
                JsLinkRequest(
                  cell.version,
                  main,
                  product.classpath,
                  JsProducts.work(request.session.repo, cell, mode),
                  request.session.jdk,
                  module.jsModule,
                  revision = module.jsVersion,
                  initialize = module.application.nonEmpty,
                  optimize = mode,
                )
              )
              .provideSomeLayer[Env](ZLayer.succeed(tools))
              .mapError(ExecutionError.Link(_))
          yield linked,
      )
    yield new BuildRuntime(new BuildExecution(leaves, scheduler.get[Histos]), services)
end BuildRuntime
