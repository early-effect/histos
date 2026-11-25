package histos.load

import histos.jdk.{Machine, Sha256, Sha256Digest}
import histos.lock.ExecutionKind
import histos.plugin.{
  ApiDigest,
  ClientId,
  CommandIdentity,
  DeclaredTask,
  HistosPlugin,
  Interaction,
  InvocationContext,
  InvocationId,
  ModuleView,
  OfflineSelection,
  PluginArgs,
  PluginEvents,
  PluginId,
  PluginLinked,
  PluginOutput,
  PluginRunError,
  PluginScopes,
  PluginTaskId,
  RunRequest,
  OutputDestinations,
  OutputName,
}
import histos.schedule.{
  Action,
  ActionCache,
  ActionDigest,
  ActionId,
  Histos,
  Input,
  InputName,
  InputValue,
  Parallelism,
  PluginManifest,
  Policy,
  RemoteCache,
  ScheduleError,
  Scheduler,
}
import histos.step.{Need, Step, StepId, StepInfo}
import histos.task.{TaskError, TaskId, TaskResult}
import histos.resolve.Resolve
import histos.zinc.GraphCompile
import histos.{Module, ProjectGraph}

import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import histos.cache.{CacheProject, ContentStore}

import scala.util.Try

import zio.{Chunk, IO, Ref, Scope, UIO, ZIO, ZLayer}

final case class PluginLink(
    id: PluginId,
    linked: PluginLinked,
    artifacts: List[String],
)

final case class TaskInvocation(
    id: PluginTaskId,
    args: PluginArgs,
    caller: InvocationContext,
    events: PluginEvents.Invocation,
)

/** Loaded plugin classes. The load `Scope` only retires the generation. That generation scope owns the build loader,
  * the class directories it reads, the plugin threads, and services acquired with `leases.use`. A task lease keeps the
  * generation open after retire, and `discard` closes it. `run` uses an inner `Scope`, so a finalizer on the
  * environment `Scope` runs when the task ends, including on interrupt. `PluginScopes` names that scope and the
  * generation scope, and a task extends the one an acquire should die with.
  */
trait PluginRuntime:
  private[histos] def awaitResources: UIO[Unit]
  private[histos] def retire: UIO[Unit]

  def generate(
      request: histos.build.ExecutionRequest,
      id: PluginTaskId,
      completed: histos.build.ExecutionProducts,
      invocation: TaskInvocation,
  ): ZIO[
    ActionCache & RemoteCache & Resolve & ContentStore & histos.resolve.ResolutionService,
    histos.build.ExecutionError,
    histos.build.GeneratedTask[histos.build.ExecutionError],
  ]

  /** Sha256 of the api jar this process loaded. Publish stamps a plugin coordinate with it. */
  def launcher: ApiDigest
  def run(
      id: PluginTaskId,
      args: PluginArgs,
      caller: InvocationContext,
      events: PluginEvents.Invocation,
  ): ZIO[
    ActionCache & RemoteCache & Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService,
    PluginFault,
    PluginOutput,
  ]

  /** Generation-time runners have no initiating caller. The daemon passes the envelope instead. */
  def run(
      id: PluginTaskId,
      args: PluginArgs,
  ): ZIO[
    ActionCache & RemoteCache & Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService,
    PluginFault,
    PluginOutput,
  ] =
    run(id, args, PluginRuntime.standIn(id), PluginEvents.closedInvocation)
  def bind: ZIO[
    ActionCache & RemoteCache & Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService,
    TaskError,
    Map[TaskId, IO[TaskError, TaskResult]],
  ]
  def links: List[PluginLink]
  def committed(id: PluginTaskId): IO[PluginFault, CommittedOutputs]
  def generated(id: PluginTaskId): ZIO[Scope, PluginFault, ProductLocations]
  def previewDirectory(id: PluginTaskId, name: OutputName): ZIO[Scope, PluginFault, Path]
  def cleanProducts(id: PluginTaskId): IO[PluginFault, Unit]
end PluginRuntime

object PluginRuntime:
  private[histos] def failure(error: histos.build.ExecutionError): PluginFault =
    import histos.build.ExecutionError
    import histos.testrun.{ScopedTestFailure, TestFailures, TestNodeFailure}
    error match
      case ExecutionError.Compile(error)       => PluginFault.Compiled(error)
      case ExecutionError.Test(error)          => PluginFault.Tested(error)
      case ExecutionError.Plugin(error)        => error
      case ExecutionError.Link(error)          => PluginFault.Linked(error)
      case ExecutionError.Graph(error)         => PluginFault.Scoped(TestFailures.graph(error))
      case ExecutionError.Roots(error)         => PluginFault.Scoped(TestFailures.root(error))
      case ExecutionError.MissingProduct(node) =>
        PluginFault.Scoped(ScopedTestFailure.Node(TestNodeFailure.Missing, TestFailures.node(node)))
      case ExecutionError.WrongProduct(node) =>
        PluginFault.Scoped(ScopedTestFailure.Node(TestNodeFailure.Wrong, TestFailures.node(node)))
      case ExecutionError.Dependency(node) =>
        PluginFault.Scoped(ScopedTestFailure.Node(TestNodeFailure.Dependency, TestFailures.node(node)))
      case ExecutionError.Skipped(steps) => PluginFault.Scoped(ScopedTestFailure.Skipped(steps))

  /** First line of a cache hit. The stored body is the rest of the text. */
  val CacheHit: String = "histos-cache hit"

  /** The stand-in P8 replaces on the daemon path. It does not read the server environment. */
  def standIn(id: PluginTaskId): InvocationContext =
    InvocationContext(
      InvocationId.derive(id.render),
      ClientId("plugin-runtime"),
      CommandIdentity.Task(id),
      ExecutionKind.Local,
      Interaction.Unattended,
      OfflineSelection.Online,
    )

  def cacheHit(body: String): String =
    s"$CacheHit\n$body"

  private[histos] def loaded(
      repo: Path,
      graph: ProjectGraph,
      loader: ClassLoader,
      threads: PluginThreads,
      links: List[PluginLink],
      launcher: ApiDigest,
      leases: GenerationLeases,
      provenance: List[ProvenanceResult],
  ): ZIO[Scope & PluginProducts, Nothing, PluginRuntime] =
    for
      environment <- (ZLayer.succeed(Parallelism.of(graph.parallelism)) >>> Histos.live).build
      products    <- ZIO.service[PluginProducts]
      retained    <- Ref.make(Map.empty[PluginTaskId, CommittedOutputs])
      inputReads  <- Ref.Synchronized.make(Set.empty[PluginTaskId])
    yield Running(
      repo,
      graph,
      loader,
      threads,
      links,
      launcher,
      environment.get[Histos],
      leases,
      provenance,
      products,
      retained,
      inputReads,
    )

  private final class Running(
      repo: Path,
      graph: ProjectGraph,
      loader: ClassLoader,
      threads: PluginThreads,
      val links: List[PluginLink],
      val launcher: ApiDigest,
      runner: Histos,
      leases: GenerationLeases,
      provenance: List[ProvenanceResult],
      products: PluginProducts,
      retained: Ref[Map[PluginTaskId, CommittedOutputs]],
      inputReads: Ref.Synchronized[Set[PluginTaskId]],
  ) extends PluginRuntime:
    private[histos] def awaitResources: UIO[Unit] = leases.awaitResources
    private[histos] def retire: UIO[Unit]         = leases.retire

    def committed(id: PluginTaskId): IO[PluginFault, CommittedOutputs] =
      retained.get.flatMap(found => ZIO.fromOption(found.get(id)).orElseFail(PluginFault.MissingProduct(id)))

    def generated(id: PluginTaskId): ZIO[Scope, PluginFault, ProductLocations] =
      committed(id).flatMap(_.read.mapError(PluginFault.Products(_)))

    def previewDirectory(id: PluginTaskId, name: OutputName): ZIO[Scope, PluginFault, Path] =
      for
        output  <- committed(id)
        _       <- output.read.mapError(PluginFault.Products(_))
        logical <- ZIO.fromEither(output.locations.directory(name)).mapError(PluginFault.Products(_))
      yield logical

    def cleanProducts(id: PluginTaskId): IO[PluginFault, Unit] =
      taskOf(id) match
        case None       => ZIO.fail(PluginFault.Task(PluginRunError.Unknown(id)))
        case Some(task) =>
          products
            .clean(ProductRequest(repo, task, PluginRuntime.standIn(id).id))
            .mapError(PluginFault.Products(_)) *> retained.update(_ - id)

    def run(
        id: PluginTaskId,
        args: PluginArgs,
        caller: InvocationContext,
        events: PluginEvents.Invocation,
    ): ZIO[
      ActionCache & RemoteCache & Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService,
      PluginFault,
      PluginOutput,
    ] =
      ZIO.scoped {
        for
          machine <- Machine
            .load(repo)
            .mapError(error => PluginFault.Compiled(histos.zinc.GraphCompileError.Runtime(error)))
          jdk     <- GraphCompile.jdk(machine).mapError(PluginFault.Compiled(_))
          lock    <- ZIO.serviceWithZIO[histos.resolve.ResolutionService](_.current)
          cache   <- ZIO.service[ActionCache]
          runtime <- histos.build.BuildRuntime.open(
            Some(this),
            Some(TaskInvocation(id, args, caller, events)),
            Some(cache),
            Parallelism.of(graph.parallelism),
          )
          completed <- runtime
            .ensure(
              histos.build.ExecutionRequest(
                histos.zinc.CompileSession(repo, graph, jdk, machine.native, lock),
                List(histos.build.BuildNode.Generate(id)),
              )
            )
            .mapError(PluginRuntime.failure)
          product <- ZIO.fromEither(completed.require(id)).mapError(PluginRuntime.failure)
        yield product.output
      }

    def generate(
        request: histos.build.ExecutionRequest,
        id: PluginTaskId,
        completed: histos.build.ExecutionProducts,
        invocation: TaskInvocation,
    ): ZIO[
      ActionCache & RemoteCache & Resolve & ContentStore & histos.resolve.ResolutionService,
      histos.build.ExecutionError,
      histos.build.GeneratedTask[histos.build.ExecutionError],
    ] =
      import histos.build.{ExecutionError, GeneratedTask}
      val graph = request.session.graph
      for
        task <- ZIO
          .fromOption(taskOf(id))
          .orElseFail(ExecutionError.Plugin(PluginFault.Task(PluginRunError.Unknown(id))))
        cells <- ZIO
          .fromEither(PluginCells.selected(graph, task))
          .mapError(error => ExecutionError.Plugin(PluginFault.Task(error)))
        compiled <- ZIO.foreach(cells) { cell =>
          val identity = histos.Cell(cell.module.name, cell.version, cell.platform)
          for
            main  <- ZIO.fromEither(completed.require(histos.CompilationStage(identity, histos.plugin.SourceSet.Main)))
            paths <- cell.scope match
              case histos.plugin.SourceSet.Main =>
                ZIO.succeed(
                  histos.plugin.CellClasspath
                    .mainOnly(identity.module, identity.version, identity.platform, main.classpath)
                )
              case histos.plugin.SourceSet.Test =>
                ZIO
                  .fromEither(completed.require(histos.CompilationStage(identity, histos.plugin.SourceSet.Test)))
                  .map(test =>
                    histos.plugin.CellClasspath.testComplete(
                      identity.module,
                      identity.version,
                      identity.platform,
                      main.classpath,
                      test.classpath,
                    )
                  )
          yield paths
        }
        links <- ZIO
          .fromEither(PluginCells.links(graph, task).map(_.distinct))
          .mapError(error => ExecutionError.Plugin(PluginFault.Task(error)))
        linked <- ZIO.foreach(links) { link =>
          val cell = histos.Cell(link.cell.module.name, link.cell.version, link.cell.platform)
          ZIO
            .fromEither(completed.require(cell, link.optimize))
            .map(product => histos.plugin.LinkedScript(cell.module, cell.version, product.script, link.optimize))
        }
        generated <- ZIO.foreach(
          completed.nodes.toList.collect { case histos.build.BuildNode.Generate(task) => task }.sortBy(_.render)
        ) { task =>
          ZIO
            .fromEither(completed.require(task))
            .flatMap(product =>
              ZIO.foreach(product.locations)(location =>
                histos.zinc.CellCapture
                  .implementation(location.path)
                  .mapError(error =>
                    ExecutionError.Plugin(
                      PluginFault
                        .Tested(histos.testrun.ModuleTestError.Scoped(histos.testrun.ScopedTestFailure.Source(error)))
                    )
                  )
              )
            )
            .map(hashes => s"${task.render}:${hashes.map(_.value).mkString(",")}")
        }
        linkKeys <- ZIO.foreach(links)(link =>
          ZIO
            .fromEither(
              completed
                .require(histos.Cell(link.cell.module.name, link.cell.version, link.cell.platform), link.optimize)
            )
            .map(_.key.value)
        )
        upstream = (completed.stages.toList.sortBy(_._1.toString).map { (stage, value) =>
          s"$stage:${value.api.value}:${value.implementation.value}"
        } ++ generated ++ linkKeys).mkString("\n")
        made    <- Ref.make(Map.empty[String, PluginOutput])
        cache   <- ZIO.service[ActionCache]
        remote  <- ZIO.service[RemoteCache]
        machine <- Machine
          .load(repo)
          .mapError(error => ExecutionError.Plugin(PluginFault.Compiled(histos.zinc.GraphCompileError.Runtime(error))))
        _ <- retainInputs(
          completed.nodes.toList.collect { case histos.build.BuildNode.Generate(task) => task }
        ).mapError(ExecutionError.Plugin(_))
        _ <- remembered(
          id,
          id,
          task,
          invocation.args,
          made,
          upstream,
          machine.env,
          Scheduler(runner, cache, remote),
          compiled,
          linked,
          invocation.caller,
          invocation.events,
        )
          .mapError(ExecutionError.Plugin(_))
        outputs <- committed(id).mapError(ExecutionError.Plugin(_))
        output  <- made.get.flatMap(found =>
          ZIO.fromOption(found.get(id.render)).orElseFail(ExecutionError.Plugin(PluginFault.MissingProduct(id)))
        )
      yield GeneratedTask(
        output,
        outputs.read
          .mapError(error => ExecutionError.Plugin(PluginFault.Products(error)))
          .map(locations =>
            locations.values.map(location =>
              histos.zinc.GeneratedLocation(
                histos.zinc.GeneratedRootId(id, location.output.name),
                location.output.kind,
                location.path,
              )
            )
          ),
      )

    def bind: ZIO[
      ActionCache & RemoteCache & Resolve & histos.cache.ContentStore & histos.resolve.ResolutionService,
      TaskError,
      Map[TaskId, IO[TaskError, TaskResult]],
    ] =
      for
        cache   <- ZIO.service[ActionCache]
        remote  <- ZIO.service[RemoteCache]
        resolve <- ZIO.service[Resolve]
        store   <- ZIO.service[histos.cache.ContentStore]
        report  <- ZIO.service[histos.resolve.ResolutionService]
        bound   <- ZIO.foreach(tasks): (id, _) =>
          for
            key <- ZIO.fromEither(TaskId.from(id.render))
            runner = run(id, PluginArgs.empty)
              .provide(
                ZLayer.succeed(cache),
                ZLayer.succeed(remote),
                ZLayer.succeed(resolve),
                ZLayer.succeed(store),
                ZLayer.succeed(report),
              )
              .map(out => TaskResult.Plugin(out))
              .mapError:
                case PluginFault.Task(error) => TaskError.Plugin(error)
                case other                   => TaskError.Fault(other)
          yield key -> runner
      yield bound.toMap

    private def remembered(
        root: PluginTaskId,
        id: PluginTaskId,
        task: DeclaredTask,
        args: PluginArgs,
        made: Ref[Map[String, PluginOutput]],
        upstream: String,
        env: Map[String, String],
        scheduler: Scheduler,
        compiled: List[histos.plugin.CellClasspath],
        linked: List[histos.plugin.LinkedScript],
        caller: InvocationContext,
        events: PluginEvents.Invocation,
    ): ZIO[Resolve & ContentStore, PluginFault, String] =
      val passed = if id == root then args else PluginArgs.empty
      instance(id.plugin) match
        case None         => ZIO.fail(PluginFault.Task(PluginRunError.Unknown(id)))
        case Some(plugin) =>
          for
            hashed <- PluginInputs
              .hash(hashRequest(id, task, env, passed))
              .mapError(error => PluginFault.Task(error))
            inputs <- withUpstream(hashed.inputs, upstream)
            action <- ZIO.fromEither(
              actionOf(id, task, inputs).left.map(error => PluginFault.Task(error))
            )
            store <- ZIO.service[ContentStore]
            request = ProductRequest(repo, task, caller.id)
            info    = StepInfo(StepId.Plugin(id), Chunk.empty, Set(Need.Cpu))
            local   = Step.run(
              Step.local[CommittedOutputs](info)(
                produce(id, task, plugin, passed, made, compiled, linked, env, caller, events)
                  .provide(ZLayer.succeed(store))
              ) @@ PluginAspects.loader(threads)
            )
            result <- scheduler
              .cached(action, ActionDigest.of(action, Map.empty), request, local)
              .provide(ZLayer.succeed(store), ZLayer.succeed(products))
              .mapError:
                case ScheduleError.Failed(_, error) => error
                case ScheduleError.Products(error)  => PluginFault.Products(error)
                case other                          => PluginFault.Scheduled(other)
            _ <- keep(result.outputs.manifest)
            _ <- retained.update(_ + (id -> result.outputs))
            _ <- made.update(found =>
              found + (id.render -> found.getOrElse(
                id.render,
                PluginManifest.output(result.outputs.manifest).copy(text = cacheHit(result.outputs.manifest.text)),
              ))
            )
          yield result.record

    private def withUpstream(
        inputs: List[Input],
        upstream: String,
    ): IO[PluginFault, List[Input]] =
      if upstream.isEmpty then ZIO.succeed(inputs)
      else
        val sha =
          Sha256Digest.of(Chunk.fromArray(upstream.getBytes(StandardCharsets.UTF_8)))
        ZIO
          .fromEither(InputName.from("upstream"))
          .mapBoth(
            _ => PluginFault.Task(PluginRunError.Defect("InputName")),
            name => inputs :+ Input(name, InputValue.Hash(sha)),
          )

    private def produce(
        id: PluginTaskId,
        task: DeclaredTask,
        plugin: HistosPlugin,
        args: PluginArgs,
        made: Ref[Map[String, PluginOutput]],
        compiled: List[histos.plugin.CellClasspath],
        linked: List[histos.plugin.LinkedScript],
        env: Map[String, String],
        caller: InvocationContext,
        events: PluginEvents.Invocation,
    ): ZIO[ContentStore, PluginFault, CommittedOutputs] =
      def call(destinations: OutputDestinations): ZIO[Any, PluginFault, PluginOutput] =
        for
          hashed <- PluginInputs.hash(hashRequest(id, task, env, args)).mapError(PluginFault.Task(_))
          output <- invoke(
            plugin,
            RunRequest(
              id,
              repo,
              views(id.plugin),
              args,
              hashed.material,
              caller,
              events,
              compiled,
              linked,
              destinations,
            ),
          )
            .mapError(PluginFault.Task(_))
        yield output
      ZIO.scoped:
        for
          staged    <- products.stage(ProductRequest(repo, task, caller.id)).mapError(PluginFault.Products(_))
          output    <- call(staged.destinations)
          manifest  <- products.seal(staged, output).mapError(PluginFault.Products(_))
          committed <- ZIO.uninterruptibleMask: restore =>
            for
              committed <- restore(products.commit(staged, manifest).mapError(PluginFault.Products(_)))
              _         <- keep(manifest)
              _         <- made.update(_ + (id.render -> PluginManifest.output(manifest)))
            yield committed
        yield committed

    private def keep(manifest: OutputManifest): ZIO[ContentStore, PluginFault, Unit] =
      ZIO
        .serviceWithZIO[ContentStore](_.retain(CacheProject.of(repo), manifest.contentDigests))
        .mapError(error => PluginFault.Products(ProductError.Cache(error)))

    /** Plugins can extend input-consuming effects into Generation. One reader per producer protects its retained trees
      * across promotions without accumulating readers on every invocation. Logical request paths stay unchanged.
      */
    private def retainInputs(ids: List[PluginTaskId]): IO[PluginFault, Unit] =
      ZIO.scoped {
        leases.lease.mapError(PluginFault.Task(_)) *>
          ZIO.foreachDiscard(ids.distinct.sortBy(_.render)) { id =>
            inputReads.updateZIO { held =>
              if held.contains(id) then ZIO.succeed(held)
              else
                committed(id).flatMap(output => leases.use(output.read).mapError(PluginFault.Products(_)).as(held + id))
            }
          }
      }

    private def invoke(
        plugin: HistosPlugin,
        request: RunRequest,
    ): IO[PluginRunError, PluginOutput] =
      val ran = ZIO
        .suspendSucceed(plugin.run(request))
        .catchSomeDefect(defect => ZIO.fail(classify(defect)))
      ZIO
        .scoped(leases.lease *> ran.provideSomeLayer(PluginScopes.live(leases.generation)))
        .flatMap(leak)
        .catchAll(refuse)

    private def leak(output: PluginOutput): IO[PluginRunError, PluginOutput] =
      refused match
        case Some(binary) => ZIO.fail(PluginRunError.EngineLeak(binary))
        case None         => ZIO.succeed(output)

    private def refuse(err: PluginRunError): IO[PluginRunError, Nothing] =
      refused match
        case Some(binary) => ZIO.fail(PluginRunError.EngineLeak(binary))
        case None         => ZIO.fail(err)

    private def actionOf(
        id: PluginTaskId,
        task: DeclaredTask,
        inputs: List[Input],
    ): Either[PluginRunError, Action] =
      for
        actionId <- ActionId
          .from(id.render)
          .left
          .map(_ => PluginRunError.Defect("ActionId"))
        deps <- dependencies(id.plugin, task)
      yield Action(
        actionId,
        inputs,
        deps,
        Policy.Plugin(
          task.seal,
          task.needs,
          Some(repo),
          task.outputs.map(_.path),
        ),
      )

    private def dependencies(
        plugin: PluginId,
        task: DeclaredTask,
    ): Either[PluginRunError, List[ActionId]] =
      task.dependsOn.foldLeft[Either[PluginRunError, List[ActionId]]](
        Right(Nil)
      ):
        case (Left(err), _)                                                => Left(err)
        case (Right(acc), histos.plugin.TaskDep.Compile(_, _, _))          => Right(acc)
        case (Right(acc), histos.plugin.TaskDep.Link(_, _, _))             => Right(acc)
        case (Right(acc), histos.plugin.TaskDep.CompileScoped(_, _, _, _)) => Right(acc)
        case (Right(acc), histos.plugin.TaskDep.EmitScoped(_, _, _, _, _)) => Right(acc)
        case (Right(acc), histos.plugin.TaskDep.Generate(_))               => Right(acc)
        case (Right(acc), histos.plugin.TaskDep.Emit(_))                   => Right(acc)
        case (Right(acc), histos.plugin.TaskDep.Same(name))                =>
          ActionId
            .from(PluginTaskId(plugin, name).render)
            .left
            .map(_ => PluginRunError.Defect("ActionId"))
            .map(dep => acc :+ dep)

    private def tasks: List[(PluginTaskId, DeclaredTask)] =
      graph.plugins.flatMap: plugin =>
        plugin.contribution.tasks.map(task => PluginTaskId(plugin.id, task.name) -> task)

    private def taskOf(id: PluginTaskId): Option[DeclaredTask] =
      tasks.collectFirst:
        case (found, task) if found == id => task

    private def instance(id: PluginId): Option[HistosPlugin] =
      graph.plugins.collectFirst:
        case registered if registered.id == id => registered.plugin

    private def hashRequest(
        id: PluginTaskId,
        task: DeclaredTask,
        env: Map[String, String],
        args: PluginArgs,
    ): PluginInputs.HashRequest =
      PluginInputs.HashRequest(
        repo,
        graph,
        id.plugin,
        task,
        id,
        jars(id.plugin),
        launcher,
        env,
        args,
        binding(id.plugin),
        provenance,
        classpath,
        dependencyInputs = Some(Nil),
      )

    private val classpath: List[Path] =
      loader match
        case build: BuildLoader => build.getURLs.toList.flatMap(fileOf)
        case _                  => Nil

    private def fileOf(url: URL): Option[Path] =
      if url.getProtocol != "file" then None
      else Try(Path.of(url.toURI)).toOption

    private def binding(id: PluginId): Option[PluginLinked] =
      links.collectFirst:
        case link if link.id == id => link.linked

    private def jars(id: PluginId): List[String] =
      links.collectFirst:
        case link if link.id == id => link.artifacts
      match
        case Some(found) => found
        case None        => Nil

    private def views(plugin: PluginId): List[ModuleView] =
      graph.projects.collect:
        case module: Module =>
          ModuleView(
            module.name,
            module.baseDirectory,
            module.notes.filter(_.plugin == plugin),
          )

    private def refused: Option[String] =
      loader match
        case build: BuildLoader => build.takeRefused.map(_.binary)
        case _                  => None

    private def classify(err: Throwable): PluginRunError =
      err match
        case missing: ClassNotFoundException =>
          PluginRunError.Linkage(binary(missing))
        case missing: NoClassDefFoundError =>
          PluginRunError.Linkage(binary(missing))
        case other => PluginRunError.Defect(other.getClass.getName)

    /** The JDK stores the missing binary name on the exception. The name is recorded. It is not compared.
      */
    private def binary(err: Throwable): String =
      Option(err.getMessage)
        .map(_.trim)
        .filter(_.nonEmpty)
        .getOrElse(err.getClass.getName)

  end Running
end PluginRuntime
