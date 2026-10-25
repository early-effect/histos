package histos.load

import histos.jdk.{Machine, Sha256}
import histos.plugin.{
  ApiDigest,
  DeclaredTask,
  HistosPlugin,
  ModuleView,
  PluginArgs,
  PluginDetail,
  PluginId,
  PluginLinked,
  PluginOutput,
  PluginRunError,
  PluginTaskId,
  RunRequest,
  WrittenFile
}
import histos.schedule.Histos.given
import histos.schedule.{
  Action,
  ActionCache,
  ActionId,
  Digest,
  Histos,
  Input,
  InputName,
  InputValue,
  Outcome,
  Parallelism,
  PluginManifest,
  Policy,
  RemoteCache,
  ScheduleError,
  Scheduler,
  Sendable,
  StepFailure
}
import histos.step.{Need, Step, StepId, StepInfo}
import histos.task.{TaskError, TaskId, TaskResult}
import histos.resolve.{Resolve, DependencyLock}
import histos.zinc.GraphCompile
import histos.{Module, ProjectGraph}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}

import zio.{Chunk, IO, NonEmptyChunk, Ref, Scope, ZIO, ZLayer}

final case class PluginLink(
    id: PluginId,
    linked: PluginLinked,
    artifacts: List[String]
)

/** Loaded plugin classes. The load `Scope` owns `loader` and `threads` and
  * closes them when the caller finishes. `run` uses an inner `Scope`, so a
  * plugin's finalizer runs when the task ends, including on interrupt.
  */
trait PluginRuntime:
  def run(
      id: PluginTaskId,
      args: PluginArgs
  ): ZIO[ActionCache & RemoteCache & Resolve, PluginFault, PluginOutput]
  def bind: ZIO[
    ActionCache & RemoteCache & Resolve,
    TaskError,
    Map[TaskId, IO[TaskError, TaskResult]]
  ]
  def links: List[PluginLink]

object PluginRuntime:
  /** First line of a cache hit. The stored body is the rest of the text. */
  val CacheHit: String = "histos-cache hit"

  def cacheHit(body: String): String =
    s"$CacheHit\n$body"

  private[histos] def loaded(
      repo: Path,
      graph: ProjectGraph,
      loader: ClassLoader,
      threads: PluginThreads,
      instances: List[HistosPlugin],
      links: List[PluginLink],
      launcher: ApiDigest
  ): ZIO[Scope, Nothing, PluginRuntime] =
    (Parallelism.live >>> Histos.live).build.map: environment =>
      Running(
        repo,
        graph,
        loader,
        threads,
        instances,
        links,
        launcher,
        environment.get[Histos]
      )

  private final class Running(
      repo: Path,
      graph: ProjectGraph,
      loader: ClassLoader,
      threads: PluginThreads,
      instances: List[HistosPlugin],
      val links: List[PluginLink],
      launcher: ApiDigest,
      runner: Histos
  ) extends PluginRuntime:
    def run(
        id: PluginTaskId,
        args: PluginArgs
    ): ZIO[ActionCache & RemoteCache & Resolve, PluginFault, PluginOutput] =
      taskOf(id) match
        case None    => ZIO.fail(PluginFault.Task(PluginRunError.Unknown(id)))
        case Some(_) =>
          for
            made <- Ref.make(Map.empty[String, PluginOutput])
            kept <- ZIO.service[ActionCache]
            remote <- ZIO.service[RemoteCache]
            machine <- Machine
              .load(repo)
              .mapError(error =>
                PluginFault.Compiled(
                  histos.zinc.GraphCompileError.Runtime(error)
                )
              )
            env = machine.env
            lock <- Resolve
              .readLock(repo)
              .mapError(PluginFault.Resolved(_))
              .memoize
            jdk <- GraphCompile
              .jdk(machine)
              .mapError(PluginFault.Compiled(_))
              .memoize
            session = jdk
              .zip(lock)
              .map((runtime, captured) =>
                histos.zinc.CompileSession(
                  repo,
                  graph,
                  runtime,
                  machine.native,
                  captured
                )
              )
            scheduler = Scheduler(runner, kept, remote)
            step <- ZIO.fromEither(
              PluginSteps.goal(
                id,
                taskOf,
                task => PluginCells.selected(graph, task),
                cell => session.flatMap(PluginCells.compile(_, cell)),
                (taskId, task, upstream) =>
                  remembered(
                    id,
                    taskId,
                    task,
                    args,
                    made,
                    upstream,
                    env,
                    scheduler,
                    lock
                  )
              )
            )
            outcome <- ZIO.scoped(runner.start(step).flatMap(_.outcome))
            output <- outcome match
              case Outcome.Done(text) =>
                made.get
                  .map(_.get(id.render))
                  .map:
                    case Some(found) => found
                    case None        => cached(id, text)
              case Outcome.Failed(failures) => failed(failures)
          yield output

    def bind: ZIO[
      ActionCache & RemoteCache & Resolve,
      TaskError,
      Map[TaskId, IO[TaskError, TaskResult]]
    ] =
      for
        cache <- ZIO.service[ActionCache]
        remote <- ZIO.service[RemoteCache]
        resolve <- ZIO.service[Resolve]
        bound <- ZIO.foreach(tasks): (id, _) =>
          for
            key <- ZIO.fromEither(TaskId.from(id.render))
            runner = run(id, PluginArgs.empty)
              .provide(
                ZLayer.succeed(cache),
                ZLayer.succeed(remote),
                ZLayer.succeed(resolve)
              )
              .map(out => TaskResult.Plugin(out))
              .mapError:
                case PluginFault.Task(error) => TaskError.Plugin(error)
                case other                   =>
                  TaskError.Plugin(
                    PluginRunError.Failed(PluginDetail.trusted(other.message))
                  )
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
        lock: IO[PluginFault, DependencyLock]
    ): ZIO[Resolve, PluginFault, String] =
      val passed = if id == root then args else PluginArgs.empty
      instance(id.plugin) match
        case None => ZIO.fail(PluginFault.Task(PluginRunError.Unknown(id)))
        case Some(plugin) =>
          for
            hashed <- PluginInputs
              .hash(hashRequest(id, task, env))
              .mapError(error => PluginFault.Task(error))
            inputs <- withUpstream(hashed.inputs, upstream)
            compiled <- PluginCells.classpath(repo, graph, task, lock)
            action <- ZIO.fromEither(
              actionOf(id, task, inputs).left.map(error =>
                PluginFault.Task(error)
              )
            )
            held <- Ref.make[Option[PluginOutput]](None)
            info = StepInfo(StepId.Plugin(id), Chunk.empty, Set(Need.Cpu))
            local = Step.run(
              Step.hermetic[String](info)(
                produce(id, task, plugin, passed, made, compiled, held, env)
                  .mapError(error => PluginFault.Task(error))
              ) @@ PluginAspects.loader(threads) @@ PluginAspects
                .declaredOutputs(repo, task, held)
            )
            text <- scheduler
              .cached(
                action,
                Digest.of(action, Map.empty),
                local,
                Sendable.ofAction
              )
              .mapError:
                case ScheduleError.Failed(_, error) => error
                case other => PluginFault.Scheduled(other)
          yield text

    private def withUpstream(
        inputs: List[Input],
        upstream: String
    ): IO[PluginFault, List[Input]] =
      if upstream.isEmpty then ZIO.succeed(inputs)
      else
        val sha =
          Sha256.of(Chunk.fromArray(upstream.getBytes(StandardCharsets.UTF_8)))
        ZIO
          .fromEither(InputName.from("upstream"))
          .mapBoth(
            _ => PluginFault.Task(PluginRunError.Defect("InputName")),
            name => inputs :+ Input(name, InputValue.Hash(sha))
          )

    private def failed(
        failures: NonEmptyChunk[StepFailure[PluginFault]]
    ): IO[PluginFault, Nothing] =
      failures.head match
        case StepFailure.Failed(_, cause) =>
          cause.failureOption match
            case Some(error) => ZIO.fail(error)
            case None        =>
              ZIO.dieMessage("plugin step failed without a typed error")
        case StepFailure.Skipped(_, _) =>
          ZIO.fail(PluginFault.Task(PluginRunError.Defect("skipped")))

    private def produce(
        id: PluginTaskId,
        task: DeclaredTask,
        plugin: HistosPlugin,
        args: PluginArgs,
        made: Ref[Map[String, PluginOutput]],
        compiled: List[histos.plugin.CellClasspath],
        held: Ref[Option[PluginOutput]],
        env: Map[String, String]
    ): IO[PluginRunError, String] =
      for
        hashed <- PluginInputs.hash(hashRequest(id, task, env))
        output <- invoke(
          plugin,
          RunRequest(
            id,
            repo,
            views(id.plugin),
            args,
            hashed.material,
            compiled
          )
        )
        _ <- check(task, output)
        _ <- held.set(Some(output))
        text <- record(task, output)
        _ <- made.update(_ + (id.render -> output))
      yield text

    private def invoke(
        plugin: HistosPlugin,
        request: RunRequest
    ): IO[PluginRunError, PluginOutput] =
      val ran = ZIO
        .suspendSucceed(plugin.run(request))
        .catchSomeDefect(defect => ZIO.fail(classify(defect)))
      ZIO.scoped(ran).flatMap(leak).catchAll(refuse)

    private def leak(output: PluginOutput): IO[PluginRunError, PluginOutput] =
      refused match
        case Some(binary) => ZIO.fail(PluginRunError.EngineLeak(binary))
        case None         => ZIO.succeed(output)

    private def refuse(err: PluginRunError): IO[PluginRunError, Nothing] =
      refused match
        case Some(binary) => ZIO.fail(PluginRunError.EngineLeak(binary))
        case None         => ZIO.fail(err)

    private def check(
        task: DeclaredTask,
        output: PluginOutput
    ): IO[PluginRunError, Unit] =
      val declared = task.outputs.map(_.path.value).toSet
      output.files.find(file => !declared.contains(file.path.value)) match
        case Some(extra) =>
          ZIO.fail(PluginRunError.ExtraOutput(extra.path.value))
        case None =>
          ZIO.foreachDiscard(task.outputs): declaredOutput =>
            val file = repo.resolve(declaredOutput.path.value)
            ZIO
              .attemptBlocking(
                Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)
              )
              .orElseFail(PluginRunError.MissingOutput(declaredOutput.name))
              .flatMap:
                case true  => ZIO.unit
                case false =>
                  ZIO.fail(PluginRunError.MissingOutput(declaredOutput.name))

    private def record(
        task: DeclaredTask,
        output: PluginOutput
    ): IO[PluginRunError, String] =
      if task.outputs.isEmpty then ZIO.succeed(output.text)
      else
        ZIO
          .attemptBlocking {
            val files = task.outputs.map: declaredOutput =>
              val file = repo.resolve(declaredOutput.path.value)
              declaredOutput.path -> Sha256.of(
                Chunk.fromArray(Files.readAllBytes(file))
              )
            PluginManifest.store(output.text, files)
          }
          .orElseFail(PluginRunError.Defect("PluginManifest"))

    private def actionOf(
        id: PluginTaskId,
        task: DeclaredTask,
        inputs: List[Input]
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
          task.outputs.map(_.path)
        )
      )

    private def dependencies(
        plugin: PluginId,
        task: DeclaredTask
    ): Either[PluginRunError, List[ActionId]] =
      task.dependsOn.foldLeft[Either[PluginRunError, List[ActionId]]](
        Right(Nil)
      ):
        case (Left(err), _)                                       => Left(err)
        case (Right(acc), histos.plugin.TaskDep.Compile(_, _, _)) => Right(acc)
        case (Right(acc), histos.plugin.TaskDep.Same(name))       =>
          ActionId
            .from(PluginTaskId(plugin, name).render)
            .left
            .map(_ => PluginRunError.Defect("ActionId"))
            .map(dep => acc :+ dep)

    private def cached(id: PluginTaskId, stored: String): PluginOutput =
      taskOf(id) match
        case Some(task) if task.outputs.nonEmpty =>
          PluginOutput(
            cacheHit(PluginManifest.text(stored)),
            task.outputs.map(output => WrittenFile(output.name, output.path))
          )
        case _ => PluginOutput(cacheHit(stored), Nil)

    private def tasks: List[(PluginTaskId, DeclaredTask)] =
      graph.plugins.flatMap: plugin =>
        plugin.contribution.tasks.map(task =>
          PluginTaskId(plugin.id, task.name) -> task
        )

    private def taskOf(id: PluginTaskId): Option[DeclaredTask] =
      tasks.collectFirst:
        case (found, task) if found == id => task

    private def instance(id: PluginId): Option[HistosPlugin] =
      instances.find(_.id == id)

    private def hashRequest(
        id: PluginTaskId,
        task: DeclaredTask,
        env: Map[String, String]
    ): PluginInputs.HashRequest =
      PluginInputs.HashRequest(
        repo,
        graph,
        id.plugin,
        task,
        id,
        jars(id.plugin),
        launcher,
        env
      )

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
            module.notes.filter(_.plugin == plugin)
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

    /** The JDK stores the missing binary name on the exception. The name is
      * recorded. It is not compared.
      */
    private def binary(err: Throwable): String =
      Option(err.getMessage)
        .map(_.trim)
        .filter(_.nonEmpty)
        .getOrElse(err.getClass.getName)

  end Running
end PluginRuntime
