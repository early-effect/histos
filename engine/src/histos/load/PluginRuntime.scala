package histos.load

import histos.jdk.Sha256
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
  WrittenFile,
}
import histos.schedule.{Action, ActionCache, ActionId, Input, PluginManifest, Policy, RemoteCache, ScheduleError, Scheduler}
import histos.task.{TaskError, TaskId, TaskKind, TaskResult}
import histos.resolve.Resolve
import histos.{Module, ProjectGraph}

import java.nio.file.{Files, LinkOption, Path}

import zio.{Chunk, IO, Ref, ZIO, ZLayer}

final case class PluginLink(id: PluginId, linked: PluginLinked, artifacts: List[String])

/**
 * Loaded plugin classes. The load `Scope` owns `loader` and closes it when the caller finishes.
 * `run` uses an inner `Scope`, so a plugin's finalizer runs when the task ends, including on interrupt.
 */
trait PluginRuntime:
  def run(id: PluginTaskId, args: PluginArgs): ZIO[ActionCache & RemoteCache & Resolve, PluginRunError, PluginOutput]
  def bind: ZIO[ActionCache & RemoteCache & Resolve, Nothing, Map[TaskId, IO[TaskError, TaskResult]]]
  def links: List[PluginLink]

object PluginRuntime:
  private[histos] def loaded(
      repo: Path,
      graph: ProjectGraph,
      loader: ClassLoader,
      instances: List[HistosPlugin],
      links: List[PluginLink],
      launcher: ApiDigest,
  ): PluginRuntime =
    Running(repo, graph, loader, instances, links, launcher)

  private final class Running(
      repo: Path,
      graph: ProjectGraph,
      loader: ClassLoader,
      instances: List[HistosPlugin],
      val links: List[PluginLink],
      launcher: ApiDigest,
  ) extends PluginRuntime:
    def run(id: PluginTaskId, args: PluginArgs): ZIO[ActionCache & RemoteCache & Resolve, PluginRunError, PluginOutput] =
      taskOf(id) match
        case None => ZIO.fail(PluginRunError.Unknown(id))
        case Some(_) =>
          for
            compiled <- classpaths(id)
            made     <- Ref.make(Map.empty[String, PluginOutput])
            planned  <- plan
            stored   <- planned.find(_.id.value == id.render) match
                          case None => ZIO.fail(PluginRunError.Unknown(id))
                          case Some(root) =>
                            ZIO.serviceWithZIO[Scheduler](_.execute(root.id, planned, local(id, args, made, compiled)))
                              .provideSome[ActionCache & RemoteCache](Scheduler.live)
                              .mapError(blown)
            output <- made.get.map(_.get(id.render)).flatMap:
                        case Some(found) => ZIO.succeed(found)
                        case None        => ZIO.succeed(cached(id, stored))
          yield output

    def bind: ZIO[ActionCache & RemoteCache & Resolve, Nothing, Map[TaskId, IO[TaskError, TaskResult]]] =
      for
        cache   <- ZIO.service[ActionCache]
        remote  <- ZIO.service[RemoteCache]
        resolve <- ZIO.service[Resolve]
      yield
        val bound = tasks.flatMap: (id, _) =>
          TaskId.from(id.render).toOption.toList.map: key =>
            key -> run(id, PluginArgs.empty)
              .provide(ZLayer.succeed(cache), ZLayer.succeed(remote), ZLayer.succeed(resolve))
              .map(out => TaskResult(TaskKind.Plugin, out.text))
              .mapError(TaskError.Plugin(_))
        bound.toMap

    private def classpaths(root: PluginTaskId): ZIO[Resolve, PluginRunError, Map[String, List[histos.plugin.CellClasspath]]] =
      val found = reachable(root)
      PluginCells.prepare(repo, graph, found.map(_._2)) *>
        ZIO.foreach(found): (id, task) =>
          PluginCells.classpath(repo, graph, task).map(id.render -> _)
        .map(_.toMap)

    private def local(
        root: PluginTaskId,
        args: PluginArgs,
        made: Ref[Map[String, PluginOutput]],
        compiled: Map[String, List[histos.plugin.CellClasspath]],
    )(action: Action): IO[PluginRunError, String] =
      if action.kind == TaskKind.Compile then ZIO.succeed("compiled")
      else
        PluginTaskId.parse(action.id.value) match
          case Left(_) => ZIO.fail(PluginRunError.Defect("ActionId"))
          case Right(id) =>
            val passed = if id == root then args else PluginArgs.empty
            (taskOf(id), instance(id.plugin)) match
              case (Some(task), Some(plugin)) => produce(id, task, plugin, passed, made, compiled.getOrElse(id.render, Nil))
              case _                          => ZIO.fail(PluginRunError.Unknown(id))

    private def produce(
        id: PluginTaskId,
        task: DeclaredTask,
        plugin: HistosPlugin,
        args: PluginArgs,
        made: Ref[Map[String, PluginOutput]],
        compiled: List[histos.plugin.CellClasspath],
    ): IO[PluginRunError, String] =
      for
        hashed <- PluginInputs.hash(repo, graph, id.plugin, task, id, jars(id.plugin), launcher)
        output <- invoke(plugin, RunRequest(id, repo, views(id.plugin), args, hashed.material, compiled))
        _      <- check(task, output)
        text   <- record(task, output)
        _      <- made.update(_ + (id.render -> output))
      yield text

    private def invoke(plugin: HistosPlugin, request: RunRequest): IO[PluginRunError, PluginOutput] =
      val ran =
        ZIO.acquireReleaseWith(swap(loader))(restoreLoader): _ =>
          ZIO.scoped(plugin.run(request).catchSomeDefect(defect => ZIO.fail(classify(defect))))
      ran.flatMap(leak).catchAll(refuse)

    private def leak(output: PluginOutput): IO[PluginRunError, PluginOutput] =
      refused match
        case Some(binary) => ZIO.fail(PluginRunError.EngineLeak(binary))
        case None         => ZIO.succeed(output)

    private def refuse(err: PluginRunError): IO[PluginRunError, Nothing] =
      refused match
        case Some(binary) => ZIO.fail(PluginRunError.EngineLeak(binary))
        case None         => ZIO.fail(err)

    private def check(task: DeclaredTask, output: PluginOutput): IO[PluginRunError, Unit] =
      val declared = task.outputs.map(_.path.value).toSet
      output.files.find(file => !declared.contains(file.path.value)) match
        case Some(extra) => ZIO.fail(PluginRunError.ExtraOutput(extra.path.value))
        case None =>
          ZIO.foreachDiscard(task.outputs): declaredOutput =>
            val file = repo.resolve(declaredOutput.path.value)
            ZIO.attempt(Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS))
              .orElseFail(PluginRunError.MissingOutput(declaredOutput.name))
              .flatMap:
                case true  => ZIO.unit
                case false => ZIO.fail(PluginRunError.MissingOutput(declaredOutput.name))

    private def record(task: DeclaredTask, output: PluginOutput): IO[PluginRunError, String] =
      if task.outputs.isEmpty then ZIO.succeed(output.text)
      else
        ZIO.attempt {
          val files = task.outputs.map: declaredOutput =>
            val file = repo.resolve(declaredOutput.path.value)
            declaredOutput.path -> Sha256.of(Chunk.fromArray(Files.readAllBytes(file)))
          PluginManifest.store(output.text, files)
        }.orElseFail(PluginRunError.Defect("PluginManifest"))

    private def plan: IO[PluginRunError, List[Action]] =
      for
        built <- ZIO.foreach(tasks): (id, task) =>
          for
            hashed <- PluginInputs.hash(repo, graph, id.plugin, task, id, jars(id.plugin), launcher)
            action <- ZIO.fromEither(actionOf(id, task, hashed.inputs))
          yield action
        compiled <- ZIO.fromEither(compileActions)
      yield compiled ++ built

    private def actionOf(id: PluginTaskId, task: DeclaredTask, inputs: List[Input]): Either[PluginRunError, Action] =
      for
        actionId <- ActionId.from(id.render).left.map(_ => PluginRunError.Defect("ActionId"))
        deps     <- dependencies(id.plugin, task)
      yield Action(actionId, TaskKind.Plugin, inputs, deps, Policy.Plugin(task.seal, task.needs, Some(repo), task.outputs.map(_.path)))

    private def dependencies(plugin: PluginId, task: DeclaredTask): Either[PluginRunError, List[ActionId]] =
      task.dependsOn.foldLeft[Either[PluginRunError, List[ActionId]]](Right(Nil)):
        case (Left(err), _) => Left(err)
        case (Right(acc), histos.plugin.TaskDep.Compile(name, version, platform)) =>
          PluginCells.named(graph, name, version, platform).flatMap: cells =>
            cells.foldLeft[Either[PluginRunError, List[ActionId]]](Right(acc)): (inner, cell) =>
              inner.flatMap: ids =>
                ActionId.from(PluginCells.id(cell)).left.map(_ => PluginRunError.Defect("ActionId")).map(ids :+ _)
        case (Right(acc), histos.plugin.TaskDep.Same(name)) =>
          ActionId.from(PluginTaskId(plugin, name).render).left.map(_ => PluginRunError.Defect("ActionId")).map(dep => acc :+ dep)

    private def compileActions: Either[PluginRunError, List[Action]] =
      tasks.map(_._2).foldLeft[Either[PluginRunError, List[Action]]](Right(Nil)): (acc, task) =>
        acc.flatMap: soFar =>
          PluginCells.selected(graph, task).flatMap: cells =>
            cells.foldLeft[Either[PluginRunError, List[Action]]](Right(soFar)): (inner, cell) =>
              inner.flatMap: actions =>
                if actions.exists(_.id.value == PluginCells.id(cell)) then Right(actions)
                else
                  ActionId.from(PluginCells.id(cell)).left.map(_ => PluginRunError.Defect("ActionId")).map: id =>
                    actions :+ Action(id, TaskKind.Compile, Nil, Nil)

    private def reachable(root: PluginTaskId): List[(PluginTaskId, DeclaredTask)] =
      def walk(id: PluginTaskId, seen: Set[String]): List[(PluginTaskId, DeclaredTask)] =
        if seen.contains(id.render) then Nil
        else
          taskOf(id) match
            case None => Nil
            case Some(task) =>
              val rest = task.dependsOn.collect:
                case histos.plugin.TaskDep.Same(name) => PluginTaskId(id.plugin, name)
              (id, task) :: rest.flatMap(next => walk(next, seen + id.render))
      walk(root, Set.empty)

    private def cached(id: PluginTaskId, stored: String): PluginOutput =
      taskOf(id) match
        case Some(task) if task.outputs.nonEmpty =>
          PluginOutput(PluginManifest.text(stored), task.outputs.map(output => WrittenFile(output.name, output.path)))
        case _ => PluginOutput(stored, Nil)

    private def tasks: List[(PluginTaskId, DeclaredTask)] =
      graph.plugins.flatMap: plugin =>
        plugin.contribution.tasks.map(task => PluginTaskId(plugin.id, task.name) -> task)

    private def taskOf(id: PluginTaskId): Option[DeclaredTask] =
      tasks.collectFirst:
        case (found, task) if found == id => task

    private def instance(id: PluginId): Option[HistosPlugin] =
      instances.find(_.id == id)

    private def jars(id: PluginId): List[String] =
      links.collectFirst:
        case link if link.id == id => link.artifacts
      match
        case Some(found) => found
        case None        => Nil

    private def views(plugin: PluginId): List[ModuleView] =
      graph.projects.collect:
        case module: Module =>
          ModuleView(module.name, module.baseDirectory, module.notes.filter(_.plugin == plugin))

    private def refused: Option[String] =
      loader match
        case build: BuildLoader => build.takeRefused
        case _                  => None

    private def swap(next: ClassLoader): ZIO[Any, Nothing, ClassLoader] =
      ZIO.succeed {
        val previous = Thread.currentThread.getContextClassLoader
        Thread.currentThread.setContextClassLoader(next)
        previous
      }

    private def restoreLoader(previous: ClassLoader): ZIO[Any, Nothing, Unit] =
      ZIO.succeed(Thread.currentThread.setContextClassLoader(previous)).ignore

    private def classify(err: Throwable): PluginRunError =
      err match
        case missing: ClassNotFoundException => PluginRunError.Linkage(binary(missing))
        case missing: NoClassDefFoundError   => PluginRunError.Linkage(binary(missing))
        case other                           => PluginRunError.Defect(other.getClass.getName)

    /** The JDK stores the missing binary name on the exception. The name is recorded. It is not compared. */
    private def binary(err: Throwable): String =
      Option(err.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(err.getClass.getName)

    private def blown(error: ScheduleError[PluginRunError]): PluginRunError =
      error match
        case ScheduleError.Failed(_, inner) => inner
        case other =>
          val text = other.message.replace('\n', ' ').trim
          val line = if text.isEmpty then "plugin task failed" else text
          PluginRunError.Failed(PluginDetail.trusted(line))
  end Running
end PluginRuntime
