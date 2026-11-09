package histos.server

import histos.cli.Lines
import histos.plugin.{DeclaredInput, PluginTaskId}
import histos.report.{BuildFault, CallResult, Rejection}
import histos.watch.SourceWatch
import histos.publish.PublishSelect
import histos.{Module, ProjectGraph, ProjectName}

import java.nio.file.Path

import zio.{Exit, Promise, Ref, ZIO}

/** Run one operation, then again after a source save. The inner exit does not end the session.
  */
object Watching:
  /** Poll beside a running program. `again` sees the paths that changed and says whether the program is replaced.
    * `false` leaves it running and polls again. The program exiting ends the session. Cancelling the watch interrupts
    * the program, and the caller stops the child.
    */
  def beside[R](
      say: Say,
      run: ZIO[R, ServerError, Unit],
      roots: ZIO[R, ServerError, Watched],
      again: List[Path] => ZIO[R, ServerError, Boolean],
  ): ZIO[R, ServerError, Unit] =
    Ref
      .make(Option.empty[zio.Fiber[ServerError, Unit]])
      .flatMap: current =>
        def remember(
            child: zio.Fiber[ServerError, Unit],
            stay: Boolean,
        ): ZIO[Any, Nothing, Option[zio.Fiber[ServerError, Unit]]] =
          val next = if stay then Some(child) else None
          current.set(next).as(next)
        def cycle(
            start: Option[SourceWatch.Snapshot],
            running: Option[zio.Fiber[ServerError, Unit]],
        ): ZIO[R & zio.Scope, ServerError, Unit] =
          // The child outlives one cycle, so a refused change can leave it running. Cancelling the watch interrupts it.
          val fiber = running match
            case Some(live) => ZIO.succeed(live)
            case None       => run.forkScoped
          fiber.flatMap: child =>
            for
              done    <- Promise.make[Nothing, Unit]
              _       <- child.await.foldCauseZIO(_ => done.succeed(()), _ => done.succeed(())).fork
              watched <- roots
              origin  <- baseline(say, watched, start)
              woke    <- poll(say, watched, origin).race(done.await.as(List.empty[Path]))
              stay    <- held(child, woke, again)
              _       <- ZIO.unless(stay)(child.interrupt)
              next    <- remember(child, stay)
              _       <- roots.flatMap: held =>
                SourceWatch
                  .take(held.paths, held.excluded)
                  .foldZIO(
                    error => Calls.finish(say, CallResult.Fault(BuildFault.Watch(error))),
                    now => cycle(Some(now), next),
                  )
            yield ()
        ZIO
          .scoped(cycle(None, None))
          .ensuring(current.get.flatMap(ZIO.foreachDiscard(_)(_.interrupt)))
          .foldCauseZIO(
            cause =>
              if cause.isInterruptedOnly then Calls.finish(say, CallResult.Fault(BuildFault.Interrupted))
              else
                cause.failureOrCause match
                  case Left(error) => ZIO.fail(error)
                  case Right(_)    => Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.WatchStopped))),
            _ => ZIO.unit,
          )

  private def baseline[R](
      say: Say,
      watched: Watched,
      start: Option[SourceWatch.Snapshot],
  ): ZIO[R, ServerError, SourceWatch.Snapshot] =
    start match
      case Some(taken) => ZIO.succeed(taken)
      case None        =>
        SourceWatch
          .take(watched.paths, watched.excluded)
          .foldZIO(
            error => Calls.finish(say, CallResult.Fault(BuildFault.Watch(error))).as(SourceWatch.Snapshot(Nil)),
            taken => Calls.tell(say, "watching").as(taken),
          )

  private def poll[R](
      say: Say,
      watched: Watched,
      origin: SourceWatch.Snapshot,
  ): ZIO[R, ServerError, List[Path]] =
    SourceWatch
      .since(watched.paths, origin, watched.excluded)
      .foldZIO(
        error => Calls.finish(say, CallResult.Fault(BuildFault.Watch(error))).as(List.empty[Path]),
        paths => ZIO.succeed(paths),
      )

  /** A program that finished is not kept. A source change is kept when `again` says the running program stays. */
  private def held[R, E](
      fiber: zio.Fiber[E, Unit],
      woke: List[Path],
      again: List[Path] => ZIO[R, E, Boolean],
  ): ZIO[R, E, Boolean] =
    if woke.isEmpty then
      fiber.await.flatMap:
        case Exit.Success(_)     => ZIO.succeed(false)
        case Exit.Failure(cause) => ZIO.failCause(cause)
    else again(woke).map(!_)

  def session[R](
      repo: Path,
      say: Say,
      run: ZIO[R, ServerError, Unit],
      roots: ZIO[R, ServerError, Watched],
  ): ZIO[R, ServerError, Unit] =
    Ref.make(false).flatMap { running =>
      def loop: ZIO[R, ServerError, Unit] =
        running.set(true) *>
          run *>
          running.set(false) *>
          roots.flatMap { watched =>
            // The baseline is taken before the line. A save during the walk is not the change that woke the watch.
            SourceWatch
              .take(watched.paths, watched.excluded)
              .foldZIO(
                error => Calls.finish(say, CallResult.Fault(BuildFault.Watch(error))),
                start =>
                  Calls.tell(say, "watching") *>
                    SourceWatch
                      .since(watched.paths, start, watched.excluded)
                      .foldZIO(
                        error => Calls.finish(say, CallResult.Fault(BuildFault.Watch(error))),
                        woke =>
                          ZIO.foreachDiscard(woke)(path =>
                            Calls.tell(
                              say,
                              Lines.watching(Lines.relative(repo, path)),
                            )
                          ) *> loop,
                      ),
              )
          }
      loop.foldCauseZIO(
        cause =>
          if cause.isInterruptedOnly then
            running.get.flatMap { on =>
              Calls.finish(
                say,
                if on then CallResult.Fault(BuildFault.Interrupted) else CallResult.reported(),
              )
            }
          else
            cause.failureOrCause match
              case Left(error) => ZIO.fail(error)
              case Right(_)    =>
                Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.WatchStopped))),
        _ => ZIO.unit,
      )
    }

  /** `path` is the root or a file the root walk would see. */
  def covers(root: Path, path: Path): Boolean =
    val base = root.toAbsolutePath.normalize
    val file = path.toAbsolutePath.normalize
    file == base || file.startsWith(base)

  final case class Watched(paths: List[Path], excluded: Set[Path])

  def roots(repo: Path, call: Call, graph: ProjectGraph): List[Path] =
    watched(repo, call, graph).paths

  /** A plugin task watches the files, directories, and modules it declared. A task with no file inputs watches the
    * build file. Declared outputs are excluded, including one that sits inside a watched directory.
    */
  def watched(repo: Path, call: Call, graph: ProjectGraph): Watched =
    call match
      case Call.Task(id, _) =>
        PluginTaskId.parse(id) match
          case Left(_)       => Watched(List(repo.resolve("histos.scala")), Set.empty)
          case Right(taskId) =>
            graph.plugins
              .find(_.id == taskId.plugin)
              .flatMap(_.contribution.tasks.find(_.name == taskId.name)) match
              case None       => Watched(List(repo.resolve("histos.scala")), Set.empty)
              case Some(task) =>
                val declared = task.inputs.flatMap(inputPath(repo, graph, _))
                val outputs  = task.outputs.map(output => repo.resolve(output.path.value)).toSet
                val paths    = if declared.isEmpty then List(repo.resolve("histos.scala")) else declared.distinct
                Watched(paths, outputs)
      case other =>
        val modules = graph.projects.collect { case module: Module => module }
        val chosen  = projects(graph, other) match
          case WatchNames.Every       => modules
          case WatchNames.Only(names) => modules.filter(module => names.contains(module.name))
        Watched(SourceWatch.roots(repo, chosen), Set.empty)

  private def inputPath(repo: Path, graph: ProjectGraph, input: DeclaredInput): List[Path] =
    input match
      case DeclaredInput.File(path)         => List(repo.resolve(path.value))
      case DeclaredInput.Directory(path)    => List(repo.resolve(path.value))
      case DeclaredInput.Module(name, _, _) =>
        graph.get(name) match
          case Some(module: Module) =>
            val base = repo.resolve(module.baseDirectory)
            List(base.resolve("src"), base.resolve("test"))
          case _ => Nil
      case DeclaredInput.Note(_, _) => Nil
      case DeclaredInput.Text(_, _) => Nil
      case DeclaredInput.Env(_)     => Nil

  private enum WatchNames:
    case Every
    case Only(names: List[ProjectName])

  private def publish(graph: ProjectGraph, tokens: List[String]): WatchNames =
    PublishSelect.watch(graph, tokens) match
      case None        => WatchNames.Every
      case Some(names) => WatchNames.Only(names)

  private def projects(graph: ProjectGraph, call: Call): WatchNames =
    def listed(names: List[ProjectName]): WatchNames =
      if names.isEmpty then WatchNames.Every else WatchNames.Only(names)
    def one(id: String): WatchNames =
      ProjectName.from(id) match
        case Right(name) => WatchNames.Only(List(name))
        case Left(_)     => WatchNames.Only(Nil)
    call match
      case Call.Test(names, _, _)               => listed(names)
      case Call.Compile(names)                  => listed(names)
      case Call.PreviewCompile(names)           => listed(names)
      case Call.PreviewTest(names)              => listed(names)
      case Call.PreviewPublishLocal(tokens)     => publish(graph, tokens)
      case Call.PreviewPublishSnapshots(tokens) => publish(graph, tokens)
      case Call.PreviewClean(names)             => listed(names)
      case Call.Clean(names)                    => listed(names)
      case Call.PreviewRun(id, _)               => one(id)
      case Call.Run(id, _)                      => one(id)
      case _                                    => WatchNames.Every
end Watching
