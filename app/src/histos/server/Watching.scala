package histos.server

import histos.cli.{CliExit, Lines}
import histos.plugin.PluginTaskId
import histos.report.{BuildFault, CallResult, Rejection}
import histos.watch.SourceWatch
import histos.publish.PublishSelect
import histos.{Module, ProjectGraph, ProjectName}

import java.nio.file.Path

import zio.{Exit, Promise, Ref, UIO, ZIO}

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
              _       <- stay match
                case Beside.Stop => ZIO.unit
                case other       =>
                  val keep = other == Beside.Keep
                  for
                    _    <- ZIO.unless(keep)(child.interrupt)
                    next <- remember(child, keep)
                    _    <- roots.flatMap: watched =>
                      SourceWatch
                        .take(watched.view)
                        .foldZIO(
                          error => Calls.finish(say, CallResult.Fault(BuildFault.Watch(error))),
                          now => cycle(Some(now), next),
                        )
                  yield ()
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
          .take(watched.view)
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
      .since(watched.view, origin)
      .foldZIO(
        error => Calls.finish(say, CallResult.Fault(BuildFault.Watch(error))).as(List.empty[Path]),
        paths => ZIO.succeed(paths),
      )

  private enum Beside:
    case Keep, Restart, Stop

  /** A finished program ends the session. A source change restarts it when `again` says so. */
  private def held[R, E](
      fiber: zio.Fiber[E, Unit],
      woke: List[Path],
      again: List[Path] => ZIO[R, E, Boolean],
  ): ZIO[R, E, Beside] =
    if woke.isEmpty then
      fiber.await.flatMap:
        case Exit.Success(_)     => ZIO.succeed(Beside.Stop)
        case Exit.Failure(cause) => ZIO.failCause(cause)
    else again(woke).map(replace => if replace then Beside.Restart else Beside.Keep)

  def session[R](
      repo: Path,
      say: Say,
      run: ZIO[R, ServerError, Unit],
      roots: ZIO[R, ServerError, Watched],
      changed: List[Path] => UIO[Unit] = _ => ZIO.unit,
  ): ZIO[R, ServerError, Unit] =
    Ref.make(false).flatMap { running =>
      def loop: ZIO[R, ServerError, Unit] =
        running.set(true) *>
          run *>
          running.set(false) *>
          roots.flatMap { watched =>
            SourceWatch
              .take(watched.view)
              .foldZIO(
                error => Calls.finish(say, CallResult.Fault(BuildFault.Watch(error))),
                start =>
                  Calls.tell(say, "watching") *>
                    SourceWatch
                      .since(watched.view, start)
                      .foldZIO(
                        error => Calls.finish(say, CallResult.Fault(BuildFault.Watch(error))),
                        woke =>
                          changed(woke) *>
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

  /** The compile a restart runs before it touches the program: the program's module, or the build. */
  def compileFor(call: Call): Call =
    call match
      case Call.Run(id, platforms, _, _, _, versions) =>
        ProjectName.from(id) match
          case Right(name) =>
            Call.Compile(
              List(name),
              platforms.getOrElse(histos.PlatformSelection.All),
              versions.getOrElse(histos.ScalaSelection.All),
            )
          case Left(_) => Call.Compile(Nil)
      case _ => Call.Compile(Nil)

  /** A change replaces the program when the compile it ran succeeded and the change is one of the program's roots. A
    * failed compile, or no result at all, leaves the program running.
    */
  def restarts(result: Option[CallResult], changed: List[Path], watched: Watched): Boolean =
    result.exists(found => CliExit.from(found) == CliExit.Success) &&
      changed.exists(path => watched.view.inputs.exists(_.contains(path)))

  /** `path` is the root or a file the root walk would see. */
  def covers(root: Path, path: Path): Boolean =
    val base = root.toAbsolutePath.normalize
    val file = path.toAbsolutePath.normalize
    file == base || file.startsWith(base)

  final case class Watched(paths: List[Path], excluded: Set[Path], projection: Option[SourceWatch.Projection] = None):
    def view: SourceWatch.Projection = projection.getOrElse(
      SourceWatch.Projection(
        paths.map(path => SourceWatch.Root(path, histos.plugin.OutputKind.Directory)),
        excluded.toList.map(path => SourceWatch.Root(path, histos.plugin.OutputKind.Directory)),
      )
    )

  def roots(repo: Path, call: Call, graph: ProjectGraph): List[Path] =
    watched(repo, call, graph).paths

  /** A plugin task watches the files, directories, and modules it declared. A task with no file inputs watches the
    * build file. Declared outputs are excluded, including one that sits inside a watched directory.
    */
  def watched(repo: Path, call: Call, graph: ProjectGraph): Watched =
    project(repo, call, graph) match
      case Right(projection) =>
        Watched(projection.inputs.map(_.path), projection.excluded.map(_.path).toSet, Some(projection))
      case Left(_) => Watched(List(repo.resolve("histos.scala")), Set(repo.resolve(".histos")))

  def project(repo: Path, call: Call, graph: ProjectGraph): Either[BuildFault, SourceWatch.Projection] =
    nodes(graph, call).flatMap { roots =>
      SourceWatch.project(repo, graph, roots).left.map {
        case error: histos.build.BuildDagError =>
          BuildFault.Test(histos.testrun.ModuleTestError.Scoped(histos.testrun.TestFailures.graph(error)))
        case error: histos.zinc.StageRootError =>
          BuildFault.Test(histos.testrun.ModuleTestError.Scoped(histos.testrun.TestFailures.root(error)))
      }
    }

  private def nodes(graph: ProjectGraph, call: Call): Either[BuildFault, List[histos.build.BuildNode]] =
    import histos.build.{BuildNode, BuildRuntime}
    def compile(
        names: List[ProjectName],
        platforms: histos.PlatformSelection,
        versions: histos.ScalaSelection = histos.ScalaSelection.All,
    ) =
      BuildRuntime.compileRoots(graph, names, platforms, versions).left.map(BuildFault.Test(_))
    def tests(selection: histos.TestSelection) =
      histos.testrun.TestCells
        .plan(graph, selection)
        .left
        .map(error =>
          BuildFault.Test(histos.testrun.ModuleTestError.Scoped(histos.testrun.ScopedTestFailure.Selection(error)))
        )
        .map(_.compileStageRoots.map(BuildNode.Compile(_)))
    def run(
        id: String,
        platforms: Option[histos.PlatformSelection],
        main: Option[histos.MainClass],
        versions: Option[histos.ScalaSelection],
    ) =
      histos.preview.Plan
        .runSelected(graph, id, platforms, main, versions)
        .left
        .map(BuildFault.Preview(_))
        .flatMap { plan =>
          plan.unit match
            case histos.preview.RunUnit.Module(name, platform, _, version) =>
              compile(
                List(name),
                histos.PlatformSelection.only(platform),
                histos.ScalaSelection.Only(histos.ScalaToken.Exact(version), Nil),
              )
            case histos.preview.RunUnit.Task(id) => task(id)
        }
    def task(id: String): Either[BuildFault, List[BuildNode]] =
      if id.contains("/") then
        PluginTaskId.parse(id).left.map(BuildFault.PluginTask(_)).map(id => List(BuildNode.Generate(id)))
      else compile(Nil, histos.PlatformSelection.All)
    call match
      case Call.Compile(names, platforms, versions)        => compile(names, platforms, versions)
      case Call.PreviewCompile(names, platforms, versions) => compile(names, platforms, versions)
      case Call.Test(selection, _, _, _)                   => tests(selection)
      case Call.PreviewTest(selection, _, _, _)            => tests(selection)
      case Call.Run(id, platforms, _, _, main, versions)   => run(id, platforms, main, versions)
      case Call.PreviewRun(id, platforms, main, versions)  => run(id, platforms, main, versions)
      case Call.Task(id, _)                                => task(id)
      case Call.Watch(inner, _)                            => nodes(graph, inner)
      case other @ (Call.PublishCheck(_, _) | Call.PublishLocal(_, _) | Call.PublishSnapshots(_) |
          Call.PublishRelease(_, _) | Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) |
          Call.PublishWithdraw | Call.PublishPromote(_) | Call.PublishDrop(_) | Call.PublishAssets(_) |
          Call.Assembly(_) | Call.Clean(_, _) | Call.PreviewClean(_, _) | Call.Format(_, _) | Call.Resolve(_) |
          Call.Workflow(_) | Call.Graph | Call.Cancel(_) | Call.Reload | Call.Reboot(_) | Call.Stop | Call.Version |
          Call.CachePath | Call.CacheList | Call.CacheClean(_) | Call.CachePromote(_, _) | Call.CacheForget(_) |
          Call.CacheVerify(_) | Call.Bsp | Call.Catalog(_)) =>
        projects(graph, other) match
          case WatchNames.Every       => compile(Nil, histos.PlatformSelection.All)
          case WatchNames.Only(names) => compile(names, histos.PlatformSelection.All)

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
    def testNames(graph: ProjectGraph, selection: histos.TestSelection): WatchNames =
      histos.testrun.TestCells.plan(graph, selection) match
        case Right(plan) => WatchNames.Only(plan.requestedCells.map(_.module).distinct)
        case Left(_)     => WatchNames.Every
    def one(id: String): WatchNames =
      ProjectName.from(id) match
        case Right(name) => WatchNames.Only(List(name))
        case Left(_)     => WatchNames.Only(Nil)
    call match
      case Call.Test(selection, _, _, _)        => testNames(graph, selection)
      case Call.Compile(names, _, _)            => listed(names)
      case Call.PreviewCompile(names, _, _)     => listed(names)
      case Call.PreviewTest(selection, _, _, _) => testNames(graph, selection)
      case Call.PublishCheck(tokens, _)         => publish(graph, tokens.map(_.value))
      case Call.PublishLocal(tokens, _)         => publish(graph, tokens.map(_.value))
      case Call.PublishSnapshots(tokens)        => publish(graph, tokens)
      case Call.PreviewPublishLocal(tokens)     => publish(graph, tokens.map(_.value))
      case Call.PreviewPublishSnapshots(tokens) => publish(graph, tokens)
      case Call.PreviewClean(names, _)          => listed(names)
      case Call.Clean(names, _)                 => listed(names)
      case Call.PreviewRun(id, _, _, _)         => one(id)
      case Call.Run(id, _, _, _, _, _)          => one(id)
      case _                                    => WatchNames.Every
end Watching
