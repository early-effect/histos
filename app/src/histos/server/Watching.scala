package histos.server

import histos.cli.Lines
import histos.report.{BuildFault, CallResult, Rejection}
import histos.watch.SourceWatch
import histos.publish.PublishSelect
import histos.{Module, ProjectGraph, ProjectName}

import java.nio.file.Path

import zio.{Ref, ZIO}

/** Run one operation, then again after a source save. The inner exit does not end the session.
  */
object Watching:
  def session[R](
      repo: Path,
      say: Say,
      run: ZIO[R, ServerError, Unit],
      roots: ZIO[R, ServerError, List[Path]],
  ): ZIO[R, ServerError, Unit] =
    Ref.make(false).flatMap { running =>
      def loop: ZIO[R, ServerError, Unit] =
        running.set(true) *>
          run *>
          running.set(false) *>
          Calls.tell(say, "watching") *>
          roots.flatMap { paths =>
            SourceWatch
              .changed(paths)
              .foldZIO(
                error => Calls.finish(say, CallResult.Fault(BuildFault.Watch(error))),
                woke =>
                  ZIO.foreachDiscard(woke)(path =>
                    Calls.tell(
                      say,
                      Lines.watching(Lines.relative(repo, path)),
                    )
                  ) *> loop,
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

  def roots(repo: Path, call: Call, graph: ProjectGraph): List[Path] =
    val modules = graph.projects.collect { case module: Module => module }
    val chosen  = projects(graph, call) match
      case WatchNames.Every       => modules
      case WatchNames.Only(names) => modules.filter(module => names.contains(module.name))
    SourceWatch.roots(repo, chosen)

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
      case Call.PreviewPublishLocal(_, tokens)  => publish(graph, tokens)
      case Call.PreviewPublishSnapshots(tokens) => publish(graph, tokens)
      case Call.PreviewClean(names)             => listed(names)
      case Call.Clean(names)                    => listed(names)
      case Call.PreviewRun(id, _)               => one(id)
      case Call.Run(id, _)                      => one(id)
      case _                                    => WatchNames.Every
end Watching
