package histos.server

import histos.cli.Lines
import histos.report.{BuildFault, CallResult, Rejection}
import histos.watch.SourceWatch
import histos.{Module, ProjectGraph}

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
    val named   = projects(call)
    val chosen  =
      if named.isEmpty then modules
      else modules.filter(module => named.contains(module.name.value))
    SourceWatch.roots(repo, chosen)

  private def projects(call: Call): List[String] =
    call match
      case Call.Test(names, _, _)     => names
      case Call.Compile(names)        => names
      case Call.PreviewCompile(names) => names
      case Call.PreviewTest(names)    => names
      case Call.PreviewPublish(names) => names
      case Call.PreviewClean(names)   => names
      case Call.PreviewRun(id, _)     => List(id)
      case Call.Clean(names)          => names
      case Call.Run(id, _)            => List(id)
      case _                          => Nil
end Watching
