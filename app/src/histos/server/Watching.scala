package histos.server

import histos.cli.{CliExit, Command, Lines}
import histos.watch.SourceWatch
import histos.{Module, ProjectGraph}

import java.nio.file.Path

import zio.{Ref, ZIO}

/** Run one operation, then again after a source save. The inner exit does not
  * end the session.
  */
object Watching:
  def session[R](
      repo: Path,
      say: Say,
      run: ZIO[R, ServerError, Unit],
      roots: ZIO[R, ServerError, List[Path]]
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
                error =>
                  Calls.finish(say, CliExit.TaskFailed.code, error.message),
                woke =>
                  ZIO.foreachDiscard(woke)(path =>
                    Calls.tell(
                      say,
                      Lines.watching(Lines.relative(repo, path))
                    )
                  ) *> loop
              )
          }
      loop.foldCauseZIO(
        cause =>
          if cause.isInterruptedOnly then
            running.get.flatMap { on =>
              Calls.finish(
                say,
                if on then CliExit.Interrupted.code else CliExit.Success.code,
                ""
              )
            }
          else
            cause.failureOrCause match
              case Left(error) => ZIO.fail(error)
              case Right(_)    =>
                Calls.finish(say, CliExit.TaskFailed.code, "watch stopped"),
        _ => ZIO.unit
      )
    }

  def roots(repo: Path, command: Command, graph: ProjectGraph): List[Path] =
    val modules = graph.projects.collect { case module: Module => module }
    val named = projects(command)
    val chosen =
      if named.isEmpty then modules
      else modules.filter(module => named.contains(module.name.value))
    SourceWatch.roots(repo, chosen)

  private def projects(command: Command): List[String] =
    command match
      case Command.Test(names)         => names
      case Command.TestOnly(names, _)  => names
      case Command.TestQuick(names, _) => names
      case Command.Compile(names)         => names
      case Command.PreviewCompile(names) => names
      case Command.Clean(names)           => names
      case Command.Run(id, _)
          if id != histos.task.TaskId.Inspect && id != histos.task.TaskId.Versions =>
        List(id.value)
      case _ => Nil
