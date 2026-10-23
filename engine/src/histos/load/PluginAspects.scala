package histos.load

import histos.plugin.{DeclaredTask, PluginOutput, PluginRunError}
import histos.step.StepAspect

import java.nio.file.{Files, LinkOption, Path}

import zio.{Trace, ZIO, ZIOAspect}

/** The build loader follows the plugin fiber, and a declared output has to be on disk. */
private[load] object PluginAspects:
  def loader(threads: PluginThreads): StepAspect[Any, PluginFault, histos.step.StepKind] =
    StepAspect.lift(new ZIOAspect[Nothing, Any, PluginFault, Any, Nothing, Any]:
      def apply[R >: Nothing <: Any, E >: PluginFault <: Any, A >: Nothing <: Any](zio: ZIO[R, E, A])(using
          Trace
      ): ZIO[R, E, A] =
        threads.run(zio)
    )

  /** After the task returns, every declared path is a real file. An extra returned path fails first. */
  def declaredOutputs(
      repo: Path,
      task: DeclaredTask,
      output: zio.Ref[Option[PluginOutput]],
  ): StepAspect[Any, PluginFault, histos.step.StepKind] =
    StepAspect.lift(new ZIOAspect[Nothing, Any, PluginFault, Any, Nothing, Any]:
      def apply[R >: Nothing <: Any, E >: PluginFault <: Any, A >: Nothing <: Any](zio: ZIO[R, E, A])(using
          Trace
      ): ZIO[R, E, A] =
        zio.zipLeft(verify(repo, task, output).mapError(error => PluginFault.Task(error)))
    )

  private def verify(
      repo: Path,
      task: DeclaredTask,
      output: zio.Ref[Option[PluginOutput]],
  ): zio.IO[PluginRunError, Unit] =
    output.get.flatMap:
      case None => ZIO.unit
      case Some(written) =>
        val declared = task.outputs.map(_.path.value).toSet
        written.files.find(file => !declared.contains(file.path.value)) match
          case Some(extra) => ZIO.fail(PluginRunError.ExtraOutput(extra.path.value))
          case None        => present(repo, task)

  private def present(repo: Path, task: DeclaredTask): zio.IO[PluginRunError, Unit] =
    ZIO.foreachDiscard(task.outputs): declared =>
      val file = repo.resolve(declared.path.value)
      ZIO
        .attempt(Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS))
        .orElseFail(PluginRunError.MissingOutput(declared.name))
        .flatMap:
          case true  => ZIO.unit
          case false => ZIO.fail(PluginRunError.MissingOutput(declared.name))
