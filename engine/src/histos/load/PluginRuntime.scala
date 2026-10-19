package histos.load

import histos.plugin.{PluginArgs, PluginId, PluginLinked, PluginOutput, PluginRunError, PluginTaskId}
import histos.task.{TaskError, TaskId, TaskResult}

import zio.{IO, Scope, ZIO}

final case class PluginLink(id: PluginId, linked: PluginLinked, artifacts: List[String])

/** Loaded plugin classes. This layer records links. `run` and `bind` arrive with command dispatch. */
trait PluginRuntime:
  def run(id: PluginTaskId, args: PluginArgs): ZIO[Scope, PluginRunError, PluginOutput]
  def bind: Map[TaskId, IO[TaskError, TaskResult]]
  def links: List[PluginLink]

object PluginRuntime:
  def apply(links: List[PluginLink]): PluginRuntime =
    Linked(links)

  private final class Linked(val links: List[PluginLink]) extends PluginRuntime:
    def run(id: PluginTaskId, args: PluginArgs): ZIO[Scope, PluginRunError, PluginOutput] =
      ZIO.fail(PluginRunError.Unknown(id))

    def bind: Map[TaskId, IO[TaskError, TaskResult]] =
      Map.empty
