package histos

import histos.load.{PluginFault, PluginLink, PluginRuntime}
import histos.plugin.{ApiDigest, PluginArgs, PluginOutput, PluginRunError, PluginTaskId}
import histos.resolve.Resolve
import histos.schedule.{ActionCache, RemoteCache}
import histos.task.{TaskError, TaskId, TaskResult}

import zio.{IO, ZIO}

/** The plugins of a build that declares none. No task runs, nothing binds, and nothing links. */
object LawPlugins:
  val none: PluginRuntime =
    new PluginRuntime:
      def launcher: ApiDigest = ApiDigest.trusted("a" * 64)

      def run(id: PluginTaskId, args: PluginArgs): ZIO[ActionCache & RemoteCache & Resolve, PluginFault, PluginOutput] =
        ZIO.fail(PluginFault.Task(PluginRunError.Unknown(id)))

      def bind: ZIO[ActionCache & RemoteCache & Resolve, TaskError, Map[TaskId, IO[TaskError, TaskResult]]] =
        ZIO.succeed(Map.empty)

      def links: List[PluginLink] = Nil
end LawPlugins
