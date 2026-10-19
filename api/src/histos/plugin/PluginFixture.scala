package histos.plugin

import java.nio.file.Path

/** Builds a `RunRequest`. It does not load a jar, open a scheduler, or hash the disk. */
object PluginFixture:
  def request(
      plugin: PluginId,
      task: TaskName,
      repo: Path,
      modules: List[ModuleView] = Nil,
      args: PluginArgs = PluginArgs.empty,
      inputs: List[MaterialInput] = Nil,
  ): RunRequest =
    RunRequest(PluginTaskId(plugin, task), repo, modules, args, inputs)
