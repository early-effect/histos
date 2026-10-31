package histos.load

import histos.{HistosBuild, ProjectGraph}

/** A graph loaded from `histos.scala`. The surrounding `Scope` closes the build classloader. */
final case class LoadedBuild(graph: ProjectGraph, plugins: PluginRuntime, build: HistosBuild)

object LoadedBuild:
  def apply(graph: ProjectGraph, plugins: PluginRuntime, build: HistosBuild): LoadedBuild =
    new LoadedBuild(graph, plugins, build)

  /** Tests that only have a graph. `local` and `sona` stay the stock layers. */
  def apply(graph: ProjectGraph, plugins: PluginRuntime): LoadedBuild =
    new LoadedBuild(graph, plugins, HistosBuild.fixed(graph))
