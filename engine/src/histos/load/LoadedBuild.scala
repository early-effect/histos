package histos.load

import histos.plugin.HistosPlugins
import histos.{HistosBuild, ProjectGraph}

/** A graph loaded from `histos.scala`. The surrounding `Scope` closes the build classloader. `provenance` is verified
  * manifests only. An unverified activation is not an empty artifact list.
  */
final case class LoadedBuild(
    graph: ProjectGraph,
    plugins: PluginRuntime,
    build: HistosBuild,
    catalog: Option[HistosPlugins] = None,
    provenance: List[ProvenanceResult] = Nil,
)
