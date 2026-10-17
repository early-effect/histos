package histos.load

import histos.ProjectGraph

/** A graph loaded from `histos.scala`. The surrounding `Scope` closes the build classloader. */
final case class LoadedBuild(graph: ProjectGraph)
