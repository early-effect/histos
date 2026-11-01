package histos.node

import histos.{Aggregate, Module, Platform, ProjectGraph}

import java.nio.file.Path

final case class NodeHome(
    path: Path,
    hasBinary: Boolean,
    version: Either[NodeVersionError, NodeVersion],
)

object Node:
  /** A JVM-only graph does not name Node. An aggregate does not either. */
  def required(graph: ProjectGraph): Boolean =
    graph.projects.exists:
      case module: Module => module.platforms.all.contains(Platform.Js)
      case _: Aggregate   => false
