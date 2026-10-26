package histos.node

import histos.{Aggregate, Module, Platform, ProjectGraph}

import java.nio.file.Path

enum NodeReject:
  case Absent
  case NoBinary
  case UnreadableVersion(error: NodeVersionError)

  def message: String = this match
    case Absent                   => "not a Node home"
    case NoBinary                 => "no node"
    case UnreadableVersion(error) => error.message
end NodeReject

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
