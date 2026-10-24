package histos.resolve

import histos.jdk.Sha256
import histos.workspace.{IoFailure, Workspace}
import histos.plugin.{LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.{
  Aggregate,
  Dep,
  DepKind,
  Module,
  Platform,
  ProjectGraph,
  ProjectName,
  ScalaVersion
}

import java.nio.file.{
  AtomicMoveNotSupportedException,
  Files,
  Path,
  StandardCopyOption
}

import zio.{IO, ULayer, ZIO, ZLayer}

final case class ResolveCell(
    module: ProjectName,
    dependencies: List[Dep],
    scalaVersion: ScalaVersion,
    platform: Platform,
    repositories: Repositories,
    scope: ClasspathScope = ClasspathScope.Compile
)

final case class ResolvedArtifact(
    organization: String,
    name: String,
    revision: String,
    path: Path,
    sha256: Sha256,
    repository: Repository,
    classifier: Option[JarClassifier] = None
)

final case class Eviction(
    organization: String,
    name: String,
    requested: String,
    selected: String
)

object Eviction:
  /** The lock is whitespace-separated. An empty or spaced field cannot be read
    * back.
    */
  def printable(eviction: Eviction): Boolean =
    List(
      eviction.organization,
      eviction.name,
      eviction.requested,
      eviction.selected
    ).forall(token)

  private def token(value: String): Boolean =
    value.nonEmpty && !value.exists(_.isWhitespace)

final case class Classpath(
    artifacts: List[ResolvedArtifact],
    evictions: List[Eviction]
)

enum RepositoryIo:
  case Fetch, Metadata
