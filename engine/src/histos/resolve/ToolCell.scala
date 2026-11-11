package histos.resolve

import histos.{Dep, DepKind, JsVersion, NativeVersion, ScalaVersion, JsEnv, Platform}

object ToolRoots:
  def of(
      dependency: Dep,
      version: ScalaVersion,
      platform: Platform,
      transitivity: Transitivity,
      origin: Option[Repository],
  ): ToolRoot = ToolRoot(
    dependency.organization,
    ArtifactName.of(dependency, version, platform),
    dependency.revision.value,
    transitivity,
    origin,
  )

final case class ToolRequest(
    id: ToolId,
    roots: List[ToolRoot],
    repositories: Repositories,
)
final case class LockedTool(
    id: ToolId,
    roots: List[ToolRoot],
    repositories: List[Repository],
    artifacts: List[LockedArtifact],
    evictions: List[Eviction],
)
