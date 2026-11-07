package histos.coverage

import histos.resolve.{Repositories, ToolId, ToolRequest, ToolRole, ToolRoots, ToolVariant, Transitivity}
import histos.{Dep, DepKind, Platform, ScalaVersion, ToolRevision}

/** The scoverage coordinates for one Scala 2.13 compiler. The plugin is the full Scala version. The runtime is binary.
  */
object Tools:
  def request(version: ScalaVersion, revision: ToolRevision, repositories: Repositories): ToolRequest =
    val plugin = s"scalac-scoverage-plugin_${version.value}"
    ToolRequest(
      ToolId(ToolRole.Scoverage, version, ToolVariant.Jvm),
      List(
        root(version, plugin, revision, Transitivity.Closure),
        root(version, "scalac-scoverage-runtime_2.13", revision, Transitivity.Single),
      ),
      repositories,
    )

  private def root(
      version: ScalaVersion,
      name: String,
      revision: ToolRevision,
      transitivity: Transitivity,
  ): histos.resolve.ToolRoot =
    ToolRoots.of(
      Dep.trusted("org.scoverage", name, revision.value, DepKind.Java),
      version,
      Platform.Jvm,
      transitivity,
      None,
    )
end Tools
