package histos.format

import histos.resolve.{
  BuildResolvers,
  Repositories,
  ToolId,
  ToolRequest,
  ToolRole,
  ToolRoot,
  ToolRoots,
  ToolVariant,
  Transitivity,
}
import histos.{Dep, DepKind, FormatTools, Formatting, Platform, ProjectGraph}

import java.nio.file.Path

import zio.{IO, ZIO}

/** The two locked cells `histos fmt` loads. `dynamic` is the scalafmt-interfaces implementation. `core` is the
  * formatter the configuration's version names, with the `scala-reflect` scalafmt-dynamic asks for beside it.
  */
final case class ScalafmtRequests(dynamic: ToolRequest, core: ToolRequest)

object ScalafmtTools:
  def dynamic(tools: FormatTools, repositories: Repositories): ToolRequest =
    ToolRequest(
      ToolId(ToolRole.ScalafmtDynamic, tools.scala, ToolVariant.Jvm),
      List(root(tools, "org.scalameta", "scalafmt-dynamic-core", tools.dynamic.value, DepKind.Binary)),
      repositories,
    )

  def core(version: ScalafmtVersion, tools: FormatTools, repositories: Repositories): ToolRequest =
    ToolRequest(
      ToolId(ToolRole.ScalafmtCore, tools.scala, ToolVariant.Jvm),
      coreRoots(version, tools),
      repositories,
    )

  /** The packages scalafmt-dynamic asks its downloader for, in the order it asks. */
  def coreRoots(version: ScalafmtVersion, tools: FormatTools): List[ToolRoot] =
    List(
      root(tools, "org.scalameta", "scalafmt-core", version.value, DepKind.Binary),
      root(tools, "org.scala-lang", "scala-reflect", tools.scala.value, DepKind.Java),
    )

  /** Nothing when the build does not format. Otherwise both cells, keyed by the configuration's version. */
  def requests(repo: Path, graph: ProjectGraph): IO[ScalafmtConfError, Option[ScalafmtRequests]] =
    graph.formatting match
      case Formatting.Off            => ZIO.none
      case Formatting.Scalafmt(conf) =>
        val repositories = BuildResolvers(graph.resolvers)
        ScalafmtVersion
          .of(repo, conf)
          .map(version =>
            Some(
              ScalafmtRequests(
                dynamic(graph.tools.format, repositories),
                core(version, graph.tools.format, repositories),
              )
            )
          )

  private def root(tools: FormatTools, organization: String, name: String, revision: String, kind: DepKind): ToolRoot =
    ToolRoots.of(Dep.trusted(organization, name, revision, kind), tools.scala, Platform.Jvm, Transitivity.Closure, None)
end ScalafmtTools
