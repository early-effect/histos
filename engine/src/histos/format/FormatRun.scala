package histos.format

import histos.resolve.{ResolutionService, ToolInput, ToolRequest}
import histos.{Formatting, ProjectGraph}

import java.nio.file.Path

import zio.ZIO

/** `histos fmt`: the declared configuration, the scalafmt the build's resolution names, the selected files, then the
  * formatter. A locked build reads the lock. A solving build solves and remembers, the same as compile.
  */
object FormatRun:
  def run(
      repo: Path,
      graph: ProjectGraph,
      targets: FormatTargets,
      mode: FormatMode,
  ): ZIO[Formatters & ResolutionService, FormatError, FormatReport] =
    graph.formatting match
      case Formatting.Off            => ZIO.fail(FormatError.NotDeclared)
      case Formatting.Scalafmt(conf) =>
        for
          requests <- ScalafmtTools
            .requests(repo, graph)
            .mapError(FormatError.Conf(_))
            .someOrFail(FormatError.NotDeclared)
          dynamic   <- fetched(requests.dynamic)
          core      <- fetched(requests.core)
          selection <- FormatScope.select(repo, graph, targets)
          jars = FormatterJars(dynamic, core, requests.core.roots.map(ScalafmtPackage.of))
          report <- Formatters.format(FormatRequest(repo, Path.of(conf.relative), selection, mode, jars))
        yield report

  private def fetched(request: ToolRequest): ZIO[ResolutionService, FormatError, ToolInput] =
    ZIO
      .serviceWithZIO[ResolutionService](_.tool(request))
      .mapBoth(FormatError.Resolved(_), classpath => ToolInput(request.id, classpath.artifacts))
end FormatRun
