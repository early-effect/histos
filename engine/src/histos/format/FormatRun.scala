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
    for
      loaded <- opened(repo, graph)
      (config, jars) = loaded
      selection <- FormatScope.select(repo, graph, targets)
      report    <- Formatters.format(FormatRequest(repo, config, selection, mode, jars))
    yield report

  /** The formatted text of one file. The build must declare scalafmt. Nothing is written. */
  def text(
      repo: Path,
      graph: ProjectGraph,
      file: Path,
      code: String,
  ): ZIO[Formatters & ResolutionService, FormatError, String] =
    opened(repo, graph).flatMap: (config, jars) =>
      Formatters.formatText(repo, config, file, code, jars)

  private def opened(
      repo: Path,
      graph: ProjectGraph,
  ): ZIO[ResolutionService, FormatError, (Path, FormatterJars)] =
    graph.formatting match
      case Formatting.Off            => ZIO.fail(FormatError.NotDeclared)
      case Formatting.Scalafmt(conf) =>
        for
          requests <- ScalafmtTools
            .requests(repo, graph)
            .mapError(FormatError.Conf(_))
            .someOrFail(FormatError.NotDeclared)
          dynamic <- fetched(requests.dynamic)
          core    <- fetched(requests.core)
        yield (Path.of(conf.relative), FormatterJars(dynamic, core, requests.core.roots.map(ScalafmtPackage.of)))

  private def fetched(request: ToolRequest): ZIO[ResolutionService, FormatError, ToolInput] =
    ZIO
      .serviceWithZIO[ResolutionService](_.tool(request))
      .mapBoth(FormatError.Resolved(_), classpath => ToolInput(request.id, classpath.artifacts))
end FormatRun
