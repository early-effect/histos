package histos.zinc

import dotty.tools.dotc.Driver
import dotty.tools.dotc.interfaces.Diagnostic.{ERROR, INFO, WARNING}
import dotty.tools.dotc.reporting.StoreReporter

import zio.{IO, ZIO}

private[histos] enum DottyError:
  case Failed

/** One shot of the embedded compiler. The reporter stays in this file. */
private[histos] enum DottyReport:
  case Failed(problems: List[CompileProblem])
  case Passed

private[histos] object Dotty:
  def process(args: List[String]): IO[DottyError, DottyReport] =
    ZIO
      .attemptBlocking:
        val reporter = new StoreReporter()
        new Driver().process(args.toArray, reporter)
        val problems = reporter.allErrors.map(note)
        if reporter.hasErrors then DottyReport.Failed(problems) else DottyReport.Passed
      .mapError(_ => DottyError.Failed)

  private def note(error: dotty.tools.dotc.reporting.Diagnostic.Error): CompileProblem =
    val pos    = error.pos
    val placed = pos.exists
    CompileProblem(
      error.message,
      None,
      severity(error.level),
      if placed then Some(pos.startLine) else None,
      if placed then Some(pos.startColumn) else None,
      if placed then Some(pos.endLine) else None,
      if placed then Some(pos.endColumn) else None,
      PositionBase.Dotty,
    )

  private def severity(level: Int): CompileSeverity =
    level match
      case ERROR   => CompileSeverity.Error
      case WARNING => CompileSeverity.Warn
      case INFO    => CompileSeverity.Info
      case _       => CompileSeverity.Info
end Dotty
