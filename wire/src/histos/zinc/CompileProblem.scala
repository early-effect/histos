package histos.zinc

import java.nio.file.Path

/** Zinc positions are 1-based. Dotty positions are already 0-based. */
enum PositionBase:
  case Zinc
  case Dotty

enum CompileSeverity:
  case Error, Warn, Info

/** One compiler message. A missing path is a log line, not a squiggle. */
final case class CompileProblem(
    message: String,
    path: Option[Path],
    severity: CompileSeverity,
    startLine: Option[Int],
    startColumn: Option[Int],
    endLine: Option[Int],
    endColumn: Option[Int],
    base: PositionBase,
):
  def text: String = message
