package histos.format

import histos.resolve.ToolInput
import histos.workspace.IoFailure

import java.nio.file.Path

/** Why one file was not formatted. Every path is relative to the repository. */
enum FormatFault:
  case Parse(file: Path, line: Int, column: Int, reason: String)
  case Read(failure: IoFailure)
  case Write(failure: IoFailure)
  case Failed(file: Path, reason: String)

  def message: String = this match
    case Parse(file, line, column, text) => s"$file:$line:$column: $text"
    case Read(failure)                   => failure.message
    case Write(failure)                  => failure.message
    case Failed(file, text)              => s"$file: $text"
end FormatFault

/** `Tracked` formats only git-tracked files when the configuration sets `project.git`. `Named` formats the files a
  * caller named, whether git tracks them or not.
  */
enum GitFilter:
  case Tracked, Named

/** Repository-relative files to consider, and whether `project.git` narrows them. */
final case class FormatSelection(files: List[Path], git: GitFilter)

/** The two locked cells, fetched. `roots` are the core packages the lock holds. */
final case class FormatterJars(dynamic: ToolInput, core: ToolInput, roots: List[ScalafmtPackage])

/** One format or check over a selection. `config` is relative to `repo`. */
final case class FormatRequest(
    repo: Path,
    config: Path,
    selection: FormatSelection,
    mode: FormatMode,
    jars: FormatterJars,
)

/** `changed` is written under `Write` and unformatted under `Check`. `skipped` is a ledger hit. `excluded` is outside
  * the configuration's project filters, or untracked when it sets `project.git`.
  */
final case class FormatReport(
    mode: FormatMode,
    changed: List[Path],
    unchanged: Int,
    skipped: Int,
    excluded: Int,
    faults: List[FormatFault],
):
  /** A check that found a file to format, or any fault, fails the call. */
  def failed: Boolean =
    faults.nonEmpty || (mode == FormatMode.Check && changed.nonEmpty)

  private def verb: String = mode match
    case FormatMode.Write => "formatted"
    case FormatMode.Check => "unformatted"

  /** One line per changed file, then one per fault. */
  def detail: List[String] =
    changed.sortBy(_.toString).map(file => s"$verb $file") ++ faults.map(_.message)

  /** The tally. A check that found files names the command that formats them. */
  def summary: String =
    val tally =
      s"$verb: ${changed.length} unchanged: $unchanged skipped: $skipped excluded: $excluded faults: ${faults.length}"
    if mode == FormatMode.Check && changed.nonEmpty then s"$tally. Run histos fmt" else tally
end FormatReport
