package histos.doctor

import histos.plugin.RepoPath

/** One finding. `summary` keeps the sentence a fault already printed. */
final case class Noted(
    check: CheckId,
    level: Level,
    title: String,
    summary: String,
    explain: String,
    fixes: ::[Fix],
    override val location: Option[Location] = None,
) extends Diagnostic

object Noted:
  def error(check: String, summary: String, explain: String, fix: Fix, more: Fix*): Noted =
    Noted(
      CheckId.trusted(check),
      Level.Error,
      summary,
      summary,
      explain,
      ::(fix, more.toList),
    )

  def at(noted: Noted, file: String, line: Option[Int]): Noted =
    RepoPath.from(file) match
      case Left(_)     => noted
      case Right(path) => noted.copy(location = Some(Location(path, line)))

  def command(line: String): Fix =
    Fix.Command(ShellLine.trusted(line))

  def steps(head: String, rest: String*): Fix =
    Fix.Steps(::(head, rest.toList))

  /** The summary is the existing fault sentence. The command is the first fix. */
  def carry(check: String, summary: String, explain: String, line: String): Noted =
    error(check, summary, explain, command(line))
end Noted
