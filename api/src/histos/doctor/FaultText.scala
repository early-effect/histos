package histos.doctor

/** The lines a fault prints. The summary stays first. A repeated explanation is omitted. */
object FaultText:
  def show(diagnostic: Diagnostic): String =
    val body =
      if diagnostic.explain.isEmpty || diagnostic.explain == diagnostic.summary then diagnostic.summary
      else s"${diagnostic.summary}\n${diagnostic.explain}"
    val where = diagnostic.location.fold(""): loc =>
      val line = loc.line.fold("")(number => s":$number")
      s"\n${loc.file.value}$line"
    s"$body$where\n${rendered(diagnostic.fixes.head)}"

  def rendered(fix: Fix): String = s"fix  ${shown(fix)}"

  /** The one line a fix shows: what to run, open, edit, or do first. */
  def shown(fix: Fix): String = fix match
    case Fix.Command(line)           => line.value
    case Fix.Histos(command)         => command.line
    case Fix.Settings(url, click, _) => s"${url.value}  $click"
    case Fix.Config(file, _)         => file.value
    case Fix.Steps(items)            => items.head
end FaultText
