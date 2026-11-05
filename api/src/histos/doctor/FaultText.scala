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

  def rendered(fix: Fix): String = fix match
    case Fix.Command(line)        => s"fix  ${line.value}"
    case Fix.Settings(url, click) => s"fix  ${url.value}  $click"
    case Fix.Config(file, _)      => s"fix  ${file.value}"
    case Fix.Steps(items)         => s"fix  ${items.head}"
end FaultText
