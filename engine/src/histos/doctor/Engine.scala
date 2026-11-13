package histos.doctor

import histos.plugin.RegisteredPlugin
import histos.supply.SupplyDiagnostic
import histos.ProjectGraph

import zio.{Duration, UIO, ZIO}

/** Collected checks, in check-id order. Plugins and jobs append after the core list. */
object Registry:
  def collect(graph: ProjectGraph, core: List[DoctorCheck]): List[DoctorCheck] =
    val contributed = graph.plugins.flatMap(pluginChecks)
    (core ++ contributed).sortBy(_.id.value)

  def pluginChecks(plugin: RegisteredPlugin): List[DoctorCheck] =
    plugin.contribution.checks ++ plugin.contribution.preflight ++ plugin.contribution.jobs.flatMap(_.preflight)
end Registry

final case class DoctorReport(outcomes: List[Outcome], timedOut: List[CheckId]):
  def errors: Int   = found.count(_.level == Level.Error)
  def warnings: Int = found.count(_.level == Level.Warning)
  def infos: Int    = found.count(_.level == Level.Info)
  def oks: Int      = outcomes.count(_.isInstanceOf[Outcome.Ok])
  def skipped: Int  = outcomes.count(_.isInstanceOf[Outcome.Skipped])

  def found: List[Diagnostic] =
    outcomes.collect { case Outcome.Found(diagnostic) => diagnostic }

  /** 0 when nothing is an error and no check timed out. A warning alone stays 0. */
  def exit: Int =
    if timedOut.nonEmpty || errors > 0 then 1 else 0
end DoctorReport

object Runner:
  def run(checks: List[DoctorCheck], ctx: DoctorContext, timeout: Duration): UIO[DoctorReport] =
    ZIO
      .foreachPar(checks)(one(ctx, timeout))
      .map: rows =>
        val outcomes = rows.flatMap(_._1)
        val late     = rows.flatMap(_._2)
        DoctorReport(outcomes, late)

  private def one(ctx: DoctorContext, timeout: Duration)(check: DoctorCheck): UIO[(List[Outcome], Option[CheckId])] =
    val missing = check.needs.filterNot(ctx.present.contains)
    missing.headOption match
      case Some(probe) =>
        val how = Fix.Steps(::(s"provide ${probe.toString}", Nil))
        ZIO.succeed((List(Outcome.Skipped(check.id, probe, how)), None))
      case None =>
        check
          .run(ctx)
          .timeout(timeout)
          .map:
            case Some(outcomes) => (outcomes, None)
            case None           => (Nil, Some(check.id))
end Runner

object Render:
  /** Errors and warnings, plus a timed-out check. `verbose` also prints ok, skipped, and info lines. */
  def text(repo: String, report: DoctorReport, verbose: Boolean): String =
    val shown = if verbose then report.outcomes.flatMap(line) else report.outcomes.flatMap(broken)
    val lines = shown ++ report.timedOut.map(late)
    ((s"histos doctor  $repo" :: lines) :+ counts(report, verbose)).mkString("\n")

  def annotations(report: DoctorReport): String =
    val found = report.found.map { diagnostic =>
      val kind = if diagnostic.level == Level.Error then "error" else "warning"
      val fix  = diagnostic.fixes.head match
        case Fix.Settings(url, _, _) => url.value
        case other                   => FaultText.shown(other)
      s"::${kind} title=histos doctor: ${diagnostic.check.value}::${diagnostic.summary} Fix: $fix"
    }
    val late = report.timedOut.map(id => s"::error title=histos doctor: ${id.value}::timed out")
    (found ++ late).mkString("\n")

  def summary(job: String, report: DoctorReport): String =
    val rows = report.found.map { diagnostic =>
      val mark = if diagnostic.level == Level.Error then ":x:" else ":white_check_mark:"
      s"| $mark | ${diagnostic.check.value} | ${diagnostic.explain} |"
    }
    val late  = report.timedOut.map(id => s"| :x: | ${id.value} | timed out |")
    val state = if report.errors > 0 || report.timedOut.nonEmpty then "failed" else "passed"
    (s"## histos preflight: $job  ($state)" :: "" :: "| | Check | What to fix |" :: "|---|---|---|" :: rows ++ late)
      .mkString("\n")

  def json(report: DoctorReport): String =
    val items = report.found.map { diagnostic =>
      s"""{"check":"${diagnostic.check.value}","level":"${diagnostic.level}","title":${quote(
          diagnostic.title
        )},"summary":${quote(diagnostic.summary)}}"""
    }
    val late = report.timedOut.map(id => quote(id.value))
    s"""{"exit":${report.exit},"timedOut":[${late.mkString(",")}],"findings":[${items.mkString(",")}]}"""

  private def counts(report: DoctorReport, verbose: Boolean): String =
    val head = s"${report.errors} errors, ${report.warnings} warnings"
    val body =
      if verbose then s"$head, ${report.oks} ok, ${report.skipped} skipped" else head
    if report.timedOut.isEmpty then body else s"$body, ${report.timedOut.size} timed out"

  /** Info is not a failure. Ok and skipped stay off the default log. */
  private def broken(outcome: Outcome): List[String] =
    outcome match
      case Outcome.Found(diagnostic) if diagnostic.level == Level.Error || diagnostic.level == Level.Warning =>
        line(outcome)
      case _ => Nil

  private def late(id: CheckId): String =
    s"  x timed out ${id.value}"

  private def line(outcome: Outcome): List[String] =
    outcome match
      case Outcome.Ok(check, note) =>
        List(s"  ok         ${check.value}           $note")
      case Outcome.Skipped(check, missing, how) =>
        val hints = how match
          case Fix.Settings(url, _, _) => List(url.value)
          case Fix.Steps(items)        => items.toList
          case other                   => List(FaultText.shown(other))
        s"  -  skipped ${check.value}  (needs $missing)" :: hints.map(hint => s"             $hint")
      case Outcome.Found(diagnostic) =>
        val mark = diagnostic.level match
          case Level.Error   => "x error   "
          case Level.Warning => "! warning "
          case Level.Info    => "i info    "
        List(
          s"  $mark ${diagnostic.check.value}",
          s"             ${diagnostic.summary}",
          s"             ${diagnostic.explain}",
          s"             ${FaultText.rendered(diagnostic.fixes.head)}",
        )

  private def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
end Render

object RefusalMap:
  def refused(call: histos.supply.ApiCall, status: Int, body: String): Option[SupplyDiagnostic] =
    val known =
      status == 401 || status == 403 || status == 404 || status == 422 ||
        body.contains("Resource not accessible by integration")
    if known then Some(SupplyDiagnostic.GitHubRefused(call, status, body)) else None

object FixApply:
  enum Answer:
    case Yes, No, NonTty

  def applyAll(
      fixes: List[Fix],
      mode: FixMode,
      confirm: Fix => Answer,
      files: Map[String, String],
  ): (Map[String, String], List[HistosCommand]) =
    mode match
      case FixMode.Print        => (files, Nil)
      case FixMode.Apply(kinds) =>
        fixes.foldLeft((files, List.empty[HistosCommand])):
          case ((current, commands), fix) =>
            kindOf(fix) match
              case Some(kind) if kinds.contains(kind) =>
                if kind == FixKind.GitHubSetting && confirm(fix) != Answer.Yes then (current, commands)
                else applyOne(current, commands, fix)
              case _ => (current, commands)

  /** The kind `--fix` applies a fix as. A shell command and a step are for a person, so they have none. */
  def kindOf(fix: Fix): Option[FixKind] =
    fix match
      case Fix.Config(_, _)      => Some(FixKind.LocalEdit)
      case Fix.Histos(_)         => Some(FixKind.HistosCommand)
      case Fix.Settings(_, _, _) => Some(FixKind.GitHubSetting)
      case Fix.Command(_)        => None
      case Fix.Steps(_)          => None

  private def applyOne(
      files: Map[String, String],
      commands: List[HistosCommand],
      fix: Fix,
  ): (Map[String, String], List[HistosCommand]) =
    fix match
      case Fix.Config(file, edit) =>
        val next =
          files.get(file.value).map(_.replace(edit.before, edit.after)).getOrElse(files.getOrElse(file.value, ""))
        (files.updated(file.value, next), commands)
      case Fix.Histos(command) =>
        (files, commands :+ command)
      case Fix.Settings(_, _, _) | Fix.Command(_) | Fix.Steps(_) =>
        (files, commands)
end FixApply
