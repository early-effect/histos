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
        val how = Fix.Command(ShellLine.trusted(s"provide ${probe.toString}"))
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
  def text(repo: String, report: DoctorReport): String =
    val lines = report.outcomes.flatMap(line) ++ report.timedOut.map(late)
    val tail  =
      val base =
        s"${report.errors} errors, ${report.warnings} warnings, ${report.oks} ok, ${report.skipped} skipped"
      if report.timedOut.isEmpty then base else s"$base, ${report.timedOut.size} timed out"
    ((s"histos doctor  $repo" :: lines) :+ tail).mkString("\n")

  def annotations(report: DoctorReport): String =
    val found = report.found.map { diagnostic =>
      val kind = if diagnostic.level == Level.Error then "error" else "warning"
      val fix  = diagnostic.fixes.head match
        case Fix.Settings(url, _) => url.value
        case Fix.Command(line)    => line.value
        case Fix.Config(file, _)  => file.value
        case Fix.Steps(items)     => items.head
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

  private def late(id: CheckId): String =
    s"  x timed out ${id.value}"

  private def line(outcome: Outcome): List[String] =
    outcome match
      case Outcome.Ok(check, note) =>
        List(s"  ok         ${check.value}           $note")
      case Outcome.Skipped(check, _, how) =>
        val hint = how match
          case Fix.Command(line)    => line.value
          case Fix.Settings(url, _) => url.value
          case Fix.Config(file, _)  => file.value
          case Fix.Steps(items)     => items.head
        List(s"  -  skipped ${check.value}", s"             $hint")
      case Outcome.Found(diagnostic) =>
        val mark = diagnostic.level match
          case Level.Error   => "x error   "
          case Level.Warning => "! warning "
          case Level.Info    => "i info    "
        val fix = diagnostic.fixes.head match
          case Fix.Command(line)        => s"fix  ${line.value}"
          case Fix.Settings(url, click) => s"fix  ${url.value}  $click"
          case Fix.Config(file, _)      => s"fix  ${file.value}"
          case Fix.Steps(items)         => s"fix  ${items.head}"
        List(
          s"  $mark ${diagnostic.check.value}",
          s"             ${diagnostic.summary}",
          s"             ${diagnostic.explain}",
          s"             $fix",
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
  ): (Map[String, String], List[String]) =
    mode match
      case FixMode.Print        => (files, Nil)
      case FixMode.Apply(kinds) =>
        fixes.foldLeft((files, List.empty[String])):
          case ((current, commands), fix) =>
            val kind = kindOf(fix)
            if !kinds.contains(kind) then (current, commands)
            else if kind == FixKind.GitHubSetting && confirm(fix) != Answer.Yes then (current, commands)
            else applyOne(current, commands, fix)

  def kindOf(fix: Fix): FixKind =
    fix match
      case Fix.Config(_, _)   => FixKind.LocalEdit
      case Fix.Command(_)     => FixKind.HistosCommand
      case Fix.Settings(_, _) => FixKind.GitHubSetting
      case Fix.Steps(_)       => FixKind.HistosCommand

  private def applyOne(
      files: Map[String, String],
      commands: List[String],
      fix: Fix,
  ): (Map[String, String], List[String]) =
    fix match
      case Fix.Config(file, edit) =>
        val next =
          files.get(file.value).map(_.replace(edit.before, edit.after)).getOrElse(files.getOrElse(file.value, ""))
        (files.updated(file.value, next), commands)
      case Fix.Command(line) =>
        (files, commands :+ line.value)
      case Fix.Settings(_, _) =>
        (files, commands)
      case Fix.Steps(_) =>
        (files, commands)
end FixApply
