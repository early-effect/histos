package histos.doctor

import histos.plugin.RepoPath
import histos.supply.ApiCall
import histos.HttpUrl

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Annotations, the summary table, and JSON name every fix. Applying a fix rewrites only the files it names. */
object DoctorRenderLawSpec extends ZIOSpecDefault:
  private val mark = CheckId.trusted("demo.mark")
  private val late = CheckId.trusted("demo.late")

  def spec = suite("doctor render laws")(
    test("annotations, the summary, and json name the preferred fix and a timeout"):
      val settings = found(Level.Error, Fix.Settings(HttpUrl.trusted("https://example.test/set"), "open settings"))
      val command  = found(Level.Warning, Fix.Command(ShellLine.trusted("histos doctor")))
      val config   = found(Level.Info, Fix.Config(RepoPath.trusted("histos.scala"), Edit("before", "after")))
      val steps    = found(Level.Info, Fix.Steps(::("first step", List("second step"))))
      val report   = DoctorReport(List(settings, command, config, steps), List(late))
      val notes    = Render.annotations(report)
      val table    = Render.summary("ci", report)
      val encoded  = Render.json(report)
      val shown    = Render.text("repo", report)
      assertTrue(
        report.infos == 2,
        report.exit == 1,
        notes.contains("::error title=histos doctor: demo.mark::sum\"mary Fix: https://example.test/set"),
        notes.contains("::warning title=histos doctor: demo.mark::sum\"mary Fix: histos doctor"),
        notes.contains("::warning title=histos doctor: demo.mark::sum\"mary Fix: histos.scala"),
        notes.contains("::warning title=histos doctor: demo.mark::sum\"mary Fix: first step"),
        notes.contains("::error title=histos doctor: demo.late::timed out"),
        table.contains("| :x: | demo.mark | why |"),
        table.contains("| :white_check_mark: | demo.mark | why |"),
        table.contains("| :x: | demo.late | timed out |"),
        table.startsWith("## histos preflight: ci  (failed)"),
        encoded.contains(""""title":"ti\\tle""""),
        encoded.contains(""""summary":"sum\"mary""""),
        encoded.contains(""""timedOut":["demo.late"]"""),
        encoded.contains(""""exit":1"""),
        shown.contains("  x error    demo.mark"),
        shown.contains("  ! warning  demo.mark"),
        shown.contains("  i info     demo.mark"),
        shown.contains("fix  https://example.test/set  open settings"),
        shown.contains("fix  histos doctor"),
        shown.contains("fix  histos.scala"),
        shown.contains("fix  first step"),
        shown.contains("  x timed out demo.late"),
        shown.contains("1 errors, 1 warnings, 0 ok, 0 skipped, 1 timed out"),
      )
    ,
    test("a skipped check names the probe it needs and prints the command, the settings url, the file, or every step"):
      val command  = Outcome.Skipped(mark, Probe.Local, Fix.Command(ShellLine.trusted("histos test")))
      val settings =
        Outcome.Skipped(mark, Probe.GitHubAdmin, Fix.Settings(HttpUrl.trusted("https://example.test/s"), "click"))
      val config =
        Outcome.Skipped(mark, Probe.JobEnv, Fix.Config(RepoPath.trusted(".github/workflows/ci.yml"), Edit("a", "b")))
      val steps = Outcome.Skipped(mark, Probe.GitHubRead, Fix.Steps(::("gh auth login", List("gh auth status"))))
      val shown = Render.text("repo", DoctorReport(List(command, settings, config, steps), Nil))
      assertTrue(
        shown.contains("histos test"),
        shown.contains("https://example.test/s"),
        shown.contains(".github/workflows/ci.yml"),
        shown.contains("gh auth login"),
        shown.contains("gh auth status"),
        List("Local", "GitHubAdmin", "JobEnv", "GitHubRead").forall(probe => shown.contains(s"(needs $probe)")),
        shown.contains("0 errors, 0 warnings, 0 ok, 4 skipped"),
      )
    ,
    test("apply rewrites a present file, records a command, and leaves a denied setting"):
      val files    = Map("histos.scala" -> "keep before tail")
      val edit     = Fix.Config(RepoPath.trusted("histos.scala"), Edit("before", "after"))
      val missing  = Fix.Config(RepoPath.trusted("gone.scala"), Edit("x", "y"))
      val command  = Fix.Command(ShellLine.trusted("histos test"))
      val settings = Fix.Settings(HttpUrl.trusted("https://example.test/set"), "open")
      val steps    = Fix.Steps(::("first step", Nil))
      val kinds    = Set(FixKind.LocalEdit, FixKind.HistosCommand, FixKind.GitHubSetting)
      val printed  = FixApply.applyAll(List(edit, command), FixMode.Print, _ => FixApply.Answer.Yes, files)
      val applied  = FixApply.applyAll(
        List(edit, missing, command, settings, steps),
        FixMode.Apply(kinds),
        {
          case Fix.Settings(_, _) => FixApply.Answer.Yes
          case _                  => FixApply.Answer.NonTty
        },
        files,
      )
      val denied =
        FixApply.applyAll(List(settings), FixMode.Apply(Set(FixKind.GitHubSetting)), _ => FixApply.Answer.No, files)
      val held =
        FixApply.applyAll(List(command), FixMode.Apply(Set(FixKind.LocalEdit)), _ => FixApply.Answer.Yes, files)
      assertTrue(
        printed == (files, Nil),
        applied._1 == Map("histos.scala" -> "keep after tail", "gone.scala" -> ""),
        applied._2 == List("histos test"),
        denied == (files, Nil),
        held == (files, Nil),
        FixApply.kindOf(edit) == FixKind.LocalEdit,
        FixApply.kindOf(command) == FixKind.HistosCommand,
        FixApply.kindOf(settings) == FixKind.GitHubSetting,
        FixApply.kindOf(steps) == FixKind.HistosCommand,
        RefusalMap.refused(ApiCall.Rules, 500, "closed") == None,
      ),
  )

  private def found(level: Level, fix: Fix): Outcome =
    Outcome.Found(Note(mark, level, fix))

  private final case class Note(check: CheckId, level: Level, fix: Fix) extends Diagnostic:
    def title: String   = "ti\\tle"
    def summary: String = "sum\"mary"
    def explain: String = "why"
    def fixes: ::[Fix]  = ::(fix, Nil)
end DoctorRenderLawSpec
