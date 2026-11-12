package histos.cli

import histos.{EnumLaw, Platform, WireSamples}
import histos.report.{BuildFault, CallResult, Diagnostic, Rejection, Report, Severity, WorkflowFault}
import histos.testrun.{ModuleTestError, TestStatus, TestedCell}
import histos.workflow.CiError
import histos.zinc.{CompileProblem, CompileSeverity, GraphCompileError, PositionBase, SourceCount}
import histos.build.BuildEvent
import histos.publish.ScriptError
import histos.testrun.TestFeed

import java.nio.file.Path

import zio.{Chunk, Ref, ZIO}
import zio.test.*

/** The exit laws pin the process status each result gives. */
object CliLawSpec extends ZIOSpecDefault:
  import CallResult.fault
  import WireSamples.*

  def spec = suite("cli laws")(
    test("a child status is 0 at 0, 2 at 2, and 1 at every other code, and each exit has its status"):
      val named = List(CliExit.Success, CliExit.Failed, CliExit.Usage)
      for other <- check(Gen.int.filter(status => status != 0 && status != 2)): status =>
          assertTrue(CliExit.fromCode(status) == CliExit.Failed)
      yield other && assertTrue(
        CliExit.fromCode(0) == CliExit.Success,
        CliExit.fromCode(2) == CliExit.Usage,
        EnumLaw.covered(named),
        named.map(_.code) == List(0, 1, 2),
      )
    ,
    test("a clean report is 0, a non-zero program status wins, and a failed test, error line, or error is 1"):
      assertTrue(
        misses(
          List(
            CallResult.Reported(Report.empty)                        -> 0,
            reported(program = Some(0))                              -> 0,
            reported(program = Some(2), failed = true, err = "boom") -> 2,
            reported(program = Some(9), failed = true, err = "boom") -> 1,
            reported(failed = true)                                  -> 1,
            reported(status = TestStatus.Passed, err = "boom")       -> 1,
            reported(status = TestStatus.NoTests)                    -> 0,
            reported(status = TestStatus.Unmatched)                  -> 0,
            reported(status = TestStatus.Passed)                     -> 0,
            reported(err = "boom")                                   -> 1,
            reported(severity = Severity.Error)                      -> 1,
            reported(severity = Severity.Warn)                       -> 0,
            reported(severity = Severity.Info)                       -> 0,
          )
        ) == Nil
      )
    ,
    test("load, toolchain, and resolve faults are 1"):
      assertTrue(
        misses(
          List(
            fault(BuildFault.Load(histos.load.LoadError.NoEntry))                                 -> 1,
            fault(BuildFault.Toolchain(histos.jdk.ToolchainError.Denied))                         -> 1,
            fault(BuildFault.Survey(histos.toolchain.SurveyError.Incomplete("report", "detail"))) -> 1,
            fault(BuildFault.Resolve(missing))                                                    -> 1,
            fault(BuildFault.Resolve(histos.resolve.ResolveError.NoLock(path)))                   -> 1,
          )
        ) == Nil
      )
    ,
    test("select, compile, test, and publish faults are 1, and a bad test pattern is usage"):
      val lock = GraphCompileError.Lock(missing)
      assertTrue(
        misses(
          List(
            fault(BuildFault.Select(histos.select.SelectError.Duplicate(core)))                     -> 1,
            fault(BuildFault.Pattern(histos.testrun.SuitePatternError.Empty))                       -> 2,
            fault(BuildFault.Compile(noRoot))                                                       -> 1,
            fault(BuildFault.Compile(lock))                                                         -> 1,
            fault(BuildFault.Compile(GraphCompileError.LockRead(path)))                             -> 1,
            fault(BuildFault.Compile(GraphCompileError.Cells(zio.NonEmptyChunk(lock))))             -> 1,
            fault(BuildFault.Test(ModuleTestError.Absent(core)))                                    -> 1,
            fault(BuildFault.Test(ModuleTestError.Pattern(histos.testrun.SuitePatternError.Empty))) -> 2,
            fault(BuildFault.Test(ModuleTestError.Lock(missing)))                                   -> 1,
            fault(BuildFault.Test(ModuleTestError.LockRead(path)))                                  -> 1,
            fault(BuildFault.Test(ModuleTestError.Compile(lock)))                                   -> 1,
            fault(BuildFault.Test(ModuleTestError.Compile(GraphCompileError.LockRead(path))))       -> 1,
            fault(BuildFault.Test(ModuleTestError.Compile(noRoot)))                                 -> 1,
            fault(BuildFault.Publish(histos.publish.LocalPublishError.NothingToPublish))            -> 1,
            fault(BuildFault.Release("closed"))                                                     -> 1,
          )
        ) == Nil
      )
    ,
    test("every other fault is 1, a project selection in fmt is usage, and a rejection is usage unless a run failed"):
      val selected = histos.format.FormatError.Selected(
        histos.select.SelectError.Unknown(histos.ProjectName.trusted("nope"))
      )
      assertTrue(
        EnumLaw.covered(rejections.map(_._1)),
        misses(
          List(
            fault(BuildFault.Workflow(WorkflowFault.Files(CiError.NoDefaultBranch)))                               -> 1,
            fault(BuildFault.Workflow(WorkflowFault.Files(CiError.Pins("x"))))                                     -> 1,
            fault(BuildFault.Workflow(WorkflowFault.Scripts(ScriptError.Revision)))                                -> 1,
            fault(BuildFault.Program(histos.program.ProgramError.Absent("nope")))                                  -> 1,
            fault(BuildFault.Program(histos.program.ProgramError.Lock(missing)))                                   -> 1,
            fault(BuildFault.Program(histos.program.ProgramError.LockRead(path)))                                  -> 1,
            fault(BuildFault.Program(histos.program.ProgramError.Compile(GraphCompileError.Lock(missing))))        -> 1,
            fault(BuildFault.Program(histos.program.ProgramError.Compile(noRoot)))                                 -> 1,
            fault(BuildFault.Platform(histos.PlatformError.Unknown("nope")))                                       -> 1,
            fault(BuildFault.Task(histos.task.TaskError.EmptyId))                                                  -> 1,
            fault(BuildFault.Plugin(histos.load.PluginFault.Tested(ModuleTestError.Absent(core))))                 -> 1,
            fault(BuildFault.PluginTask(histos.plugin.PluginTaskError.Shape("nope")))                              -> 1,
            fault(BuildFault.Preview(histos.preview.PreviewError.Cycle(core)))                                     -> 1,
            fault(BuildFault.Assemble(histos.assembly.AssembleError.NoModule("nope")))                             -> 1,
            fault(BuildFault.Launcher(histos.publish.LauncherError.Missing))                                       -> 1,
            fault(BuildFault.Cache(histos.schedule.CacheError.Io(io)))                                             -> 1,
            fault(BuildFault.Remote(histos.schedule.RemoteError.Off))                                              -> 1,
            fault(BuildFault.Store(io))                                                                            -> 1,
            fault(BuildFault.Io(io))                                                                               -> 1,
            fault(BuildFault.Version(histos.version.VersionError.Unreadable))                                      -> 1,
            fault(BuildFault.Watch(histos.watch.WatchError.Read(path)))                                            -> 1,
            fault(BuildFault.Format(histos.format.FormatError.NotDeclared))                                        -> 1,
            fault(BuildFault.Format(histos.format.FormatError.Resolved(histos.resolve.ResolveError.NoLock(path)))) -> 1,
            fault(BuildFault.Format(selected))                                                                     -> 2,
            fault(BuildFault.Interrupted)                                                                          -> 1,
            fault(BuildFault.StaleApi("", "0.2.9-0123456789ab-SNAPSHOT"))                                          -> 1,
          ) ++ rejections.map((rejection, code) => fault(BuildFault.Rejected(rejection)) -> code)
        ) == Nil,
      )
    ,
    test("an unchanged compile names the skip, and a changed compile prints only counts it has"):
      check(Gen.int(0, 30), Gen.int(0, 30), Gen.int(0, 30)): (compiled, skipped, classes) =>
        val sources = SourceCount(compiled, skipped)
        val id      = s"${core.value} ${scala3.value} jvm"
        val js      = s"${core.value} ${scala3.value} js"
        assertTrue(
          Lines.compiling(core, scala3, Platform.Jvm) == s"compiling $id",
          Lines.outcome(core, scala3, Platform.Jvm, false, None, None).contains(s"$id skipped unchanged"),
          Lines
            .outcome(core, scala3, Platform.Jvm, false, Some(sources), None)
            .contains(s"$id skipped unchanged files compiled: $compiled skipped: $skipped"),
          Lines.outcome(core, scala3, Platform.Jvm, true, None, Some(classes)).isEmpty,
          Lines
            .outcome(core, scala3, Platform.Jvm, true, Some(sources), Some(classes))
            .contains(s"$id files compiled: $compiled skipped: $skipped classes: $classes"),
          Lines
            .outcome(core, scala3, Platform.Jvm, true, Some(sources), None)
            .contains(s"$id files compiled: $compiled skipped: $skipped"),
          Lines.testOutcome(core, scala3, Platform.Js, true, None, None) == s"$js test compiled",
          Lines.testOutcome(core, scala3, Platform.Js, false, None, None) == s"$js test compile unchanged",
          Lines.testOutcome(core, scala3, Platform.Js, true, Some(sources), Some(classes)) ==
            s"$js test files compiled: $compiled skipped: $skipped classes: $classes",
          Lines.testOutcome(core, scala3, Platform.Js, false, Some(sources), None) ==
            s"$js test compile unchanged files compiled: $compiled skipped: $skipped",
        )
    ,
    test("a tally names modules and only the counts it has, zero modules is silence, and a test tally says test"):
      val files   = Gen.option(Gen.int(0, 40).zipWith(Gen.int(0, 40))(SourceCount(_, _)))
      val written = Gen.option(Gen.int(0, 40))
      for
        silent <- check(Gen.int(0, 12), files, written): (failed, counted, classes) =>
          assertTrue(
            Lines.tally(0, failed, counted, classes).isEmpty,
            Lines.testTally(0, failed, counted, classes).isEmpty,
          )
        spoken <- check(Gen.int(1, 12), Gen.int(0, 12), files, written): (modules, failed, counted, classes) =>
          val line = Lines.tally(modules, failed, counted, classes)
          assertTrue(
            line.exists(_.contains(s"modules: $modules")),
            line.exists(_.startsWith("files compiled: ")) == counted.isDefined,
            line.exists(_.contains("classes: ")) == classes.isDefined,
            line.exists(_.endsWith(s" failed: $failed")) == (failed > 0),
            Lines.testTally(modules, failed, counted, classes) == line.map(text => s"test $text"),
          )
        suites <- check(Gen.int(0, 2), Gen.int(0, 2), Gen.int(0, 2), Gen.int(0, 2)):
          (passed, failed, cached, modules) =>
            assertTrue(
              Lines.suites(passed, failed, cached, modules).isEmpty == (passed + failed + cached + modules == 0)
            )
      yield silent && spoken && suites && assertTrue(
        Lines
          .tally(3, 1, Some(SourceCount(5, 2)), Some(9))
          .contains(
            "files compiled: 5 skipped: 2; modules: 3; classes: 9 failed: 1"
          ),
        Lines.suites(4, 1, 2, 3).contains("tests passed: 4 failed: 1 cached: 2 modules: 3"),
      )
    ,
    test("a dotty position is one past the compiler, and a zinc position is already displayed"):
      val repo    = Path.of("/work/repo")
      val source  = repo.resolve("src/A.scala")
      val problem = CompileProblem("nope", Some(source), CompileSeverity.Error, Some(3), Some(4), Some(3), Some(8), _)
      val (zinc, zincLine)   = Lines.heard(repo, problem(PositionBase.Zinc))
      val (dotty, dottyLine) = Lines.heard(repo, problem(PositionBase.Dotty))
      val logged = CompileProblem("nope", None, CompileSeverity.Warn, None, None, None, None, PositionBase.Zinc)
      assertTrue(
        Lines.relative(repo, source) == "src/A.scala",
        Lines.relative(repo, other) == other.toString,
        zinc.line.contains(3),
        zinc.column.contains(4),
        zinc.severity == Severity.Error,
        zincLine == "src/A.scala:3:4: error: nope",
        dotty.line.contains(4),
        dotty.column.contains(5),
        dottyLine == "src/A.scala:4:5: error: nope",
        Lines.diagnostic(repo, logged) == "warn: nope",
        Lines.cell(core, scala3, Platform.Native, "") == s"${core.value} ${scala3.value} native",
        Lines.cell(core, scala3, Platform.Native, "hi") == s"${core.value} ${scala3.value} native hi",
        Lines.watching("/work") == "watching /work",
        Lines.tested(TestedCell(core, scala3, Platform.Jvm, TestStatus.Failed(2))) ==
          s"${core.value} ${scala3.value} jvm failed 2",
        Lines.cached(core, scala3, Platform.Jvm, "demo.Suite") == s"${core.value} ${scala3.value} jvm demo.Suite cached",
      )
    ,
    test("the console keeps the tally after it is read, and a failed cell poisons the counts"):
      for
        session <- CompileConsole.open(path)
        began   <- session.hear(histos.zinc.CompileFeed.Began(core, scala3, Platform.Jvm))
        before  <- session.tally
        _       <- session.hear(
          histos.zinc.CompileFeed.Done(
            histos.zinc
              .CompiledCell(core, scala3, Platform.Jvm, path, 1, Some(sha), true, Some(SourceCount(2, 1)), Some(3))
          )
        )
        first    <- session.tally
        second   <- session.tally
        _        <- session.hear(histos.zinc.CompileFeed.Failed(noRoot))
        poisoned <- session.tally
        seen     <- session.observation
        notes    <- CompileConsole.notes
        quiet    <- notes.tally
        line     <- notes(core, scala3, Platform.Js, histos.zinc.CompileFacts(true, Some(SourceCount(1, 0)), Some(2)))
        noted    <- notes.tally
        again    <- notes.tally
      yield assertTrue(
        began.map(_.text) == Chunk(Lines.compiling(core, scala3, Platform.Jvm)),
        before.isEmpty,
        first.contains("files compiled: 2 skipped: 1; modules: 1; classes: 3"),
        second == first,
        poisoned.contains("modules: 2 failed: 1"),
        poisoned.exists(!_.contains("files compiled")),
        seen.map(_.moduleCount).contains(2),
        seen.exists(_.compiled.isEmpty),
        quiet.isEmpty,
        line == Lines.testOutcome(core, scala3, Platform.Js, true, Some(SourceCount(1, 0)), Some(2)),
        noted == again,
        noted.contains("test files compiled: 1 skipped: 0; modules: 1; classes: 2"),
      )
    ,
    test("show routes compile, test, and plain lines without mixing the channels"):
      val done   = TestFeed.Done(TestedCell(core, scala3, Platform.Jvm, TestStatus.Passed))
      val events = Chunk[BuildEvent](
        BuildEvent.Compile(histos.zinc.CompileFeed.Began(core, scala3, Platform.Jvm)),
        BuildEvent.Test(TestFeed.Err(core, scala3, Platform.Jvm, "no")),
        BuildEvent.Out("ok"),
        BuildEvent.Err("bad"),
        BuildEvent.Test(done),
      )
      for
        out <- Ref.make(Chunk.empty[String])
        err <- Ref.make(Chunk.empty[String])
        _   <- ZIO.foreachDiscard(events): event =>
          CompileConsole.show(Show(path, event), line => out.update(_ :+ line), line => err.update(_ :+ line))
        outs <- out.get
        errs <- err.get
      yield
        val (left, right) = Show.transcript(path, events)
        assertTrue(
          outs == left,
          errs == right,
          left == Chunk(
            Lines.compiling(core, scala3, Platform.Jvm),
            "ok",
            Lines.tested(TestedCell(core, scala3, Platform.Jvm, TestStatus.Passed)),
            "",
          ),
          right == Chunk(Lines.cell(core, scala3, Platform.Jvm, "no"), "bad"),
        ),
  )

  /** The rows whose result does not give the row's status. */
  private def misses(rows: List[(CallResult, Int)]): List[(CallResult, Int)] =
    rows.filterNot((result, code) => CliExit.from(result).code == code)

  private def reported(
      status: TestStatus = TestStatus.Passed,
      failed: Boolean = false,
      err: String = "",
      severity: Severity = Severity.Info,
      program: Option[Int] = None,
  ): CallResult =
    val tests = Chunk(TestedCell(core, scala3, Platform.Jvm, if failed then TestStatus.Failed(1) else status))
    val lines = if err.isEmpty then Chunk.empty else Chunk(err)
    val found = Chunk(Diagnostic(None, None, None, None, None, severity, "note"))
    CallResult.Reported(Report(Chunk.empty, lines, None, None, found, tests, program))

  /** One rejection per case, and its status. A failed run is 1, and every other refusal is usage. */
  private val rejections: List[(Rejection, Int)] =
    List(
      Rejection.ResolveAction("nope")                                        -> 2,
      Rejection.WorkflowAction("nope")                                       -> 2,
      Rejection.LauncherAction("nope")                                       -> 2,
      Rejection.AssemblyDestination("..")                                    -> 1,
      Rejection.TaskArguments("inspect")                                     -> 1,
      Rejection.NotATool                                                     -> 2,
      Rejection.Watch("again")                                               -> 2,
      Rejection.RunFailed                                                    -> 1,
      Rejection.WatchStopped                                                 -> 1,
      Rejection.PreviewProjects                                              -> 2,
      Rejection.PreviewOperation("graph")                                    -> 2,
      Rejection.MissingRunName                                               -> 2,
      Rejection.Parameter(histos.plugin.ParamNameError.Empty)                -> 2,
      Rejection.ProjectName(histos.ProjectNameError.Empty)                   -> 2,
      Rejection.SuitePattern(histos.testrun.SuitePatternError.Empty)         -> 2,
      Rejection.RunId(histos.task.TaskError.EmptyId)                         -> 2,
      Rejection.TestMode(histos.testrun.TestModeError.Unknown("nope"))       -> 2,
      Rejection.CacheClean(histos.cache.CacheCleanModeError.Unknown("nope")) -> 2,
      Rejection.Flag(histos.plugin.FlagNameError.Empty)                      -> 2,
      Rejection.TaskPlatform("inspect")                                      -> 2,
      Rejection.CommandHelp(List("nope"))                                    -> 2,
      Rejection.FormatMode("diff")                                           -> 2,
      Rejection.FormatTargets                                                -> 2,
      Rejection.FormatPath("/etc")                                           -> 2,
    )

  extension (line: CompileConsole.Line)
    private def text: String = line match
      case CompileConsole.Line.Out(text) => text
      case CompileConsole.Line.Err(text) => text
end CliLawSpec
