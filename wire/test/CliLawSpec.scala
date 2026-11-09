package histos.cli

import histos.{EnumLaw, Platform, WireSamples}
import histos.report.{CallResult, Diagnostic, Rejection, Report, Severity, WorkflowFault}
import histos.server.{Call, CallSamples}
import histos.testrun.{ModuleTestError, TestStatus, TestedCell}
import histos.workflow.CiError
import histos.zinc.{CompileProblem, CompileSeverity, GraphCompileError, PositionBase, SourceCount}
import histos.build.BuildEvent
import histos.publish.ScriptError
import histos.testrun.TestFeed

import java.nio.file.Path

import zio.{Chunk, Ref, ZIO}
import zio.test.*

object CliLawSpec extends ZIOSpecDefault:
  import WireSamples.*

  def spec = suite("cli laws")(
    test("a child status is success at 0, usage at 2, and a failed task at every other code"):
      check(Gen.int): code =>
        val exit  = CliExit.fromCode(code)
        val named =
          if code == 0 then CliExit.Success
          else if code == 2 then CliExit.Usage
          else CliExit.TaskFailed
        val status = if code == 0 then 0 else if code == 2 then 2 else 1
        assertTrue(exit == named, exit.code == status)
    ,
    test("process status is 0 or 2 only for success, usage, and those program codes"):
      val named = List[CliExit](
        CliExit.Success,
        CliExit.TaskFailed,
        CliExit.Usage,
        CliExit.LoadFailed,
        CliExit.ToolchainFailed,
        CliExit.ResolveFailed,
        CliExit.WorkflowFailed,
        CliExit.TestFailed,
        CliExit.CompileFailed,
        CliExit.PublishFailed,
        CliExit.Program(7),
        CliExit.Interrupted,
      )
      check(Gen.int): status =>
        val code = CliExit.Program(status).code
        assertTrue(
          EnumLaw.covered(named),
          named.map(_.code) == List(0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1),
          code == (if status == 0 then 0 else if status == 2 then 2 else 1),
        )
    ,
    test("a clean report succeeds, a non-zero program status wins, and a failed test is not a compile"):
      val all = everyCall
      assertTrue(all.nonEmpty, EnumLaw.covered(CallSamples.oneEach)) &&
      agrees(all, CallResult.Reported(Report.empty), _ => CliExit.Success) &&
      agrees(all, reported(program = Some(0)), _ => CliExit.Success) &&
      agrees(all, reported(program = Some(2), failed = true, err = "boom"), _ => CliExit.Usage) &&
      agrees(all, reported(program = Some(9), failed = true, err = "boom"), _ => CliExit.TaskFailed) &&
      agrees(all, reported(failed = true), _ => CliExit.TestFailed) &&
      agrees(all, reported(status = TestStatus.Passed, err = "boom"), failedCall) &&
      agrees(all, reported(status = TestStatus.NoTests), _ => CliExit.Success) &&
      agrees(all, reported(status = TestStatus.Unmatched), _ => CliExit.Success) &&
      agrees(all, reported(status = TestStatus.Passed), _ => CliExit.Success) &&
      agrees(all, reported(err = "boom"), failedCall) &&
      agrees(all, reported(severity = Severity.Error), failedCall) &&
      agrees(all, reported(severity = Severity.Warn), _ => CliExit.Success) &&
      agrees(all, reported(severity = Severity.Info), _ => CliExit.Success)
    ,
    test("load, toolchain, and resolve faults stay on those exits, and the call retargets resolve"):
      val all    = everyCall
      val loaded = CallResult.fault(histos.report.BuildFault.Load(histos.load.LoadError.NoEntry))
      val chain  = CallResult.fault(histos.report.BuildFault.Toolchain(histos.jdk.ToolchainError.Denied))
      val survey = CallResult.fault(
        histos.report.BuildFault.Survey(histos.toolchain.SurveyError.Incomplete("report", "detail"))
      )
      val resolve = CallResult.fault(histos.report.BuildFault.Resolve(missing))
      val other   = CallResult.fault(histos.report.BuildFault.Resolve(ResolveErrorNoLock))
      agrees(all, loaded, _ => CliExit.LoadFailed) &&
      agrees(all, chain, _ => CliExit.ToolchainFailed) &&
      agrees(all, survey, _ => CliExit.ToolchainFailed) &&
      agrees(all, resolve, resolveExit) &&
      agrees(all, other, resolveExit)
    ,
    test("a failed publish call is publish, a failed test call is test, and a failed compile is compile"):
      val all = everyCall
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Select(histos.select.SelectError.Duplicate(core))),
        selectExit,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Pattern(histos.testrun.SuitePatternError.Empty)),
        _ => CliExit.Usage,
      ) &&
      agrees(all, CallResult.fault(histos.report.BuildFault.Compile(noRoot)), call => compileExit(call, noRoot)) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Compile(GraphCompileError.Lock(missing))),
        call => compileExit(call, GraphCompileError.Lock(missing)),
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Compile(GraphCompileError.LockRead(path))),
        call => compileExit(call, GraphCompileError.LockRead(path)),
      ) &&
      agrees(
        all,
        CallResult.fault(
          histos.report.BuildFault.Compile(GraphCompileError.Cells(zio.NonEmptyChunk(GraphCompileError.Lock(missing))))
        ),
        call => compileExit(call, GraphCompileError.Cells(zio.NonEmptyChunk(GraphCompileError.Lock(missing)))),
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Test(ModuleTestError.Absent(core))),
        _ => CliExit.TestFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(
          histos.report.BuildFault.Test(ModuleTestError.Pattern(histos.testrun.SuitePatternError.Empty))
        ),
        _ => CliExit.Usage,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Test(ModuleTestError.Lock(missing))),
        _ => CliExit.ResolveFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Test(ModuleTestError.LockRead(path))),
        _ => CliExit.ResolveFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Test(ModuleTestError.Compile(GraphCompileError.Lock(missing)))),
        _ => CliExit.ResolveFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Test(ModuleTestError.Compile(GraphCompileError.LockRead(path)))),
        _ => CliExit.ResolveFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Test(ModuleTestError.Compile(noRoot))),
        _ => CliExit.TestFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Publish(histos.publish.LocalPublishError.NothingToPublish)),
        _ => CliExit.PublishFailed,
      ) &&
      agrees(all, CallResult.fault(histos.report.BuildFault.Release("closed")), _ => CliExit.PublishFailed)
    ,
    test("workflow, program, preview, io, and a rejection each keep their own exit"):
      val all    = everyCall
      val branch = CallResult.fault(
        histos.report.BuildFault.Workflow(WorkflowFault.Files(CiError.NoDefaultBranch))
      )
      val files   = CallResult.fault(histos.report.BuildFault.Workflow(WorkflowFault.Files(CiError.Pins("x"))))
      val scripts = CallResult.fault(
        histos.report.BuildFault.Workflow(WorkflowFault.Scripts(ScriptError.Revision))
      )
      agrees(all, branch, _ => CliExit.TaskFailed) &&
      agrees(all, files, _ => CliExit.WorkflowFailed) &&
      agrees(all, scripts, _ => CliExit.WorkflowFailed) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Program(histos.program.ProgramError.Absent("nope"))),
        _ => CliExit.TaskFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Program(histos.program.ProgramError.Lock(missing))),
        _ => CliExit.ResolveFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Program(histos.program.ProgramError.LockRead(path))),
        _ => CliExit.ResolveFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(
          histos.report.BuildFault.Program(histos.program.ProgramError.Compile(GraphCompileError.Lock(missing)))
        ),
        _ => CliExit.ResolveFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Program(histos.program.ProgramError.Compile(noRoot))),
        _ => CliExit.CompileFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Platform(histos.PlatformError.Unknown("nope"))),
        _ => CliExit.TaskFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Task(histos.task.TaskError.EmptyId)),
        _ => CliExit.TaskFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Plugin(histos.load.PluginFault.Tested(ModuleTestError.Absent(core)))),
        _ => CliExit.TaskFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.PluginTask(histos.plugin.PluginTaskError.Shape("nope"))),
        _ => CliExit.TaskFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Preview(histos.preview.PreviewError.Cycle(core))),
        previewExit,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Assemble(histos.assembly.AssembleError.NoModule("nope"))),
        _ => CliExit.CompileFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Launcher(histos.publish.LauncherError.Missing)),
        _ => CliExit.ResolveFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Cache(histos.schedule.CacheError.Io(io))),
        _ => CliExit.LoadFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Remote(histos.schedule.RemoteError.Off)),
        _ => CliExit.LoadFailed,
      ) &&
      agrees(all, CallResult.fault(histos.report.BuildFault.Store(io)), _ => CliExit.LoadFailed) &&
      agrees(all, CallResult.fault(histos.report.BuildFault.Io(io)), ioExit) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Version(histos.version.VersionError.Unreadable)),
        _ => CliExit.TaskFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Watch(histos.watch.WatchError.Read(path))),
        _ => CliExit.TaskFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Format(histos.format.FormatError.NotDeclared)),
        _ => CliExit.TaskFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(histos.report.BuildFault.Format(histos.format.FormatError.Resolved(ResolveErrorNoLock))),
        _ => CliExit.ResolveFailed,
      ) &&
      agrees(
        all,
        CallResult.fault(
          histos.report.BuildFault.Format(
            histos.format.FormatError.Selected(histos.select.SelectError.Unknown(histos.ProjectName.trusted("nope")))
          )
        ),
        _ => CliExit.Usage,
      ) &&
      agrees(all, CallResult.fault(histos.report.BuildFault.Interrupted), _ => CliExit.Interrupted) &&
      assertTrue(EnumLaw.covered(oneRejection)) &&
      rejections.foldLeft(assertTrue(oneRejection.length == rejections.length - 1)) { (acc, rejection) =>
        acc && agrees(all, CallResult.fault(histos.report.BuildFault.Rejected(rejection)), _ => rejected(rejection))
      }
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
    test("a tally names modules, and zero modules is silence"):
      check(Gen.int(0, 12), Gen.int(0, 12), Gen.int(0, 40), Gen.int(0, 40), Gen.int(0, 40)):
        (modules, failed, compiled, skipped, classes) =>
          val files                                     = Some(SourceCount(compiled, skipped))
          val written                                   = Some(classes)
          def line(parts: List[String]): Option[String] =
            if modules == 0 then None
            else
              val text = parts.mkString("; ")
              Some(if failed == 0 then text else s"$text failed: $failed")
          val full = line(
            List(
              s"files compiled: $compiled skipped: $skipped",
              s"modules: $modules",
              s"classes: $classes",
            )
          )
          val filesOnly   = line(List(s"files compiled: $compiled skipped: $skipped", s"modules: $modules"))
          val classesOnly = line(List(s"modules: $modules", s"classes: $classes"))
          val modulesOnly = line(List(s"modules: $modules"))
          val suites      =
            if compiled + failed + skipped + modules == 0 then None
            else Some(s"tests passed: $compiled failed: $failed cached: $skipped modules: $modules")
          assertTrue(
            Lines.tally(modules, failed, files, written) == full,
            Lines.tally(modules, failed, files, None) == filesOnly,
            Lines.tally(modules, failed, None, written) == classesOnly,
            Lines.tally(modules, failed, None, None) == modulesOnly,
            Lines.testTally(modules, failed, files, written) == full.map(text => s"test $text"),
            Lines.testTally(modules, failed, None, None) == modulesOnly.map(text => s"test $text"),
            Lines.suites(compiled, failed, skipped, modules) == suites,
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
      val shown = events.toList match
        case head :: _ => head
        case Nil       => BuildEvent.Err("no event")
      for
        out <- Ref.make(Chunk.empty[String])
        err <- Ref.make(Chunk.empty[String])
        _   <- CompileConsole.show(
          Show(path, shown),
          line => out.update(lines => lines :+ line).unit,
          line => err.update(lines => lines :+ line).unit,
        )
        outs <- out.get
        errs <- err.get
      yield
        val (left, right) = Show.transcript(path, events)
        assertTrue(
          shown match
            case BuildEvent.Compile(feed) =>
              Show(path, shown) == CompileConsole.lines(path, feed)
            case _ => false
          ,
          outs == Chunk(Lines.compiling(core, scala3, Platform.Jvm)),
          errs.isEmpty,
          left == Chunk(
            Lines.compiling(core, scala3, Platform.Jvm),
            "ok",
            Lines.tested(TestedCell(core, scala3, Platform.Jvm, TestStatus.Passed)),
            "",
          ),
          right == Chunk(Lines.cell(core, scala3, Platform.Jvm, "no"), "bad"),
        ),
  )

  private def everyCall: List[Call] =
    CallSamples.every

  private def agrees(calls: List[Call], result: CallResult, expect: Call => CliExit): TestResult =
    assertTrue(calls.map(call => CliExit.from(call, result)) == calls.map(expect))

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

  private def publish(call: Call): Boolean =
    call match
      case Call.PublishLocal(_) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PublishPromote(_) |
          Call.PublishDrop(_) | Call.PublishAssets(_) | Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) =>
        true
      case _ => false

  private def testCall(call: Call): Boolean =
    call match
      case Call.Test(_, _, _) | Call.PreviewTest(_) => true
      case _                                        => false

  private def compileLike(call: Call): Boolean =
    call match
      case Call.Compile(_) | Call.PreviewCompile(_) | Call.Assembly(_) => true
      case _                                                           => false

  private def failedCall(call: Call): CliExit =
    if testCall(call) then CliExit.TestFailed
    else if publish(call) then CliExit.PublishFailed
    else
      call match
        case Call.Format(_, _) => CliExit.TaskFailed
        case _                 => CliExit.CompileFailed

  private def resolveExit(call: Call): CliExit =
    if compileLike(call) then CliExit.CompileFailed
    else if publish(call) then CliExit.PublishFailed
    else CliExit.ResolveFailed

  private def selectExit(call: Call): CliExit =
    if compileLike(call) then CliExit.CompileFailed
    else if testCall(call) then CliExit.TestFailed
    else if publish(call) then CliExit.PublishFailed
    else CliExit.TaskFailed

  private def compileExit(call: Call, error: GraphCompileError): CliExit =
    error match
      case GraphCompileError.Lock(_) | GraphCompileError.LockRead(_) => resolveExit(call)
      case _                                                         =>
        if testCall(call) then CliExit.TestFailed
        else if publish(call) then CliExit.PublishFailed
        else CliExit.CompileFailed

  private def previewExit(call: Call): CliExit =
    call match
      case Call.PreviewCompile(_) | Call.Compile(_) => CliExit.CompileFailed
      case Call.PreviewTest(_) | Call.Test(_, _, _) => CliExit.TestFailed
      case other if publish(other)                  => CliExit.PublishFailed
      case _                                        => CliExit.TaskFailed

  private def ioExit(call: Call): CliExit =
    call match
      case Call.Test(_, _, _) | Call.Run(_, _)                                            => CliExit.ResolveFailed
      case Call.Compile(_) | Call.Assembly(_) | Call.PreviewCompile(_)                    => CliExit.CompileFailed
      case other if publish(other)                                                        => CliExit.PublishFailed
      case Call.CachePath | Call.CacheList | Call.CacheClean(_) | Call.CachePromote(_, _) =>
        CliExit.LoadFailed
      case _ => CliExit.TaskFailed

  private def rejected(rejection: Rejection): CliExit =
    rejection match
      case Rejection.AssemblyDestination(_)                                          => CliExit.CompileFailed
      case Rejection.TaskArguments(_) | Rejection.RunFailed | Rejection.WatchStopped => CliExit.TaskFailed
      case _                                                                         => CliExit.Usage

  private val oneRejection: List[Rejection] =
    List(
      Rejection.ResolveAction("nope"),
      Rejection.WorkflowAction("nope"),
      Rejection.LauncherAction("nope"),
      Rejection.AssemblyDestination(".."),
      Rejection.TaskArguments("inspect"),
      Rejection.NotATool,
      Rejection.Watch("again"),
      Rejection.RunFailed,
      Rejection.WatchStopped,
      Rejection.PreviewProjects,
      Rejection.PreviewOperation("graph"),
      Rejection.MissingRunName,
      Rejection.Parameter(histos.plugin.ParamNameError.Empty),
      Rejection.ProjectName(histos.ProjectNameError.Empty),
      Rejection.SuitePattern(histos.testrun.SuitePatternError.Empty),
      Rejection.RunId(histos.task.TaskError.EmptyId),
      Rejection.TestMode(histos.testrun.TestModeError.Unknown("nope")),
      Rejection.CacheClean(histos.cache.CacheCleanModeError.Unknown("nope")),
      Rejection.Flag(histos.plugin.FlagNameError.Empty),
      Rejection.TaskPlatform("inspect"),
      Rejection.CommandHelp(List("nope")),
      Rejection.FormatMode("diff"),
      Rejection.FormatTargets,
      Rejection.FormatPath("/etc"),
    )

  private val rejections: List[Rejection] =
    List(
      Rejection.ResolveAction("nope"),
      Rejection.WorkflowAction("nope"),
      Rejection.LauncherAction("nope"),
      Rejection.AssemblyDestination(".."),
      Rejection.TaskArguments("inspect"),
      Rejection.NotATool,
      Rejection.Watch("again"),
      Rejection.RunFailed,
      Rejection.WatchStopped,
      Rejection.PreviewProjects,
      Rejection.PreviewOperation("graph"),
      Rejection.MissingRunName,
      Rejection.Parameter(histos.plugin.ParamNameError.Empty),
      Rejection.ProjectName(histos.ProjectNameError.Empty),
      Rejection.SuitePattern(histos.testrun.SuitePatternError.Empty),
      Rejection.RunId(histos.task.TaskError.EmptyId),
      Rejection.TestMode(histos.testrun.TestModeError.Unknown("nope")),
      Rejection.CacheClean(histos.cache.CacheCleanModeError.Unknown("nope")),
      Rejection.Flag(histos.plugin.FlagNameError.Empty),
      Rejection.TaskPlatform("inspect"),
      Rejection.CommandHelp(List("nope")),
      Rejection.CommandHelp(Nil),
      Rejection.FormatMode("diff"),
      Rejection.FormatTargets,
      Rejection.FormatPath("/etc"),
    )

  private val ResolveErrorNoLock: histos.resolve.ResolveError =
    histos.resolve.ResolveError.NoLock(path)

  extension (line: CompileConsole.Line)
    private def text: String = line match
      case CompileConsole.Line.Out(text) => text
      case CompileConsole.Line.Err(text) => text
end CliLawSpec
