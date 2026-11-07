package histos.cli

import histos.assembly.AssembleError
import histos.jdk.ToolchainError
import histos.load.PluginFault
import histos.plugin.PluginTaskError
import histos.preview.PreviewError
import histos.program.ProgramError
import histos.publish.LauncherError
import histos.report.{BuildFault, CallResult, Diagnostic, Rejection, Report, Severity, WorkflowFault}
import histos.resolve.ResolveError
import histos.schedule.{CacheError, RemoteError}
import histos.select.SelectError
import histos.server.Call
import histos.task.TaskError
import histos.testrun.{ModuleTestError, SuitePatternError, TestStatus, TestedCell}
import histos.toolchain.SurveyError
import histos.version.VersionError
import histos.watch.WatchError
import histos.workflow.CiError
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.GraphCompileError
import histos.{Platform, PlatformError, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.{Chunk, NonEmptyChunk}
import zio.test.*

object CliExitLawSpec extends ZIOSpecDefault:
  private val missing = Path.of("missing")
  private val lock    = ResolveError.NoLock(Path.of("lock"))
  private val noRoot  = GraphCompileError.NoRoot(ProjectName("api"))
  private val locked  = GraphCompileError.Lock(lock)
  private val io      = IoFailure(missing, IoFault.Read)
  private val cell    =
    TestedCell(ProjectName("api"), ScalaVersion("3.3.4"), Platform.Jvm, TestStatus.Failed(1))
  private val quiet =
    TestedCell(ProjectName("api"), ScalaVersion("3.3.4"), Platform.Jvm, TestStatus.NoTests)

  def spec = suite("exit laws")(
    test("a process status is 0, 1, or 2"):
      assertTrue(
        CliExit.Success.code == 0,
        CliExit.Usage.code == 2,
        CliExit.Program(0).code == 0,
        CliExit.Program(2).code == 2,
        CliExit.Program(7).code == 1,
        CliExit.Interrupted.code == 1,
        CliExit.fromCode(0) == CliExit.Success,
        CliExit.fromCode(2) == CliExit.Usage,
        CliExit.fromCode(7) == CliExit.TaskFailed,
      )
    ,
    test("a nonzero program status wins over a failed test"):
      val failed = CallResult.reported(tests = Chunk(cell), program = Some(2))
      assertTrue(
        CliExit.from(Call.Compile(Nil), failed) == CliExit.Usage,
        CliExit.from(Call.Version, CallResult.reported(program = Some(7))) == CliExit.TaskFailed,
        CliExit.from(Call.Version, CallResult.reported(program = Some(0))) == CliExit.Success,
        CliExit.from(Call.Compile(Nil), CallResult.reported(tests = Chunk(cell))) == CliExit.TestFailed,
        CliExit.from(Call.Compile(Nil), CallResult.reported(tests = Chunk(quiet))) == CliExit.Success,
        CliExit.from(Call.Compile(Nil), CallResult.reported()) == CliExit.Success,
      )
    ,
    test("an error line or an error diagnostic follows the call"):
      val warn = noted(Severity.Warn, "careful")
      val boom = noted(Severity.Error, "boom")
      val rows = List(
        (Call.Test(Nil, Nil, histos.testrun.TestMode.Run), CallResult.reported(summary = "boom"), CliExit.TestFailed),
        (Call.PublishLocal(Nil), CallResult.reported(summary = "boom"), CliExit.PublishFailed),
        (Call.Compile(Nil), CallResult.reported(summary = "boom"), CliExit.CompileFailed),
        (Call.Compile(Nil), warn, CliExit.Success),
        (Call.Compile(Nil), boom, CliExit.CompileFailed),
      )
      assertTrue(missed(rows) == Nil)
    ,
    test("a fault names the call that failed"):
      assertTrue(missed(faults) == Nil),
  )

  private def noted(severity: Severity, message: String): CallResult =
    CallResult.Reported(
      Report.empty.copy(diagnostics = Chunk(Diagnostic(None, None, None, None, None, severity, message)))
    )

  private def missed(rows: List[(Call, CallResult, CliExit)]): List[(Call, CliExit, CliExit)] =
    rows.flatMap: (call, result, expected) =>
      val got = CliExit.from(call, result)
      if got == expected then Nil else List((call, expected, got))

  private def faults: List[(Call, CallResult, CliExit)] =
    val compile                            = Call.Compile(Nil)
    val test                               = Call.Test(Nil, Nil, histos.testrun.TestMode.Run)
    val publish                            = Call.PublishLocal(Nil)
    val version                            = Call.Version
    val run                                = Call.Run("api", None)
    def fault(why: BuildFault): CallResult = CallResult.Fault(why)
    List(
      (version, fault(BuildFault.Load(histos.load.LoadError.MissingFile(missing))), CliExit.LoadFailed),
      (compile, fault(BuildFault.Resolve(lock)), CliExit.CompileFailed),
      (version, fault(BuildFault.Resolve(lock)), CliExit.ResolveFailed),
      (publish, fault(BuildFault.Resolve(lock)), CliExit.PublishFailed),
      (version, fault(BuildFault.Toolchain(ToolchainError.NoArchitecture)), CliExit.ToolchainFailed),
      (version, fault(BuildFault.Survey(SurveyError.Incomplete("report", "detail"))), CliExit.ToolchainFailed),
      (test, fault(BuildFault.Select(SelectError.Unknown(ProjectName("api")))), CliExit.TestFailed),
      (compile, fault(BuildFault.Select(SelectError.Unknown(ProjectName("api")))), CliExit.CompileFailed),
      (version, fault(BuildFault.Select(SelectError.Unknown(ProjectName("api")))), CliExit.TaskFailed),
      (version, fault(BuildFault.Pattern(SuitePatternError.Empty)), CliExit.Usage),
      (test, fault(BuildFault.Compile(locked)), CliExit.ResolveFailed),
      (test, fault(BuildFault.Compile(noRoot)), CliExit.TestFailed),
      (compile, fault(BuildFault.Compile(noRoot)), CliExit.CompileFailed),
      (publish, fault(BuildFault.Compile(noRoot)), CliExit.PublishFailed),
      (
        compile,
        fault(BuildFault.Compile(GraphCompileError.Cells(NonEmptyChunk.single(noRoot)))),
        CliExit.CompileFailed,
      ),
      (test, fault(BuildFault.Test(ModuleTestError.Pattern(SuitePatternError.Empty))), CliExit.Usage),
      (test, fault(BuildFault.Test(ModuleTestError.NoneMatched(Nil))), CliExit.TestFailed),
      (test, fault(BuildFault.Test(ModuleTestError.Lock(lock))), CliExit.ResolveFailed),
      (test, fault(BuildFault.Test(ModuleTestError.Compile(locked))), CliExit.ResolveFailed),
      (
        publish,
        fault(
          BuildFault.Publish(
            histos.publish.LocalPublishError.NoSources(ProjectName("api"), ScalaVersion("3.3.4"), Platform.Jvm)
          )
        ),
        CliExit.PublishFailed,
      ),
      (version, fault(BuildFault.Release("held")), CliExit.PublishFailed),
      (version, fault(BuildFault.Workflow(WorkflowFault.Files(CiError.NoDefaultBranch))), CliExit.TaskFailed),
      (version, fault(BuildFault.Workflow(WorkflowFault.Files(CiError.BadName("1")))), CliExit.WorkflowFailed),
      (version, fault(BuildFault.Program(ProgramError.Absent("api"))), CliExit.TaskFailed),
      (version, fault(BuildFault.Program(ProgramError.Lock(lock))), CliExit.ResolveFailed),
      (version, fault(BuildFault.Program(ProgramError.Compile(noRoot))), CliExit.CompileFailed),
      (version, fault(BuildFault.Platform(PlatformError.Unknown("bogus"))), CliExit.TaskFailed),
      (version, fault(BuildFault.Task(TaskError.EmptyId)), CliExit.TaskFailed),
      (version, fault(BuildFault.Plugin(PluginFault.Compiled(noRoot))), CliExit.TaskFailed),
      (version, fault(BuildFault.PluginTask(PluginTaskError.Shape("nope"))), CliExit.TaskFailed),
      (
        Call.PreviewCompile(Nil),
        fault(BuildFault.Preview(PreviewError.Cycle(ProjectName("api")))),
        CliExit.CompileFailed,
      ),
      (Call.PreviewTest(Nil), fault(BuildFault.Preview(PreviewError.Cycle(ProjectName("api")))), CliExit.TestFailed),
      (version, fault(BuildFault.Preview(PreviewError.Cycle(ProjectName("api")))), CliExit.TaskFailed),
      (
        Call.PreviewPublishLocal(Nil),
        fault(BuildFault.Preview(PreviewError.Cycle(ProjectName("api")))),
        CliExit.PublishFailed,
      ),
      (version, fault(BuildFault.Assemble(AssembleError.NoModule("api"))), CliExit.CompileFailed),
      (version, fault(BuildFault.Launcher(LauncherError.Absent(missing))), CliExit.ResolveFailed),
      (version, fault(BuildFault.Cache(CacheError.Io(io))), CliExit.LoadFailed),
      (version, fault(BuildFault.Remote(RemoteError.Off)), CliExit.LoadFailed),
      (version, fault(BuildFault.Store(io)), CliExit.LoadFailed),
      (test, fault(BuildFault.Io(io)), CliExit.ResolveFailed),
      (run, fault(BuildFault.Io(io)), CliExit.ResolveFailed),
      (compile, fault(BuildFault.Io(io)), CliExit.CompileFailed),
      (publish, fault(BuildFault.Io(io)), CliExit.PublishFailed),
      (Call.CachePath, fault(BuildFault.Io(io)), CliExit.LoadFailed),
      (version, fault(BuildFault.Io(io)), CliExit.TaskFailed),
      (version, fault(BuildFault.Version(VersionError.Unreadable)), CliExit.TaskFailed),
      (version, fault(BuildFault.Watch(WatchError.Read(missing))), CliExit.TaskFailed),
      (version, fault(BuildFault.Interrupted), CliExit.Interrupted),
      (version, fault(BuildFault.Rejected(Rejection.AssemblyDestination(".."))), CliExit.CompileFailed),
      (version, fault(BuildFault.Rejected(Rejection.TaskArguments("api"))), CliExit.TaskFailed),
      (version, fault(BuildFault.Rejected(Rejection.RunFailed)), CliExit.TaskFailed),
      (version, fault(BuildFault.Rejected(Rejection.WatchStopped)), CliExit.TaskFailed),
      (version, fault(BuildFault.Rejected(Rejection.PreviewProjects)), CliExit.Usage),
    )
end CliExitLawSpec
