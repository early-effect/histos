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
        CliExit.Failed.code == 1,
        CliExit.Usage.code == 2,
        CliExit.fromCode(0) == CliExit.Success,
        CliExit.fromCode(2) == CliExit.Usage,
        CliExit.fromCode(7) == CliExit.Failed,
      )
    ,
    test("a nonzero program status wins over a failed test"):
      val failed = CallResult.reported(tests = Chunk(cell), program = Some(2))
      assertTrue(
        CliExit.from(failed) == CliExit.Usage,
        CliExit.from(CallResult.reported(program = Some(7))) == CliExit.Failed,
        CliExit.from(CallResult.reported(program = Some(0))) == CliExit.Success,
        CliExit.from(CallResult.reported(tests = Chunk(cell))) == CliExit.Failed,
        CliExit.from(CallResult.reported(tests = Chunk(quiet))) == CliExit.Success,
        CliExit.from(CallResult.reported()) == CliExit.Success,
      )
    ,
    test("an error line or an error diagnostic fails, and a warning does not"):
      val rows = List(
        (CallResult.reported(summary = "boom"), CliExit.Failed),
        (noted(Severity.Warn, "careful"), CliExit.Success),
        (noted(Severity.Error, "boom"), CliExit.Failed),
      )
      assertTrue(missed(rows) == Nil)
    ,
    test("a fault fails, and a bad pattern or a refused request is usage"):
      assertTrue(missed(faults) == Nil),
  )

  private def noted(severity: Severity, message: String): CallResult =
    CallResult.Reported(
      Report.empty.copy(diagnostics = Chunk(Diagnostic(None, None, None, None, None, severity, message)))
    )

  private def missed(rows: List[(CallResult, CliExit)]): List[(CallResult, CliExit, CliExit)] =
    rows.flatMap: (result, expected) =>
      val got = CliExit.from(result)
      if got == expected then Nil else List((result, expected, got))

  private def faults: List[(CallResult, CliExit)] =
    def fault(why: BuildFault): CallResult = CallResult.Fault(why)
    List(
      (fault(BuildFault.Load(histos.load.LoadError.MissingFile(missing))), CliExit.Failed),
      (fault(BuildFault.Resolve(lock)), CliExit.Failed),
      (fault(BuildFault.Toolchain(ToolchainError.NoArchitecture)), CliExit.Failed),
      (fault(BuildFault.Survey(SurveyError.Incomplete("report", "detail"))), CliExit.Failed),
      (fault(BuildFault.Select(SelectError.Unknown(ProjectName("api")))), CliExit.Failed),
      (fault(BuildFault.Pattern(SuitePatternError.Empty)), CliExit.Usage),
      (fault(BuildFault.Compile(locked)), CliExit.Failed),
      (fault(BuildFault.Compile(noRoot)), CliExit.Failed),
      (fault(BuildFault.Compile(GraphCompileError.Cells(NonEmptyChunk.single(noRoot)))), CliExit.Failed),
      (fault(BuildFault.Test(ModuleTestError.Pattern(SuitePatternError.Empty))), CliExit.Usage),
      (fault(BuildFault.Test(ModuleTestError.NoneMatched(Nil))), CliExit.Failed),
      (fault(BuildFault.Test(ModuleTestError.Lock(lock))), CliExit.Failed),
      (fault(BuildFault.Test(ModuleTestError.Compile(locked))), CliExit.Failed),
      (
        fault(
          BuildFault.Publish(
            histos.publish.LocalPublishError.NoSources(ProjectName("api"), ScalaVersion("3.3.4"), Platform.Jvm)
          )
        ),
        CliExit.Failed,
      ),
      (fault(BuildFault.Release("held")), CliExit.Failed),
      (fault(BuildFault.Workflow(WorkflowFault.Files(CiError.NoDefaultBranch))), CliExit.Failed),
      (fault(BuildFault.Workflow(WorkflowFault.Files(CiError.BadName("1")))), CliExit.Failed),
      (fault(BuildFault.Program(ProgramError.Absent("api"))), CliExit.Failed),
      (fault(BuildFault.Program(ProgramError.Lock(lock))), CliExit.Failed),
      (fault(BuildFault.Program(ProgramError.Compile(noRoot))), CliExit.Failed),
      (fault(BuildFault.Platform(PlatformError.Unknown("bogus"))), CliExit.Failed),
      (fault(BuildFault.Task(TaskError.EmptyId)), CliExit.Failed),
      (fault(BuildFault.Plugin(PluginFault.Compiled(noRoot))), CliExit.Failed),
      (fault(BuildFault.PluginTask(PluginTaskError.Shape("nope"))), CliExit.Failed),
      (fault(BuildFault.Preview(PreviewError.Cycle(ProjectName("api")))), CliExit.Failed),
      (fault(BuildFault.Assemble(AssembleError.NoModule("api"))), CliExit.Failed),
      (fault(BuildFault.Launcher(LauncherError.Absent(missing))), CliExit.Failed),
      (fault(BuildFault.Cache(CacheError.Io(io))), CliExit.Failed),
      (fault(BuildFault.Remote(RemoteError.Off)), CliExit.Failed),
      (fault(BuildFault.Store(io)), CliExit.Failed),
      (fault(BuildFault.Io(io)), CliExit.Failed),
      (fault(BuildFault.Version(VersionError.Unreadable)), CliExit.Failed),
      (fault(BuildFault.Watch(WatchError.Read(missing))), CliExit.Failed),
      (fault(BuildFault.Interrupted), CliExit.Failed),
      (fault(BuildFault.Rejected(Rejection.AssemblyDestination(".."))), CliExit.Failed),
      (fault(BuildFault.Rejected(Rejection.TaskArguments("api"))), CliExit.Failed),
      (fault(BuildFault.Rejected(Rejection.RunFailed)), CliExit.Failed),
      (fault(BuildFault.Rejected(Rejection.WatchStopped)), CliExit.Failed),
      (fault(BuildFault.Rejected(Rejection.PreviewProjects)), CliExit.Usage),
    )
end CliExitLawSpec
