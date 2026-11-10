package histos

import histos.assembly.AssembleError
import histos.build.BuildEvent
import histos.cache.CacheCleanModeError
import histos.load.{LoadError, PluginFault}
import histos.plugin.{FlagNameError, ParamNameError, PluginTaskError}
import histos.preview.PreviewError
import histos.program.ProgramError
import histos.publish.{LauncherError, LocalPublishError, ScriptError}
import histos.report.{
  BuildFault,
  CallResult,
  Diagnostic,
  ModuleTally,
  Severity,
  Observation,
  ObserveError,
  Rejection,
  Report,
  Tally,
  WireJson,
  WorkflowFault,
}
import histos.resolve.ResolveError
import histos.schedule.{CacheError, RemoteError}
import histos.select.SelectError
import histos.task.TaskError
import histos.testrun.{ModuleTestError, SuitePatternError, TestFeed, TestModeError, TestStatus, TestedCell}
import histos.toolchain.SurveyError
import histos.version.VersionError
import histos.watch.WatchError
import histos.workflow.CiError
import histos.zinc.{
  CompileError,
  CompileFeed,
  CompileInput,
  CompileProblem,
  CompileSeverity,
  CompiledCell,
  GraphCompileError,
  KeyMiss,
  PositionBase,
  SourceCount,
}
import zio.json.*
import zio.test.*
import zio.{Cause, Chunk, Config, NonEmptyChunk}

import WireJson.given

object ReportLawSpec extends ZIOSpecDefault:
  import WireSamples.*

  private val count = SourceCount(3, 1)
  private val cell  = CompiledCell(
    core,
    scala3,
    Platform.Jvm,
    path,
    4,
    Some(sha),
    true,
    Some(count),
    Some(2),
  )
  private val problem = CompileProblem(
    "bad",
    Some(other),
    CompileSeverity.Error,
    Some(1),
    Some(2),
    Some(3),
    Some(4),
    PositionBase.Dotty,
  )
  private val logLine = CompileProblem(
    "log",
    None,
    CompileSeverity.Info,
    None,
    None,
    None,
    None,
    PositionBase.Zinc,
  )
  private val failed = GraphCompileError.Failed(core, scala3, Platform.Jvm, CompileError.NoSources)
  private val tested = TestedCell(core, scala3, Platform.Jvm, TestStatus.Failed(2))

  private val compileFeeds: List[CompileFeed] = List(
    CompileFeed.Began(core, scala3, Platform.Jvm),
    CompileFeed.Diagnostic(core, scala3, Platform.Jvm, problem),
    CompileFeed.Miss(core, scala3, Platform.Js, KeyMiss.Changed(List(CompileInput.Sources))),
    CompileFeed.Done(cell),
    CompileFeed.Failed(GraphCompileError.NoRoot(core)),
  )

  private val testFeeds: List[TestFeed] = List(
    TestFeed.Begin(core, scala3, Platform.Jvm),
    TestFeed.Out(core, scala3, Platform.Jvm, "ok"),
    TestFeed.Err(core, scala3, Platform.Js, "bad"),
    TestFeed.Done(tested),
    TestFeed.Cached(core, scala3, Platform.Native, "demo.Suite"),
  )

  private val events: List[BuildEvent] = List(
    BuildEvent.Compile(CompileFeed.Began(core, scala3, Platform.Jvm)),
    BuildEvent.Test(TestFeed.Begin(core, scala3, Platform.Jvm)),
    BuildEvent.Out("ok"),
    BuildEvent.Err("bad"),
  )

  private val faults: List[BuildFault] = List(
    BuildFault.Load(LoadError.NoEntry),
    BuildFault.Resolve(ResolveError.NotFound("dev.zio", "zio", "0")),
    BuildFault.Toolchain(histos.jdk.ToolchainError.Denied),
    BuildFault.Survey(SurveyError.Incomplete("report", "detail")),
    BuildFault.Select(SelectError.Duplicate(core)),
    BuildFault.Pattern(SuitePatternError.Empty),
    BuildFault.Compile(GraphCompileError.NoRoot(core)),
    BuildFault.Test(ModuleTestError.Absent(core)),
    BuildFault.Publish(LocalPublishError.NothingToPublish),
    BuildFault.Release("no release"),
    BuildFault.Workflow(WorkflowFault.Files(CiError.NoDefaultBranch)),
    BuildFault.Program(ProgramError.Absent("gone", List(api.value))),
    BuildFault.Platform(PlatformError.Unknown("nope")),
    BuildFault.Task(TaskError.EmptyId),
    BuildFault.Plugin(PluginFault.Tested(ModuleTestError.Absent(core))),
    BuildFault.PluginTask(PluginTaskError.Shape("nope")),
    BuildFault.Preview(PreviewError.Cycle(core)),
    BuildFault.Assemble(AssembleError.NoModule("nope")),
    BuildFault.Launcher(LauncherError.Missing),
    BuildFault.Cache(CacheError.Io(io)),
    BuildFault.Remote(RemoteError.Off),
    BuildFault.Store(io),
    BuildFault.Io(io),
    BuildFault.Version(VersionError.Unreadable),
    BuildFault.Watch(WatchError.Read(path)),
    BuildFault.Format(histos.format.FormatError.CoreDrift(Nil, List(histos.format.ScalafmtPackage("o", "a", "1")))),
    BuildFault.Interrupted,
    BuildFault.Rejected(Rejection.NotATool),
  )

  private val rejections: List[Rejection] = List(
    Rejection.ResolveAction("nope"),
    Rejection.WorkflowAction("nope"),
    Rejection.LauncherAction("nope"),
    Rejection.AssemblyDestination("nope"),
    Rejection.TaskArguments("inspect"),
    Rejection.NotATool,
    Rejection.Watch("stopped"),
    Rejection.RunFailed,
    Rejection.WatchStopped,
    Rejection.PreviewProjects,
    Rejection.PreviewOperation("nope"),
    Rejection.MissingRunName,
    Rejection.Parameter(ParamNameError.Bad("1")),
    Rejection.ProjectName(ProjectNameError.Bad("1core")),
    Rejection.SuitePattern(SuitePatternError.Blank("a b")),
    Rejection.RunId(TaskError.EmptyId),
    Rejection.TestMode(TestModeError.Unknown("nope")),
    Rejection.CacheClean(CacheCleanModeError.Unknown("nope")),
    Rejection.Flag(FlagNameError.Bad("1flag")),
    Rejection.TaskPlatform("inspect"),
    Rejection.CommandHelp(List("nope")),
    Rejection.FormatMode("diff"),
    Rejection.FormatTargets,
    Rejection.FormatPath("/etc"),
  )

  private val results: List[CallResult] = List(
    CallResult.Reported(Report.empty),
    CallResult.Fault(BuildFault.Interrupted),
  )

  def spec = suite("report laws")(
    JsonLaw.enumLaw("every call result round-trips, and a wrong kind is rejected", results),
    test("a report keeps its lines, tally, diagnostics, tests, and program"):
      val placed = Diagnostic(Some("A.scala"), Some(1), Some(2), Some(3), Some(4), Severity.Error, "bad")
      val bare   = Diagnostic(None, None, None, None, None, Severity.Warn, "note")
      val tally  = ModuleTally(core.value, scala3.value, "jvm", Some(3), Some(1), Some(9))
      val seen   = Observation(Chunk(tally), Some(3), Some(1), Some(9), 1)
      val full   = Report(
        Chunk("out"),
        Chunk("err"),
        Some("demo"),
        Some(seen),
        Chunk(placed, bare),
        Chunk(tested),
        Some(0),
      )
      val empty = Report.empty.toJson
      assertTrue(
        full.toJson.fromJson[Report] == Right(full),
        empty.fromJson[Report] == Right(Report.empty),
        empty == """{"out":[],"err":[],"diagnostics":[],"tests":[]}""",
        "1".fromJson[Report] == Left("(expected '{' got '1')"),
        "{".fromJson[Report] == Left("Unexpected end of input"),
      )
    ,
    JsonLaw.enumLaw("every build fault round-trips, and a wrong kind is rejected", faults),
    JsonLaw.enumLaw("every rejection round-trips, and a wrong kind is rejected", rejections),
    test("an empty command help keeps its name list on the document"):
      val help = Rejection.CommandHelp(Nil)
      assertTrue(help.toJson == """{"CommandHelp":{"names":[]}}""")
    ,
    JsonLaw.enumLaw(
      "every workflow fault round-trips, and a wrong kind is rejected",
      List(WorkflowFault.Files(CiError.Pins("x")), WorkflowFault.Scripts(ScriptError.Revision)),
    ),
    test("a tally records classes only with sources, and only four failures name a cell"):
      val full      = Tally.recorded(core, scala3, Platform.Js, Some(count), Some(9))
      val uncounted = Tally.recorded(core, scala3, Platform.Js, Some(count), None)
      val unknown   = Tally.recorded(core, scala3, Platform.Native, None, Some(9))
      val named     = List(
        failed,
        GraphCompileError.Assets(core, scala3, Platform.Jvm, AssetError.Collides(ResourceName.trusted("Demo.class"))),
        GraphCompileError.MissingUpstream(core, api, scala3, Platform.Jvm),
        GraphCompileError.IncompatibleUpstream(core, api, scala3, scala213, Platform.Js),
      )
      val poison = List(
        GraphCompileError.Runtime(histos.jdk.ToolchainError.Denied),
        GraphCompileError.Target(SelectError.Duplicate(core)),
        GraphCompileError.UnknownCell(core, scala3),
        GraphCompileError.NoRoot(core),
        GraphCompileError.NoSources(core, path),
        GraphCompileError.Cycle(core),
        GraphCompileError.Lock(ResolveError.NotFound("dev.zio", "zio", "0")),
        GraphCompileError.LockRead(path),
        GraphCompileError.RootRead(path),
        GraphCompileError.Output(path),
        GraphCompileError.Cells(NonEmptyChunk(failed)),
      )
      assertTrue(
        full == ModuleTally(core.value, scala3.value, "js", Some(3), Some(1), Some(9)),
        uncounted.compiled.contains(3),
        uncounted.classes.isEmpty,
        unknown.compiled.isEmpty,
        unknown.skipped.isEmpty,
        unknown.classes.isEmpty,
        unknown.platform == "native",
        named.map(Tally.failed) == List(
          Some(ModuleTally(core.value, scala3.value, "jvm", None, None, None)),
          Some(ModuleTally(core.value, scala3.value, "jvm", None, None, None)),
          Some(ModuleTally(core.value, scala3.value, "jvm", None, None, None)),
          Some(ModuleTally(core.value, scala3.value, "js", None, None, None)),
        ),
        poison.map(error => Tally.failed(error).isEmpty) == poison.map(_ => true),
      )
    ,
    test("merging observations sums counts, and a missing count poisons that side"):
      val row    = Tally.recorded(core, scala3, Platform.Jvm, Some(count), Some(9))
      val left   = Observation(Chunk(row), Some(3), Some(1), Some(9), 1)
      val right  = Observation(Chunk.empty, Some(4), Some(2), Some(5), 2)
      val poison = Observation(Chunk.empty, None, None, None, 1)
      val both   = Tally.merge(Some(left), Some(right))
      val lost   = Tally.merge(Some(left), Some(poison))
      assertTrue(
        Tally.merge(None, None).isEmpty,
        Tally.merge(None, Some(left)).contains(left),
        Tally.merge(Some(left), None).contains(left),
        both.exists(item =>
          item.compiled.contains(7) && item.skipped.contains(3) && item.classes.contains(14) &&
            item.moduleCount == 3 && item.modules.length == 1
        ),
        lost.exists(item =>
          item.compiled.isEmpty && item.skipped.isEmpty && item.classes.isEmpty && item.moduleCount == 2
        ),
        Tally.combine(List(Some(left), None, Some(right))).exists(_.compiled.contains(7)),
      )
    ,
    test("a config error round-trips, and a failed cause keeps its message"):
      val invalid     = Config.Error.InvalidData(Chunk("jdk"), "bad")
      val missing     = Config.Error.MissingData(Chunk.empty, "gone")
      val unsupported = Config.Error.Unsupported(Chunk("node"), "no")
      val quiet       = List(
        invalid,
        missing,
        unsupported,
        Config.Error.And(invalid, missing),
        Config.Error.Or(missing, unsupported),
        Config.Error.SourceUnavailable(Chunk("home"), "down", Cause.empty),
      )
      val blown: Config.Error =
        Config.Error.SourceUnavailable(Chunk("home"), "down", Cause.fail(new IllegalStateException("boom")))
      val died: Config.Error =
        Config.Error.SourceUnavailable(Chunk("home"), "down", Cause.die(new IllegalStateException("died")))
      val blownJson = blown.toJson
      val diedJson  = died.toJson
      val again     = blownJson.fromJson[Config.Error]
      val diedAgain = diedJson.fromJson[Config.Error]
      assertTrue(quiet.map(error => error.toJson.fromJson[Config.Error]) == quiet.map(Right(_))) &&
      assertTrue(again.map(_.toJson) == Right(blownJson), diedAgain.map(_.toJson) == Right(diedJson)) &&
      causeMessage(again, _.failureOption, "boom") &&
      causeMessage(diedAgain, _.dieOption, "died") &&
      assertTrue(
        "1".fromJson[Config.Error] == Left("(expected '{' got '1')"),
        "{".fromJson[Config.Error] == Left("Unexpected end of input"),
      )
    ,
    test("a non-empty chunk and a cons reject an empty document"):
      val chunk = NonEmptyChunk("a")
      val cons  = ::("a", Nil)
      assertTrue(
        chunk.toJson.fromJson[NonEmptyChunk[String]] == Right(chunk),
        "[]".fromJson[NonEmptyChunk[String]] == Left("(Chunk was empty)"),
        "1".fromJson[NonEmptyChunk[String]] == Left("(expected '[' got '1')"),
        "[".fromJson[NonEmptyChunk[String]] == Left("Unexpected end of input"),
        cons.toJson.fromJson[::[String]] == Right(cons),
        "[]".fromJson[::[String]] == Left("(empty list)"),
        "1".fromJson[::[String]] == Left("(expected '[' got '1')"),
      )
    ,
    JsonLaw.enumLaw("every compile feed round-trips, and a wrong kind is rejected", compileFeeds),
    JsonLaw.enumLaw("every test feed round-trips, and a wrong kind is rejected", testFeeds),
    JsonLaw.enumLaw("every build event round-trips, and a wrong kind is rejected", events),
    JsonLaw.enumLaw("every test status round-trips, and a wrong kind is rejected", testStatuses),
    JsonLaw.enumLaw("every key miss round-trips, and a wrong kind is rejected", keyMisses),
    JsonLaw.enumLaw("every compile severity round-trips, and a wrong kind is rejected", CompileSeverity.values.toList),
    JsonLaw.enumLaw("every position base round-trips, and a wrong kind is rejected", PositionBase.values.toList),
    JsonLaw.values("a compile problem keeps a path, and a log line keeps none", List(problem, logLine)),
    JsonLaw.enumLaw(
      "every observe error round-trips, and a wrong kind is rejected",
      List(
        ObserveError.Missing("tally.json"),
        ObserveError.Unreadable("tally.json"),
        ObserveError.Malformed("tally.json"),
      ),
    ),
  )

  private val testStatuses: List[TestStatus] = List(
    TestStatus.NoTests,
    TestStatus.Unmatched,
    TestStatus.Passed,
    TestStatus.Failed(2),
  )

  private val keyMisses: List[KeyMiss] = List(
    KeyMiss.NoPrior,
    KeyMiss.Changed(List(CompileInput.Scala, CompileInput.Sources)),
  )

  private def causeMessage(
      error: Either[String, Config.Error],
      pick: Cause[Throwable] => Option[Throwable],
      text: String,
  ): TestResult =
    error match
      case Right(Config.Error.SourceUnavailable(_, _, cause)) =>
        assertTrue(pick(cause).map(_.getMessage).contains(text))
      case other =>
        assertTrue(other == Left(text))
end ReportLawSpec
