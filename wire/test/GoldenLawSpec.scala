package histos.server

import histos.assembly.AssembleError
import histos.build.BuildEvent
import histos.format.FormatError
import histos.load.{LoadError, PluginFault}
import histos.plugin.{PluginArgs, PluginTaskError}
import histos.preview.PreviewError
import histos.program.ProgramError
import histos.publish.{LauncherError, LocalPublishError, ScriptError}
import histos.report.{
  BuildFault,
  CallResult,
  Diagnostic,
  ModuleTally,
  Observation,
  Rejection,
  Report,
  Severity,
  WireJson,
  WorkflowFault,
}
import histos.resolve.ResolveError
import histos.schedule.{CacheError, RemoteError}
import histos.select.SelectError
import histos.task.{TaskError, TaskId}
import histos.testrun.{ModuleTestError, SuitePatternError, TestFeed, TestMode, TestStatus, TestedCell}
import histos.toolchain.SurveyError
import histos.version.VersionError
import histos.watch.WatchError
import histos.workflow.CiError
import histos.workspace.IoFault
import histos.zinc.{
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
import histos.{CiFault, JsonLaw, Platform, PlatformError, ProjectNameError, WireSamples}

import java.nio.file.Path

import zio.{Chunk, NonEmptyChunk}
import zio.test.*

import WireJson.given

/** The bytes between the JVM server and the native client. Each document is a literal, and the wire module runs these
  * laws on the JVM and on Scala Native, so an encode on one platform and a decode on the other read the same text.
  */
object GoldenLawSpec extends ZIOSpecDefault:
  import WireSamples.*

  private val cell     = """"module":"core","version":"3.10.0""""
  private val ioDoc    = """{"path":"/work/repo","fault":"Read"}"""
  private val emptyDoc = """{"out":[],"err":[],"diagnostics":[],"tests":[]}"""

  private val tested  = TestedCell(core, scala3, Platform.Jvm, TestStatus.Failed(2))
  private val problem =
    CompileProblem("bad", Some(other), CompileSeverity.Error, Some(1), Some(2), Some(3), Some(4), PositionBase.Dotty)
  private val compiled =
    CompiledCell(core, scala3, Platform.Jvm, path, 4, Some(sha), true, Some(SourceCount(3, 1)), Some(2))

  private val full = Report(
    Chunk("out"),
    Chunk("err"),
    Some("demo"),
    Some(
      Observation(
        Chunk(ModuleTally(core.value, scala3.value, "jvm", Some(3), Some(1), Some(9))),
        Some(3),
        Some(1),
        Some(9),
        1,
      )
    ),
    Chunk(
      Diagnostic(Some("A.scala"), Some(1), Some(2), Some(3), Some(4), Severity.Error, "bad"),
      Diagnostic(None, None, None, None, None, Severity.Warn, "note"),
    ),
    Chunk(tested),
    Some(0),
  )

  private val fullDoc =
    s"""{"out":["out"],"err":["err"],"run":"demo","observation":{"modules":[{$cell,"platform":"jvm","compiled":3,""" +
      """"skipped":1,"classes":9}],"compiled":3,"skipped":1,"classes":9,"moduleCount":1},"diagnostics":[{"path":""" +
      """"A.scala","line":1,"column":2,"endLine":3,"endColumn":4,"severity":"error","message":"bad"},""" +
      s"""{"severity":"warn","message":"note"}],"tests":[{$cell,"platform":"jvm","status":{"Failed":{"exit":2}}}],""" +
      """"program":0}"""

  private val messages: List[(Message, String)] = List(
    Message.Ready                                   -> """{"Ready":{}}""",
    Message.Event(BuildEvent.Out("ok"))             -> """{"Event":{"event":{"Out":{"text":"ok"}}}}""",
    Message.Done(CallResult.Reported(Report.empty)) -> s"""{"Done":{"result":{"Reported":{"report":$emptyDoc}}}}""",
    Message.Marker(1)                               -> """{"Marker":{"histosExit":1}}""",
  )

  private val events: List[(BuildEvent, String)] = List(
    BuildEvent.Compile(CompileFeed.Began(core, scala3, Platform.Jvm)) ->
      s"""{"Compile":{"feed":{"Began":{$cell,"platform":"jvm"}}}}""",
    BuildEvent.Test(TestFeed.Begin(core, scala3, Platform.Jvm)) ->
      s"""{"Test":{"feed":{"Begin":{$cell,"platform":"jvm"}}}}""",
    BuildEvent.Out("ok")  -> """{"Out":{"text":"ok"}}""",
    BuildEvent.Err("bad") -> """{"Err":{"text":"bad"}}""",
  )

  private val compileFeeds: List[(CompileFeed, String)] = List(
    CompileFeed.Began(core, scala3, Platform.Jvm)               -> s"""{"Began":{$cell,"platform":"jvm"}}""",
    CompileFeed.Diagnostic(core, scala3, Platform.Jvm, problem) ->
      (s"""{"Diagnostic":{$cell,"platform":"jvm","problem":{"message":"bad","path":"/other/A.scala",""" +
        """"severity":"Error","startLine":1,"startColumn":2,"endLine":3,"endColumn":4,"base":"Dotty"}}}"""),
    CompileFeed.Miss(core, scala3, Platform.Js, KeyMiss.Changed(List(CompileInput.Sources))) ->
      s"""{"Miss":{$cell,"platform":"js","change":{"Changed":{"names":["Sources"]}}}}""",
    CompileFeed.Done(compiled) ->
      (s"""{"Done":{"cell":{$cell,"platform":"jvm","classes":"/work/repo","classFiles":4,"key":"${sha.value}",""" +
        """"changed":true,"sources":{"compiled":3,"skipped":1},"classesWritten":2}}}"""),
    CompileFeed.Failed(GraphCompileError.NoRoot(core)) -> """{"Failed":{"error":{"NoRoot":{"module":"core"}}}}""",
  )

  private val testFeeds: List[(TestFeed, String)] = List(
    TestFeed.Begin(core, scala3, Platform.Jvm)     -> s"""{"Begin":{$cell,"platform":"jvm"}}""",
    TestFeed.Out(core, scala3, Platform.Jvm, "ok") -> s"""{"Out":{$cell,"platform":"jvm","text":"ok"}}""",
    TestFeed.Err(core, scala3, Platform.Js, "bad") -> s"""{"Err":{$cell,"platform":"js","text":"bad"}}""",
    TestFeed.Done(tested)                          ->
      s"""{"Done":{"cell":{$cell,"platform":"jvm","status":{"Failed":{"exit":2}}}}}""",
    TestFeed.Cached(core, scala3, Platform.Native, "demo.Suite") ->
      s"""{"Cached":{$cell,"platform":"native","suite":"demo.Suite"}}""",
  )

  private val statuses: List[(TestStatus, String)] = List(
    TestStatus.NoTests   -> """{"NoTests":{}}""",
    TestStatus.Unmatched -> """{"Unmatched":{}}""",
    TestStatus.Passed    -> """{"Passed":{}}""",
    TestStatus.Failed(2) -> """{"Failed":{"exit":2}}""",
  )

  private val keyMisses: List[(KeyMiss, String)] = List(
    KeyMiss.NoPrior                                                 -> """{"NoPrior":{}}""",
    KeyMiss.Changed(List(CompileInput.Scala, CompileInput.Sources)) -> """{"Changed":{"names":["Scala","Sources"]}}""",
  )

  private val results: List[(CallResult, String)] = List(
    CallResult.Reported(full)                -> s"""{"Reported":{"report":$fullDoc}}""",
    CallResult.Fault(BuildFault.Interrupted) -> """{"Fault":{"fault":{"Interrupted":{}}}}""",
  )

  private val faults: List[(BuildFault, String)] = List(
    BuildFault.Load(LoadError.NoEntry) -> """{"Load":{"error":{"NoEntry":{}}}}""",
    BuildFault.Resolve(missing)        ->
      """{"Resolve":{"error":{"NotFound":{"organization":"dev.zio","name":"zio","revision":"0"}}}}""",
    BuildFault.Toolchain(histos.jdk.ToolchainError.Denied)        -> """{"Toolchain":{"error":{"Denied":{}}}}""",
    BuildFault.Survey(SurveyError.Incomplete("report", "detail")) ->
      """{"Survey":{"error":{"Incomplete":{"report":"report","detail":"detail"}}}}""",
    BuildFault.Select(SelectError.Duplicate(core)) -> """{"Select":{"error":{"Duplicate":{"name":"core"}}}}""",
    BuildFault.Pattern(SuitePatternError.Empty)    -> """{"Pattern":{"error":{"Empty":{}}}}""",
    BuildFault.Compile(GraphCompileError.Cells(NonEmptyChunk(noRoot))) ->
      """{"Compile":{"error":{"Cells":{"errors":[{"NoRoot":{"module":"core"}}]}}}}""",
    BuildFault.Test(ModuleTestError.Absent(core))          -> """{"Test":{"error":{"Absent":{"module":"core"}}}}""",
    BuildFault.Publish(LocalPublishError.NothingToPublish) -> """{"Publish":{"error":{"NothingToPublish":{}}}}""",
    BuildFault.Release("no release")                       -> """{"Release":{"detail":"no release"}}""",
    BuildFault.Workflow(WorkflowFault.Files(CiError.NoDefaultBranch)) ->
      """{"Workflow":{"error":{"Files":{"error":{"NoDefaultBranch":{}}}}}}""",
    BuildFault.Program(ProgramError.Absent("gone", List(api.value))) ->
      """{"Program":{"error":{"Absent":{"name":"gone","near":["api"]}}}}""",
    BuildFault.Platform(PlatformError.Unknown("nope")) -> """{"Platform":{"error":{"Unknown":{"raw":"nope"}}}}""",
    BuildFault.Task(TaskError.EmptyId)                 -> """{"Task":{"error":{"EmptyId":{}}}}""",
    BuildFault.Plugin(PluginFault.Tested(ModuleTestError.Absent(core))) ->
      """{"Plugin":{"error":{"Tested":{"error":{"Absent":{"module":"core"}}}}}}""",
    BuildFault.PluginTask(PluginTaskError.Shape("nope")) -> """{"PluginTask":{"error":{"Shape":{"raw":"nope"}}}}""",
    BuildFault.Preview(PreviewError.Cycle(core))         -> """{"Preview":{"error":{"Cycle":{"module":"core"}}}}""",
    BuildFault.Assemble(AssembleError.NoModule("nope"))  -> """{"Assemble":{"error":{"NoModule":{"name":"nope"}}}}""",
    BuildFault.Launcher(LauncherError.Missing)           -> """{"Launcher":{"error":{"Missing":{}}}}""",
    BuildFault.Cache(CacheError.Io(io))                  -> s"""{"Cache":{"error":{"Io":{"failure":$ioDoc}}}}""",
    BuildFault.Remote(RemoteError.Off)                   -> """{"Remote":{"error":{"Off":{}}}}""",
    BuildFault.Store(io)                                 -> s"""{"Store":{"failure":$ioDoc}}""",
    BuildFault.Io(io)                                    -> s"""{"Io":{"failure":$ioDoc}}""",
    BuildFault.Version(VersionError.Unreadable)          -> """{"Version":{"error":{"Unreadable":{}}}}""",
    BuildFault.Watch(WatchError.Read(path))              -> """{"Watch":{"error":{"Read":{"path":"/work/repo"}}}}""",
    BuildFault.Format(FormatError.NotDeclared)           -> """{"Format":{"error":{"NotDeclared":{}}}}""",
    BuildFault.Interrupted                               -> """{"Interrupted":{}}""",
    BuildFault.StaleApi("0.2.9-0123456789ab-SNAPSHOT", "0.2.9-ba9876543210-SNAPSHOT") ->
      """{"StaleApi":{"have":"0.2.9-0123456789ab-SNAPSHOT","want":"0.2.9-ba9876543210-SNAPSHOT"}}""",
    BuildFault.Rejected(Rejection.NotATool) -> """{"Rejected":{"rejection":{"NotATool":{}}}}""",
  )

  private val rejections: List[(Rejection, String)] = List(
    Rejection.ResolveAction("nope")                            -> """{"ResolveAction":{"raw":"nope"}}""",
    Rejection.WorkflowAction("nope")                           -> """{"WorkflowAction":{"raw":"nope"}}""",
    Rejection.LauncherAction("nope")                           -> """{"LauncherAction":{"raw":"nope"}}""",
    Rejection.AssemblyDestination("nope")                      -> """{"AssemblyDestination":{"raw":"nope"}}""",
    Rejection.TaskArguments("inspect")                         -> """{"TaskArguments":{"id":"inspect"}}""",
    Rejection.NotATool                                         -> """{"NotATool":{}}""",
    Rejection.Watch("stopped")                                 -> """{"Watch":{"detail":"stopped"}}""",
    Rejection.RunFailed                                        -> """{"RunFailed":{}}""",
    Rejection.WatchStopped                                     -> """{"WatchStopped":{}}""",
    Rejection.PreviewProjects                                  -> """{"PreviewProjects":{}}""",
    Rejection.PreviewOperation("nope")                         -> """{"PreviewOperation":{"raw":"nope"}}""",
    Rejection.MissingRunName                                   -> """{"MissingRunName":{}}""",
    Rejection.Parameter(histos.plugin.ParamNameError.Bad("1")) -> """{"Parameter":{"error":{"Bad":{"raw":"1"}}}}""",
    Rejection.ProjectName(ProjectNameError.Bad("1core"))   -> """{"ProjectName":{"error":{"Bad":{"raw":"1core"}}}}""",
    Rejection.SuitePattern(SuitePatternError.Blank("a b")) -> """{"SuitePattern":{"error":{"Blank":{"raw":"a b"}}}}""",
    Rejection.RunId(TaskError.EmptyId)                     -> """{"RunId":{"error":{"EmptyId":{}}}}""",
    Rejection.TestMode(histos.testrun.TestModeError.Unknown("nope")) ->
      """{"TestMode":{"error":{"Unknown":{"raw":"nope"}}}}""",
    Rejection.CacheClean(histos.cache.CacheCleanModeError.Unknown("nope")) ->
      """{"CacheClean":{"error":{"Unknown":{"raw":"nope"}}}}""",
    Rejection.Flag(histos.plugin.FlagNameError.Bad("1flag")) -> """{"Flag":{"error":{"Bad":{"raw":"1flag"}}}}""",
    Rejection.TaskPlatform("inspect")                        -> """{"TaskPlatform":{"name":"inspect"}}""",
    Rejection.CommandHelp(List("nope"))                      -> """{"CommandHelp":{"names":["nope"]}}""",
    Rejection.FormatMode("diff")                             -> """{"FormatMode":{"raw":"diff"}}""",
    Rejection.FormatTargets                                  -> """{"FormatTargets":{}}""",
    Rejection.FormatPath("/etc")                             -> """{"FormatPath":{"raw":"/etc"}}""",
    Rejection.Retired("testOnly", "test --only")             ->
      """{"Retired":{"word":"testOnly","use":"test --only"}}""",
    Rejection.PublishToken(histos.publish.PublishToken.Error.Empty) -> """{"PublishToken":{"error":{"Empty":{}}}}""",
  )

  private val workflowFaults: List[(WorkflowFault, String)] = List(
    WorkflowFault.Files(CiError.NoDefaultBranch) -> """{"Files":{"error":{"NoDefaultBranch":{}}}}""",
    WorkflowFault.Scripts(ScriptError.Revision)  -> """{"Scripts":{"error":{"Revision":{}}}}""",
  )

  /** A job the graph cannot render carries its `CiFault`. */
  private val arranged: List[(WorkflowFault, String)] = List(
    WorkflowFault.Files(CiError.Fault(CiFault.UnknownNeed(job, histos.plugin.JobName("release")))) ->
      """{"Files":{"error":{"Fault":{"fault":{"UnknownNeed":{"job":"docs","need":"release"}}}}}}"""
  )

  private val severities: List[(CompileSeverity, String)] = List(
    CompileSeverity.Error -> "\"Error\"",
    CompileSeverity.Warn  -> "\"Warn\"",
    CompileSeverity.Info  -> "\"Info\"",
  )

  private val bases: List[(PositionBase, String)] = List(
    PositionBase.Zinc  -> "\"Zinc\"",
    PositionBase.Dotty -> "\"Dotty\"",
  )

  private val ioFaults: List[(IoFault, String)] = List(
    IoFault.Read   -> "\"Read\"",
    IoFault.Write  -> "\"Write\"",
    IoFault.Stat   -> "\"Stat\"",
    IoFault.Delete -> "\"Delete\"",
    IoFault.Create -> "\"Create\"",
    IoFault.Walk   -> "\"Walk\"",
    IoFault.Temp   -> "\"Temp\"",
  )

  /** Every call the native client sends: the ones `histos.client.Batch` builds, and the reboot that replaces a server
    * of another identity. The JVM server reads them.
    */
  private val batchCalls: List[(Call, String)] = List(
    Call.Reboot(Successor(Some("histos 0.3.1"), Some(Path.of("/tmp/next.jar")))) ->
      """{"Reboot":{"successor":{"version":"histos 0.3.1","jar":"/tmp/next.jar"}}}""",
    Call.Reboot(Successor(None, None))       -> """{"Reboot":{"successor":{}}}""",
    Call.Compile(List(core, api))            -> """{"Compile":{"projects":["core","api"]}}""",
    Call.Test(List(core), Nil, TestMode.Run) -> """{"Test":{"projects":["core"],"patterns":[],"mode":"run"}}""",
    Call.Run("core", Some(Platform.Js))      -> """{"Run":{"id":"core","platform":"js"}}""",
    Call.Run("core", None)                   -> """{"Run":{"id":"core"}}""",
    Call.Task(TaskId.Inspect.value, PluginArgs.empty) ->
      """{"Task":{"id":"inspect","args":{"texts":[],"flags":[]}}}""",
    Call.Clean(List(core, api), CleanExtent.Products) ->
      """{"Clean":{"projects":["core","api"],"extent":"products"}}""",
    Call.Clean(Nil, CleanExtent.Full) -> """{"Clean":{"projects":[],"extent":"full"}}""",
  )

  def spec = suite("golden document laws")(
    JsonLaw.golden("every message is the document the native client reads", messages),
    JsonLaw.golden("every build event is its document", events),
    JsonLaw.golden("every compile feed is its document", compileFeeds),
    JsonLaw.golden("every test feed is its document", testFeeds),
    JsonLaw.golden("every test status is its document", statuses),
    JsonLaw.golden("every key miss is its document", keyMisses),
    JsonLaw.golden("every call result is its document", results),
    JsonLaw.golden("every build fault is its document", faults),
    JsonLaw.golden("every rejection is its document", rejections),
    JsonLaw.golden("every workflow fault is its document", workflowFaults),
    JsonLaw.sent("a ci error from a job the graph arranges is its ci fault's document", arranged),
    JsonLaw.golden("every compile severity is its case name", severities),
    JsonLaw.golden("every position base is its case name", bases),
    JsonLaw.golden("every io fault is its case name", ioFaults),
    JsonLaw.sent("every call the native client sends is the document the server reads", batchCalls),
    JsonLaw.sent[SelectError](
      "an unknown selection names its near projects as names",
      List(SelectError.Unknown(core, List(api)) -> """{"Unknown":{"name":"core","near":["api"]}}"""),
    ),
    JsonLaw.sent[CiError](
      "an unknown workflow project names its near projects as names",
      List(CiError.Unknown(core, List(api)) -> """{"Unknown":{"name":"core","near":["api"]}}"""),
    ),
  )
end GoldenLawSpec
