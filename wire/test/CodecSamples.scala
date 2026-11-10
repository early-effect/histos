package histos

import histos.assembly.AssembleError
import histos.central.{MetadataFault, PublishError, PublishFault, SnapshotRevisionError, TransportFault}
import histos.jdk.{
  JdkError,
  JdkMajor,
  JdkSurvey,
  JdkVersion,
  OverrideError,
  Reject,
  ReleaseError,
  Sha256,
  Sha256Error,
  ToolchainError,
}
import histos.js.{JsLinkError, JsRunError, JsSuiteError}
import histos.load.{BinaryName, CompilerMessage, LoadError, PluginFault}
import histos.native.{
  ClangMiss,
  ClangText,
  ClassShapeError,
  LinkError,
  NativeDeclaration,
  NativeError,
  NativeFetch,
  NativeSuiteError,
  NativeToolchainError,
}
import histos.node.{
  NodeBinary,
  NodeError,
  NodeFileFault,
  NodeMajor,
  NodeMiss,
  NodeOverrideError,
  NodeReject,
  NodeSource,
  NodeSurvey,
  NodeVersion,
  NodeVersionError,
}
import histos.pack.{AssemblyError, JarError, JarIo, LazyValError, LazyValFrame, MainClassError as PackMainError}
import histos.plugin.*
import histos.preview.{PreviewError, RunMiss}
import histos.process.{ChildError, GroupOp, ProcessOutput}
import histos.program.ProgramError
import histos.publish.{LauncherError, LocalPublishError, ScriptError}
import histos.resolve.*
import histos.schedule.*
import histos.select.SelectError
import histos.task.{RunId, TaskError, TaskId}
import histos.testrun.{ModuleTestError, QuickError, StampError, SuiteNameError, SuitePattern, SuitePatternError}
import histos.toolchain.{PlatformToolError, Reached, RuntimeIdentity, RuntimeLocationError, SurveyError}
import histos.version.*
import histos.watch.WatchError
import histos.workflow.CiError
import histos.workspace.IoFailure
import histos.zinc.*
import histos.native.NativeSamples

import zio.{Chunk, Config, NonEmptyChunk}

/** One value per constructor, in declaration order. A defaulted field is not the default. */
object CodecSamples:
  import WireSamples.*

  private val spot        = SourcePos("other.scala", 2, 3)
  private val otherSha    = Sha256("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdee")
  private val otherApi    = ApiDigest("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdee")
  private val otherId     = PluginId("other")
  private val otherShip   = ShipName("other")
  private val release     = JobName("release")
  private val docs        = TargetName("docs")
  private val setup       = ActionName("actions/setup")
  private val macos       = RunnerName("macos")
  private val otherSecret = SecretName("OTHER_TOKEN")
  private val jdk17       = JdkVersion.trusted("17.0.2", JdkMajor.trusted(17))
  private val node20      = NodeVersion.trusted("20.0.0", NodeMajor.trusted(20))
  private val blob        = Digest.trusted("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdee")
  private val published   = PluginCoordinate.Published(dep)
  private val sourced     = PluginCoordinate.Module(core)
  private val tool        = ToolId(ToolRole.Compiler, scala3, ToolVariant.Jvm)
  private val found       = ResolveError.NotFound("dev.zio", "zio", "0")
  private val absent      = ModuleTestError.Absent(core)
  private val noRoot      = GraphCompileError.NoRoot(core)
  private val noSources   = CompileError.NoSources
  private val configBad   = Config.Error.InvalidData(Chunk("jdk"), "bad")

  val toolRoot: ToolRoot =
    ToolRoot("dev.zio", "zio", "2.1.27", Transitivity.Closure, Some(Repository.Central))
  val coordinates: List[PluginCoordinate] = List(published, sourced)
  val identity: RuntimeIdentity           = RuntimeIdentity.Local(path, sha)
  val jdkSurvey: JdkSurvey                = JdkSurvey(path, jdk, identity)
  val nodeSurvey: NodeSurvey              = NodeSurvey(NodeBinary(path), node, identity)
  val reached: Reached                    = Reached(jdkSurvey, Some(nodeSurvey))

  val selectErrors: List[SelectError] = List(
    SelectError.Duplicate(core),
    SelectError.Unknown(core, List(api.value)),
  )

  val childErrors: List[ChildError] = List(
    ChildError.UnsupportedPlatform(Platform.Native),
    ChildError.Java(path),
    ChildError.Classes(path),
    ChildError.Artifact(path),
    ChildError.Work(path),
    ChildError.Group(GroupOp.Lookup),
    ChildError.Bootstrap(ProcessOutput.from("out")),
    ChildError.NoProgram,
    ChildError.Start("java"),
    ChildError.Property("user.dir"),
    ChildError.Status,
    ChildError.Console,
    ChildError.Io(io),
  )

  val pluginIos: List[PluginIo] = List(
    PluginIo.Read(path),
    PluginIo.Resolve(path),
    PluginIo.Compile(path),
    PluginIo.Open(path),
  )

  val pluginLoads: List[PluginLoadError] = List(
    PluginLoadError.Unlocked("demo"),
    PluginLoadError.Unversioned("demo"),
    PluginLoadError.VersionMismatch(dep, digest, otherApi),
    PluginLoadError.ApiLink(dep),
    PluginLoadError.NoSource(published),
    PluginLoadError.ReservedPackage("scala.None"),
    PluginLoadError.EngineLeak(published, "histos.engine.X"),
    PluginLoadError.BuildLeak("histos.build.X"),
    PluginLoadError.IdMismatch(plugin, otherId),
    PluginLoadError.Defect(published, "boom"),
    PluginLoadError.Io(PluginIo.Read(path)),
    PluginLoadError.Construction("broke"),
  )

  val pluginRuns: List[PluginRunError] = List(
    PluginRunError.Failed(detail),
    PluginRunError.MissingOutput(output),
    PluginRunError.ExtraOutput("extra"),
    PluginRunError.MissingArg(param),
    PluginRunError.MissingNote(core, note),
    PluginRunError.Unknown(taskId),
    PluginRunError.Cycle(List(taskId)),
    PluginRunError.Defect("boom"),
    PluginRunError.EngineLeak("histos.engine.X"),
    PluginRunError.Linkage("missing.Class"),
  )

  val pluginTasks: List[PluginTaskError] = List(
    PluginTaskError.Shape("nope"),
    PluginTaskError.Plugin(PluginIdError.Empty),
    PluginTaskError.Task(TaskNameError.Empty),
  )

  val pluginTaskExtras: List[PluginTaskError] = List(
    PluginTaskError.Plugin(PluginIdError.Bad("1")),
    PluginTaskError.Task(TaskNameError.Bad("1")),
  )

  val resolveErrors: List[ResolveError] = List(
    ResolveError.ToolRoot(tool, toolRoot, ToolRootFault.Missing),
    ResolveError.NoTool(tool),
    ResolveError.ToolDrift(tool),
    ResolveError.CompilerVersion(ScalaVersionError.NotFull("3")),
    ResolveError.NativeOrigin(NativeToolchainError.Empty),
    ResolveError.Range(dep),
    found,
    ResolveError.Checksum(path, sha, otherSha),
    ResolveError.LockDrift(core, scala3, Platform.Jvm, ClasspathScope.Compile, List("a")),
    ResolveError.PluginDrift(dep, List("moved")),
    ResolveError.NoLock(path),
    ResolveError.UnsupportedPlatform(Platform.Js),
    ResolveError.Lock(LockError.Empty),
    ResolveError.Io(io),
    ResolveError.Plugin(PluginLoadError.Unlocked("x")),
    ResolveError.Repository(RepositoryIo.Fetch),
    ResolveError.Scalafmt(histos.format.ScalafmtConfError.NoVersion(path)),
  )

  val lockErrors: List[LockError] = List(
    LockError.Empty,
    LockError.BadLine("nope"),
    LockError.DuplicateCell(core, scala3, Platform.Jvm, ClasspathScope.Test),
    LockError.DuplicateTool(tool),
    LockError.DuplicatePlugin("dev.zio", "zio", "2"),
    LockError.Missing(LockField.Cell),
    LockError.Classifier(ClassifierError.Empty),
    LockError.BadSha256("zz"),
    LockError.BadRepository("nope"),
  )

  val toolchainMisses: List[ToolchainMiss] = List(
    ToolchainMiss.Fetch(scala3),
    ToolchainMiss.Missing(scala3, CompilerPart.Library),
    ToolchainMiss.Duplicated(scala3, CompilerPart.Compiler),
    ToolchainMiss.Unexpected(scala3, List(path)),
    ToolchainMiss.Library(scala3, scala213),
    ToolchainMiss.Plugin(scala3),
    ToolchainMiss.Coverage(scala3),
  )

  val nativeFetches: List[NativeFetch] = List(
    NativeFetch.Compiler(noSources),
    NativeFetch.Resolved(found),
    NativeFetch.Missing("nativelib", "0.5.9"),
    NativeFetch.Duplicated("nativelib"),
  )

  val compileErrors: List[CompileError] = List(
    CompileError.Resolved(found),
    CompileError.UnsupportedPlatform(Platform.Native),
    CompileError.UnsupportedScala(scala213),
    noSources,
    CompileError.Banned(path, ClasspathBan.Histos),
    CompileError.Failed(List(CompileDiagnostic.fromCompiler("boom"))),
    CompileError.Toolchain(ToolchainMiss.Fetch(scala3)),
    CompileError.Io(io),
    CompileError.JsLibrary(JsLibraryMiss.Empty),
    CompileError.NativeFetch(NativeFetch.Missing("nativelib", "0.5.9")),
    CompileError.NativeOrigin(NativeToolchainError.Empty),
    CompileError.NativeLibrary("missing"),
    CompileError.Stored(CacheError.Io(io)),
  )

  val graphErrors: List[GraphCompileError] = List(
    GraphCompileError.Runtime(ToolchainError.Denied),
    GraphCompileError.Target(SelectError.Duplicate(core)),
    GraphCompileError.UnknownCell(core, scala3),
    noRoot,
    GraphCompileError.NoSources(core, path),
    GraphCompileError.MissingUpstream(core, api, scala3, Platform.Jvm),
    GraphCompileError.IncompatibleUpstream(core, api, scala3, scala213, Platform.Js),
    GraphCompileError.Cycle(core),
    GraphCompileError.Lock(found),
    GraphCompileError.LockRead(path),
    GraphCompileError.RootRead(path),
    GraphCompileError.Output(path),
    GraphCompileError.Failed(core, scala3, Platform.Jvm, noSources),
    GraphCompileError.Assets(core, scala3, Platform.Jvm, AssetError.Collides(ResourceName.trusted("Demo.class"))),
    GraphCompileError.Cells(NonEmptyChunk(noRoot)),
  )

  val ciFaults: List[CiFault] = List(
    CiFault.IllegalSite(job),
    CiFault.UnknownNeed(job, release),
    CiFault.DuplicateNeed(job, release),
    CiFault.UnknownModule(job, core),
    CiFault.DuplicateTarget(job, target),
    CiFault.UnpinnedAction(job, actionName),
    CiFault.GrantMissing(job, actionName),
    CiFault.UnknownRunner(job, runner),
    CiFault.UnknownSecret(secret),
    CiFault.UnreachableCond(job),
    CiFault.DispatchInputConflict("name"),
    CiFault.ConcurrencyConflict("ci.yml"),
    CiFault.UnpinnedWorkflowRun("build"),
  )

  val buildErrors: List[BuildError] = List(
    BuildError.EmptyName(pos),
    BuildError.BadName("1core", pos),
    BuildError.DuplicateName(core, pos, spot),
    BuildError.DuplicatePlatform(core, Platform.Jvm, pos),
    BuildError.DuplicateScalaVersion(core, scala3, pos),
    BuildError.DuplicateChild(core, api, pos),
    BuildError.AggregateIncludesItself(core, pos),
    BuildError.AggregateChild(core, api, pos),
    BuildError.MissingProject("missing", pos),
    BuildError.DependsOnAggregate(core, api, pos),
    BuildError.SelfDependency(core, pos),
    BuildError.DuplicateDependency(core, api, pos),
    BuildError.EdgeWithoutPlatform(core, Platform.Js, pos),
    BuildError.UpstreamMissingPlatform(core, api, Platform.Native, pos),
    BuildError.Cycle(Platform.Jvm, List(core, api)),
    BuildError.MissingClang(core, pos),
    BuildError.EmptyShipName(pos),
    BuildError.BadShipName("1ship", pos),
    BuildError.DuplicateShip(ship, pos, spot),
    BuildError.RepeatedShipMember(ship, core, pos),
    BuildError.AggregateShipMember(ship, core, pos),
    BuildError.MemberInTwoShips(core, ship, otherShip, pos),
    BuildError.DuplicateResolver("central"),
    BuildError.MixedTest(core, pos),
    BuildError.DuplicateDirectory(core, api, "modules"),
    BuildError.DuplicatePlugin(plugin),
    BuildError.DuplicatePluginTask(plugin, taskName),
    BuildError.DuplicateCommand(command),
    BuildError.DuplicateJob(job),
    BuildError.ReservedJob(job),
    BuildError.DuplicateSite(job, release),
    BuildError.Ci(CiFault.IllegalSite(job)),
    BuildError.DuplicateNote(core, plugin, note),
    BuildError.MissingNote(core, plugin, note),
    BuildError.PluginMissingModule(plugin, core),
    BuildError.PluginMissingCell(plugin, core, Some(scala3), Some(Platform.Jvm)),
    BuildError.UnknownPluginTask(plugin, taskName),
    BuildError.CommandShadow(command),
    BuildError.PluginDefect(plugin, "boom"),
    BuildError.TwoPublications(core, pos),
    BuildError.AssemblyShape(core, pos),
    BuildError.AssetWithoutAssembly(core, pos),
    BuildError.TwoAssets(core, pos),
    BuildError.DuplicateResource(core, ResourceName.trusted("note.txt"), pos),
    BuildError.RemoteNeeds(core, Set(Need.Network, Need.Docker), pos),
    BuildError.UnknownPluginModule("missing", pos),
    BuildError.PluginOnAggregate(core, pos),
    BuildError.DuplicatePluginCoordinate("core", pos),
  )

  val loadErrors: List[LoadError] = List(
    LoadError.Runtime(noRoot),
    LoadError.MissingFile(path),
    LoadError.CompileFailed(List(CompilerMessage.fromCompiler("boom"))),
    LoadError.NoEntry,
    LoadError.AmbiguousEntry(List(BinaryName.trusted("demo.Build$"))),
    LoadError.BrokenGraph(BuildError.EmptyName(pos)),
    LoadError.Plugin(PluginLoadError.Unlocked("x")),
    LoadError.Resolve(found),
    LoadError.MissingApi(path),
    LoadError.NoHome,
    LoadError.Io(io),
    LoadError.Jar(JarError.Empty(path)),
  )

  val cacheErrors: List[CacheError] = List(
    CacheError.Io(io),
    CacheError.Corrupt(ScheduleSamples.digest),
  )

  val exchanges: List[CacheExchange] = List(
    CacheExchange.Target,
    CacheExchange.Connect("repo.example", 443),
    CacheExchange.ConnectTimeout("repo.example", 443),
    CacheExchange.Tls("repo.example", 443),
    CacheExchange.ReadTimeout("repo.example", 443),
    CacheExchange.Io("repo.example", 443),
    CacheExchange.Protocol("repo.example", 443),
    CacheExchange.Pool("repo.example", 443),
    CacheExchange.Trust,
    CacheExchange.Body,
  )

  val namespaceErrors: List[NamespaceError] = List(
    NamespaceError.Shape("other"),
    NamespaceError.Number(PullRequestError.NotPositive(0)),
    NamespaceError.Name(DevNameError.Segment("")),
  )

  val remoteErrors: List[RemoteError] = List(
    RemoteError.Off,
    RemoteError.ReadOnly,
    RemoteError.Dangling(ScheduleSamples.digest, blob),
    RemoteError.Text(ScheduleSamples.digest),
    RemoteError.Exchange(CacheExchange.Body),
    RemoteError.Status(500),
    RemoteError.Index(ScheduleSamples.digest),
    RemoteError.Mismatch(ScheduleSamples.digest, blob),
    RemoteError.Catalog("not a line"),
    RemoteError.Scheme("ftp"),
    RemoteError.Mode("nope"),
    RemoteError.Address("nope"),
    RemoteError.Namespace(NamespaceError.Shape("other")),
    RemoteError.Rpc("INTERNAL"),
    RemoteError.HttpOnly("put"),
  )

  val jdkErrors: List[JdkError] = List(
    JdkError.BadMajor(0),
    JdkError.Io(io),
  )

  val releaseErrors: List[ReleaseError] = List(
    ReleaseError.NoFile,
    ReleaseError.MissingKey,
    ReleaseError.Unclosed,
    ReleaseError.BadVersion("nope"),
  )

  val rejects: List[Reject] = List(
    Reject.Absent,
    Reject.NoJavac,
    Reject.UnreadableVersion(ReleaseError.NoFile),
  )

  val overrideErrors: List[OverrideError] = List(
    OverrideError.Empty,
    OverrideError.Multiple,
    OverrideError.Relative("jdk"),
    OverrideError.Invalid("::"),
  )

  val toolchainErrors: List[ToolchainError] = List(
    ToolchainError.Probe(JdkError.BadMajor(0)),
    ToolchainError.Override(OverrideError.Empty),
    ToolchainError.TooOld(jdk, jdk17, path),
    ToolchainError.Absent(jdk),
    ToolchainError.NoUserHome,
    ToolchainError.NoArchitecture,
    ToolchainError.Denied,
    ToolchainError.Config(configBad),
    ToolchainError.Io(io),
    ToolchainError.Rejected(path, Reject.Absent),
  )

  val nodeVersionErrors: List[NodeVersionError] = List(
    NodeVersionError.Bad("nope"),
    NodeVersionError.Status(1),
    NodeVersionError.Missing,
    NodeVersionError.MissingStatus,
    NodeVersionError.Process(ChildError.NoProgram),
    NodeVersionError.NotExecutable(path),
  )

  val nodeOverrideErrors: List[NodeOverrideError] = List(
    NodeOverrideError.Empty,
    NodeOverrideError.Multiple,
    NodeOverrideError.Relative("node"),
    NodeOverrideError.Invalid("::"),
  )

  val nodeRejects: List[NodeReject] = List(
    NodeReject.Absent,
    NodeReject.NoBinary,
    NodeReject.UnreadableVersion(NodeVersionError.Missing),
  )

  val nodePathErrors: List[NodePathError] = List(
    NodePathError.Empty,
    NodePathError.Relative("node"),
    NodePathError.NotBinary("/bin/sh"),
    NodePathError.Invalid("::"),
  )

  val nodeErrors: List[NodeError] = List(
    NodeError.BadMajor(0),
    NodeError.Unreadable(path),
    NodeError.Locations(RuntimeLocationError.InvalidPath("::")),
    NodeError.Version(path, NodeVersionError.Bad("nope")),
    NodeError.Override(NodeOverrideError.Empty),
    NodeError.Io(io),
  )

  val nodeMisses: List[NodeMiss] = List(
    NodeMiss.Unresolved(NodeError.BadMajor(0)),
    NodeMiss.Rejected(path, NodeReject.Absent, NodeSource.Environment),
    NodeMiss.TooOld(node, node20, path, NodeSource.Path),
    NodeMiss.Absent(node, NodeSource.Config),
    NodeMiss.Unusable(path, NodeSource.Local, NodeFileFault.Missing),
    NodeMiss.Refused(NodeSource.Config, NodePathError.Empty),
  )

  val locationErrors: List[RuntimeLocationError] = List(
    RuntimeLocationError.InvalidPath("::"),
    RuntimeLocationError.Io(io),
  )

  val clangTexts: List[ClangText] = List(
    ClangText.Empty,
    ClangText.NoVersion,
    ClangText.BadVersion("nope"),
    ClangText.NoTarget,
  )

  val clangMisses: List[ClangMiss] = List(
    ClangMiss.Absent,
    ClangMiss.NotOnPath(clang),
    ClangMiss.BadPath("nope"),
    ClangMiss.NoClangPP(path),
    ClangMiss.Unreadable(path),
    ClangMiss.Status(path, 1),
    ClangMiss.Text(path, ClangText.BadVersion("nope")),
    ClangMiss.WrongMajor(NativeSamples.version, clang, path),
    ClangMiss.Mismatched(NativeSamples.version, NativeSamples.older),
  )

  val nativeOrigins: List[NativeToolchainError] = List(
    NativeToolchainError.Empty,
    NativeToolchainError.Origin("nope"),
    NativeToolchainError.Revision("nope"),
    NativeToolchainError.Repository(RepositoryUrlError.Bad("nope")),
  )

  val surveyErrors: List[SurveyError] = List(
    SurveyError.Jdk(ToolchainError.Denied),
    SurveyError.Node(jdkSurvey, NodeMiss.Absent(node, NodeSource.Config)),
    SurveyError.Undeclared(reached, core),
    SurveyError.Split(reached, core, api),
    SurveyError.Clang(reached, ClangMiss.Absent),
    SurveyError.Incomplete("report", "detail"),
  )

  val linkErrors: List[LinkError] = List(
    LinkError.Interrupted,
    LinkError.Linker("ld"),
    LinkError.Unexpected("signal"),
  )

  val shapeErrors: List[ClassShapeError] = List(
    ClassShapeError.Failed("nir"),
    ClassShapeError.Read(path),
    ClassShapeError.ClassFile("bad"),
  )

  val jsLinkErrors: List[JsLinkError] = List(
    JsLinkError.Resolved(found),
    JsLinkError.Unavailable,
    JsLinkError.Library(noSources),
    JsLinkError.Product(noSources),
    JsLinkError.Linker("link"),
  )

  val jsRunErrors: List[JsRunError] = List(
    JsRunError.Resolved(found),
    JsRunError.Unavailable,
    JsRunError.Compile(noSources),
    JsRunError.Write(path),
  )

  val jsSuiteErrors: List[JsSuiteError] = List(
    JsSuiteError.Resolved(found),
    JsSuiteError.Unavailable,
    JsSuiteError.Library(noSources),
    JsSuiteError.Run("boom"),
  )

  val nativeErrors: List[NativeError] = List(
    NativeError.Clang(ClangMiss.Absent),
    NativeError.Fetch(NativeFetch.Missing("nativelib", "0.5.9")),
    NativeError.Compile(noSources),
    NativeError.Link(LinkError.Interrupted),
  )

  val nativeSuiteErrors: List[NativeSuiteError] = List(
    NativeSuiteError.Resolved(found),
    NativeSuiteError.Unavailable,
    NativeSuiteError.Shapes(ClassShapeError.Failed("nir")),
    NativeSuiteError.Compile(noSources),
    NativeSuiteError.Write(path),
  )

  val platformTools: List[PlatformToolError] = List(
    PlatformToolError.JsLink(JsLinkError.Unavailable),
    PlatformToolError.JsRun(JsRunError.Unavailable),
    PlatformToolError.JsSuite(JsSuiteError.Unavailable),
    PlatformToolError.Native(NativeError.Link(LinkError.Interrupted)),
    PlatformToolError.NativeSuite(NativeSuiteError.Unavailable),
    PlatformToolError.NativeOrigin(NativeToolchainError.Empty),
  )

  def moduleTests(pattern: SuitePattern): List[ModuleTestError] = List(
    ModuleTestError.Tool(core, scala3, PlatformToolError.JsRun(JsRunError.Unavailable)),
    ModuleTestError.Target(SelectError.Duplicate(core)),
    ModuleTestError.Compile(noRoot),
    ModuleTestError.Lock(found),
    ModuleTestError.LockRead(path),
    ModuleTestError.Sources(core, scala3, noSources),
    ModuleTestError.MissingSources(core, testMain),
    ModuleTestError.MissingProvider(core, provider),
    ModuleTestError.ProviderDependency(core, provider),
    ModuleTestError.NoneDiscovered(core, provider),
    ModuleTestError.NoneMatched(List(pattern)),
    ModuleTestError.Mixed(core),
    ModuleTestError.Process(core, scala3, ChildError.NoProgram),
    ModuleTestError.Main(PackMainError.Empty),
    absent,
    ModuleTestError.MissingUpstream(core, api, scala3),
    ModuleTestError.Work(path),
    ModuleTestError.Root(path),
    ModuleTestError.Runner(path),
    ModuleTestError.LinkFailed(core, scala3, Platform.Js, "link"),
    ModuleTestError.NoNode(core, scala3, NodeMiss.Absent(node, NodeSource.Config)),
    ModuleTestError.Stamp(StampError.Missing("App")),
    ModuleTestError.Record(QuickError.Shape(path)),
    ModuleTestError.Pattern(SuitePatternError.Empty),
    ModuleTestError.Api(core, scala3, Platform.Native),
  )

  val programErrors: List[ProgramError] = List(
    ProgramError.Tool(core, scala3, PlatformToolError.Native(NativeError.Link(LinkError.Interrupted))),
    ProgramError.Absent("gone", List(api.value)),
    ProgramError.Aggregate(core),
    ProgramError.NoMain(core),
    ProgramError.Choose(core, List(Platform.Jvm, Platform.Js)),
    ProgramError.Unbuilt(core, Platform.Native, List(Platform.Jvm)),
    ProgramError.NoClang(core, scala3),
    ProgramError.Main(PackMainError.Bad("1")),
    ProgramError.Compile(noRoot),
    ProgramError.Library(noSources),
    ProgramError.Lock(found),
    ProgramError.LockRead(path),
    ProgramError.Link(core, scala3, Platform.Native, "ld"),
    ProgramError.Process(core, scala3, "died"),
    ProgramError.NoNode(core, scala3, NodeMiss.Absent(node, NodeSource.Path)),
    ProgramError.MissingUpstream(core, api, scala3),
    ProgramError.Work(path),
    ProgramError.Root(path),
  )

  val transports: List[TransportFault] = List(
    TransportFault.Target("nope"),
    TransportFault.Connect("repo.example", 443),
    TransportFault.ConnectTimeout("repo.example", 443),
    TransportFault.Tls("repo.example", 443),
    TransportFault.ReadTimeout("repo.example", 443),
    TransportFault.Io("repo.example", 443),
    TransportFault.Protocol("repo.example", 443),
    TransportFault.Pool("repo.example", 443),
    TransportFault.Trust,
    TransportFault.Body,
  )

  val publishErrors: List[PublishError] = List(
    PublishError.EmptyJar,
    PublishError.NotAJar,
    PublishError.NotADirectory(path),
    PublishError.Escaped(path),
    PublishError.Io(path, PublishFault.Stat),
    PublishError.Unauthorized,
    PublishError.NotSnapshot(SnapshotRevisionError.NotSnapshot("1.2.3")),
    PublishError.Status(500),
    PublishError.Transport(TransportFault.Trust),
    PublishError.Metadata(MetadataFault.MissingBuild),
  )

  val jarIos: List[JarIo] = List(
    JarIo.Read(path),
    JarIo.Write(path),
    JarIo.Move(path),
  )

  val jarErrors: List[JarError] = List(
    JarError.NotADirectory(path),
    JarError.Empty(path),
    JarError.Escaped(".."),
    JarError.Duplicate("A.class"),
    JarError.DestinationInside(path),
    JarError.Io(JarIo.Read(path)),
    JarError.Scaladoc("boom"),
  )

  val lazyFrames: List[LazyValFrame] = List(
    LazyValFrame.Rejected,
    LazyValFrame.MethodTooLarge("demo.Exit", "m", "()V", 1),
    LazyValFrame.ClassTooLarge("demo.Exit", 1),
    LazyValFrame.Limit,
    LazyValFrame.Bounds,
  )

  val lazyErrors: List[LazyValError] = List(
    LazyValError.StillCalls("demo.Exit", "m"),
    LazyValError.MissingHandle("demo.Exit", "demo.Exit", "0"),
    LazyValError.OffsetShape("demo.Exit", "m"),
    LazyValError.Frames("demo.Exit", LazyValFrame.Rejected),
  )

  val assemblyErrors: List[AssemblyError] = List(
    AssemblyError.NotADirectory(path),
    AssemblyError.Empty(path),
    AssemblyError.Escaped(".."),
    AssemblyError.DestinationInside(path),
    AssemblyError.Overlap(path),
    AssemblyError.NotAJar(path),
    AssemblyError.MissingMain(packed),
    AssemblyError.NoClasses,
    AssemblyError.Duplicate("A.class"),
    AssemblyError.NotAFile(path),
    AssemblyError.LazyVal(LazyValError.StillCalls("demo.Exit", "m")),
    AssemblyError.Io(JarIo.Write(path)),
  )

  val localPublishes: List[LocalPublishError] = List(
    LocalPublishError.Version(histos.version.VersionError.Unreadable),
    LocalPublishError.Target(SelectError.Duplicate(core)),
    LocalPublishError.Compile(noRoot),
    LocalPublishError.Jar(core, scala3, Platform.Jvm, JarError.Empty(path)),
    LocalPublishError.SourceRead(core, scala3, Platform.Jvm, noSources),
    LocalPublishError.NoSources(core, scala3, Platform.Jvm),
    LocalPublishError.Publish(PublishError.EmptyJar),
    LocalPublishError.Read(path),
    LocalPublishError.NoOrganization,
    LocalPublishError.NothingToPublish,
    LocalPublishError.Development(ship, CiVersion(number)),
    LocalPublishError.NoCommit,
    LocalPublishError.UnknownShip(ship),
    LocalPublishError.NotPublished(core),
    LocalPublishError.SnapshotDependency(ship, "dev.zio:zio:2.1.27-SNAPSHOT"),
    LocalPublishError.Unknown("nope"),
    LocalPublishError.UnpublishedDependency(core, api),
    LocalPublishError.Facts(::(PomFact.License, Nil)),
    LocalPublishError.BadDependency("dev.zio", "zio", "2"),
    LocalPublishError.Credentials,
    LocalPublishError.Io("/work/repo"),
    LocalPublishError.Stock("broke"),
    LocalPublishError.Classpath(found),
    LocalPublishError.Runtime(noSources),
    LocalPublishError.Pack(core, scala3, Platform.Jvm, AssemblyError.NoClasses),
    LocalPublishError.MissingClasses(core, scala3, Platform.Native),
    LocalPublishError.MissingClient(ship, path),
  )

  val runMisses: List[RunMiss] = List(
    RunMiss.Absent("gone", List(api.value)),
    RunMiss.Aggregate(core),
    RunMiss.NoMain(core),
    RunMiss.Choose(core, List(Platform.Jvm, Platform.Js)),
    RunMiss.Unbuilt(core, Platform.Native, List(Platform.Jvm)),
    RunMiss.UnknownTask("nope"),
    RunMiss.BadId("nope"),
  )

  val previewErrors: List[PreviewError] = List(
    PreviewError.Target(SelectError.Duplicate(core)),
    PreviewError.Cycle(core),
    PreviewError.Sources(io),
    PreviewError.Version(histos.version.VersionError.Unreadable),
    PreviewError.Publish(LocalPublishError.NothingToPublish),
    PreviewError.Run(RunMiss.Absent("gone", List(api.value))),
    PreviewError.Pattern(SuitePatternError.Blank("a b")),
  )

  val watchErrors: List[WatchError] = List(WatchError.Read(path))

  val commitErrors: List[CommitShaError] = List(
    CommitShaError.Empty,
    CommitShaError.NotHex("zz"),
  )

  val branchErrors: List[BranchError] = List(BranchError.Unknown("main"))

  val pullErrors: List[PullRequestError] = List(
    PullRequestError.NotANumber("01"),
    PullRequestError.NotPositive(0),
  )

  val sessionErrors: List[SessionError] = List(
    SessionError.Unknown("nope"),
    SessionError.MissingSha,
    SessionError.MissingBranch,
    SessionError.MissingPullRequest,
    SessionError.Sha(CommitShaError.Empty),
    SessionError.BadBranch(BranchError.Unknown("main")),
    SessionError.PullRequest(PullRequestError.NotPositive(0)),
  )

  val registryErrors: List[RegistryError] = List(
    RegistryError.BadLine("x"),
    RegistryError.Name(ShipNameError.Empty),
    RegistryError.Relation(TagRelationError.Unknown("nope")),
    RegistryError.Duplicate(ship),
  )

  val refusals: List[Refusal] = List(
    Refusal.Unchecked,
    Refusal.Shadowed(ship, number),
    Refusal.Unproven(ship, number),
    Refusal.NothingToRelease,
  )

  val versionErrors: List[histos.version.VersionError] = List(
    histos.version.VersionError.Session(SessionError.MissingSha),
    histos.version.VersionError.Registry(RegistryError.BadLine("x")),
    histos.version.VersionError.UnknownShip(ship),
    histos.version.VersionError.Refused(::(Refusal.Unchecked, Nil)),
    histos.version.VersionError.Unreadable,
    histos.version.VersionError.Config(configBad),
  )

  val taskErrors: List[TaskError] = List(
    TaskError.EmptyId,
    TaskError.NoRunner(TaskId.Inspect),
    TaskError.UnknownRun(RunId.trusted("run-1")),
    TaskError.Version(histos.version.VersionError.Unreadable),
    TaskError.Plugin(PluginRunError.Failed(detail)),
    TaskError.Doc(NonEmptyChunk("scaladoc")),
  )

  val ciErrors: List[CiError] = List(
    CiError.BadName("1job"),
    CiError.Unknown(core, List(api.value)),
    CiError.NotModule(core),
    CiError.Duplicate(core),
    CiError.Missing(path),
    CiError.Drift("ci.yml", "hint"),
    CiError.Io(io),
    CiError.DuplicateSite(job, release),
    CiError.IllegalSite(job),
    CiError.UnknownNeed(job, release),
    CiError.DuplicateNeed(job, release),
    CiError.UnknownModule(job, core),
    CiError.DuplicateTarget(job, docs),
    CiError.UnpinnedAction(job, setup),
    CiError.GrantMissing(job, setup),
    CiError.UnknownRunner(job, macos),
    CiError.UnknownSecret(otherSecret),
    CiError.UnreachableCond(job),
    CiError.DispatchInputConflict("name"),
    CiError.ConcurrencyConflict("ci.yml"),
    CiError.UnpinnedWorkflowRun("build"),
    CiError.Pins("x"),
    CiError.NoDefaultBranch,
    CiError.JobTimeout(2),
    CiError.LlvmChecksum("clang.tar.gz"),
    CiError.SnapshotClients(ship),
  )

  val scriptErrors: List[ScriptError] = List(
    ScriptError.Drift(path),
    ScriptError.Missing(path),
    ScriptError.Io(io),
    ScriptError.Revision,
    ScriptError.Pin(LauncherError.Missing),
  )

  val assembleErrors: List[AssembleError] = List(
    AssembleError.Toolchain(noSources),
    AssembleError.Compile(noRoot),
    AssembleError.Resolve(found),
    AssembleError.Jar(JarError.Empty(path)),
    AssembleError.Pack(AssemblyError.NoClasses),
    AssembleError.NoModule("nope"),
    AssembleError.NoClasses(core, scala3),
    AssembleError.Io(path),
  )

  val launcherErrors: List[LauncherError] = List(
    LauncherError.Line("nope"),
    LauncherError.Unknown("nope"),
    LauncherError.Duplicate,
    LauncherError.Missing,
    LauncherError.Version("nope"),
    LauncherError.Release("nope"),
    LauncherError.Repository("nope"),
    LauncherError.Resolve("missing"),
    LauncherError.Io(io),
    LauncherError.InvalidPath(path),
    LauncherError.Absent(path),
  )

  val scheduleErrors: List[ScheduleError[PluginFault]] = List(
    ScheduleError.UnknownAction(ScheduleSamples.compile),
    ScheduleError.DuplicateAction(ScheduleSamples.compile),
    ScheduleError.MissingDependency(ScheduleSamples.compile, ScheduleSamples.link),
    ScheduleError.Cycle(List(ScheduleSamples.compile, ScheduleSamples.link)),
    ScheduleError.DuplicateInput(ScheduleSamples.compile, ScheduleSamples.sources),
    ScheduleError.Failed(ScheduleSamples.compile, PluginFault.Task(PluginRunError.Failed(detail))),
    ScheduleError.Cache(CacheError.Io(io)),
    ScheduleError.Remote(RemoteError.Off),
  )

  val pluginFaults: List[PluginFault] = List(
    PluginFault.Task(PluginRunError.Failed(detail)),
    PluginFault.Compiled(noRoot),
    PluginFault.Resolved(found),
    PluginFault.Tested(absent),
    PluginFault.Scheduled(ScheduleError.Failed(ScheduleSamples.link, PluginFault.Tested(absent))),
  )

  val nameErrors: List[ProjectNameError]      = List(ProjectNameError.Empty, ProjectNameError.Bad("1core"))
  val scalaErrors: List[ScalaVersionError]    = List(ScalaVersionError.Empty, ScalaVersionError.NotFull("3"))
  val shipNameErrors: List[ShipNameError]     = List(ShipNameError.Empty, ShipNameError.Bad("1ship"))
  val shipNumberErrors: List[ShipNumberError] =
    List(ShipNumberError.Empty, ShipNumberError.NotAShip("01.2.3"))
  val depErrors: List[DepError]              = List(DepError.Empty, DepError.BadShape("nope"))
  val paramErrors: List[ParamNameError]      = List(ParamNameError.Empty, ParamNameError.Bad("1"))
  val flagErrors: List[FlagNameError]        = List(FlagNameError.Empty, FlagNameError.Bad("1flag"))
  val shaErrors: List[Sha256Error]           = List(Sha256Error.Bad("zz"))
  val patternErrors: List[SuitePatternError] =
    List(SuitePatternError.Empty, SuitePatternError.Blank("a b"))
  val classifierErrors: List[ClassifierError]     = List(ClassifierError.Empty, ClassifierError.Invalid("has space"))
  val repoUrlErrors: List[RepositoryUrlError]     = List(RepositoryUrlError.Bad("nope"))
  val packMainErrors: List[PackMainError]         = List(PackMainError.Empty, PackMainError.Bad("1"))
  val tagErrors: List[TagRelationError]           = List(TagRelationError.Unknown("Matches"))
  val suiteNameErrors: List[SuiteNameError]       = List(SuiteNameError.Empty)
  val devNameErrors: List[DevNameError]           = List(DevNameError.Segment(""))
  val snapshotErrors: List[SnapshotRevisionError] =
    List(SnapshotRevisionError.NotSnapshot("1.2.3"))
  val platformErrors: List[PlatformError] = List(PlatformError.Unknown("Jvm"))

  val stampErrors: List[StampError] = List(
    StampError.Io(io),
    StampError.Class(path),
    StampError.Missing("App"),
  )

  val quickErrors: List[QuickError] = List(
    QuickError.Save(io),
    QuickError.Clear(io),
    QuickError.Read(io),
    QuickError.Shape(path),
    QuickError.Hash(path, Sha256Error.Bad("zz")),
    QuickError.Suite(SuiteNameError.Empty),
  )

  val declarations: List[NativeDeclaration] = List(
    NativeDeclaration.NotNative,
    NativeDeclaration.Major(clang),
    NativeDeclaration.Missing(core),
    NativeDeclaration.Disagree(core, api),
  )

  val inputs: List[InputValue] = List(InputValue.Text("src"), InputValue.Hash(sha))
end CodecSamples
