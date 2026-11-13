package histos.report

import heddle.endpoint.{Schema, SchemaDoc}
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
import histos.load.{BinaryName, CompilerMessage, LoadError, PluginFault}
import histos.plugin.{
  ActionName,
  ApiDigest,
  CommandName,
  ContentHash,
  EntryName,
  FlagName,
  FlagNameError,
  JobName,
  NoteKey,
  OutputName,
  ParamName,
  ParamNameError,
  PluginCoordinate,
  PluginDetail,
  PluginId,
  PluginIo,
  PluginLoadError,
  PluginRunError,
  PluginTaskError,
  PluginTaskId,
  RepoPath,
  RunnerName,
  SecretName,
  TargetName,
  TaskName,
}
import histos.assembly.AssembleError
import histos.central.{MetadataFault, PublishError, PublishFault, SnapshotRevisionError, TransportFault}
import histos.js.{JsLinkError, JsRunError, JsSuiteError}
import histos.native.{
  ClangMiss,
  ClangTarget,
  ClangText,
  ClangVersion,
  ClassShapeError,
  LinkError,
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
import histos.pack.{
  AssemblyError,
  JarError,
  JarIo,
  LazyValError,
  LazyValFrame,
  MainClass as PackMain,
  MainClassError as PackMainError,
}
import histos.preview.PreviewError
import histos.process.{ChildError, GroupOp, ProcessOutput}
import histos.program.ProgramError
import histos.publish.{LauncherError, LocalPublishError, ScriptError}
import histos.resolve.{
  ArtifactMiss,
  ClasspathScope,
  ClassifierError,
  DownloadFault,
  JarClassifier,
  LockError,
  LockField,
  RepoUrl,
  Repository,
  RepositoryIo,
  ResolveError,
  ToolId,
  ToolRole,
  ToolRoot,
  ToolRootFault,
  ToolVariant,
  Transitivity,
}
import histos.schedule.{
  ActionId,
  ActionIdError,
  CacheError,
  CacheExchange,
  DevNameError,
  Digest,
  InputName,
  InputNameError,
  NamespaceError,
  RemoteError,
  ScheduleError,
}
import histos.select.SelectError
import histos.task.{RunId, TaskError, TaskId}
import histos.build.BuildEvent
import histos.testrun.{
  ModuleTestError,
  QuickError,
  StampError,
  SuiteName,
  SuiteNameError,
  SuitePattern,
  SuitePatternError,
  TestFeed,
  TestStatus,
  TestedCell,
}
import histos.toolchain.{PlatformToolError, Reached, RuntimeIdentity, RuntimeLocationError, SurveyError}
import histos.version.{
  BranchError,
  CiVersion,
  CommitShaError,
  PullRequestError,
  Refusal,
  RegistryError,
  SessionError,
  ShipName,
  ShipNameError,
  ShipNumber,
  ShipNumberError,
  TagRelation,
  TagRelationError,
  VersionError,
}
import histos.watch.WatchError
import histos.workflow.CiError
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{
  ClasspathBan,
  CompileDiagnostic,
  CompileError,
  CompileFeed,
  CompileInput,
  CompileProblem,
  CompileSeverity,
  CompiledCell,
  CompilerPart,
  GraphCompileError,
  JsLibraryMiss,
  KeyMiss,
  PositionBase,
  SourceCount,
  ToolchainMiss,
}
import histos.{
  BuildError,
  CiFault,
  PluginGraphFault,
  ToolchainClientFault,
  ToolchainLauncherFault,
  ClangMajor,
  Dep,
  DepError,
  FrameworkClass,
  JsEnv,
  JsVersion,
  MainClass,
  NativeVersion,
  Organization,
  Platform,
  PlatformError,
  PomFact,
  ProjectName,
  ProjectNameError,
  NodePathError,
  ProjectPath,
  ProviderLibrary,
  RepositoryUrl,
  RepositoryUrlError,
  ScalaVersion,
  ScalaVersionError,
  SourcePos,
  AssetError,
  ResourceName,
  TestMain,
  TestProvider,
  ToolRevision,
}

import java.nio.file.{InvalidPathException, Path}

import zio.json.*
import zio.{Cause, Chunk, Config, FiberId, NonEmptyChunk}

/** JVM document schemas for the socket types. The JSON codecs live in wire.
  */
private trait WireSchemaSupport:
  protected def text[A](title: String): Schema[A] =
    Schema.of(SchemaDoc.Str(None), Some(title))

  protected def enumeration[A](title: String, values: List[String]): Schema[A] =
    Schema.of(SchemaDoc.Enum(Some(title), values))

end WireSchemaSupport
private object WireSchemaLeaves extends WireSchemaSupport:
  given Schema[Path] = Schema.of(SchemaDoc.Str(None), Some("Path"))

  given Schema[Platform] =
    Schema.of(SchemaDoc.Enum(Some("Platform"), List("jvm", "js", "native")))

  given Schema[PomFact] = enumeration("PomFact", List("License", "Developer", "Scm"))

  given [A: Schema]: Schema[NonEmptyChunk[A]] =
    Schema.of(SchemaDoc.Array(summon[Schema[A]].doc))

  given [A: Schema]: Schema[Set[A]] =
    Schema.of(SchemaDoc.Array(summon[Schema[A]].doc))

  given [A: Schema]: Schema[::[A]] =
    Schema.of(SchemaDoc.Array(summon[Schema[A]].doc))

  // NonEmptyChunk is not one of heddle's deferred collections, so this cycle is a Ref.
  given Schema[NonEmptyChunk[GraphCompileError]] =
    Schema.of(SchemaDoc.Array(SchemaDoc.Ref("GraphCompileError")))

  given Schema[ProjectName] = text("ProjectName")

  given Schema[ScalaVersion] = text("ScalaVersion")

  given Schema[Sha256] = text("Sha256")

  given Schema[CompilerMessage] = text("CompilerMessage")

  given Schema[BinaryName] = text("BinaryName")

  given Schema[CompileDiagnostic] = text("CompileDiagnostic")

  given Schema[SuitePattern] = text("SuitePattern")

  given Schema[Digest] = text("Digest")

  given Schema[ActionId] = text("ActionId")

  given Schema[InputName] = text("InputName")

  given Schema[TaskId] = text("TaskId")

  given Schema[RunId] = text("RunId")

  given Schema[ShipName] = text("ShipName")

  given Schema[ShipNumber] = text("ShipNumber")

  given Schema[CiVersion] = text("CiVersion")

  given Schema[Dep] = text("Dep")

  given Schema[ParamName] = text("ParamName")

  given Schema[FlagName] = text("FlagName")

  given Schema[NoteKey] = text("NoteKey")

  given Schema[OutputName] = text("OutputName")

  given Schema[PluginId] = text("PluginId")

  given Schema[EntryName] = text("EntryName")

  given Schema[ApiDigest] = text("ApiDigest")

  given Schema[ContentHash] = text("ContentHash")

  given Schema[PluginDetail] = text("PluginDetail")

  given Schema[PluginTaskId] = text("PluginTaskId")

  given Schema[TestStatus] = Schema.derived

  given Schema[TestedCell] = Schema.derived

  given Schema[IoFault] = Schema.derived

  given Schema[IoFailure] = Schema.derived

  given Schema[PlatformError] = Schema.derived

  given Schema[SelectError] = Schema.derived

  given Schema[SuitePatternError] = Schema.derived

  given Schema[ParamNameError] = Schema.derived

  given Schema[FlagNameError] = Schema.derived

  given Schema[Sha256Error] = Schema.derived

  given Schema[ProjectNameError] = Schema.derived

  given Schema[ScalaVersionError] = Schema.derived

  given Schema[ShipNameError] = Schema.derived

  given Schema[ShipNumberError] = Schema.derived

  given Schema[DepError] = Schema.derived

  given Schema[OverrideError] = Schema.derived

  given Schema[JdkError] = Schema.derived

  given Schema[JdkMajor] = Schema.of(SchemaDoc.Integer(Some("int32")), Some("JdkMajor"))

  given Schema[JdkVersion] = text("JdkVersion")

  given Schema[ClangMajor] = Schema.of(SchemaDoc.Integer(Some("int32")), Some("ClangMajor"))

  given Schema[NodeMajor] = Schema.of(SchemaDoc.Integer(Some("int32")), Some("NodeMajor"))

  given Schema[JsVersion]      = text("JsVersion")
  given Schema[NativeVersion]  = text("NativeVersion")
  given Schema[TaskName]       = text("TaskName")
  given Schema[CommandName]    = text("CommandName")
  given Schema[JobName]        = text("JobName")
  given Schema[ActionName]     = text("ActionName")
  given Schema[RunnerName]     = text("RunnerName")
  given Schema[SecretName]     = text("SecretName")
  given Schema[TargetName]     = text("TargetName")
  given Schema[ProjectPath]    = text("ProjectPath")
  given Schema[RepoPath]       = text("RepoPath")
  given Schema[ResourceName]   = text("ResourceName")
  given Schema[Organization]   = text("Organization")
  given Schema[ToolRevision]   = text("ToolRevision")
  given Schema[TestMain]       = text("TestMain")
  given Schema[FrameworkClass] = text("FrameworkClass")
  given Schema[MainClass]      = text("MainClass")
  given Schema[PackMain]       = text("MainClass")

  given Schema[SourcePos]        = Schema.derived
  given Schema[PluginCoordinate] = Schema.derived
  given Schema[PluginIo]         = Schema.derived

  given Schema[RepoUrl] = text("RepoUrl")

  given Schema[ReleaseError] = Schema.derived
  given Schema[Reject]       = Schema.derived

  // Config.Error carries Cause, which is not a derivable ADT. The document is the wire enum.
  given Schema[Config.Error] =
    new Schema[Config.Error]:
      def doc: SchemaDoc                  = summon[Schema[ConfigError]].doc
      override def named: Option[String]  = summon[Schema[ConfigError]].named
      override def cases: List[SchemaDoc] = summon[Schema[ConfigError]].cases

  private enum ConfigError:
    case InvalidData(path: Chunk[String], message: String)
    case MissingData(path: Chunk[String], message: String)
    case Unsupported(path: Chunk[String], message: String)
    case And(left: ConfigError, right: ConfigError)
    case Or(left: ConfigError, right: ConfigError)
    case SourceUnavailable(path: Chunk[String], message: String, cause: WireCause)

  private enum WireCause:
    case Empty
    case Fail(kind: String, message: String)
    case Die(kind: String, message: String)
    case Interrupt
    case Then(left: WireCause, right: WireCause)
    case Both(left: WireCause, right: WireCause)
    case Stackless(cause: WireCause, stackless: Boolean)

  private given Schema[WireCause]   = Schema.derived
  private given Schema[ConfigError] = Schema.derived

  private final case class ConfigThrown(kind: String, detail: String) extends RuntimeException(detail)
end WireSchemaLeaves
private object WireSchemaBuild extends WireSchemaSupport:
  import WireSchemaLeaves.given

  given Schema[ToolchainError] = Schema.derived

  given Schema[CacheError] = Schema.derived

  given Schema[CacheExchange] = Schema.derived

  given Schema[DevNameError] = Schema.derived

  given Schema[NamespaceError] = Schema.derived

  given Schema[RemoteError] = Schema.derived

  given Schema[AssetError] = Schema.derived

  given Schema[GraphCompileError] = Schema.derived

  given Schema[JsEnv] = enumeration("JsEnv", JsEnv.values.toList.map(_.token))

  given Schema[ToolRole] = enumeration("ToolRole", ToolRole.values.toList.map(_.token))

  given Schema[ToolVariant] = text("ToolVariant")

  given Schema[Transitivity] = enumeration("Transitivity", List("closure", "single"))

  given Schema[Repository] = text("Repository")

  given Schema[ClasspathScope] = enumeration("ClasspathScope", List("cell", "test"))

  given Schema[LockField] = enumeration("LockField", LockField.values.toList.map(LockField.token))

  given Schema[JarClassifier] = text("JarClassifier")

  given Schema[ClassifierError] = Schema.derived

  given Schema[RepositoryUrl] = text("RepositoryUrl")

  given Schema[RepositoryUrlError] = Schema.derived

  given Schema[ToolId] = Schema.derived

  given Schema[ToolRoot] = Schema.derived

  given Schema[ToolRootFault] = Schema.derived

  given Schema[NativeToolchainError] = Schema.derived

  given Schema[LockError] = Schema.derived

  given Schema[RepositoryIo] = Schema.derived

  given Schema[DownloadFault] = Schema.derived

  given Schema[ArtifactMiss] = Schema.derived

  given Schema[PluginLoadError] = Schema.derived

  given Schema[histos.format.ScalafmtConfError] = Schema.derived

  private val resolveErrorSchema: Schema[ResolveError] = Schema.derived
  given Schema[ResolveError]                           = resolveErrorSchema

  given Schema[ClasspathBan] = Schema.derived

  given Schema[CompilerPart] = Schema.derived

  given Schema[ToolchainMiss] = Schema.derived

  given Schema[JsLibraryMiss] = Schema.derived

  given Schema[NativeFetch] = Schema.derived

  private val compileErrorSchema: Schema[CompileError] = Schema.derived
  given Schema[CompileError]                           = compileErrorSchema

  given Schema[histos.plugin.Need] =
    enumeration("Need", List("network", "docker"))

  given Schema[CiFault]                = Schema.derived
  given Schema[PluginGraphFault]       = Schema.derived
  given Schema[ToolchainLauncherFault] = Schema.derived
  given Schema[ToolchainClientFault]   = Schema.derived
  given Schema[BuildError]             = Schema.derived

  given Schema[LoadError] = Schema.derived

  given Schema[ProcessOutput] = text("ProcessOutput")

  given Schema[GroupOp] = Schema.derived

  given Schema[ChildError] = Schema.derived

  given Schema[NodeOverrideError] = Schema.derived

  given Schema[RuntimeLocationError] = Schema.derived

  given Schema[NodeVersionError] = Schema.derived

  given Schema[NodeError] = Schema.derived

  given Schema[NodeReject] = Schema.derived

  given Schema[NodeSource] = Schema.derived

  given Schema[NodeFileFault] = Schema.derived

  given Schema[NodePathError] = Schema.derived

  given Schema[NodeVersion] = text("NodeVersion")

  given Schema[NodeBinary] = Schema.derived

  given Schema[NodeMiss] = Schema.derived

  given Schema[RuntimeIdentity] = Schema.derived

  given Schema[JdkSurvey] = Schema.derived

  given Schema[NodeSurvey] = Schema.derived

  given Schema[Reached] = Schema.derived

  given Schema[ClangText] = Schema.derived

  given Schema[ClangVersion] = text("ClangVersion")

  given Schema[ClangTarget] = text("ClangTarget")

  given Schema[ClangMiss] = Schema.derived

  given Schema[SurveyError] = Schema.derived

end WireSchemaBuild
private object WireSchemaReport extends WireSchemaSupport:
  import WireSchemaLeaves.given

  given Schema[Severity] =
    enumeration("Severity", List("error", "warn", "info"))

  given Schema[ModuleTally]  = Schema.derived
  given Schema[Observation]  = Schema.derived
  given Schema[Diagnostic]   = Schema.derived
  given Schema[Report]       = Schema.derived
  given Schema[ObserveError] = Schema.derived
end WireSchemaReport

private object WireSchemaCall extends WireSchemaSupport:
  import WireSchemaLeaves.given
  import WireSchemaBuild.given
  import WireSchemaReport.given

  given Schema[SuiteName] = text("SuiteName")

  given Schema[SuiteNameError] = Schema.derived

  given Schema[ProviderLibrary] = Schema.derived

  given Schema[TestProvider] = Schema.derived

  given Schema[PackMainError] = Schema.derived

  given Schema[StampError] = Schema.derived

  given Schema[QuickError] = Schema.derived

  given Schema[LinkError] = Schema.derived

  given Schema[ClassShapeError] = Schema.derived

  given Schema[JsLinkError] = Schema.derived

  given Schema[JsRunError] = Schema.derived

  given Schema[JsSuiteError] = Schema.derived

  given Schema[NativeError] = Schema.derived

  given Schema[NativeSuiteError] = Schema.derived

  given Schema[PlatformToolError] = Schema.derived

  given Schema[ModuleTestError] = Schema.derived

  given Schema[ProgramError] = Schema.derived

  given Schema[JarIo] = Schema.derived

  private val jarErrorSchema: Schema[JarError] = Schema.derived
  given Schema[JarError]                       = jarErrorSchema

  given Schema[PublishFault] = Schema.derived

  given Schema[TransportFault] = Schema.derived

  given Schema[MetadataFault] = Schema.derived

  given Schema[SnapshotRevisionError] = Schema.derived

  given Schema[PublishError] = Schema.derived

  given Schema[LazyValFrame] = Schema.derived

  given Schema[LazyValError] = Schema.derived

  private val assemblyErrorSchema: Schema[AssemblyError] = Schema.derived
  given Schema[AssemblyError]                            = assemblyErrorSchema

  given Schema[LocalPublishError] = Schema.derived

  given Schema[PreviewError] = Schema.derived

  given Schema[WatchError] = Schema.derived

  given Schema[histos.format.ScalafmtPackage] = Schema.derived

  given Schema[histos.format.FormatError] = Schema.derived

  given Schema[CommitShaError] = Schema.derived

  given Schema[BranchError] = Schema.derived

  given Schema[PullRequestError] = Schema.derived

  given Schema[SessionError] = Schema.derived

  given Schema[TagRelation] = enumeration("TagRelation", List("matches", "moved", "unknown"))

  given Schema[TagRelationError] = Schema.derived

  given Schema[RegistryError] = Schema.derived

  given Schema[Refusal] = Schema.derived

  given Schema[VersionError] = Schema.derived

  given Schema[TaskError] = Schema.derived

  given Schema[PluginTaskError] = Schema.derived

  given Schema[PluginRunError] = Schema.derived

  given Schema[CiError] = Schema.derived

  given Schema[ScriptError] = Schema.derived

  given Schema[AssembleError] = Schema.derived

  given Schema[LauncherError] = Schema.derived

  given Schema[ScheduleError[PluginFault]] = Schema.derived

  given Schema[PluginFault] = Schema.derived

  given Schema[WorkflowFault] = Schema.derived

  given Schema[Rejection] = Schema.derived

  given Schema[BuildFault] = Schema.derived

  given Schema[CallResult] = Schema.derived

end WireSchemaCall
private object WireSchemaEvent extends WireSchemaSupport:
  import WireSchemaLeaves.given
  import WireSchemaBuild.given

  given Schema[CompileSeverity] = Schema.derived

  given Schema[PositionBase] = Schema.derived

  given Schema[SourceCount] = Schema.derived

  given Schema[CompileProblem] = Schema.derived

  given Schema[CompiledCell] = Schema.derived

  given Schema[CompileInput] = Schema.derived

  given Schema[KeyMiss] = Schema.derived

  given Schema[CompileFeed] = Schema.derived

  given Schema[TestFeed] = Schema.derived

  given Schema[BuildEvent] = Schema.derived
end WireSchemaEvent
object WireSchema:
  export WireSchemaLeaves.given
  export WireSchemaBuild.given
  export WireSchemaReport.given
  export WireSchemaCall.given
  export WireSchemaEvent.given
