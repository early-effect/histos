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
  NodeMajor,
  NodeMiss,
  NodeOverrideError,
  NodeReject,
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
import histos.publish.{
  LauncherError,
  LauncherField,
  LauncherMetadataError,
  LauncherPayload,
  LocalPublishError,
  ScriptError,
}
import histos.resolve.{
  ClasspathScope,
  ClassifierError,
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
  Digest,
  InputName,
  InputNameError,
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
  ManualVersion,
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
  ProjectPath,
  ProviderLibrary,
  RepositoryUrl,
  RepositoryUrlError,
  ScalaVersion,
  ScalaVersionError,
  SourcePos,
  TestMain,
  TestProvider,
  ToolRevision,
}

import java.nio.file.{InvalidPathException, Path}

import zio.json.*
import zio.{Cause, Chunk, Config, FiberId, NonEmptyChunk}

/** App-only codecs. Engine and api stay free of zio-json. A derived given is one type, so a parent summons it instead
  * of inlining the tree.
  *
  * The givens are split across objects. One object overflows the JVM class constant pool.
  */
private trait WireSupport:
  protected def parsed[E, A](read: String => Either[E, A], explain: E => String, write: A => String): JsonCodec[A] =
    JsonCodec.string.transformOrFail(raw => read(raw).left.map(explain), write)

  protected def text[A](title: String): Schema[A] =
    Schema.of(SchemaDoc.Str(None), Some(title))

  protected def enumeration[A](title: String, values: List[String]): Schema[A] =
    Schema.of(SchemaDoc.Enum(Some(title), values))

  protected def choose[A](kind: String, pairs: List[(String, A)], write: A => String): JsonCodec[A] =
    JsonCodec.string.transformOrFail(
      raw => pairs.collectFirst { case (token, value) if token == raw => value }.toRight(s"'$raw' is not a $kind"),
      write,
    )

  protected def actionIdMessage(error: ActionIdError): String = error match
    case ActionIdError.Empty => "an action id is not empty"

  protected def inputNameMessage(error: InputNameError): String = error match
    case InputNameError.Empty => "an input name is not empty"

  protected def depText(dep: Dep): String =
    val sep = dep.kind match
      case histos.DepKind.Java   => ":"
      case histos.DepKind.Binary => "::"
      case histos.DepKind.Full   => ":::"
    s"${dep.organization}$sep${dep.name}:${dep.revision}"
end WireSupport

private object WireLeaves extends WireSupport:
  private def readPath(raw: String): Either[String, Path] =
    try Right(Path.of(raw))
    catch case _: InvalidPathException => Left(s"invalid path $raw")

  given JsonCodec[Path] =
    JsonCodec.string.transformOrFail(readPath, _.toString)

  given Schema[Path] = Schema.of(SchemaDoc.Str(None), Some("Path"))

  given JsonCodec[Platform] =
    JsonCodec.string.transformOrFail(
      raw => Platform.from(raw).left.map(_.message),
      _.token,
    )

  given Schema[Platform] =
    Schema.of(SchemaDoc.Enum(Some("Platform"), List("jvm", "js", "native")))

  given JsonCodec[PomFact] =
    choose(
      "pom fact",
      List(
        "License"   -> PomFact.License,
        "Developer" -> PomFact.Developer,
        "Scm"       -> PomFact.Scm,
      ),
      {
        case PomFact.License   => "License"
        case PomFact.Developer => "Developer"
        case PomFact.Scm       => "Scm"
      },
    )

  given Schema[PomFact] = enumeration("PomFact", List("License", "Developer", "Scm"))

  given [A: JsonCodec]: JsonCodec[NonEmptyChunk[A]] =
    JsonCodec[Chunk[A]].transformOrFail(
      chunk => NonEmptyChunk.fromChunk(chunk).toRight("empty chunk"),
      _.toChunk,
    )

  given [A: Schema]: Schema[NonEmptyChunk[A]] =
    Schema.of(SchemaDoc.Array(summon[Schema[A]].doc))

  given [A: Schema]: Schema[Set[A]] =
    Schema.of(SchemaDoc.Array(summon[Schema[A]].doc))

  given [A: JsonCodec]: JsonCodec[::[A]] =
    JsonCodec[List[A]].transformOrFail(
      {
        case head :: tail => Right(::(head, tail))
        case Nil          => Left("empty list")
      },
      list => list,
    )

  given [A: Schema]: Schema[::[A]] =
    Schema.of(SchemaDoc.Array(summon[Schema[A]].doc))

  // NonEmptyChunk is not one of heddle's deferred collections, so this cycle is a Ref.
  given Schema[NonEmptyChunk[GraphCompileError]] =
    Schema.of(SchemaDoc.Array(SchemaDoc.Ref("GraphCompileError")))

  given JsonCodec[ProjectName] = parsed(ProjectName.from, _.message, _.value)
  given Schema[ProjectName]    = text("ProjectName")

  given JsonCodec[ScalaVersion] = parsed(ScalaVersion.from, _.message, _.value)
  given Schema[ScalaVersion]    = text("ScalaVersion")

  given JsonCodec[Sha256] = parsed(Sha256.from, _.message, _.value)
  given Schema[Sha256]    = text("Sha256")

  given JsonCodec[CompilerMessage] =
    JsonCodec.string.transform(CompilerMessage.fromCompiler, _.text)
  given Schema[CompilerMessage] = text("CompilerMessage")

  given JsonCodec[BinaryName] =
    JsonCodec.string.transform(BinaryName.trusted, _.value)
  given Schema[BinaryName] = text("BinaryName")

  given JsonCodec[CompileDiagnostic] =
    JsonCodec.string.transform(CompileDiagnostic.fromCompiler, _.text)
  given Schema[CompileDiagnostic] = text("CompileDiagnostic")

  given JsonCodec[SuitePattern] = parsed(SuitePattern.from, _.message, _.value)
  given Schema[SuitePattern]    = text("SuitePattern")

  given JsonCodec[Digest] =
    JsonCodec.string.transformOrFail(
      raw => Digest.from(raw).toRight(s"'$raw' is not a digest"),
      _.value,
    )
  given Schema[Digest] = text("Digest")

  given JsonCodec[ActionId] = parsed(ActionId.from, actionIdMessage, _.value)
  given Schema[ActionId]    = text("ActionId")

  given JsonCodec[InputName] = parsed(InputName.from, inputNameMessage, _.value)
  given Schema[InputName]    = text("InputName")

  given JsonCodec[TaskId] = parsed(TaskId.from, _.message, _.value)
  given Schema[TaskId]    = text("TaskId")

  given JsonCodec[RunId] = parsed(RunId.from, _.message, _.value)
  given Schema[RunId]    = text("RunId")

  given JsonCodec[ShipName] = parsed(ShipName.from, _.message, _.value)
  given Schema[ShipName]    = text("ShipName")

  given JsonCodec[ShipNumber] = parsed(ShipNumber.from, _.message, _.render)
  given Schema[ShipNumber]    = text("ShipNumber")

  given JsonCodec[CiVersion] =
    JsonCodec[ShipNumber].transform(CiVersion(_), _.ship)
  given Schema[CiVersion] = text("CiVersion")

  given JsonCodec[Dep] = parsed(Dep.from, _.message, depText)
  given Schema[Dep]    = text("Dep")

  given JsonCodec[ParamName] = parsed(ParamName.from, _.message, _.value)
  given Schema[ParamName]    = text("ParamName")

  given JsonCodec[FlagName] = parsed(FlagName.from, _.message, _.value)
  given Schema[FlagName]    = text("FlagName")

  given JsonCodec[NoteKey] = parsed(NoteKey.from, _.message, _.value)
  given Schema[NoteKey]    = text("NoteKey")

  given JsonCodec[OutputName] = parsed(OutputName.from, _.message, _.value)
  given Schema[OutputName]    = text("OutputName")

  given JsonCodec[PluginId] = parsed(PluginId.from, _.message, _.value)
  given Schema[PluginId]    = text("PluginId")

  given JsonCodec[EntryName] = parsed(EntryName.from, _.message, _.value)
  given Schema[EntryName]    = text("EntryName")

  given JsonCodec[ApiDigest] = parsed(ApiDigest.from, _.message, _.value)
  given Schema[ApiDigest]    = text("ApiDigest")

  given JsonCodec[ContentHash] = parsed(ContentHash.from, _.message, _.value)
  given Schema[ContentHash]    = text("ContentHash")

  given JsonCodec[PluginDetail] =
    JsonCodec.string.transform(PluginDetail.trusted, _.value)
  given Schema[PluginDetail] = text("PluginDetail")

  given JsonCodec[PluginTaskId] = parsed(PluginTaskId.parse, _.message, _.render)
  given Schema[PluginTaskId]    = text("PluginTaskId")

  given JsonCodec[TestStatus] = JsonCodec.derived
  given Schema[TestStatus]    = Schema.derived

  given JsonCodec[TestedCell] = JsonCodec.derived
  given Schema[TestedCell]    = Schema.derived

  given JsonCodec[IoFault] = JsonCodec.derived
  given Schema[IoFault]    = Schema.derived

  given JsonCodec[IoFailure] = JsonCodec.derived
  given Schema[IoFailure]    = Schema.derived

  given JsonCodec[PlatformError] = JsonCodec.derived
  given Schema[PlatformError]    = Schema.derived

  given JsonCodec[SelectError] = JsonCodec.derived
  given Schema[SelectError]    = Schema.derived

  given JsonCodec[SuitePatternError] = JsonCodec.derived
  given Schema[SuitePatternError]    = Schema.derived

  given JsonCodec[ParamNameError] = JsonCodec.derived
  given Schema[ParamNameError]    = Schema.derived

  given JsonCodec[FlagNameError] = JsonCodec.derived
  given Schema[FlagNameError]    = Schema.derived

  given JsonCodec[Sha256Error] = JsonCodec.derived
  given Schema[Sha256Error]    = Schema.derived

  given JsonCodec[ProjectNameError] = JsonCodec.derived
  given Schema[ProjectNameError]    = Schema.derived

  given JsonCodec[ScalaVersionError] = JsonCodec.derived
  given Schema[ScalaVersionError]    = Schema.derived

  given JsonCodec[ShipNameError] = JsonCodec.derived
  given Schema[ShipNameError]    = Schema.derived

  given JsonCodec[ShipNumberError] = JsonCodec.derived
  given Schema[ShipNumberError]    = Schema.derived

  given JsonCodec[DepError] = JsonCodec.derived
  given Schema[DepError]    = Schema.derived

  given JsonCodec[OverrideError] = JsonCodec.derived
  given Schema[OverrideError]    = Schema.derived

  given JsonCodec[JdkError] = JsonCodec.derived
  given Schema[JdkError]    = Schema.derived

  given JsonCodec[JdkMajor] =
    JsonCodec.int.transformOrFail(raw => JdkMajor.from(raw).left.map(_.message), _.value)
  given Schema[JdkMajor] = Schema.of(SchemaDoc.Integer(Some("int32")), Some("JdkMajor"))

  given JsonCodec[JdkVersion] = parsed(JdkVersion.from, _.message, _.value)
  given Schema[JdkVersion]    = text("JdkVersion")

  given JsonCodec[ClangMajor] =
    JsonCodec.int.transformOrFail(raw => ClangMajor.from(raw).left.map(_.message), _.value)
  given Schema[ClangMajor] = Schema.of(SchemaDoc.Integer(Some("int32")), Some("ClangMajor"))

  given JsonCodec[NodeMajor] =
    JsonCodec.int.transformOrFail(raw => NodeMajor.from(raw).left.map(_.message), _.value)
  given Schema[NodeMajor] = Schema.of(SchemaDoc.Integer(Some("int32")), Some("NodeMajor"))

  given JsonCodec[JsVersion]      = parsed(JsVersion.from, _.message, _.value)
  given Schema[JsVersion]         = text("JsVersion")
  given JsonCodec[NativeVersion]  = parsed(NativeVersion.from, _.message, _.value)
  given Schema[NativeVersion]     = text("NativeVersion")
  given JsonCodec[TaskName]       = parsed(TaskName.from, _.message, _.value)
  given Schema[TaskName]          = text("TaskName")
  given JsonCodec[CommandName]    = parsed(CommandName.from, _.message, _.value)
  given Schema[CommandName]       = text("CommandName")
  given JsonCodec[JobName]        = parsed(JobName.from, _.message, _.value)
  given Schema[JobName]           = text("JobName")
  given JsonCodec[ProjectPath]    = parsed(ProjectPath.from, _.message, _.value)
  given Schema[ProjectPath]       = text("ProjectPath")
  given JsonCodec[RepoPath]       = parsed(RepoPath.from, _.message, _.value)
  given Schema[RepoPath]          = text("RepoPath")
  given JsonCodec[Organization]   = parsed(Organization.from, _.message, _.value)
  given Schema[Organization]      = text("Organization")
  given JsonCodec[ToolRevision]   = parsed(ToolRevision.from, _.message, _.value)
  given Schema[ToolRevision]      = text("ToolRevision")
  given JsonCodec[ManualVersion]  = parsed(ManualVersion.from, _.message, _.value)
  given Schema[ManualVersion]     = text("ManualVersion")
  given JsonCodec[TestMain]       = parsed(TestMain.from, _.message, _.value)
  given Schema[TestMain]          = text("TestMain")
  given JsonCodec[FrameworkClass] = parsed(FrameworkClass.from, _.message, _.value)
  given Schema[FrameworkClass]    = text("FrameworkClass")
  given JsonCodec[MainClass]      = parsed(MainClass.from, _.message, _.value)
  given Schema[MainClass]         = text("MainClass")
  given JsonCodec[PackMain]       = parsed(PackMain.from, _.message, _.binary)
  given Schema[PackMain]          = text("MainClass")

  given JsonCodec[SourcePos]        = JsonCodec.derived
  given Schema[SourcePos]           = Schema.derived
  given JsonCodec[PluginCoordinate] = JsonCodec.derived
  given Schema[PluginCoordinate]    = Schema.derived
  given JsonCodec[PluginIo]         = JsonCodec.derived
  given Schema[PluginIo]            = Schema.derived

  given JsonCodec[RepoUrl] = parsed(RepoUrl.from, _.message, _.value)
  given Schema[RepoUrl]    = text("RepoUrl")

  given JsonCodec[ReleaseError] = JsonCodec.derived
  given Schema[ReleaseError]    = Schema.derived
  given JsonCodec[Reject]       = JsonCodec.derived
  given Schema[Reject]          = Schema.derived

  given JsonCodec[Config.Error] =
    summon[JsonCodec[ConfigError]].transform(wireConfig, configWire)

  // Config.Error carries Cause, which is not a derivable ADT. The document is the wire enum.
  given Schema[Config.Error] =
    new Schema[Config.Error]:
      def doc: SchemaDoc                  = summon[Schema[ConfigError]].doc
      override def named: Option[String]  = summon[Schema[ConfigError]].named
      override def cases: List[SchemaDoc] = summon[Schema[ConfigError]].cases

  private def wireConfig(error: ConfigError): Config.Error = error match
    case ConfigError.InvalidData(path, message)              => Config.Error.InvalidData(path, message)
    case ConfigError.MissingData(path, message)              => Config.Error.MissingData(path, message)
    case ConfigError.Unsupported(path, message)              => Config.Error.Unsupported(path, message)
    case ConfigError.And(left, right)                        => Config.Error.And(wireConfig(left), wireConfig(right))
    case ConfigError.Or(left, right)                         => Config.Error.Or(wireConfig(left), wireConfig(right))
    case ConfigError.SourceUnavailable(path, message, cause) =>
      Config.Error.SourceUnavailable(path, message, wireCause(cause))

  private def configWire(error: Config.Error): ConfigError = error match
    case Config.Error.InvalidData(path, message)              => ConfigError.InvalidData(path, message)
    case Config.Error.MissingData(path, message)              => ConfigError.MissingData(path, message)
    case Config.Error.Unsupported(path, message)              => ConfigError.Unsupported(path, message)
    case Config.Error.And(left, right)                        => ConfigError.And(configWire(left), configWire(right))
    case Config.Error.Or(left, right)                         => ConfigError.Or(configWire(left), configWire(right))
    case Config.Error.SourceUnavailable(path, message, cause) =>
      ConfigError.SourceUnavailable(path, message, causeWire(cause))

  private def wireCause(cause: WireCause): Cause[Throwable] = cause match
    case WireCause.Empty                       => Cause.empty
    case WireCause.Fail(kind, message)         => Cause.fail(ConfigThrown(kind, message))
    case WireCause.Die(kind, message)          => Cause.die(ConfigThrown(kind, message))
    case WireCause.Interrupt                   => Cause.interrupt(FiberId.None)
    case WireCause.Then(left, right)           => Cause.Then(wireCause(left), wireCause(right))
    case WireCause.Both(left, right)           => Cause.Both(wireCause(left), wireCause(right))
    case WireCause.Stackless(inner, stackless) => Cause.Stackless(wireCause(inner), stackless)

  private def causeWire(cause: Cause[Throwable]): WireCause = cause match
    case Cause.Empty                                => WireCause.Empty
    case Cause.Fail(ConfigThrown(kind, message), _) => WireCause.Fail(kind, message)
    case Cause.Fail(thrown, _)                      => WireCause.Fail(thrown.getClass.getName, thrownText(thrown))
    case Cause.Die(ConfigThrown(kind, message), _)  => WireCause.Die(kind, message)
    case Cause.Die(thrown, _)                       => WireCause.Die(thrown.getClass.getName, thrownText(thrown))
    case Cause.Interrupt(_, _)                      => WireCause.Interrupt
    case Cause.Then(left, right)                    => WireCause.Then(causeWire(left), causeWire(right))
    case Cause.Both(left, right)                    => WireCause.Both(causeWire(left), causeWire(right))
    case Cause.Stackless(inner, stackless)          => WireCause.Stackless(causeWire(inner), stackless)

  private def thrownText(thrown: Throwable): String =
    Option(thrown.getMessage).getOrElse("")

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

  private given JsonCodec[WireCause]   = JsonCodec.derived
  private given Schema[WireCause]      = Schema.derived
  private given JsonCodec[ConfigError] = JsonCodec.derived
  private given Schema[ConfigError]    = Schema.derived

  private final case class ConfigThrown(kind: String, detail: String) extends RuntimeException(detail)
end WireLeaves

private object WireBuild extends WireSupport:
  import WireLeaves.given

  given JsonCodec[ToolchainError] = JsonCodec.derived
  given Schema[ToolchainError]    = Schema.derived

  given JsonCodec[CacheError] = JsonCodec.derived
  given Schema[CacheError]    = Schema.derived

  given JsonCodec[CacheExchange] = JsonCodec.derived
  given Schema[CacheExchange]    = Schema.derived

  given JsonCodec[RemoteError] = JsonCodec.derived
  given Schema[RemoteError]    = Schema.derived

  given JsonCodec[GraphCompileError] = JsonCodec.derived
  given Schema[GraphCompileError]    = Schema.derived

  given JsonCodec[JsEnv] =
    choose("js environment", JsEnv.values.toList.map(env => env.token -> env), _.token)
  given Schema[JsEnv] = enumeration("JsEnv", JsEnv.values.toList.map(_.token))

  given JsonCodec[ToolRole] =
    choose("tool role", ToolRole.values.toList.map(role => role.token -> role), _.token)
  given Schema[ToolRole] = enumeration("ToolRole", ToolRole.values.toList.map(_.token))

  given JsonCodec[ToolVariant] =
    JsonCodec.string.transformOrFail(
      raw => ToolVariant.from(raw).toRight(s"'$raw' is not a tool variant"),
      _.token,
    )
  given Schema[ToolVariant] = text("ToolVariant")

  given JsonCodec[Transitivity] =
    choose(
      "transitivity",
      List("closure" -> Transitivity.Closure, "single" -> Transitivity.Single),
      _.token,
    )
  given Schema[Transitivity] = enumeration("Transitivity", List("closure", "single"))

  given JsonCodec[Repository] =
    JsonCodec.string.transformOrFail(raw => Repository.parse(raw).left.map(_.message), Repository.token)
  given Schema[Repository] = text("Repository")

  given JsonCodec[ClasspathScope] =
    choose(
      "classpath scope",
      List("cell" -> ClasspathScope.Compile, "test" -> ClasspathScope.Test),
      _.header,
    )
  given Schema[ClasspathScope] = enumeration("ClasspathScope", List("cell", "test"))

  given JsonCodec[LockField] =
    choose("lock field", LockField.values.toList.map(field => LockField.token(field) -> field), LockField.token)
  given Schema[LockField] = enumeration("LockField", LockField.values.toList.map(LockField.token))

  given JsonCodec[JarClassifier] = parsed(JarClassifier.from, _.message, _.value)
  given Schema[JarClassifier]    = text("JarClassifier")

  given JsonCodec[ClassifierError] = JsonCodec.derived
  given Schema[ClassifierError]    = Schema.derived

  given JsonCodec[RepositoryUrl] = parsed(RepositoryUrl.from, _.message, _.value)
  given Schema[RepositoryUrl]    = text("RepositoryUrl")

  given JsonCodec[RepositoryUrlError] = JsonCodec.derived
  given Schema[RepositoryUrlError]    = Schema.derived

  given JsonCodec[ToolId] = JsonCodec.derived
  given Schema[ToolId]    = Schema.derived

  given JsonCodec[ToolRoot] = JsonCodec.derived
  given Schema[ToolRoot]    = Schema.derived

  given JsonCodec[ToolRootFault] = JsonCodec.derived
  given Schema[ToolRootFault]    = Schema.derived

  given JsonCodec[NativeToolchainError] = JsonCodec.derived
  given Schema[NativeToolchainError]    = Schema.derived

  given JsonCodec[LockError] = JsonCodec.derived
  given Schema[LockError]    = Schema.derived

  given JsonCodec[RepositoryIo] = JsonCodec.derived
  given Schema[RepositoryIo]    = Schema.derived

  given JsonCodec[PluginLoadError] = JsonCodec.derived
  given Schema[PluginLoadError]    = Schema.derived

  private val resolveErrorCodec: JsonCodec[ResolveError] = JsonCodec.derived
  private val resolveErrorSchema: Schema[ResolveError]   = Schema.derived
  given JsonCodec[ResolveError]                          = resolveErrorCodec
  given JsonEncoder[ResolveError]                        = resolveErrorCodec.encoder
  given JsonDecoder[ResolveError]                        = resolveErrorCodec.decoder
  given Schema[ResolveError]                             = resolveErrorSchema

  given JsonCodec[ClasspathBan] = JsonCodec.derived
  given Schema[ClasspathBan]    = Schema.derived

  given JsonCodec[CompilerPart] = JsonCodec.derived
  given Schema[CompilerPart]    = Schema.derived

  given JsonCodec[ToolchainMiss] = JsonCodec.derived
  given Schema[ToolchainMiss]    = Schema.derived

  given JsonCodec[JsLibraryMiss] = JsonCodec.derived
  given Schema[JsLibraryMiss]    = Schema.derived

  given JsonCodec[NativeFetch] = JsonCodec.derived
  given Schema[NativeFetch]    = Schema.derived

  private val compileErrorCodec: JsonCodec[CompileError] = JsonCodec.derived
  private val compileErrorSchema: Schema[CompileError]   = Schema.derived
  given JsonCodec[CompileError]                          = compileErrorCodec
  given JsonEncoder[CompileError]                        = compileErrorCodec.encoder
  given JsonDecoder[CompileError]                        = compileErrorCodec.decoder
  given Schema[CompileError]                             = compileErrorSchema

  given JsonCodec[histos.plugin.Need] =
    choose(
      "need",
      List(
        "network" -> histos.plugin.Need.Network,
        "docker"  -> histos.plugin.Need.Docker,
      ),
      {
        case histos.plugin.Need.Network => "network"
        case histos.plugin.Need.Docker  => "docker"
      },
    )
  given Schema[histos.plugin.Need] =
    enumeration("Need", List("network", "docker"))

  given JsonCodec[BuildError] = JsonCodec.derived
  given Schema[BuildError]    = Schema.derived

  given JsonCodec[LoadError] = JsonCodec.derived
  given Schema[LoadError]    = Schema.derived

  given JsonCodec[ProcessOutput] = JsonCodec.string.transform(ProcessOutput.from, _.text)
  given Schema[ProcessOutput]    = text("ProcessOutput")

  given JsonCodec[GroupOp] = JsonCodec.derived
  given Schema[GroupOp]    = Schema.derived

  given JsonCodec[ChildError] = JsonCodec.derived
  given Schema[ChildError]    = Schema.derived

  given JsonCodec[NodeOverrideError] = JsonCodec.derived
  given Schema[NodeOverrideError]    = Schema.derived

  given JsonCodec[RuntimeLocationError] = JsonCodec.derived
  given Schema[RuntimeLocationError]    = Schema.derived

  given JsonCodec[NodeVersionError] = JsonCodec.derived
  given Schema[NodeVersionError]    = Schema.derived

  given JsonCodec[NodeError] = JsonCodec.derived
  given Schema[NodeError]    = Schema.derived

  given JsonCodec[NodeReject] = JsonCodec.derived
  given Schema[NodeReject]    = Schema.derived

  given JsonCodec[NodeVersion] = parsed(nodeVersion, _.message, _.value)
  given Schema[NodeVersion]    = text("NodeVersion")

  /** A probe is `major.minor.patch`. The floor is a trusted major, and a miss carries that text. */
  private def nodeVersion(raw: String): Either[NodeVersionError, NodeVersion] =
    NodeVersion
      .from(raw)
      .orElse:
        raw.toIntOption.filter(major => major > 0 && major.toString == raw) match
          case Some(major) => Right(NodeVersion.trusted(raw, NodeMajor.trusted(major)))
          case None        => Left(NodeVersionError.Bad(raw))

  given JsonCodec[NodeBinary] = JsonCodec.derived
  given Schema[NodeBinary]    = Schema.derived

  given JsonCodec[NodeMiss] = JsonCodec.derived
  given Schema[NodeMiss]    = Schema.derived

  given JsonCodec[RuntimeIdentity] = JsonCodec.derived
  given Schema[RuntimeIdentity]    = Schema.derived

  given JsonCodec[JdkSurvey] = JsonCodec.derived
  given Schema[JdkSurvey]    = Schema.derived

  given JsonCodec[NodeSurvey] = JsonCodec.derived
  given Schema[NodeSurvey]    = Schema.derived

  given JsonCodec[Reached] = JsonCodec.derived
  given Schema[Reached]    = Schema.derived

  given JsonCodec[ClangText] = JsonCodec.derived
  given Schema[ClangText]    = Schema.derived

  given JsonCodec[ClangVersion] = parsed(ClangVersion.from, _.message, _.value)
  given Schema[ClangVersion]    = text("ClangVersion")

  given JsonCodec[ClangTarget] = parsed(ClangTarget.from, _.message, _.value)
  given Schema[ClangTarget]    = text("ClangTarget")

  given JsonCodec[ClangMiss] = JsonCodec.derived
  given Schema[ClangMiss]    = Schema.derived

  given JsonCodec[SurveyError] = JsonCodec.derived
  given Schema[SurveyError]    = Schema.derived

end WireBuild

private object WireCall extends WireSupport:
  import WireLeaves.given
  import WireBuild.given

  given JsonCodec[SuiteName] = parsed(SuiteName.from, _.message, _.value)
  given Schema[SuiteName]    = text("SuiteName")

  given JsonCodec[SuiteNameError] = JsonCodec.derived
  given Schema[SuiteNameError]    = Schema.derived

  given JsonCodec[ProviderLibrary] = JsonCodec[WireLibrary].transformOrFail(
    wire => ProviderLibrary.from(wire.organization, wire.name).left.map(_.message),
    library => WireLibrary(library.organization, library.name),
  )
  given Schema[ProviderLibrary] = Schema.derived

  given JsonCodec[TestProvider] = JsonCodec[WireProvider].transformOrFail(
    wire =>
      ProviderLibrary
        .from(wire.organization, wire.name)
        .left
        .map(_.message)
        .map(library => TestProvider.trusted(wire.framework, library)),
    provider => WireProvider(provider.framework, provider.library.organization, provider.library.name),
  )
  given Schema[TestProvider] = Schema.derived

  given JsonCodec[PackMainError] = JsonCodec.derived
  given Schema[PackMainError]    = Schema.derived

  given JsonCodec[StampError] = JsonCodec.derived
  given Schema[StampError]    = Schema.derived

  given JsonCodec[QuickError] = JsonCodec.derived
  given Schema[QuickError]    = Schema.derived

  given JsonCodec[LinkError] = JsonCodec.derived
  given Schema[LinkError]    = Schema.derived

  given JsonCodec[ClassShapeError] = JsonCodec.derived
  given Schema[ClassShapeError]    = Schema.derived

  given JsonCodec[JsLinkError] = JsonCodec.derived
  given Schema[JsLinkError]    = Schema.derived

  given JsonCodec[JsRunError] = JsonCodec.derived
  given Schema[JsRunError]    = Schema.derived

  given JsonCodec[JsSuiteError] = JsonCodec.derived
  given Schema[JsSuiteError]    = Schema.derived

  given JsonCodec[NativeError] = JsonCodec.derived
  given Schema[NativeError]    = Schema.derived

  given JsonCodec[NativeSuiteError] = JsonCodec.derived
  given Schema[NativeSuiteError]    = Schema.derived

  given JsonCodec[PlatformToolError] = JsonCodec.derived
  given Schema[PlatformToolError]    = Schema.derived

  given JsonCodec[ModuleTestError] = JsonCodec.derived
  given Schema[ModuleTestError]    = Schema.derived

  given JsonCodec[ProgramError] = JsonCodec.derived
  given Schema[ProgramError]    = Schema.derived

  given JsonCodec[JarIo] = JsonCodec.derived
  given Schema[JarIo]    = Schema.derived

  private val jarErrorCodec: JsonCodec[JarError] = JsonCodec.derived
  private val jarErrorSchema: Schema[JarError]   = Schema.derived
  given JsonCodec[JarError]                      = jarErrorCodec
  given JsonEncoder[JarError]                    = jarErrorCodec.encoder
  given JsonDecoder[JarError]                    = jarErrorCodec.decoder
  given Schema[JarError]                         = jarErrorSchema

  given JsonCodec[PublishFault] = JsonCodec.derived
  given Schema[PublishFault]    = Schema.derived

  given JsonCodec[TransportFault] = JsonCodec.derived
  given Schema[TransportFault]    = Schema.derived

  given JsonCodec[MetadataFault] = JsonCodec.derived
  given Schema[MetadataFault]    = Schema.derived

  given JsonCodec[SnapshotRevisionError] = JsonCodec.derived
  given Schema[SnapshotRevisionError]    = Schema.derived

  given JsonCodec[PublishError] = JsonCodec.derived
  given Schema[PublishError]    = Schema.derived

  given JsonCodec[LazyValFrame] = JsonCodec.derived
  given Schema[LazyValFrame]    = Schema.derived

  given JsonCodec[LazyValError] = JsonCodec.derived
  given Schema[LazyValError]    = Schema.derived

  private val assemblyErrorCodec: JsonCodec[AssemblyError] = JsonCodec.derived
  private val assemblyErrorSchema: Schema[AssemblyError]   = Schema.derived
  given JsonCodec[AssemblyError]                           = assemblyErrorCodec
  given JsonEncoder[AssemblyError]                         = assemblyErrorCodec.encoder
  given JsonDecoder[AssemblyError]                         = assemblyErrorCodec.decoder
  given Schema[AssemblyError]                              = assemblyErrorSchema

  given JsonCodec[LocalPublishError] = JsonCodec.derived
  given Schema[LocalPublishError]    = Schema.derived

  given JsonCodec[PreviewError] = JsonCodec.derived
  given Schema[PreviewError]    = Schema.derived

  given JsonCodec[WatchError] = JsonCodec.derived
  given Schema[WatchError]    = Schema.derived

  given JsonCodec[CommitShaError] = JsonCodec.derived
  given Schema[CommitShaError]    = Schema.derived

  given JsonCodec[BranchError] = JsonCodec.derived
  given Schema[BranchError]    = Schema.derived

  given JsonCodec[PullRequestError] = JsonCodec.derived
  given Schema[PullRequestError]    = Schema.derived

  given JsonCodec[SessionError] = JsonCodec.derived
  given Schema[SessionError]    = Schema.derived

  given JsonCodec[TagRelation] =
    choose(
      "tag relation",
      List(
        "matches" -> TagRelation.Matches,
        "moved"   -> TagRelation.Moved,
        "unknown" -> TagRelation.Unknown,
      ),
      {
        case TagRelation.Matches => "matches"
        case TagRelation.Moved   => "moved"
        case TagRelation.Unknown => "unknown"
      },
    )
  given Schema[TagRelation] = enumeration("TagRelation", List("matches", "moved", "unknown"))

  given JsonCodec[TagRelationError] = JsonCodec.derived
  given Schema[TagRelationError]    = Schema.derived

  given JsonCodec[RegistryError] = JsonCodec.derived
  given Schema[RegistryError]    = Schema.derived

  given JsonCodec[Refusal] = JsonCodec.derived
  given Schema[Refusal]    = Schema.derived

  given JsonCodec[VersionError] = JsonCodec.derived
  given Schema[VersionError]    = Schema.derived

  given JsonCodec[TaskError] = JsonCodec.derived
  given Schema[TaskError]    = Schema.derived

  given JsonCodec[PluginTaskError] = JsonCodec.derived
  given Schema[PluginTaskError]    = Schema.derived

  given JsonCodec[PluginRunError] = JsonCodec.derived
  given Schema[PluginRunError]    = Schema.derived

  given JsonCodec[CiError] = JsonCodec.derived
  given Schema[CiError]    = Schema.derived

  given JsonCodec[ScriptError] = JsonCodec.derived
  given Schema[ScriptError]    = Schema.derived

  given JsonCodec[LauncherField] =
    JsonCodec.string.transformOrFail(raw => LauncherField.named(raw).left.map(_.message), _.name)
  given Schema[LauncherField] = enumeration("LauncherField", List("repository", "version", "revision", "sha256"))

  given JsonCodec[LauncherMetadataError] = JsonCodec.derived
  given Schema[LauncherMetadataError]    = Schema.derived

  given JsonCodec[LauncherPayload] = JsonCodec.derived
  given Schema[LauncherPayload]    = Schema.derived

  given JsonCodec[AssembleError] = JsonCodec.derived
  given Schema[AssembleError]    = Schema.derived

  given JsonCodec[LauncherError] = JsonCodec.derived
  given Schema[LauncherError]    = Schema.derived

  given Schema[ScheduleError[PluginFault]] = Schema.derived

  given JsonCodec[ScheduleError[PluginFault]] = JsonCodec.derived

  given JsonCodec[PluginFault] = JsonCodec.derived
  given Schema[PluginFault]    = Schema.derived

  given JsonCodec[WorkflowFault] = JsonCodec.derived
  given Schema[WorkflowFault]    = Schema.derived

  given JsonCodec[Rejection] = JsonCodec.derived
  given Schema[Rejection]    = Schema.derived

  given JsonCodec[BuildFault] = JsonCodec.derived
  given Schema[BuildFault]    = Schema.derived

  given JsonCodec[CallResult] = JsonCodec.derived
  given Schema[CallResult]    = Schema.derived

  private final case class WireLibrary(organization: String, name: String)

  private final case class WireProvider(framework: FrameworkClass, organization: String, name: String)

  private given JsonCodec[WireLibrary]  = JsonCodec.derived
  private given JsonCodec[WireProvider] = JsonCodec.derived
end WireCall

private object WireEvent extends WireSupport:
  import WireLeaves.given
  import WireBuild.given

  given JsonCodec[CompileSeverity] = JsonCodec.derived
  given Schema[CompileSeverity]    = Schema.derived

  given JsonCodec[PositionBase] = JsonCodec.derived
  given Schema[PositionBase]    = Schema.derived

  given JsonCodec[SourceCount] = JsonCodec.derived
  given Schema[SourceCount]    = Schema.derived

  given JsonCodec[CompileProblem] = JsonCodec.derived
  given Schema[CompileProblem]    = Schema.derived

  given JsonCodec[CompiledCell] = JsonCodec.derived
  given Schema[CompiledCell]    = Schema.derived

  given JsonCodec[CompileInput] = JsonCodec.derived
  given Schema[CompileInput]    = Schema.derived

  given JsonCodec[KeyMiss] = JsonCodec.derived
  given Schema[KeyMiss]    = Schema.derived

  given JsonCodec[CompileFeed] = JsonCodec.derived
  given Schema[CompileFeed]    = Schema.derived

  given JsonCodec[TestFeed] = JsonCodec.derived
  given Schema[TestFeed]    = Schema.derived

  given JsonCodec[BuildEvent] = JsonCodec.derived
  given Schema[BuildEvent]    = Schema.derived
end WireEvent

object WireJson:
  export WireLeaves.given
  export WireBuild.given
  export WireCall.given
  export WireEvent.given
