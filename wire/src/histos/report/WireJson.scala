package histos.report

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
import histos.publish.{LauncherError, LocalPublishError, ScriptError}
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

/** Socket codecs. The document schema stays in the JVM app. A derived given is one type, so a parent summons it instead
  * of inlining the tree.
  *
  * The givens are split across objects. One object overflows the JVM class constant pool.
  */
private trait WireSupport:
  protected def parsed[E, A](read: String => Either[E, A], explain: E => String, write: A => String): JsonCodec[A] =
    JsonCodec.string.transformOrFail(raw => read(raw).left.map(explain), write)

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

  given JsonCodec[Platform] =
    JsonCodec.string.transformOrFail(
      raw => Platform.from(raw).left.map(_.message),
      _.token,
    )

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

  given [A: JsonCodec]: JsonCodec[NonEmptyChunk[A]] =
    JsonCodec[Chunk[A]].transformOrFail(
      chunk => NonEmptyChunk.fromChunk(chunk).toRight("empty chunk"),
      _.toChunk,
    )

  given [A: JsonCodec]: JsonCodec[::[A]] =
    JsonCodec[List[A]].transformOrFail(
      {
        case head :: tail => Right(::(head, tail))
        case Nil          => Left("empty list")
      },
      list => list,
    )

  given JsonCodec[ProjectName]     = parsed(ProjectName.from, _.message, _.value)
  given JsonCodec[ScalaVersion]    = parsed(ScalaVersion.from, _.message, _.value)
  given JsonCodec[Sha256]          = parsed(Sha256.from, _.message, _.value)
  given JsonCodec[CompilerMessage] =
    JsonCodec.string.transform(CompilerMessage.fromCompiler, _.text)
  given JsonCodec[BinaryName] =
    JsonCodec.string.transform(BinaryName.trusted, _.value)
  given JsonCodec[CompileDiagnostic] =
    JsonCodec.string.transform(CompileDiagnostic.fromCompiler, _.text)
  given JsonCodec[SuitePattern] = parsed(SuitePattern.from, _.message, _.value)
  given JsonCodec[Digest]       =
    JsonCodec.string.transformOrFail(
      raw => Digest.from(raw).toRight(s"'$raw' is not a digest"),
      _.value,
    )
  given JsonCodec[ActionId]   = parsed(ActionId.from, actionIdMessage, _.value)
  given JsonCodec[InputName]  = parsed(InputName.from, inputNameMessage, _.value)
  given JsonCodec[TaskId]     = parsed(TaskId.from, _.message, _.value)
  given JsonCodec[RunId]      = parsed(RunId.from, _.message, _.value)
  given JsonCodec[ShipName]   = parsed(ShipName.from, _.message, _.value)
  given JsonCodec[ShipNumber] = parsed(ShipNumber.from, _.message, _.render)
  given JsonCodec[CiVersion]  =
    JsonCodec[ShipNumber].transform(CiVersion(_), _.ship)
  given JsonCodec[Dep]          = parsed(Dep.from, _.message, depText)
  given JsonCodec[ParamName]    = parsed(ParamName.from, _.message, _.value)
  given JsonCodec[FlagName]     = parsed(FlagName.from, _.message, _.value)
  given JsonCodec[NoteKey]      = parsed(NoteKey.from, _.message, _.value)
  given JsonCodec[OutputName]   = parsed(OutputName.from, _.message, _.value)
  given JsonCodec[PluginId]     = parsed(PluginId.from, _.message, _.value)
  given JsonCodec[EntryName]    = parsed(EntryName.from, _.message, _.value)
  given JsonCodec[ApiDigest]    = parsed(ApiDigest.from, _.message, _.value)
  given JsonCodec[ContentHash]  = parsed(ContentHash.from, _.message, _.value)
  given JsonCodec[PluginDetail] =
    JsonCodec.string.transform(PluginDetail.trusted, _.value)
  given JsonCodec[PluginTaskId]      = parsed(PluginTaskId.parse, _.message, _.render)
  given JsonCodec[TestStatus]        = JsonCodec.derived
  given JsonCodec[TestedCell]        = JsonCodec.derived
  given JsonCodec[IoFault]           = JsonCodec.derived
  given JsonCodec[IoFailure]         = JsonCodec.derived
  given JsonCodec[PlatformError]     = JsonCodec.derived
  given JsonCodec[SelectError]       = JsonCodec.derived
  given JsonCodec[SuitePatternError] = JsonCodec.derived
  given JsonCodec[ParamNameError]    = JsonCodec.derived
  given JsonCodec[FlagNameError]     = JsonCodec.derived
  given JsonCodec[Sha256Error]       = JsonCodec.derived
  given JsonCodec[ProjectNameError]  = JsonCodec.derived
  given JsonCodec[ScalaVersionError] = JsonCodec.derived
  given JsonCodec[ShipNameError]     = JsonCodec.derived
  given JsonCodec[ShipNumberError]   = JsonCodec.derived
  given JsonCodec[DepError]          = JsonCodec.derived
  given JsonCodec[OverrideError]     = JsonCodec.derived
  given JsonCodec[JdkError]          = JsonCodec.derived
  given JsonCodec[JdkMajor]          =
    JsonCodec.int.transformOrFail(raw => JdkMajor.from(raw).left.map(_.message), _.value)
  given JsonCodec[JdkVersion] = parsed(JdkVersion.from, _.message, _.value)
  given JsonCodec[ClangMajor] =
    JsonCodec.int.transformOrFail(raw => ClangMajor.from(raw).left.map(_.message), _.value)
  given JsonCodec[NodeMajor] =
    JsonCodec.int.transformOrFail(raw => NodeMajor.from(raw).left.map(_.message), _.value)
  given JsonCodec[JsVersion]        = parsed(JsVersion.from, _.message, _.value)
  given JsonCodec[NativeVersion]    = parsed(NativeVersion.from, _.message, _.value)
  given JsonCodec[TaskName]         = parsed(TaskName.from, _.message, _.value)
  given JsonCodec[CommandName]      = parsed(CommandName.from, _.message, _.value)
  given JsonCodec[JobName]          = parsed(JobName.from, _.message, _.value)
  given JsonCodec[ProjectPath]      = parsed(ProjectPath.from, _.message, _.value)
  given JsonCodec[RepoPath]         = parsed(RepoPath.from, _.message, _.value)
  given JsonCodec[Organization]     = parsed(Organization.from, _.message, _.value)
  given JsonCodec[ToolRevision]     = parsed(ToolRevision.from, _.message, _.value)
  given JsonCodec[ManualVersion]    = parsed(ManualVersion.from, _.message, _.value)
  given JsonCodec[TestMain]         = parsed(TestMain.from, _.message, _.value)
  given JsonCodec[FrameworkClass]   = parsed(FrameworkClass.from, _.message, _.value)
  given JsonCodec[MainClass]        = parsed(MainClass.from, _.message, _.value)
  given JsonCodec[PackMain]         = parsed(PackMain.from, _.message, _.binary)
  given JsonCodec[SourcePos]        = JsonCodec.derived
  given JsonCodec[PluginCoordinate] = JsonCodec.derived
  given JsonCodec[PluginIo]         = JsonCodec.derived
  given JsonCodec[RepoUrl]          = parsed(RepoUrl.from, _.message, _.value)
  given JsonCodec[ReleaseError]     = JsonCodec.derived
  given JsonCodec[Reject]           = JsonCodec.derived
  given JsonCodec[Config.Error]     =
    summon[JsonCodec[ConfigError]].transform(wireConfig, configWire)

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
  private given JsonCodec[ConfigError] = JsonCodec.derived
  private final case class ConfigThrown(kind: String, detail: String) extends RuntimeException(detail)
end WireLeaves
private object WireBuild extends WireSupport:
  import WireLeaves.given

  given JsonCodec[ToolchainError]    = JsonCodec.derived
  given JsonCodec[CacheError]        = JsonCodec.derived
  given JsonCodec[CacheExchange]     = JsonCodec.derived
  given JsonCodec[RemoteError]       = JsonCodec.derived
  given JsonCodec[GraphCompileError] = JsonCodec.derived
  given JsonCodec[JsEnv]             =
    choose("js environment", JsEnv.values.toList.map(env => env.token -> env), _.token)
  given JsonCodec[ToolRole] =
    choose("tool role", ToolRole.values.toList.map(role => role.token -> role), _.token)
  given JsonCodec[ToolVariant] =
    JsonCodec.string.transformOrFail(
      raw => ToolVariant.from(raw).toRight(s"'$raw' is not a tool variant"),
      _.token,
    )
  given JsonCodec[Transitivity] =
    choose(
      "transitivity",
      List("closure" -> Transitivity.Closure, "single" -> Transitivity.Single),
      _.token,
    )
  given JsonCodec[Repository] =
    JsonCodec.string.transformOrFail(raw => Repository.parse(raw).left.map(_.message), Repository.token)
  given JsonCodec[ClasspathScope] =
    choose(
      "classpath scope",
      List("cell" -> ClasspathScope.Compile, "test" -> ClasspathScope.Test),
      _.header,
    )
  given JsonCodec[LockField] =
    choose("lock field", LockField.values.toList.map(field => LockField.token(field) -> field), LockField.token)
  given JsonCodec[JarClassifier]                         = parsed(JarClassifier.from, _.message, _.value)
  given JsonCodec[ClassifierError]                       = JsonCodec.derived
  given JsonCodec[RepositoryUrl]                         = parsed(RepositoryUrl.from, _.message, _.value)
  given JsonCodec[RepositoryUrlError]                    = JsonCodec.derived
  given JsonCodec[ToolId]                                = JsonCodec.derived
  given JsonCodec[ToolRoot]                              = JsonCodec.derived
  given JsonCodec[ToolRootFault]                         = JsonCodec.derived
  given JsonCodec[NativeToolchainError]                  = JsonCodec.derived
  given JsonCodec[LockError]                             = JsonCodec.derived
  given JsonCodec[RepositoryIo]                          = JsonCodec.derived
  given JsonCodec[PluginLoadError]                       = JsonCodec.derived
  private val resolveErrorCodec: JsonCodec[ResolveError] = JsonCodec.derived
  given JsonCodec[ResolveError]                          = resolveErrorCodec
  given JsonEncoder[ResolveError]                        = resolveErrorCodec.encoder
  given JsonDecoder[ResolveError]                        = resolveErrorCodec.decoder
  given JsonCodec[ClasspathBan]                          = JsonCodec.derived
  given JsonCodec[CompilerPart]                          = JsonCodec.derived
  given JsonCodec[ToolchainMiss]                         = JsonCodec.derived
  given JsonCodec[JsLibraryMiss]                         = JsonCodec.derived
  given JsonCodec[NativeFetch]                           = JsonCodec.derived
  private val compileErrorCodec: JsonCodec[CompileError] = JsonCodec.derived
  given JsonCodec[CompileError]                          = compileErrorCodec
  given JsonEncoder[CompileError]                        = compileErrorCodec.encoder
  given JsonDecoder[CompileError]                        = compileErrorCodec.decoder
  given JsonCodec[histos.plugin.Need]                    =
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
  given JsonCodec[BuildError]           = JsonCodec.derived
  given JsonCodec[LoadError]            = JsonCodec.derived
  given JsonCodec[ProcessOutput]        = JsonCodec.string.transform(ProcessOutput.from, _.text)
  given JsonCodec[GroupOp]              = JsonCodec.derived
  given JsonCodec[ChildError]           = JsonCodec.derived
  given JsonCodec[NodeOverrideError]    = JsonCodec.derived
  given JsonCodec[RuntimeLocationError] = JsonCodec.derived
  given JsonCodec[NodeVersionError]     = JsonCodec.derived
  given JsonCodec[NodeError]            = JsonCodec.derived
  given JsonCodec[NodeReject]           = JsonCodec.derived
  given JsonCodec[NodeVersion]          = parsed(nodeVersion, _.message, _.value)

  /** A probe is `major.minor.patch`. The floor is a trusted major, and a miss carries that text. */
  private def nodeVersion(raw: String): Either[NodeVersionError, NodeVersion] =
    NodeVersion
      .from(raw)
      .orElse:
        raw.toIntOption.filter(major => major > 0 && major.toString == raw) match
          case Some(major) => Right(NodeVersion.trusted(raw, NodeMajor.trusted(major)))
          case None        => Left(NodeVersionError.Bad(raw))

  given JsonCodec[NodeBinary]      = JsonCodec.derived
  given JsonCodec[NodeMiss]        = JsonCodec.derived
  given JsonCodec[RuntimeIdentity] = JsonCodec.derived
  given JsonCodec[JdkSurvey]       = JsonCodec.derived
  given JsonCodec[NodeSurvey]      = JsonCodec.derived
  given JsonCodec[Reached]         = JsonCodec.derived
  given JsonCodec[ClangText]       = JsonCodec.derived
  given JsonCodec[ClangVersion]    = parsed(ClangVersion.from, _.message, _.value)
  given JsonCodec[ClangTarget]     = parsed(ClangTarget.from, _.message, _.value)
  given JsonCodec[ClangMiss]       = JsonCodec.derived
  given JsonCodec[SurveyError]     = JsonCodec.derived
end WireBuild
private object WireCall extends WireSupport:
  import WireLeaves.given
  import WireBuild.given

  given JsonCodec[SuiteName]       = parsed(SuiteName.from, _.message, _.value)
  given JsonCodec[SuiteNameError]  = JsonCodec.derived
  given JsonCodec[ProviderLibrary] = JsonCodec[WireLibrary].transformOrFail(
    wire => ProviderLibrary.from(wire.organization, wire.name).left.map(_.message),
    library => WireLibrary(library.organization, library.name),
  )
  given JsonCodec[TestProvider] = JsonCodec[WireProvider].transformOrFail(
    wire =>
      ProviderLibrary
        .from(wire.organization, wire.name)
        .left
        .map(_.message)
        .map(library => TestProvider.trusted(wire.framework, library)),
    provider => WireProvider(provider.framework, provider.library.organization, provider.library.name),
  )
  given JsonCodec[PackMainError]                           = JsonCodec.derived
  given JsonCodec[StampError]                              = JsonCodec.derived
  given JsonCodec[QuickError]                              = JsonCodec.derived
  given JsonCodec[LinkError]                               = JsonCodec.derived
  given JsonCodec[ClassShapeError]                         = JsonCodec.derived
  given JsonCodec[JsLinkError]                             = JsonCodec.derived
  given JsonCodec[JsRunError]                              = JsonCodec.derived
  given JsonCodec[JsSuiteError]                            = JsonCodec.derived
  given JsonCodec[NativeError]                             = JsonCodec.derived
  given JsonCodec[NativeSuiteError]                        = JsonCodec.derived
  given JsonCodec[PlatformToolError]                       = JsonCodec.derived
  given JsonCodec[ModuleTestError]                         = JsonCodec.derived
  given JsonCodec[ProgramError]                            = JsonCodec.derived
  given JsonCodec[JarIo]                                   = JsonCodec.derived
  private val jarErrorCodec: JsonCodec[JarError]           = JsonCodec.derived
  given JsonCodec[JarError]                                = jarErrorCodec
  given JsonEncoder[JarError]                              = jarErrorCodec.encoder
  given JsonDecoder[JarError]                              = jarErrorCodec.decoder
  given JsonCodec[PublishFault]                            = JsonCodec.derived
  given JsonCodec[TransportFault]                          = JsonCodec.derived
  given JsonCodec[MetadataFault]                           = JsonCodec.derived
  given JsonCodec[SnapshotRevisionError]                   = JsonCodec.derived
  given JsonCodec[PublishError]                            = JsonCodec.derived
  given JsonCodec[LazyValFrame]                            = JsonCodec.derived
  given JsonCodec[LazyValError]                            = JsonCodec.derived
  private val assemblyErrorCodec: JsonCodec[AssemblyError] = JsonCodec.derived
  given JsonCodec[AssemblyError]                           = assemblyErrorCodec
  given JsonEncoder[AssemblyError]                         = assemblyErrorCodec.encoder
  given JsonDecoder[AssemblyError]                         = assemblyErrorCodec.decoder
  given JsonCodec[LocalPublishError]                       = JsonCodec.derived
  given JsonCodec[PreviewError]                            = JsonCodec.derived
  given JsonCodec[WatchError]                              = JsonCodec.derived
  given JsonCodec[CommitShaError]                          = JsonCodec.derived
  given JsonCodec[BranchError]                             = JsonCodec.derived
  given JsonCodec[PullRequestError]                        = JsonCodec.derived
  given JsonCodec[SessionError]                            = JsonCodec.derived
  given JsonCodec[TagRelation]                             =
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
  given JsonCodec[TagRelationError]           = JsonCodec.derived
  given JsonCodec[RegistryError]              = JsonCodec.derived
  given JsonCodec[Refusal]                    = JsonCodec.derived
  given JsonCodec[VersionError]               = JsonCodec.derived
  given JsonCodec[TaskError]                  = JsonCodec.derived
  given JsonCodec[PluginTaskError]            = JsonCodec.derived
  given JsonCodec[PluginRunError]             = JsonCodec.derived
  given JsonCodec[CiError]                    = JsonCodec.derived
  given JsonCodec[ScriptError]                = JsonCodec.derived
  given JsonCodec[AssembleError]              = JsonCodec.derived
  given JsonCodec[LauncherError]              = JsonCodec.derived
  given JsonCodec[ScheduleError[PluginFault]] = JsonCodec.derived

  given JsonCodec[PluginFault]   = JsonCodec.derived
  given JsonCodec[WorkflowFault] = JsonCodec.derived
  given JsonCodec[Rejection]     = JsonCodec.derived
  given JsonCodec[BuildFault]    = JsonCodec.derived
  given JsonCodec[CallResult]    = JsonCodec.derived
  private final case class WireLibrary(organization: String, name: String)

  private final case class WireProvider(framework: FrameworkClass, organization: String, name: String)

  private given JsonCodec[WireLibrary]  = JsonCodec.derived
  private given JsonCodec[WireProvider] = JsonCodec.derived
end WireCall
private object WireEvent extends WireSupport:
  import WireLeaves.given
  import WireBuild.given

  given JsonCodec[CompileSeverity] = JsonCodec.derived
  given JsonCodec[PositionBase]    = JsonCodec.derived
  given JsonCodec[SourceCount]     = JsonCodec.derived
  given JsonCodec[CompileProblem]  = JsonCodec.derived
  given JsonCodec[CompiledCell]    = JsonCodec.derived
  given JsonCodec[CompileInput]    = JsonCodec.derived
  given JsonCodec[KeyMiss]         = JsonCodec.derived
  given JsonCodec[CompileFeed]     = JsonCodec.derived
  given JsonCodec[TestFeed]        = JsonCodec.derived
  given JsonCodec[BuildEvent]      = JsonCodec.derived
end WireEvent
object WireJson:
  export WireLeaves.given
  export WireBuild.given
  export WireCall.given
  export WireEvent.given
