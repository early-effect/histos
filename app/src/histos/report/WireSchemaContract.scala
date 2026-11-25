package histos.report

import heddle.endpoint.Schema
import histos.doctor.{CheckId, Edit, Fix, GitHubPut, HistosCommand, Level, Location, Noted, ShellLine}
import histos.lock.{
  DeclarationDrift,
  ExecutionKind,
  LockBlocked,
  LockCommitted,
  LockChange,
  LockDecision,
  LockDefaults,
  LockDiff,
  LockDiffEntry,
  LockEvent,
  LockMode,
  LockOffer,
  LockPhase,
  LockRefusal,
  LockReply,
  LockRevisionState,
  LockStage,
  LockSubject,
  LockTransactionError,
  NecessaryScope,
  ProposalSummary,
}
import histos.plugin.{
  ActivationManifest,
  ArtifactEntry,
  ArtifactFormat,
  ArtifactIdentity,
  ArtifactManifestError,
  ArtifactPath,
  ArtifactPathError,
  BindingClass,
  BindingKind,
  BootstrapError,
  BootstrapPlanError,
  BootstrapSource,
  BootstrapSteps,
  ClientId,
  CommandIdentity,
  CommandToken,
  CompilerConstraint,
  EnvName,
  FrozenDirectory,
  FrozenEnv,
  FrozenFile,
  ClosureArtifact,
  ClosureRole,
  GenerationId,
  GenerationPluginEvent,
  InputKey,
  InputRecord,
  Interaction,
  InvocationContext,
  InvocationId,
  InvocationPluginEvent,
  LinkStrategy,
  LockRevision,
  OfflineSelection,
  PluginEventError,
  PluginCatalog,
  PluginOrigin,
  PluginResolvers,
  ProposalId,
  RecipeError,
  RecipeFeature,
  RecipeInput,
  RecipeNode,
  RecipeNodeId,
  RecipeRole,
  HostRole,
  HostSubstitution,
  ProducerBinding,
  RecipeVersion,
  RecordedInputs,
  ResolutionIntent,
  ResolveVerb,
  SealedArtifactManifest,
  ServiceKey,
  SelectedInput,
  SemanticSetting,
  SourceId,
  SourceKind,
  SourceProvenance,
  SourceSet,
  StageAssurance,
  StageBinding,
  StageClock,
  StageContract,
  StageContractError,
  StageCwd,
  StageInputs,
  StageNetwork,
  StageProcess,
  StageRandom,
  StageRestrictions,
}
import histos.toolchain.{
  HostCompilerError,
  HostCompilerManifest,
  HostCompilerSession,
  HostToolEntry,
  HostToolRole,
  MaterializedTool,
}
import histos.zinc.{CompilationCapture, CompilationSnapshot}
import histos.{HttpUrl, PluginPublication, PluginRef, Resolution, Resolver}

/** Document schemas for the plugin contracts. JSON codecs stay in wire. */
private object WireSchemaContract extends WireSchemaSupport:
  import WireSchemaLeaves.given
  import WireSchemaStages.given Schema[histos.PlatformSelection]
  import WireSchemaStages.given Schema[histos.ScalaSelection]
  import WireSchemaStages.given Schema[histos.TestSelection]

  given Schema[SourceId]          = text("SourceId")
  given Schema[ServiceKey]        = text("ServiceKey")
  given Schema[LockRevision]      = text("LockRevision")
  given Schema[ProposalId]        = text("ProposalId")
  given Schema[GenerationId]      = text("GenerationId")
  given Schema[InvocationId]      = text("InvocationId")
  given Schema[RecipeNodeId]      = text("RecipeNodeId")
  given Schema[ClientId]          = text("ClientId")
  given Schema[CommandToken]      = text("CommandToken")
  given Schema[RecipeFeature]     = text("RecipeFeature")
  given Schema[RecipeInput]       = text("RecipeInput")
  given Schema[ArtifactPath]      = text("ArtifactPath")
  given Schema[InputKey]          = text("InputKey")
  given Schema[EnvName]           = text("EnvName")
  given Schema[CheckId]           = text("CheckId")
  given Schema[ShellLine]         = text("ShellLine")
  given Schema[HttpUrl]           = text("HttpUrl")
  given Schema[Resolution]        = enumeration("Resolution", List("solve", "lock"))
  given Schema[PluginPublication] =
    enumeration("PluginPublication", List("rebuildable", "binary-only"))

  given Schema[Level]         = Schema.derived
  given Schema[Edit]          = Schema.derived
  given Schema[GitHubPut]     = Schema.derived
  given Schema[HistosCommand] = Schema.derived
  given Schema[Fix]           = Schema.derived
  given Schema[Location]      = Schema.derived
  given Schema[Noted]         = Schema.derived

  given Schema[ArtifactFormat]         = Schema.derived
  given Schema[ArtifactIdentity]       = Schema.derived
  given Schema[ArtifactEntry]          = Schema.derived
  given Schema[ArtifactManifestError]  = Schema.derived
  given Schema[SealedArtifactManifest] = Schema.derived

  given Schema[BindingKind]        = Schema.derived
  given Schema[StageAssurance]     = Schema.derived
  given Schema[BindingClass]       = Schema.derived
  given Schema[StageCwd]           = Schema.derived
  given Schema[StageNetwork]       = Schema.derived
  given Schema[StageClock]         = Schema.derived
  given Schema[StageRandom]        = Schema.derived
  given Schema[StageProcess]       = Schema.derived
  given Schema[StageRestrictions]  = Schema.derived
  given Schema[StageBinding]       = Schema.derived
  given Schema[FrozenFile]         = Schema.derived
  given Schema[FrozenDirectory]    = Schema.derived
  given Schema[FrozenEnv]          = Schema.derived
  given Schema[StageInputs]        = Schema.derived
  given Schema[CompilerConstraint] = Schema.derived
  given Schema[StageContract]      = Schema.derived
  given Schema[StageContractError] = Schema.derived

  given Schema[RecipeVersion]       = Schema.derived
  given Schema[RecipeRole]          = Schema.derived
  given Schema[SourceSet]           = Schema.derived
  given Schema[SourceKind]          = Schema.derived
  given Schema[SourceProvenance]    = Schema.derived
  given Schema[SelectedInput]       = Schema.derived
  given Schema[SemanticSetting]     = Schema.derived
  given Schema[ClosureRole]         = Schema.derived
  given Schema[HostRole]            = Schema.derived
  given Schema[HostSubstitution]    = Schema.derived
  given Schema[ClosureArtifact]     = Schema.derived
  given Schema[ProducerBinding]     = Schema.derived
  given Schema[RecordedInputs]      = Schema.derived
  given Schema[InputRecord]         = Schema.derived
  given Schema[RecipeNode]          = Schema.derived
  given Schema[RecipeError]         = Schema.derived
  given Schema[CompilationSnapshot] = Schema.derived
  given Schema[CompilationCapture]  = Schema.derived

  given Schema[HostToolRole]         = Schema.derived
  given Schema[HostToolEntry]        = Schema.derived
  given Schema[HostCompilerManifest] = Schema.derived
  given Schema[MaterializedTool]     = Schema.derived
  given Schema[HostCompilerSession]  = Schema.derived
  given Schema[ArtifactPathError]    = Schema.derived
  given Schema[HostCompilerError]    = Schema.derived

  given Schema[ExecutionKind]         = Schema.derived
  given Schema[LockMode]              = Schema.derived
  given Schema[LockDefaults]          = Schema.derived
  given Schema[LockPhase]             = Schema.derived
  given Schema[LockStage]             = Schema.derived
  given Schema[LockRevisionState]     = Schema.derived
  given Schema[LockChange]            = Schema.derived
  given Schema[LockSubject]           = Schema.derived
  given Schema[LockDiffEntry]         = Schema.derived
  given Schema[LockDiff]              = Schema.derived
  given Schema[DeclarationDrift]      = Schema.derived
  given Schema[NecessaryScope]        = Schema.derived
  given Schema[ProposalSummary]       = Schema.derived
  given Schema[LockRefusal]           = Schema.derived
  given Schema[LockDecision]          = Schema.derived
  given Schema[Interaction]           = Schema.derived
  given Schema[OfflineSelection]      = Schema.derived
  given Schema[ResolveVerb]           = Schema.derived
  given Schema[CommandIdentity]       = Schema.derived
  given Schema[InvocationContext]     = Schema.derived
  given Schema[ResolutionIntent]      = Schema.derived
  given Schema[PluginEventError]      = Schema.derived
  given Schema[GenerationPluginEvent] = Schema.derived
  given Schema[InvocationPluginEvent] = Schema.derived
  given Schema[LockOffer]             = Schema.derived
  given Schema[LockReply]             = Schema.derived
  given Schema[LockCommitted]         = Schema.derived
  given Schema[LockBlocked]           = Schema.derived
  given Schema[LockTransactionError]  = Schema.derived
  given Schema[LockEvent]             = Schema.derived

  given Schema[PluginOrigin]       = Schema.derived
  given Schema[ActivationManifest] = Schema.derived
  given Schema[LinkStrategy]       = Schema.derived
  given Schema[PluginRef]          = Schema.derived
  given Schema[PluginCatalog]      = Schema.derived
  given Schema[Resolver]           = Schema.derived
  given Schema[BootstrapSource]    = Schema.derived
  given Schema[BootstrapSteps]     = Schema.derived
  given Schema[BootstrapPlanError] = Schema.derived
  given Schema[BootstrapError]     = Schema.derived
  given Schema[PluginResolvers]    = Schema.derived
end WireSchemaContract
