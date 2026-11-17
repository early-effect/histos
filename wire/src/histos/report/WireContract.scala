package histos.report

import histos.doctor.{CheckId, Edit, Fix, GitHubPut, HistosCommand, Level, Location, Noted, ShellLine}
import histos.lock.{
  DeclarationDrift,
  ExecutionKind,
  InvocationEnvelope,
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
  BootstrapError,
  BootstrapPlanError,
  BootstrapSource,
  BootstrapSteps,
  ClientId,
  CommandIdentity,
  CommandToken,
  CompilerConstraint,
  ContentHash,
  EnvName,
  FrozenDirectory,
  FrozenEnv,
  FrozenFile,
  BindingKind,
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
  LockRevision,
  OfflineSelection,
  PluginEventError,
  PluginCatalog,
  PluginCoordinate,
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
  RebuildRecipe,
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
  VerifiedPluginManifest,
  LinkStrategy,
}
import histos.server.Call
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

import zio.json.*

/** Codecs for the plugin contracts. Split from [[WireJson]] so one class stays under the constant-pool limit.
  */
private[histos] object WireContract extends WireSupport:
  import WireLeaves.given

  given JsonCodec[SourceId]      = parsed(SourceId.from, _.message, _.value)
  given JsonCodec[ServiceKey]    = parsed(ServiceKey.from, _.message, _.value)
  given JsonCodec[LockRevision]  = parsed(LockRevision.from, _.message, _.value)
  given JsonCodec[ProposalId]    = parsed(ProposalId.from, _.message, _.value)
  given JsonCodec[GenerationId]  = parsed(GenerationId.from, _.message, _.value)
  given JsonCodec[InvocationId]  = parsed(InvocationId.from, _.message, _.value)
  given JsonCodec[RecipeNodeId]  = parsed(RecipeNodeId.from, _.message, _.value)
  given JsonCodec[ClientId]      = parsed(ClientId.from, _.message, _.value)
  given JsonCodec[CommandToken]  = parsed(CommandToken.from, _.message, _.value)
  given JsonCodec[RecipeFeature] = parsed(RecipeFeature.from, _.message, _.value)
  given JsonCodec[RecipeInput]   = parsed(RecipeInput.from, _.message, _.value)
  given JsonCodec[ArtifactPath]  = parsed(ArtifactPath.from, _.message, _.value)
  given JsonCodec[InputKey]      = parsed(InputKey.from, _.message, _.value)
  given JsonCodec[EnvName]       = parsed(EnvName.from, _.message, _.value)
  given JsonCodec[CheckId]       = parsed(CheckId.from, _.message, _.value)
  given JsonCodec[ShellLine]     = parsed(ShellLine.from, _.message, _.value)
  given JsonCodec[HttpUrl]       = parsed(HttpUrl.from, _.message, _.value)
  given JsonCodec[Resolution]    =
    choose(
      "resolution",
      List("solve" -> Resolution.Solve, "lock" -> Resolution.Lock),
      {
        case Resolution.Solve => "solve"
        case Resolution.Lock  => "lock"
      },
    )
  given JsonCodec[PluginPublication] =
    choose(
      "plugin publication",
      List("rebuildable" -> PluginPublication.Rebuildable, "binary-only" -> PluginPublication.BinaryOnly),
      {
        case PluginPublication.Rebuildable => "rebuildable"
        case PluginPublication.BinaryOnly  => "binary-only"
      },
    )

  given JsonCodec[Level]         = JsonCodec.derived
  given JsonCodec[Edit]          = JsonCodec.derived
  given JsonCodec[GitHubPut]     = JsonCodec.derived
  given JsonCodec[HistosCommand] = JsonCodec.derived
  given JsonCodec[Fix]           = JsonCodec.derived
  given JsonCodec[Location]      = JsonCodec.derived
  given JsonCodec[Noted]         = JsonCodec.derived

  given JsonCodec[ArtifactFormat]        = JsonCodec.derived
  given JsonCodec[ArtifactIdentity]      = JsonCodec.derived
  given JsonCodec[ArtifactEntry]         = JsonCodec.derived
  given JsonCodec[ArtifactManifestError] = JsonCodec.derived

  private final case class WireManifest(version: Int, entries: List[ArtifactEntry])
  private given JsonCodec[WireManifest] = JsonCodec.derived

  given JsonCodec[SealedArtifactManifest] =
    summon[JsonCodec[WireManifest]].transformOrFail(
      wire =>
        if wire.version == SealedArtifactManifest.Current then
          SealedArtifactManifest.from(wire.entries).left.map(_.message)
        else Left(s"artifact manifest version ${wire.version} is not supported"),
      manifest => WireManifest(manifest.version, manifest.entries),
    )

  given JsonCodec[BindingKind]        = JsonCodec.derived
  given JsonCodec[StageAssurance]     = JsonCodec.derived
  given JsonCodec[BindingClass]       = JsonCodec.derived
  given JsonCodec[StageCwd]           = JsonCodec.derived
  given JsonCodec[StageNetwork]       = JsonCodec.derived
  given JsonCodec[StageClock]         = JsonCodec.derived
  given JsonCodec[StageRandom]        = JsonCodec.derived
  given JsonCodec[StageProcess]       = JsonCodec.derived
  given JsonCodec[StageRestrictions]  = JsonCodec.derived
  given JsonCodec[StageBinding]       = JsonCodec.derived
  given JsonCodec[FrozenFile]         = JsonCodec.derived
  given JsonCodec[FrozenDirectory]    = JsonCodec.derived
  given JsonCodec[FrozenEnv]          = JsonCodec.derived
  given JsonCodec[StageInputs]        = JsonCodec.derived
  given JsonCodec[CompilerConstraint] = JsonCodec.derived
  given JsonCodec[StageContract]      = JsonCodec.derived
  given JsonCodec[StageContractError] = JsonCodec.derived

  given JsonCodec[RecipeVersion]    = JsonCodec.derived
  given JsonCodec[RecipeRole]       = JsonCodec.derived
  given JsonCodec[SourceSet]        = JsonCodec.derived
  given JsonCodec[SourceKind]       = JsonCodec.derived
  given JsonCodec[SourceProvenance] = JsonCodec.derived
  given JsonCodec[SelectedInput]    = JsonCodec.derived
  given JsonCodec[SemanticSetting]  = JsonCodec.derived
  given JsonCodec[ClosureRole]      = JsonCodec.derived
  given JsonCodec[HostRole]         = JsonCodec.derived
  given JsonCodec[HostSubstitution] = JsonCodec.derived
  given JsonCodec[ClosureArtifact]  = JsonCodec.derived
  given JsonCodec[ProducerBinding]  = JsonCodec.derived
  given JsonCodec[RecordedInputs]   = JsonCodec.derived
  given JsonCodec[InputRecord]      = JsonCodec.derived
  given JsonCodec[RecipeNode]       = JsonCodec.derived
  given JsonCodec[RecipeError]      = JsonCodec.derived

  private final case class WireRecipe(version: RecipeVersion, root: RecipeNodeId, nodes: List[RecipeNode])
  private given JsonCodec[WireRecipe] = JsonCodec.derived

  given JsonCodec[RebuildRecipe] =
    summon[JsonCodec[WireRecipe]].transformOrFail(
      wire => RebuildRecipe.from(wire.version, wire.root, wire.nodes).left.map(_.message),
      recipe => WireRecipe(recipe.version, recipe.root, recipe.nodes),
    )

  given JsonCodec[CompilationSnapshot] = JsonCodec.derived
  given JsonCodec[CompilationCapture]  = JsonCodec.derived

  given JsonCodec[HostToolRole]         = JsonCodec.derived
  given JsonCodec[HostToolEntry]        = JsonCodec.derived
  given JsonCodec[HostCompilerManifest] = JsonCodec.derived
  given JsonCodec[MaterializedTool]     = JsonCodec.derived
  given JsonCodec[HostCompilerSession]  = JsonCodec.derived
  given JsonCodec[ArtifactPathError]    = JsonCodec.derived
  given JsonCodec[HostCompilerError]    = JsonCodec.derived

  given JsonCodec[ExecutionKind]         = JsonCodec.derived
  given JsonCodec[LockMode]              = JsonCodec.derived
  given JsonCodec[LockDefaults]          = JsonCodec.derived
  given JsonCodec[LockPhase]             = JsonCodec.derived
  given JsonCodec[LockStage]             = JsonCodec.derived
  given JsonCodec[LockRevisionState]     = JsonCodec.derived
  given JsonCodec[LockChange]            = JsonCodec.derived
  given JsonCodec[LockSubject]           = JsonCodec.derived
  given JsonCodec[LockDiffEntry]         = JsonCodec.derived
  given JsonCodec[LockDiff]              = JsonCodec.derived
  given JsonCodec[DeclarationDrift]      = JsonCodec.derived
  given JsonCodec[NecessaryScope]        = JsonCodec.derived
  given JsonCodec[ProposalSummary]       = JsonCodec.derived
  given JsonCodec[LockRefusal]           = JsonCodec.derived
  given JsonCodec[LockDecision]          = JsonCodec.derived
  given JsonCodec[Interaction]           = JsonCodec.derived
  given JsonCodec[OfflineSelection]      = JsonCodec.derived
  given JsonCodec[ResolveVerb]           = JsonCodec.derived
  given JsonCodec[CommandIdentity]       = JsonCodec.derived
  given JsonCodec[InvocationContext]     = JsonCodec.derived
  given JsonCodec[ResolutionIntent]      = JsonCodec.derived
  given JsonCodec[PluginEventError]      = JsonCodec.derived
  given JsonCodec[GenerationPluginEvent] = JsonCodec.derived
  given JsonCodec[InvocationPluginEvent] = JsonCodec.derived
  given JsonCodec[InvocationEnvelope]    = JsonCodec.derived
  given JsonCodec[LockOffer]             = JsonCodec.derived
  given JsonCodec[LockReply]             = JsonCodec.derived
  given JsonCodec[LockCommitted]         = JsonCodec.derived
  given JsonCodec[LockBlocked]           = JsonCodec.derived
  given JsonCodec[LockTransactionError]  = JsonCodec.derived
  given JsonCodec[LockEvent]             = JsonCodec.derived

  given JsonCodec[PluginOrigin]           = JsonCodec.derived
  given JsonCodec[ActivationManifest]     = JsonCodec.derived
  given JsonCodec[VerifiedPluginManifest] = JsonCodec.derived
  given JsonCodec[BootstrapSource]        = JsonCodec.derived
  given JsonCodec[BootstrapSteps]         = JsonCodec.derived
  given JsonCodec[BootstrapPlanError]     = JsonCodec.derived
  given JsonCodec[LinkStrategy]           = JsonCodec.derived
  given JsonCodec[PluginRef]              = JsonCodec.derived
  given JsonCodec[PluginCatalog]          = JsonCodec.derived
  given JsonCodec[Resolver]               = JsonCodec.derived
  given JsonCodec[BootstrapError]         = JsonCodec.derived
  given JsonCodec[PluginResolvers]        = JsonCodec.derived
end WireContract
