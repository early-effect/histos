package histos.plugin

import _root_.scala.annotation.publicInBinary

import java.nio.charset.StandardCharsets

/** A plugin source identity. Dot-separated segments, the same grammar as a plugin id. */
opaque type SourceId = String

object SourceId:
  given CanEqual[SourceId, SourceId] = CanEqual.derived

  inline def apply(inline raw: String): SourceId = ${ PluginMacros.sourceId('raw) }

  def from(raw: String): Either[SourceIdError, SourceId] =
    if Tokens.pluginId(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(SourceIdError.Empty)
    else Left(SourceIdError.Bad(raw))

  extension (id: SourceId) def value: String = id

  @publicInBinary private[histos] def trusted(raw: String): SourceId = raw
end SourceId

enum SourceIdError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a source id is not empty"
    case Bad(raw) => s"'$raw' is not a source id"
end SourceIdError

/** The capability interface a service is registered under. */
opaque type ServiceKey = String

object ServiceKey:
  given CanEqual[ServiceKey, ServiceKey] = CanEqual.derived

  inline def apply(inline raw: String): ServiceKey = ${ PluginMacros.serviceKey('raw) }

  def from(raw: String): Either[ServiceKeyError, ServiceKey] =
    if Tokens.serviceKey(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(ServiceKeyError.Empty)
    else Left(ServiceKeyError.Bad(raw))

  extension (key: ServiceKey) def value: String = key

  @publicInBinary private[histos] def trusted(raw: String): ServiceKey = raw
end ServiceKey

enum ServiceKeyError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a service key is not empty"
    case Bad(raw) => s"'$raw' is not a service key"
end ServiceKeyError

/** SHA-256 of the committed lock bytes. Absence is [[histos.lock.LockRevisionState.Absent]], not this value. */
opaque type LockRevision = String

object LockRevision:
  given CanEqual[LockRevision, LockRevision] = CanEqual.derived

  inline def apply(inline raw: String): LockRevision = ${ PluginMacros.lockRevision('raw) }

  def from(raw: String): Either[LockRevisionError, LockRevision] =
    Tokens.sha256(raw) match
      case Some(hex) => Right(trusted(hex))
      case None      => Left(LockRevisionError.Bad(raw))

  extension (revision: LockRevision) def value: String = revision

  @publicInBinary private[histos] def trusted(raw: String): LockRevision = raw
end LockRevision

enum LockRevisionError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a lock revision"
end LockRevisionError

/** SHA-256 identity of one lock proposal. */
opaque type ProposalId = String

object ProposalId:
  given CanEqual[ProposalId, ProposalId] = CanEqual.derived

  inline def apply(inline raw: String): ProposalId = ${ PluginMacros.proposalId('raw) }

  def from(raw: String): Either[ProposalIdError, ProposalId] =
    Tokens.sha256(raw) match
      case Some(hex) => Right(trusted(hex))
      case None      => Left(ProposalIdError.Bad(raw))

  extension (id: ProposalId) def value: String = id

  @publicInBinary private[histos] def trusted(raw: String): ProposalId = raw
end ProposalId

enum ProposalIdError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a proposal id"
end ProposalIdError

/** One node in a rebuild recipe. A letter, then letters, digits, or hyphens. */
opaque type RecipeNodeId = String

object RecipeNodeId:
  given CanEqual[RecipeNodeId, RecipeNodeId] = CanEqual.derived

  inline def apply(inline raw: String): RecipeNodeId = ${ PluginMacros.recipeNodeId('raw) }

  def from(raw: String): Either[RecipeNodeIdError, RecipeNodeId] =
    Tokens.parseSimple(raw, RecipeNodeIdError.Empty, RecipeNodeIdError.Bad(_)).map(trusted)

  extension (id: RecipeNodeId) def value: String = id

  @publicInBinary private[histos] def trusted(raw: String): RecipeNodeId = raw
end RecipeNodeId

enum RecipeNodeIdError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a recipe node id is not empty"
    case Bad(raw) => s"'$raw' is not a recipe node id"
end RecipeNodeIdError

/** SHA-256 identity of one candidate generation. Not a command and not a client. */
opaque type GenerationId = String

object GenerationId:
  given CanEqual[GenerationId, GenerationId] = CanEqual.derived

  inline def apply(inline raw: String): GenerationId = ${ PluginMacros.generationId('raw) }

  def from(raw: String): Either[GenerationIdError, GenerationId] =
    Tokens.sha256(raw) match
      case Some(hex) => Right(trusted(hex))
      case None      => Left(GenerationIdError.Bad(raw))

  extension (id: GenerationId) def value: String = id

  @publicInBinary private[histos] def trusted(raw: String): GenerationId = raw
end GenerationId

enum GenerationIdError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a generation id"
end GenerationIdError

/** SHA-256 identity of one invocation. */
opaque type InvocationId = String

object InvocationId:
  given CanEqual[InvocationId, InvocationId] = CanEqual.derived

  inline def apply(inline raw: String): InvocationId = ${ PluginMacros.invocationId('raw) }

  def from(raw: String): Either[InvocationIdError, InvocationId] =
    Tokens.sha256(raw) match
      case Some(hex) => Right(trusted(hex))
      case None      => Left(InvocationIdError.Bad(raw))

  /** Stable id for a command name this process already has. Not a caller-supplied invocation. */
  def derive(raw: String): InvocationId =
    trusted(Digests.sha256(raw.getBytes(StandardCharsets.UTF_8)))

  extension (id: InvocationId) def value: String = id

  @publicInBinary private[histos] def trusted(raw: String): InvocationId = raw
end InvocationId

enum InvocationIdError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not an invocation id"
end InvocationIdError

/** The caller that owns one invocation. Not the server environment. */
opaque type ClientId = String

object ClientId:
  given CanEqual[ClientId, ClientId] = CanEqual.derived

  inline def apply(inline raw: String): ClientId = ${ PluginMacros.clientId('raw) }

  def from(raw: String): Either[ClientIdError, ClientId] =
    Tokens.parseSimple(raw, ClientIdError.Empty, ClientIdError.Bad(_)).map(trusted)

  extension (id: ClientId) def value: String = id

  @publicInBinary private[histos] def trusted(raw: String): ClientId = raw
end ClientId

enum ClientIdError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a client id is not empty"
    case Bad(raw) => s"'$raw' is not a client id"
end ClientIdError

/** A command name that is not one of the closed command cases. */
opaque type CommandToken = String

object CommandToken:
  given CanEqual[CommandToken, CommandToken] = CanEqual.derived

  inline def apply(inline raw: String): CommandToken = ${ PluginMacros.commandToken('raw) }

  def from(raw: String): Either[CommandTokenError, CommandToken] =
    Tokens.parseSimple(raw, CommandTokenError.Empty, CommandTokenError.Bad(_)).map(trusted)

  extension (token: CommandToken) def value: String = token

  @publicInBinary private[histos] def trusted(raw: String): CommandToken = raw
end CommandToken

enum CommandTokenError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a command token is not empty"
    case Bad(raw) => s"'$raw' is not a command token"
end CommandTokenError

/** A recipe feature the host does not represent. */
opaque type RecipeFeature = String

object RecipeFeature:
  given CanEqual[RecipeFeature, RecipeFeature] = CanEqual.derived

  inline def apply(inline raw: String): RecipeFeature = ${ PluginMacros.recipeFeature('raw) }

  def from(raw: String): Either[RecipeFeatureError, RecipeFeature] =
    Tokens.parseSimple(raw, RecipeFeatureError.Empty, RecipeFeatureError.Bad(_)).map(trusted)

  extension (feature: RecipeFeature) def value: String = feature

  @publicInBinary private[histos] def trusted(raw: String): RecipeFeature = raw
end RecipeFeature

enum RecipeFeatureError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a recipe feature is not empty"
    case Bad(raw) => s"'$raw' is not a recipe feature"
end RecipeFeatureError

/** A recipe input that was not captured. */
opaque type RecipeInput = String

object RecipeInput:
  given CanEqual[RecipeInput, RecipeInput] = CanEqual.derived

  inline def apply(inline raw: String): RecipeInput = ${ PluginMacros.recipeInput('raw) }

  def from(raw: String): Either[RecipeInputError, RecipeInput] =
    Tokens.parseSimple(raw, RecipeInputError.Empty, RecipeInputError.Bad(_)).map(trusted)

  extension (input: RecipeInput) def value: String = input

  @publicInBinary private[histos] def trusted(raw: String): RecipeInput = raw
end RecipeInput

enum RecipeInputError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a recipe input is not empty"
    case Bad(raw) => s"'$raw' is not a recipe input"
end RecipeInputError

/** One relative path inside a sealed archive. Not a repository path: class names may contain `$`. */
opaque type ArtifactPath = String

object ArtifactPath:
  given CanEqual[ArtifactPath, ArtifactPath] = CanEqual.derived

  inline def apply(inline raw: String): ArtifactPath = ${ PluginMacros.artifactPath('raw) }

  def from(raw: String): Either[ArtifactPathError, ArtifactPath] =
    if Tokens.artifactPath(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(ArtifactPathError.Empty)
    else Left(ArtifactPathError.Escapes(raw))

  extension (path: ArtifactPath) def value: String = path

  @publicInBinary private[histos] def trusted(raw: String): ArtifactPath = raw
end ArtifactPath

enum ArtifactPathError:
  case Empty
  case Escapes(raw: String)

  def message: String = this match
    case Empty        => "an artifact path is not empty"
    case Escapes(raw) => s"'$raw' is not an artifact entry path"
end ArtifactPathError
