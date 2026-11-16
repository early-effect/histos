package histos.plugin

/** Where the classes that defined one activation came from. */
enum PluginOrigin:
  case Source(id: SourceId, digest: ContentHash)
  case Published(coordinate: PluginCoordinate, digest: ContentHash)

/** One activation's identity. The artifact list is the sealed closure, never a stand-in for a missing check. */
final case class ActivationManifest(
    source: SourceId,
    activation: ServiceKey,
    origin: PluginOrigin,
    defining: ArtifactIdentity,
    closure: List[ArtifactIdentity],
    host: ApiDigest,
    derived: ContentHash,
)

/** A verified manifest names its artifacts. Unverified is not this type and is not an empty closure. */
final case class VerifiedPluginManifest(
    activation: ActivationManifest,
    artifacts: List[SealedArtifactManifest],
):
  def identities: List[ArtifactIdentity] =
    artifacts.map(_.identity)
