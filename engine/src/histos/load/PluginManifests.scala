package histos.load

import histos.plugin.{SealedArtifactManifest, SourceId, VerifiedPluginManifest}

/** Provenance of one activation. Unverified is absence, not an empty artifact list. */
enum ProvenanceResult:
  case Unverified(source: SourceId)
  case Verified(manifest: VerifiedPluginManifest)

object ProvenanceResult:
  /** Verified artifacts, including a verified empty closure. Unverified is `None`. */
  def artifacts(result: ProvenanceResult): Option[List[SealedArtifactManifest]] =
    result match
      case ProvenanceResult.Unverified(_)      => None
      case ProvenanceResult.Verified(manifest) => Some(manifest.artifacts)
