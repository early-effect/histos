package histos.resolve

import histos.jdk.Sha256
import java.nio.charset.StandardCharsets
import zio.Chunk

/** A selected request and its checksum-verified classpath, never the whole lock.
  */
final case class ToolInput(id: ToolId, artifacts: List[ResolvedArtifact]):
  def fingerprint: Sha256 =
    ToolInput.fingerprint(
      id,
      artifacts.map(artifact =>
        ToolInput.Row(
          artifact.organization,
          artifact.name,
          artifact.revision,
          artifact.classifier,
          artifact.sha256,
        )
      ),
    )

object ToolInput:
  /** The coordinates [[fingerprint]] hashes. The jar bytes are not an input.
    */
  private[histos] final case class Row(
      organization: String,
      name: String,
      revision: String,
      classifier: Option[JarClassifier],
      sha256: Sha256,
  )

  /** Tool id, then each artifact's coordinates and lock sha. */
  private[histos] def fingerprint(id: ToolId, artifacts: List[Row]): Sha256 =
    val lines = id.token :: artifacts.map(artifact =>
      s"${artifact.organization} ${artifact.name} ${artifact.revision} ${artifact.classifier.fold("")(_.value)} ${artifact.sha256.value}"
    )
    Sha256.of(
      Chunk.fromArray(lines.mkString("\n").getBytes(StandardCharsets.UTF_8))
    )
end ToolInput
