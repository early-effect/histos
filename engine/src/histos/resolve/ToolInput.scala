package histos.resolve

import histos.jdk.Sha256
import java.nio.charset.StandardCharsets
import zio.Chunk

/** A selected request and its checksum-verified classpath, never the whole lock.
  */
final case class ToolInput(id: ToolId, artifacts: List[ResolvedArtifact]):
  def fingerprint: Sha256 =
    val lines = id.token :: artifacts.map(artifact =>
      s"${artifact.organization} ${artifact.name} ${artifact.revision} ${artifact.classifier.fold("")(_.value)} ${artifact.sha256.value}"
    )
    Sha256.of(
      Chunk.fromArray(lines.mkString("\n").getBytes(StandardCharsets.UTF_8))
    )
