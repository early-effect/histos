package histos.plugin

import java.nio.charset.StandardCharsets

/** Archive layout this identity was computed for. The digest covers every entry, including TASTy and metadata. */
enum ArtifactFormat:
  case Jar

/** Versioned canonical digest of one sealed entry set. Not a semantic class digest. */
final case class ArtifactIdentity(format: ArtifactFormat, digest: ContentHash)

/** One packaged entry. `digest` is the raw bytes, including a `.class` TASTy attribute. */
final case class ArtifactEntry(path: ArtifactPath, length: Long, digest: ContentHash)

/** Ordered entry set of one archive. Identity is derived. It is not an entry of the archive it describes. */
final case class SealedArtifactManifest private (
    version: Int,
    entries: List[ArtifactEntry],
    identity: ArtifactIdentity,
)

object SealedArtifactManifest:
  val Current: Int = 1

  def from(entries: List[ArtifactEntry]): Either[ArtifactManifestError, SealedArtifactManifest] =
    duplicate(entries) match
      case Some(path) => Left(ArtifactManifestError.Duplicate(path))
      case None       =>
        entries.find(_.length < 0) match
          case Some(entry) => Left(ArtifactManifestError.NegativeLength(entry.path, entry.length))
          case None        =>
            val ordered  = entries.sortBy(_.path.value)
            val identity = ArtifactIdentity(ArtifactFormat.Jar, Digests.content(canonical(ordered)))
            Right(SealedArtifactManifest(Current, ordered, identity))

  private def duplicate(entries: List[ArtifactEntry]): Option[ArtifactPath] =
    entries
      .foldLeft((Set.empty[ArtifactPath], Option.empty[ArtifactPath])):
        case ((_, found @ Some(_)), _)                          => (Set.empty, found)
        case ((seen, None), entry) if seen.contains(entry.path) => (seen, Some(entry.path))
        case ((seen, None), entry)                              => (seen + entry.path, None)
      ._2

  private def canonical(entries: List[ArtifactEntry]): Array[Byte] =
    val body = entries
      .map(entry => s"${entry.path.value}\t${entry.length}\t${entry.digest.value}")
      .mkString(s"histos-artifact-manifest/$Current\n${ArtifactFormat.Jar}\n", "\n", "\n")
    body.getBytes(StandardCharsets.UTF_8)
end SealedArtifactManifest

enum ArtifactManifestError:
  case Duplicate(path: ArtifactPath)
  case NegativeLength(path: ArtifactPath, length: Long)

  def message: String = this match
    case Duplicate(path)              => s"artifact entry ${path.value} is repeated"
    case NegativeLength(path, length) => s"artifact entry ${path.value} has length $length"
end ArtifactManifestError
