package histos.pack

import histos.jdk.{Sha256, Sha256Digest}
import histos.PluginPublication
import histos.plugin.{ArtifactFormat, SealedArtifactManifest}

import java.nio.charset.StandardCharsets

import zio.Chunk

/** Identity of a package. [[of]] is the semantic product digest and stays the policy outside plugin and helper jars.
  * [[sealedOf]] and [[manifestOf]] hash the sealed entry manifest, so TASTy bytes and injected metadata participate.
  */
object PackageKey:
  def of(products: List[Sha256]): Sha256 =
    hashed(s"""products
         |${products.map(_.value).mkString("\n")}
         |""")

  /** Plugin and helper jars. The mode is part of the key. Binary-only still hashes TASTy and metadata. */
  def sealedOf(format: ArtifactFormat, mode: PluginPublication, manifest: SealedArtifactManifest): Sha256 =
    hashed(s"""sealed
         |${formatName(format)}
         |${modeName(mode)}
         |${manifest.version}
         |${manifest.identity.digest.value}
         |""")

  /** A stamped jar that is not a plugin publication. The stamp entry is in the manifest. */
  def manifestOf(format: ArtifactFormat, manifest: SealedArtifactManifest): Sha256 =
    hashed(s"""manifest
         |${formatName(format)}
         |${manifest.version}
         |${manifest.identity.digest.value}
         |""")

  /** Assembly metadata. Product digests stay, and stamp bytes are named entries. An empty stamp is not this key. */
  def withMetadata(products: List[Sha256], metadata: List[(String, Chunk[Byte])]): Sha256 =
    val rows = metadata
      .sortBy(_._1)
      .map: (name, bytes) =>
        s"$name ${bytes.length} ${Sha256Digest.of(bytes).value}"
    hashed(s"""products
         |${products.map(_.value).mkString("\n")}
         |metadata
         |${rows.mkString("\n")}
         |""")

  private def hashed(text: String): Sha256 =
    Sha256Digest.of(Chunk.fromArray(text.stripMargin.getBytes(StandardCharsets.UTF_8)))

  private def formatName(format: ArtifactFormat): String =
    format match
      case ArtifactFormat.Jar       => "jar"
      case ArtifactFormat.Directory => "directory"

  private def modeName(mode: PluginPublication): String =
    mode match
      case PluginPublication.Rebuildable => "rebuildable"
      case PluginPublication.BinaryOnly  => "binary-only"
end PackageKey
