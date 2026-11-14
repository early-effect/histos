package histos.publish

import histos.jdk.Sha256Digest

import zio.Chunk

/** Flat names on the dist base the shim fetches. A channel file is one resolved version. */
object DistLayout:
  val Shim: String    = "shim"
  val ShimSha: String = "shim.sha256"

  /** `shim-<sha256>`. The bytes at this name do not move. */
  def pinnedShim(sha: String): String = s"shim-$sha"

  /** `dist/client-kit.zip` in the checkout. Publish compiles it and does not link it. */
  val BuiltKit: String = "dist/client-kit.zip"

  val KitFile: String = "client-kit.zip"

  def channel(name: String): String = s"channel-$name"

  def manifest(version: String): String = s"$version-manifest.json"

  def jar(version: String): String = s"$version-histos.jar"

  def kit(version: String): String = s"$version-client-kit.zip"

  final case class IvyPin(organisation: String, module: String, revision: String)

  /** `kit` is the asset name and its sha. `ivy` is only on the manifest `publish local` writes for a local installer.
    */
  def manifestJson(
      version: String,
      jarSha256: String,
      kit: Option[(String, String)],
      ivy: Option[IvyPin] = None,
  ): String =
    val kitLines = kit match
      case None              => ""
      case Some((name, sha)) =>
        s""",
           |  "clientKit": "$name",
           |  "clientKitSha256": "$sha"""".stripMargin
    val ivyLines = ivy match
      case None      => ""
      case Some(pin) =>
        s""",
           |  "ivyOrganisation": "${pin.organisation}",
           |  "ivyModule": "${pin.module}",
           |  "ivyRevision": "${pin.revision}"""".stripMargin
    s"""{
       |  "version": "$version",
       |  "jar": "${jar(version)}",
       |  "jarSha256": "$jarSha256"$kitLines$ivyLines
       |}
       |""".stripMargin

  def sha256(bytes: Array[Byte]): String = Sha256Digest.of(Chunk.fromArray(bytes)).value
end DistLayout
