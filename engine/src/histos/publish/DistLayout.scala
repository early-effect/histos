package histos.publish

import histos.jdk.{Sha256, Sha256Digest}
import histos.program.ClientBinary

import zio.Chunk

/** Flat names on the dist base the shim fetches. A channel file is one resolved version. */
object DistLayout:
  val Shim: String    = "shim"
  val ShimSha: String = "shim.sha256"

  /** `shim-<sha256>`. The bytes at this name do not move. */
  def pinnedShim(sha: String): String = s"shim-$sha"

  /** The workflow uploads each linked client as `client-<platform>`. */
  val ClientArtifact: String = "client-"

  /** Where the snapshots workflow downloads those artifacts, and where `publish snapshots` reads them. */
  val BuiltClients: String = "dist/clients"

  def channel(name: String): String = s"channel-$name"

  def manifest(version: String): String = s"$version-manifest.json"

  def jar(version: String): String = s"$version-histos.jar"

  def client(version: String, platform: DistPlatform): String = s"$version-client-${platform.token}"

  /** `dist/clients/client-<platform>/histos-client`, relative to the repository. */
  def builtClient(platform: DistPlatform): String =
    s"$BuiltClients/$ClientArtifact${platform.token}/${ClientBinary.fileName}"

  def manifestJson(
      version: String,
      jarSha256: String,
      clients: List[(DistPlatform, String)],
  ): String =
    val rows = clients.sortBy(_._1.token).map { (platform, sha) =>
      s"""    "${platform.token}": "$sha""""
    }
    s"""{
       |  "version": "$version",
       |  "jar": "${jar(version)}",
       |  "jarSha256": "$jarSha256",
       |  "clients": {
       |${rows.mkString(",\n")}
       |  }
       |}
       |""".stripMargin

  def sha256(bytes: Array[Byte]): String = Sha256Digest.of(Chunk.fromArray(bytes)).value
end DistLayout
