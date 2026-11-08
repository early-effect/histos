package histos.publish

import histos.jdk.{Sha256, Sha256Digest}
import histos.program.ClientBinary

import zio.Chunk

/** Flat names on the dist base the shim fetches. A channel file is one resolved version. */
object DistLayout:
  val Shim: String    = "shim"
  val ShimSha: String = "shim.sha256"

  /** The workflow uploads each linked client as `client-<platform>`. */
  val ClientArtifact: String = "client-"

  /** Where the snapshots workflow downloads those artifacts, and where `publish snapshots` reads them. */
  val BuiltClients: String = "dist/clients"

  def channel(name: String): String = s"channel-$name"

  def manifest(version: String): String = s"$version-manifest.json"

  def jar(version: String): String = s"$version-histos.jar"

  def client(version: String, platform: DistPlatform): String = s"$version-client-${platform.token}"

  def sources(version: String): String = s"$version-sources.tar"

  /** `dist/clients/client-<platform>/histos-client`, relative to the repository. */
  def builtClient(platform: DistPlatform): String =
    s"$BuiltClients/$ClientArtifact${platform.token}/${ClientBinary.fileName}"

  def manifestJson(
      version: String,
      jarSha256: String,
      clients: List[(DistPlatform, String)],
      sourcesSha256: Option[String],
  ): String =
    val rows = clients.sortBy(_._1.token).map { (platform, sha) =>
      s"""    "${platform.token}": "$sha""""
    }
    val body    = rows.mkString(",\n")
    val sources = sourcesSha256 match
      case Some(sha) => s""",\n  "sourcesSha256": "$sha""""
      case None      => ""
    s"""{
       |  "version": "$version",
       |  "jar": "${jar(version)}",
       |  "jarSha256": "$jarSha256",
       |  "clients": {
       |$body
       |  }$sources
       |}
       |""".stripMargin

  def sha256(bytes: Array[Byte]): String = Sha256Digest.of(Chunk.fromArray(bytes)).value
end DistLayout
