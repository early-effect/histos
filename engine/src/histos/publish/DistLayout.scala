package histos.publish

import histos.jdk.Sha256

import zio.Chunk

/** Flat names on the dist base the shim fetches. A channel file is one resolved version. */
object DistLayout:
  val Shim: String    = "shim"
  val ShimSha: String = "shim.sha256"

  def channel(name: String): String = s"channel-$name"

  def manifest(version: String): String = s"$version-manifest.json"

  def jar(version: String): String = s"$version-histos.jar"

  def client(version: String, platform: String): String = s"$version-client-$platform"

  def sources(version: String): String = s"$version-sources.tar"

  def manifestJson(
      version: String,
      jarSha256: String,
      clients: List[(String, String)],
      sourcesSha256: Option[String],
  ): String =
    val rows = clients.sortBy(_._1).map { (platform, sha) =>
      s"""    "$platform": "$sha""""
    }
    val body = rows.mkString(",\n")
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

  def sha256(bytes: Array[Byte]): String = Sha256.of(Chunk.fromArray(bytes)).value
end DistLayout
