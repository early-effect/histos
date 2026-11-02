package histos.publish

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

/** The files a published version puts on the dist base: a manifest, the jar, prebuilt clients, and client sources. */
object Dist:
  val clientPlatforms: List[String] = List("linux-x86_64", "macos-aarch64")

  /** The build the shim compiles with `java -jar <histos.jar> client`. It names api, wire, and client only. */
  val buildFile: String =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val scala3  = scala"3.10.0"
      |    val zio     = dep"dev.zio::zio:2.1.27"
      |    val zioJson = dep"dev.zio::zio-json:1.1.1"
      |    val api = module("api", Jvm, Native)(scala3)
      |      .clang(ClangMajor(21))
      |      .libraryDependencies(zio)
      |    val wire = module("wire", Jvm, Native)(scala3)
      |      .clang(ClangMajor(21))
      |      .dependsOn(api)
      |      .libraryDependencies(zio, zioJson)
      |    val client = module("client", Native)(scala3)
      |      .clang(ClangMajor(21))
      |      .dependsOn(wire)
      |      .libraryDependencies(zio, zioJson)
      |      .main(MainClass("histos.client.Main"))
      |    Projects(api, wire, client)
      |""".stripMargin

  def sources(repo: Path): Array[Byte] =
    val files = List("api", "wire", "client").flatMap: name =>
      walk(repo.resolve(name).resolve("src"), s"$name/src")
    Tar.pack(("histos.scala", buildFile.getBytes(StandardCharsets.UTF_8)) :: files)

  def bundle(
      version: String,
      jar: Array[Byte],
      clients: List[(String, Array[Byte])],
      sources: Array[Byte],
  ): List[(String, Array[Byte])] =
    val manifest = DistLayout.manifestJson(
      version,
      DistLayout.sha256(jar),
      clients.map((platform, bytes) => platform -> DistLayout.sha256(bytes)),
      Some(DistLayout.sha256(sources)),
    )
    List(
      DistLayout.manifest(version) -> manifest.getBytes(StandardCharsets.UTF_8),
      DistLayout.jar(version)      -> jar,
      DistLayout.sources(version)  -> sources,
    ) ++ clients.map((platform, bytes) => DistLayout.client(version, platform) -> bytes)

  private def walk(root: Path, prefix: String): List[(String, Array[Byte])] =
    if !Files.isDirectory(root) then Nil
    else
      val stream = Files.walk(root)
      try
        stream.iterator.asScala.filter(Files.isRegularFile(_)).map { path =>
          val relative = root.relativize(path).iterator.asScala.map(_.toString).mkString("/")
          s"$prefix/$relative" -> Files.readAllBytes(path)
        }.toList
      finally stream.close()

  private object Tar:
    def pack(files: List[(String, Array[Byte])]): Array[Byte] =
      val out = new java.io.ByteArrayOutputStream
      files.foreach: (path, bytes) =>
        val (prefix, name) =
          if path.length <= 100 then ("", path)
          else
            val cut = path.lastIndexOf('/')
            (path.substring(0, cut), path.substring(cut + 1))
        out.write(header(prefix, name, bytes.length))
        out.write(bytes)
        val remainder = bytes.length % 512
        if remainder != 0 then out.write(new Array[Byte](512 - remainder))
      out.write(new Array[Byte](1024))
      out.toByteArray

    private def header(prefix: String, name: String, size: Int): Array[Byte] =
      val block = new Array[Byte](512)
      put(block, 0, name, 100)
      put(block, 100, "0000644\u0000", 8)
      put(block, 108, "0000000\u0000", 8)
      put(block, 116, "0000000\u0000", 8)
      put(block, 124, f"$size%011o\u0000", 12)
      put(block, 136, "00000000000\u0000", 12)
      java.util.Arrays.fill(block, 148, 156, ' '.toByte)
      block(156) = '0'.toByte
      put(block, 257, "ustar\u0000", 6)
      put(block, 263, "00", 2)
      if prefix.nonEmpty then put(block, 345, prefix, 155)
      val sum = block.foldLeft(0)((acc, byte) => acc + (byte & 0xff))
      put(block, 148, f"$sum%06o\u0000 ", 8)
      block

    private def put(block: Array[Byte], at: Int, text: String, width: Int): Unit =
      val bytes = text.getBytes(StandardCharsets.US_ASCII)
      val count = math.min(bytes.length, width)
      System.arraycopy(bytes, 0, block, at, count)
end Dist
