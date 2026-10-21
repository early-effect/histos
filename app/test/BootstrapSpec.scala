package histos.cli

import histos.jdk.{ArchiveSource, JdkLock, JdkMajor, JdkVersion, Sha256}

import com.sun.net.httpserver.{HttpExchange, HttpServer}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object BootstrapSpec extends ZIOSpecDefault:
  private def sha256(path: Path): String =
    val digest = MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(path))
    digest.map(b => f"${b & 0xff}%02x").mkString

  private def script: Path =
    val root = sys.env.get("HISTOS_REPO").filter(_.nonEmpty).map(Path.of(_))
    val candidates = root.toList.map(_.resolve("histos")) ++ List(Path.of("histos"), Path.of("../histos"))
    candidates.find(Files.isRegularFile(_)) match
      case Some(path) => path
      case None       => Path.of("histos")

  private def layout(jarBytes: Array[Byte], sha: String): Path =
    val root = Files.createTempDirectory("histos-boot")
    Files.copy(script, root.resolve("histos"), StandardCopyOption.REPLACE_EXISTING)
    Files.writeString(root.resolve(".histos-version"), "0.0.1-SNAPSHOT\n")
    Files.createDirectories(root.resolve(".histos"))
    Files.writeString(root.resolve(".histos/assembly.sha256"), sha + "\n")
    val jar = root.resolve("app.jar")
    Files.write(jar, jarBytes)
    val bin = root.resolve("bin")
    Files.createDirectories(bin)
    Files.writeString(
      bin.resolve("java"),
      """#!/bin/sh
        |printf '%s\n' "$@" > "$HISTOS_JAVA_LOG"
        |""".stripMargin,
    )
    bin.resolve("java").toFile.setExecutable(true)
    linkTools(bin)
    root
  end layout

  private def linkTools(bin: Path): Unit =
    List(
      "/usr/bin/dirname",
      "/usr/bin/tr",
      "/usr/bin/awk",
      "/usr/bin/shasum",
      "/usr/bin/sha256sum",
      "/usr/bin/curl",
      "/usr/bin/tar",
      "/bin/mkdir",
      "/usr/bin/find",
      "/bin/chmod",
      "/bin/rm",
      "/bin/mv",
    ).foreach: source =>
      val path = Path.of(source)
      if Files.isRegularFile(path) then
        val link = bin.resolve(path.getFileName)
        if !Files.exists(link) then Files.createSymbolicLink(link, path)

  private def run(root: Path, shaMatches: Boolean, withJava: Boolean, args: List[String]): (Int, String) =
    val jar = root.resolve("app.jar")
    val sha = if shaMatches then sha256(jar) else "0" * 64
    Files.writeString(root.resolve(".histos/assembly.sha256"), sha + "\n")
    val log = root.resolve("java.log")
    val bin = root.resolve("bin")
    val command = List("/bin/sh", root.resolve("histos").toString) ++ args
    val process = new ProcessBuilder(command.asJava)
    val env = process.environment()
    env.put("HISTOS_ASSEMBLY", jar.toString)
    env.put("HISTOS_JAVA_LOG", log.toString)
    env.put("XDG_CACHE_HOME", root.resolve("xdg").toString)
    env.put("HOME", root.toString)
    val path =
      if withJava then bin.toString
      else
        val tools = root.resolve("tools")
        Files.createDirectories(tools)
        linkTools(tools)
        tools.toString
    env.put("PATH", path)
    process.redirectErrorStream(true)
    val started = process.start()
    val output = new String(started.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    val code = started.waitFor()
    (code, output + (if Files.isRegularFile(log) then Files.readString(log) else ""))

  private def download(root: Path, snapshots: String, args: List[String]): (Int, String) =
    val log = root.resolve("java.log")
    val bin = root.resolve("bin")
    val command = List("/bin/sh", root.resolve("histos").toString) ++ args
    val process = new ProcessBuilder(command.asJava)
    val env = process.environment()
    env.remove("HISTOS_ASSEMBLY")
    env.put("HISTOS_SNAPSHOTS", snapshots)
    env.put("HISTOS_JAVA_LOG", log.toString)
    env.put("XDG_CACHE_HOME", root.resolve("xdg").toString)
    env.put("HOME", root.toString)
    env.put("PATH", bin.toString)
    process.redirectErrorStream(true)
    val started = process.start()
    val output = new String(started.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val code = started.waitFor()
    (code, output + (if Files.isRegularFile(log) then Files.readString(log) else ""))

  private def snapshot(jar: Array[Byte], checksum: String): HttpServer =
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext("/", (exchange: HttpExchange) => serve(exchange, jar, checksum))
    server.start()
    server

  private def serve(exchange: HttpExchange, jar: Array[Byte], checksum: String): Unit =
    val path = exchange.getRequestURI.getPath
    val body =
      if path.endsWith("maven-metadata.xml") then Some(metadata.getBytes(StandardCharsets.UTF_8))
      else if path.endsWith(".jar.sha256") then Some(s"$checksum\n".getBytes(StandardCharsets.UTF_8))
      else if path.endsWith(".jar") then Some(jar)
      else None
    body match
      case None =>
        exchange.sendResponseHeaders(404, -1)
        exchange.close()
      case Some(bytes) =>
        exchange.sendResponseHeaders(200, bytes.length)
        val out = exchange.getResponseBody
        out.write(bytes)
        out.close()
        exchange.close()

  private val metadata =
    """<metadata>
      |  <snapshot>
      |    <timestamp>20261021.140102</timestamp>
      |    <buildNumber>1</buildNumber>
      |  </snapshot>
      |</metadata>
      |""".stripMargin

  private val checks = suite("bootstrap script")(
    test("verifies the fixture and execs java -jar"):
      val root = layout("fixture".getBytes(java.nio.charset.StandardCharsets.UTF_8), "pending")
      val (code, output) = run(root, shaMatches = true, withJava = true, args = List("graph"))
      assertTrue(
        code == 0 && output.contains("--enable-native-access=ALL-UNNAMED") && output.contains("-jar") && output.contains("app.jar") && output.contains("graph"),
      ),
    test("a checksum mismatch does not exec"):
      val root = layout("fixture".getBytes(java.nio.charset.StandardCharsets.UTF_8), "pending")
      val (code, output) = run(root, shaMatches = false, withJava = true, args = Nil)
      assertTrue(code == 1 && output.contains("checksum mismatch") && !output.contains("-jar")),
    test("missing java without a lock is an error"):
      val root = layout("fixture".getBytes(java.nio.charset.StandardCharsets.UTF_8), "pending")
      val (code, output) = run(root, shaMatches = true, withJava = false, args = Nil)
      assertTrue(code == 1 && output.contains("java is not on PATH") && output.contains("jdk.lock") && !output.contains("-jar")),
    test("missing java is taken from a file url in the lock"):
      val root = layout("fixture".getBytes(java.nio.charset.StandardCharsets.UTF_8), "pending")
      val packed = lockedJdk(root)
      val (code, output) = run(root, shaMatches = true, withJava = false, args = List("graph"))
      assertTrue(
        packed == 0 && code == 0 && output.contains("--enable-native-access=ALL-UNNAMED") && output.contains("-jar") && output.contains("app.jar") && output.contains("graph"),
      ),
    test("a snapshot launcher is downloaded and exec'd"):
      val bytes = "fixture".getBytes(StandardCharsets.UTF_8)
      val root = layout(bytes, "pending")
      val server = snapshot(bytes, sha256(root.resolve("app.jar")))
      try
        val (code, output) = download(root, s"http://127.0.0.1:${server.getAddress.getPort}", List("graph"))
        assertTrue(
          code == 0 && output.contains("0.0.1-20261021.140102-1/histos.jar") && output.contains("graph") && output.contains("-jar"),
        )
      finally server.stop(0)
    ,
    test("a snapshot checksum mismatch does not exec"):
      val bytes = "fixture".getBytes(StandardCharsets.UTF_8)
      val root = layout(bytes, "pending")
      val server = snapshot(bytes, "0" * 64)
      try
        val (code, output) = download(root, s"http://127.0.0.1:${server.getAddress.getPort}", List("graph"))
        assertTrue(code == 1 && output.contains("checksum mismatch") && !output.contains("-jar"))
      finally server.stop(0)
    ,
  )

  private def lockedJdk(root: Path): Int =
    val jdkRoot = root.resolve("fake-jdk")
    Files.createDirectories(jdkRoot.resolve("bin"))
    Files.writeString(
      jdkRoot.resolve("bin").resolve("java"),
      """#!/bin/sh
        |printf '%s\n' "$@" > "$HISTOS_JAVA_LOG"
        |""".stripMargin,
    )
    jdkRoot.resolve("bin").resolve("java").toFile.setExecutable(true)
    val archive = root.resolve("jdk.tgz")
    val tar = new ProcessBuilder("tar", "-czf", archive.toString, "-C", jdkRoot.toString, ".").inheritIO().start()
    val code = tar.waitFor()
    if code == 0 then
      val major   = JdkMajor.trusted(21)
      val version = JdkVersion.trusted("21.0.2", major)
      val sha     = Sha256.of(Chunk.fromArray(Files.readAllBytes(archive)))
      val lock    = JdkLock(major, version, ArchiveSource.Local(archive), sha)
      Files.writeString(root.resolve(".histos").resolve("jdk.lock"), JdkLock.render(lock))
    code

  def spec = checks
