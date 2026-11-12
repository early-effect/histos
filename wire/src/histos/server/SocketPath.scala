package histos.server

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{UIO, ZIO}

/** Where a checkout's server listens. A Unix socket path holds 104 bytes on macOS and 108 on Linux, so the socket does
  * not live under the checkout, which can be any depth. It lives under the Histos home, named by a 64-bit FNV-1a hash
  * of the checkout's absolute path. The JVM server and the native client compute the same name.
  */
object SocketPath:
  def of(repo: Path, home: Path): Path =
    home.resolve("run").resolve(s"${hash(repo.toAbsolutePath.normalize.toString)}.sock")

  def at(repo: Path): UIO[Path] = home.map(of(repo, _))

  /** `HISTOS_HOME`, or `.histos` under the user home. */
  val home: UIO[Path] =
    zio.System
      .env("HISTOS_HOME")
      .orElseSucceed(None)
      .flatMap:
        case Some(dir) if dir.trim.nonEmpty => ZIO.succeed(Path.of(dir.trim))
        case _                              =>
          zio.System.property("user.home").orElseSucceed(None).map(user => Path.of(user.getOrElse("."), ".histos"))

  /** Sixteen lowercase hex digits. */
  private def hash(text: String): String =
    val sum = text
      .getBytes(StandardCharsets.UTF_8)
      .foldLeft(0xcbf29ce484222325L)((acc, byte) => (acc ^ (byte & 0xff).toLong) * 0x100000001b3L)
    val hex = java.lang.Long.toHexString(sum)
    "0" * (16 - hex.length) + hex
end SocketPath
