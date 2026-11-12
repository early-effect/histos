package histos.publish

import java.nio.charset.StandardCharsets

import zio.Chunk
import zio.test.*

/** What a snapshot uploads to `dist`: the jar, each client, the manifest, and last the channel. */
object DistLawSpec extends ZIOSpecDefault:
  def spec = suite("dist")(
    test("the channel goes last, and the manifest names the jar and each client sorted by platform"):
      val jar   = Chunk[Byte](9)
      val linux = Chunk[Byte](1, 2)
      val mac   = Chunk[Byte](3)
      val files = DistChannel.files(
        "9.9.9-SNAPSHOT",
        "9.9.9-20261112.042525-1",
        jar,
        List(DistPlatform.MacosAarch64 -> mac, DistPlatform.LinuxX86_64 -> linux),
      )
      val text = files
        .collectFirst { case ("9.9.9-20261112.042525-1-manifest.json", bytes) =>
          new String(bytes.toArray, StandardCharsets.UTF_8)
        }
        .getOrElse("")
      assertTrue(
        files.map(_._1) == List(
          "9.9.9-20261112.042525-1-histos.jar",
          "9.9.9-20261112.042525-1-client-macos-aarch64",
          "9.9.9-20261112.042525-1-client-linux-x86_64",
          "9.9.9-20261112.042525-1-manifest.json",
          "channel-9.9.9-SNAPSHOT",
        ),
        files.collectFirst { case ("channel-9.9.9-SNAPSHOT", bytes) => bytes } ==
          Some(Chunk.fromArray("9.9.9-20261112.042525-1\n".getBytes(StandardCharsets.UTF_8))),
        text.contains("\"version\": \"9.9.9-20261112.042525-1\""),
        text.contains(DistLayout.sha256(jar.toArray)),
        text.indexOf("\"linux-x86_64\"") >= 0,
        text.indexOf("\"macos-aarch64\"") > text.indexOf("\"linux-x86_64\""),
        text.contains(DistLayout.sha256(linux.toArray)),
        text.contains(DistLayout.sha256(mac.toArray)),
        !text.contains("sources"),
      )
  )
