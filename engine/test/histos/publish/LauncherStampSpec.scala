package histos.publish

import histos.jdk.Sha256
import histos.pack.ClassJar
import histos.workspace.Workspace
import histos.ToolchainIdentity

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{Chunk, ZIO}
import zio.test.*

object LauncherStampSpec extends ZIOSpecDefault:
  private val digest   = Sha256.trusted("ab" * 32)
  private val identity = ToolchainIdentity(
    "0.5.0-aaaaaaaaaaaa-SNAPSHOT",
    "a" * 40,
    false,
  )

  def spec = suite("a launcher stamp")(
    test("stores the toolchain identity and replaces an older copy") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-launcher-stamp").orDieWith(error => new java.io.IOException(error.message))
          jar = root.resolve("app.jar")
          _ <- ClassJar
            .writeBytes(
              List(
                "histos/cli/Main.class" -> bytes(1, 2, 3),
                ToolchainIdentity.entry -> Chunk.fromArray("old".getBytes(StandardCharsets.UTF_8)),
                PublishedShim.entry     -> Chunk.fromArray("old\n".getBytes(StandardCharsets.UTF_8)),
              ),
              jar,
            )
            .orDieWith(error => new java.io.IOException(error.message))
          stamped <- LauncherStamp
            .stamp(jar, identity, digest)
            .orDieWith(error => new java.io.IOException(error.message))
          out = root.resolve("stamped.jar")
          _     <- ZIO.attemptBlocking(Files.write(out, stamped.toArray)).orDie
          names <- entries(out)
          found <- text(out, ToolchainIdentity.entry)
          shim  <- text(out, PublishedShim.entry)
          main  <- entry(out, "histos/cli/Main.class")
        yield assertTrue(
          ToolchainIdentity.parse(found).contains(identity),
          shim == digest.value + "\n",
          main.sameElements(Array[Byte](1, 2, 3)),
          names.count(_ == ToolchainIdentity.entry) == 1,
          names.count(_ == PublishedShim.entry) == 1,
        )
      }
    }
  )

  private def bytes(values: Int*): Chunk[Byte] =
    Chunk.fromArray(values.map(_.toByte).toArray)

  private def entries(jar: Path): ZIO[Any, Throwable, List[String]] =
    ZIO.attemptBlocking {
      val zip = new ZipFile(jar.toFile)
      try zip.entries.asScala.map(_.getName).toList
      finally zip.close()
    }

  private def entry(jar: Path, name: String): ZIO[Any, Throwable, Array[Byte]] =
    ZIO.attemptBlocking {
      val zip = new ZipFile(jar.toFile)
      try
        Option(zip.getEntry(name)) match
          case None       => Array.emptyByteArray
          case Some(item) =>
            val input = zip.getInputStream(item)
            try input.readAllBytes()
            finally input.close()
      finally zip.close()
    }

  private def text(jar: Path, name: String): ZIO[Any, Throwable, String] =
    entry(jar, name).map(found => new String(found, StandardCharsets.UTF_8))
end LauncherStampSpec
