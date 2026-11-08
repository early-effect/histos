package histos.toolchain

import histos.jdk.Sha256
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import zio.{Chunk, ZIO}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A class-directory digest is the root, then each relative name and its bytes. PATH returns the first regular file. */
object RuntimeFilesLawSpec extends ZIOSpecDefault:
  def spec = suite("runtime file laws")(
    test("a directory digest follows names and bytes, and PATH skips a directory"):
      Workspace.work("histos-runtime-files").flatMap: dir =>
        val root   = dir.resolve("classes")
        val nested = root.resolve("b").resolve("A.txt")
        val leaf   = root.resolve("a.txt")
        val bin    = dir.resolve("bin")
        val tool   = bin.resolve("tool")
        val skip   = bin.resolve("skip")
        val one    = "one".getBytes(StandardCharsets.UTF_8)
        for
          _         <- Workspace.createDirectories(root.resolve("b")).catchAll(die)
          _         <- Workspace.createDirectories(skip).catchAll(die)
          _         <- Workspace.writeString(nested, "one").catchAll(die)
          _         <- Workspace.writeString(leaf, "one").catchAll(die)
          _         <- Workspace.writeString(tool, "tool").catchAll(die)
          file      <- RuntimeFiles.sha256(leaf)
          same      <- RuntimeFiles.fingerprint(root)
          again     <- RuntimeFiles.fingerprint(root)
          _         <- Workspace.writeString(leaf, "two").catchAll(die)
          changed   <- RuntimeFiles.fingerprint(root)
          _         <- ZIO.attemptBlocking(Files.delete(leaf)).orDie
          _         <- Workspace.writeString(root.resolve("c.txt"), "two").catchAll(die)
          renamed   <- RuntimeFiles.fingerprint(root)
          emptyDir = dir.resolve("empty")
          _         <- Workspace.createDirectories(emptyDir).catchAll(die)
          empty     <- RuntimeFiles.fingerprint(emptyDir)
          missing   <- RuntimeFiles.fingerprint(dir.resolve("missing-root")).either
          found     <- RuntimeFiles.firstOnPath(Some(bin.toString), List("skip", "absent", "tool"))
          canonical <- ZIO.attemptBlocking(tool.toRealPath().normalize).orDie
        yield
          val absent = missing match
            case Left(error) => error == IoFailure(dir.resolve("missing-root"), IoFault.Stat)
            case Right(_)    => false
          assertTrue(
            file == Sha256.of(Chunk.fromArray(one)),
            same == again,
            changed != same,
            renamed != changed,
            empty != same,
            absent,
            found.contains(canonical),
          )
  )

  private def die(error: IoFailure): ZIO[Any, Nothing, Nothing] =
    ZIO.dieMessage(error.message)
end RuntimeFilesLawSpec
