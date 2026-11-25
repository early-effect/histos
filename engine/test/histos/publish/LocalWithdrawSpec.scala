package histos.publish

import histos.suite.HistosSpec
import histos.workspace.Workspace

import java.nio.file.Files

import zio.{Chunk, ZIO}
import zio.test.*

object LocalWithdrawSpec extends HistosSpec:
  private val revision = "0.5.0-abcdefabcdef-SNAPSHOT"
  private val kept     = "0.5.0-111111111111-SNAPSHOT"

  def spec = suite("publish withdraw")(
    test("a revision directory goes and another revision stays") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-withdraw").orDieWith(error => new java.io.IOException(error.message))
          removed = root.resolve("org.example").resolve("api").resolve(revision)
          _     <- write(removed.resolve("jars"), "api.jar")
          stay  <- write(root.resolve("org.example").resolve("api").resolve(kept).resolve("jars"), "api.jar")
          found <- IvyLocal
            .removeRevision(IvyRoot.at(root), revision)
            .orDieWith(error => new java.io.IOException(error.message))
          // assertTrue reads the path when the result runs, after this scope deletes the directory.
          removedGone <- ZIO.attemptBlocking(!Files.exists(removed)).orDie
          keptFile    <- ZIO.attemptBlocking(Files.isRegularFile(stay)).orDie
        yield assertTrue(
          found == List(removed),
          removedGone,
          keptFile,
        )
      }
    }
  )

  private def write(dir: java.nio.file.Path, name: String) =
    ZIO.attempt {
      Files.createDirectories(dir)
      val file = dir.resolve(name)
      Files.write(file, Chunk[Byte](1).toArray)
      file
    }.orDie
end LocalWithdrawSpec
