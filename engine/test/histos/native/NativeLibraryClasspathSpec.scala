package histos.native

import histos.suite.HistosSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.ZIO
import zio.test.*

object NativeLibraryClasspathSpec extends HistosSpec:
  def spec = suite("native library classpath")(
    test("a renamed standard library jar is dropped and another jar stays") {
      ZIO.scoped {
        ZIO
          .acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-native-cp")).orDie)(dir =>
            ZIO.attemptBlocking(remove(dir)).orDie
          )
          .flatMap { root =>
            val nightly = root.resolve("nativelib_native0.5_3-nightly.jar")
            val renamed = root.resolve("9.jar")
            val other   = root.resolve("zio_native0.5_3-2.jar")
            for
              _ <- ZIO.attemptBlocking {
                jar(nightly, "nativelib")
                jar(renamed, "nativelib")
                jar(other, "zio")
                ()
              }.orDie
              kept = NativeLink.withoutLibraries(List(renamed, other), List(nightly))
            yield assertTrue(kept == List(other))
          }
      }
    }
  )

  private def jar(path: Path, title: String): Unit =
    val stream = new ZipOutputStream(Files.newOutputStream(path))
    try
      stream.putNextEntry(new ZipEntry("META-INF/MANIFEST.MF"))
      stream.write(s"Implementation-Title: $title\n".getBytes(StandardCharsets.UTF_8))
      stream.closeEntry()
    finally stream.close()

  private def remove(root: Path): Unit =
    if Files.exists(root) then
      val walk  = Files.walk(root)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
end NativeLibraryClasspathSpec
