package histos.load

import histos.plugin.Digests
import histos.plugin.PluginLoadError
import histos.suite.HistosSpec
import histos.workspace.Workspace

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import zio.{UIO, ZIO}
import zio.test.*

object ClasspathConflictSpec extends HistosSpec:
  def spec = suite("a classpath conflict names both jars")(
    test("the toolchain api is named ahead of the resolved coordinate") {
      ZIO.scoped {
        Workspace
          .work("classpath-conflict")
          .flatMap: root =>
            val api      = root.resolve("cache").resolve("api").resolve("abc").resolve("histos-api.jar")
            val resolved = root.resolve("histos-api_3.jar")
            val entry    = "histos/plugin/Guard.class"
            for
              _              <- jar(api, entry, Array[Byte](1, 2, 3))
              _              <- jar(resolved, entry, Array[Byte](4, 5, 6))
              apiDigest      <- digest(api)
              resolvedDigest <- digest(resolved)
              exit           <- ClasspathIndex.includeAll(List(resolved, api)).either
            yield exit match
              case Left(LoadError.Plugin(error: PluginLoadError.ClasspathConflict)) =>
                assertTrue(
                  error.binary == entry,
                  error.left == resolved,
                  error.right == api,
                  error.leftDigest == resolvedDigest,
                  error.rightDigest == apiDigest,
                  error.message ==
                    s"class $entry is defined by the toolchain API $api ($apiDigest) and the resolved coordinate $resolved ($resolvedDigest)",
                )
              case other =>
                assertTrue(false, other.toString == "toolchain conflict")
      }
    },
    test("any other pair names both paths and both digests") {
      ZIO.scoped {
        Workspace
          .work("classpath-other")
          .flatMap: root =>
            val left  = root.resolve("left.jar")
            val right = root.resolve("right.jar")
            val entry = "demo/Use.class"
            for
              _           <- jar(left, entry, Array[Byte](1))
              _           <- jar(right, entry, Array[Byte](2))
              leftDigest  <- digest(left)
              rightDigest <- digest(right)
              exit        <- ClasspathIndex.includeAll(List(left, right)).either
            yield exit match
              case Left(LoadError.Plugin(error: PluginLoadError.ClasspathConflict)) =>
                assertTrue(
                  error.message ==
                    s"class $entry is defined by $left ($leftDigest) and $right ($rightDigest)"
                )
              case other =>
                assertTrue(false, other.toString == "pair conflict")
      }
    },
  )

  private def jar(path: Path, entry: String, bytes: Array[Byte]): UIO[Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(dir => Files.createDirectories(dir))
      val out = new ZipOutputStream(Files.newOutputStream(path))
      try
        out.putNextEntry(new ZipEntry(entry))
        out.write(bytes)
        out.closeEntry()
      finally out.close()
    }.orDie

  private def digest(path: Path): UIO[String] =
    ZIO.attemptBlocking(Digests.sha256(Files.readAllBytes(path))).orDie
end ClasspathConflictSpec
