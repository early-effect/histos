package histos.preview

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}
import zio.test.*

/** The public compiler answers invalid sources by compiling. Reading that
  * surface must not be described as a read-only invalidation.
  */
object ZincInvalidationSpec extends ZIOSpecDefault:
  private val asked = suite(
    "zinc answers invalid sources only by compiling"
  )(
    test("IncrementalCompiler exposes compile and compileAllJava"):
      val names =
        classOf[xsbti.compile.IncrementalCompiler].getDeclaredMethods
          .map(_.getName)
          .toSet
      assertTrue(names == Set("compile", "compileAllJava"))
    ,
    test(
      "reading that surface leaves a fixture class file and writes no inc.zip"
    ):
      ZIO.scoped {
        for
          dir <- keep
          sample = dir.resolve("classes").resolve("A.class")
          marker = Array[Byte](1, 2, 3, 4)
          _ <- ZIO.attempt {
            Files.createDirectories(sample.getParent)
            Files.write(sample, marker)
          }
          before <- ZIO.attempt(Files.readAllBytes(sample).toList)
          _ <- ZIO.attempt {
            val _ =
              classOf[xsbti.compile.IncrementalCompiler].getDeclaredMethods
          }
          after <- ZIO.attempt(Files.readAllBytes(sample).toList)
          entries <- ZIO.attempt(tree(dir))
        yield assertTrue(
          after == before,
          !entries.contains("inc.zip"),
          entries == List("classes", "classes/A.class")
        )
      }
  )

  /** The class file is a fixture. The question is the declared methods. */
  private def keep: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-zinc-ro"))
    )(delete)

  private def tree(root: Path): List[String] =
    val walk = Files.walk(root)
    try
      walk.iterator.asScala
        .map(path =>
          root.relativize(path).iterator.asScala.map(_.toString).mkString("/")
        )
        .filter(_.nonEmpty)
        .toList
        .sorted
    finally walk.close()

  private def delete(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
    }.ignore

  def spec = asked
