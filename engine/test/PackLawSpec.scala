package histos.pack

import histos.jdk.Sha256

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Clock, ZIO}
import zio.test.*

object PackLawSpec extends ZIOSpecDefault:
  def spec = suite("pack laws")(
    test("class bytes round-trip, and an empty, escaped, or repeated entry is refused"):
      ZIO.acquireRelease(directory("histos-jar"))(delete).flatMap { root =>
        val dest  = root.resolve("classes.jar")
        val bytes = Chunk.fromArray("class".getBytes(StandardCharsets.UTF_8))
        for
          _       <- Clock.instant
          empty   <- ClassJar.writeBytes(Nil, dest).either
          escaped <- ClassJar.writeBytes(List(".." -> bytes), dest).either
          slash   <- ClassJar.writeBytes(List("/abs" -> bytes), dest).either
          again   <- ClassJar.writeBytes(List("A.class" -> bytes, "A.class" -> bytes), dest).either
          written <- ClassJar.writeBytes(List("b/B.class" -> bytes, "a/A.class" -> Chunk.single(7.toByte)), dest)
          first   <- zipBytes(dest, "a/A.class")
          second  <- zipBytes(dest, "b/B.class")
        yield assertTrue(
          empty == Left(JarError.Empty(dest)),
          escaped == Left(JarError.Escaped("..")),
          slash == Left(JarError.Escaped("/abs")),
          again == Left(JarError.Duplicate("A.class")),
          written.entries == 2,
          first.exists(_.sameElements(Array[Byte](7))),
          second.exists(_.sameElements(bytes.toArray)),
        )
      }
    ,
    test("a package key is the product list, and order is part of it"):
      val left  = Sha256.of(Chunk.single(1.toByte))
      val right = Sha256.of(Chunk.single(2.toByte))
      assertTrue(
        PackageKey.of(List(left)) != left,
        PackageKey.of(List(left, right)) != PackageKey.of(List(right, left)),
        PackageKey.of(Nil) != left,
        PackageKey.of(List(left)) == PackageKey.of(List(left)),
      )
    ,
    test("resource names are relative to main resources, and a missing directory is empty"):
      ZIO.acquireRelease(directory("histos-resources"))(delete).flatMap { root =>
        val sources = root.resolve("src")
        val note    = sources.resolve("main").resolve("resources").resolve("notes").resolve("hello.txt")
        for
          _       <- Clock.instant
          missing <- Resources.files(sources)
          _       <- ZIO.attemptBlocking {
            Option(note.getParent).foreach(Files.createDirectories(_))
            Files.writeString(note, "hi")
          }.orDie
          found <- Resources.files(sources)
        yield assertTrue(
          missing == Nil,
          found.map(_._1) == List("notes/hello.txt"),
        )
      },
  )

  private def zipBytes(path: Path, name: String): ZIO[Any, Nothing, Option[Array[Byte]]] =
    ZIO.attemptBlocking {
      val zip = new ZipFile(path.toFile)
      try
        Option(zip.getEntry(name)).map { item =>
          val in = zip.getInputStream(item)
          try in.readAllBytes
          finally in.close()
        }
      finally zip.close()
    }.orDie

  private def directory(prefix: String): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking(Files.createTempDirectory(prefix)).orDie

  private def delete(root: Path): zio.UIO[Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.orDie
end PackLawSpec
