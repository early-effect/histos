package histos.publish

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Ref, Scope, ZIO}
import zio.test.*

object BundleSpec extends histos.SuiteClock:
  private val group    = "com/example/core_3/1.0.0"
  private val artifact = "core_3-1.0.0"
  private val pieces   = List(s"$artifact.jar", s"$artifact.pom", s"$artifact-sources.jar", s"$artifact-javadoc.jar")

  private val signed: List[String] =
    pieces
      .flatMap(file => List(file, s"$file.asc", s"$file.md5", s"$file.sha1", s"$file.sha256"))
      .map(file => s"$group/$file")
      .sorted

  def spec =
    suite("bundle")(
      test("a signed stage zips in Maven layout"):
        ZIO.scoped:
          for
            dir   <- temp
            stage <- lay(dir.resolve("stage"))
            to = dir.resolve("bundle.zip")
            _     <- Bundle.zip(stage, to)
            names <- entries(to)
            pom   <- text(to, s"$group/$artifact.pom")
          yield assertTrue(
            names == signed,
            names.contains(s"$group/$artifact.jar"),
            names.contains(s"$group/$artifact.jar.asc"),
            names.contains(s"$group/$artifact.jar.md5"),
            names.contains(s"$group/$artifact.jar.sha1"),
            names.contains(s"$group/$artifact.pom"),
            names.contains(s"$group/$artifact.pom.asc"),
            names.contains(s"$group/$artifact-sources.jar"),
            names.contains(s"$group/$artifact-sources.jar.asc"),
            names.contains(s"$group/$artifact-javadoc.jar"),
            names.contains(s"$group/$artifact-javadoc.jar.asc"),
            names.count(_.endsWith(".asc")) == 4,
            names.count(_.endsWith(".md5")) == 4,
            names.count(_.endsWith(".sha1")) == 4,
            names.count(_.endsWith(".sha256")) == 4,
            pom.contains("pom\n"),
          )
      ,
      test("a declared size over 1GB is refused without writing a file"):
        refused(Bundle.Cap + 1, BundleError.TooLarge(Bundle.Cap + 1))
      ,
      test("a declared size of 1GB is a bundle"):
        ZIO.scoped:
          for
            dir <- temp
            to = dir.resolve("bundle.zip")
            reads <- Ref.make(0)
            path  <- ZIO.fromEither(BundlePath.from(s"$group/$artifact.jar"))
            _     <- Bundle.zip(Chunk(Staged(path, Bundle.Cap, reads.update(_ + 1).as(Chunk.single(1.toByte)))), to)
            names <- entries(to)
            n     <- reads.get
          yield assertTrue(names == List(s"$group/$artifact.jar"), n == 1)
      ,
      test("an empty stage writes nothing"):
        ZIO.scoped:
          for
            dir   <- temp
            stage <- ZIO.attempt(Files.createDirectory(dir.resolve("stage")))
            to = dir.resolve("bundle.zip")
            opened <- Bundle.zip(stage, to).either
            empty  <- Bundle.zip(Chunk.empty, to).either
            there  <- present(to)
          yield assertTrue(
            opened == Left(BundleError.Empty),
            empty == Left(BundleError.Empty),
            !there,
          )
      ,
      test("a negative declared size is refused before the body is read"):
        refused(-1L, BundleError.Size(-1L))
      ,
      test("a path in the bundle twice is refused before a body is read"):
        ZIO.scoped:
          for
            dir <- temp
            to = dir.resolve("bundle.zip")
            reads <- Ref.make(0)
            path  <- ZIO.fromEither(BundlePath.from(s"$group/$artifact.jar"))
            staged = Staged(path, 1L, reads.update(_ + 1).as(Chunk.single(1.toByte)))
            opened <- Bundle.zip(Chunk(staged, staged), to).either
            n      <- reads.get
            there  <- present(to)
          yield assertTrue(
            opened == Left(BundleError.Duplicate(s"$group/$artifact.jar")),
            n == 0,
            !there,
          )
      ,
      test("a path that leaves the stage is not a bundle path"):
        ZIO.succeed:
          assertTrue(
            BundlePath.from("../secret") == Left(BundleError.Escaped("../secret")),
            BundlePath.from("/etc/passwd") == Left(BundleError.Escaped("/etc/passwd")),
            BundlePath.from("com//example") == Left(BundleError.Escaped("com//example")),
          )
      ,
      test("a zip inside the stage is refused"):
        ZIO.scoped:
          for
            dir   <- temp
            stage <- lay(dir.resolve("stage"))
            to = stage.resolve("bundle.zip")
            opened <- Bundle.zip(stage, to).either
            there  <- present(to)
          yield assertTrue(opened == Left(BundleError.Contained(to.toString)), !there)
      ,
      test("a missing stage is refused"):
        ZIO.scoped:
          for
            dir <- temp
            missing = dir.resolve("absent")
            opened <- Bundle.zip(missing, dir.resolve("bundle.zip")).either
          yield assertTrue(opened == Left(BundleError.Missing(missing.toString))),
    )

  private def refused(bytes: Long, error: BundleError) =
    ZIO.scoped:
      for
        dir <- temp
        to = dir.resolve("bundle.zip")
        reads  <- Ref.make(0)
        path   <- ZIO.fromEither(BundlePath.from(s"$group/$artifact.jar"))
        opened <- Bundle
          .zip(Chunk(Staged(path, bytes, reads.update(_ + 1).as(Chunk.single(1.toByte)))), to)
          .either
        n     <- reads.get
        there <- present(to)
      yield assertTrue(opened == Left(error), n == 0, !there)

  private def present(path: Path): ZIO[Any, Throwable, Boolean] =
    ZIO.attempt(Files.exists(path))

  private def lay(stage: Path): ZIO[Any, Throwable, Path] =
    ZIO.attempt:
      val directory = stage.resolve(group)
      Files.createDirectories(directory)
      signed.foreach: name =>
        val file = name.split("/", -1).findLast(_.nonEmpty).getOrElse(name)
        val body = if file == s"$artifact.pom" then "pom\n" else s"$file\n"
        Files.writeString(directory.resolve(file), body)
      stage

  private def entries(zip: Path): ZIO[Any, Throwable, List[String]] =
    ZIO.attemptBlocking:
      val file = new ZipFile(zip.toFile)
      try file.entries().asScala.map(_.getName).toList.sorted
      finally file.close()

  private def text(zip: Path, name: String): ZIO[Any, Throwable, Option[String]] =
    ZIO.attemptBlocking:
      val file = new ZipFile(zip.toFile)
      try
        file.getEntry(name) match
          case null  => None
          case entry =>
            val input = file.getInputStream(entry)
            try Some(new String(input.readAllBytes, StandardCharsets.UTF_8))
            finally input.close()
      finally file.close()

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-bundle"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator().asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore
end BundleSpec
