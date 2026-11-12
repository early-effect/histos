package histos.publish

import histos.workspace.Workspace

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{Chunk, ZIO}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A stage zips under the cap. An empty stage, a path that leaves, and a second copy of a name are refused. */
object BundleLawSpec extends ZIOSpecDefault:
  def spec = suite("bundle laws")(
    test("one file is a bundle, the cap is a bundle, and a size past the cap or below zero is refused unread"):
      Workspace
        .work("histos-bundle")
        .flatMap: dir =>
          val stage  = dir.resolve("stage")
          val file   = stage.resolve("notes").resolve("readme.txt")
          val dest   = dir.resolve("out").resolve("bundle.zip")
          val inside = stage.resolve("nested.zip")
          val unread = ZIO.dieMessage("the body was read")
          val half   = Bundle.Cap / 2 + 1
          for
            path    <- ZIO.fromEither(BundlePath.from("notes/readme.txt"))
            _       <- Workspace.writeString(file, "kept\n")
            missing <- Bundle.zip(dir.resolve("absent"), dest).either
            empty   <- Bundle.zip(dir.resolve("empty-stage"), dest).either
            _       <- Workspace
              .createDirectories(dir.resolve("empty-stage"))
              .catchAll(error => ZIO.dieMessage(error.message))
            vacant <- Bundle.zip(dir.resolve("empty-stage"), dest).either
            held   <- Bundle.zip(stage, inside).either
            _      <- Bundle.zip(stage, dest)
            names  <- entries(dest)
            capped <- Bundle.zip(
              Chunk(Staged(path, Bundle.Cap, ZIO.succeed(Chunk[Byte](1)))),
              dir.resolve("capped.zip"),
            )
            cappedNames <- entries(dir.resolve("capped.zip"))
            over        <- Bundle.zip(Chunk(Staged(path, Bundle.Cap + 1, unread)), dir.resolve("over.zip")).either
            summed      <- Bundle
              .zip(Chunk(Staged(path, half, unread), Staged(path, half, unread)), dir.resolve("summed.zip"))
              .either
            sized <- Bundle.zip(Chunk(Staged(path, -1, unread)), dir.resolve("sized.zip")).either
            none  <- Bundle.zip(Chunk.empty, dir.resolve("none.zip")).either
            twice <- Bundle
              .zip(
                Chunk(
                  Staged(path, 1, ZIO.succeed(Chunk[Byte](1))),
                  Staged(path, 1, ZIO.succeed(Chunk[Byte](2))),
                ),
                dir.resolve("twice.zip"),
              )
              .either
          yield assertTrue(
            BundlePath.from("") == Left(BundleError.Escaped("")),
            BundlePath.from("..") == Left(BundleError.Escaped("..")),
            BundlePath.from("/abs") == Left(BundleError.Escaped("/abs")),
            BundlePath.from("a/../b") == Left(BundleError.Escaped("a/../b")),
            BundlePath.from("a/./b") == Left(BundleError.Escaped("a/./b")),
            BundlePath.from("a\\b").map(_.name) == Right("a/b"),
            missing == Left(BundleError.Missing(dir.resolve("absent").toString)),
            empty == Left(BundleError.Missing(dir.resolve("empty-stage").toString)),
            vacant == Left(BundleError.Empty),
            held == Left(BundleError.Contained(inside.toString)),
            names == Set("notes/readme.txt"),
            cappedNames == Set("notes/readme.txt"),
            over == Left(BundleError.TooLarge(Bundle.Cap + 1)),
            summed == Left(BundleError.TooLarge(half + half)),
            sized == Left(BundleError.Size(-1)),
            none == Left(BundleError.Empty),
            twice == Left(BundleError.Duplicate("notes/readme.txt")),
            capped == (),
          )
  )

  private def entries(path: Path): ZIO[Any, Nothing, Set[String]] =
    ZIO.attemptBlocking {
      if !Files.isRegularFile(path) then Set.empty[String]
      else
        val zip = new ZipFile(path.toFile)
        try
          val names = zip.entries().asScala.map(_.getName).toSet
          val times = zip.entries().asScala.map(_.getTime).toList
          if times.forall(_ == 0L) && names.nonEmpty then names else Set.empty
        finally zip.close()
    }.orDie
end BundleLawSpec
