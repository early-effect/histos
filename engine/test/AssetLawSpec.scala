package histos.publish

import java.nio.charset.StandardCharsets

import zio.{Chunk, Clock, IO, Ref, ZIO}
import zio.test.*

object AssetLawSpec extends ZIOSpecDefault:
  private val named = Chunk.fromArray("jar".getBytes(StandardCharsets.UTF_8))

  def spec = suite("asset laws")(
    test("a replace retries one 422, and any other status stands"):
      for
        _     <- Clock.instant
        once  <- replaced(List(AssetError.Status(422)))
        stuck <- replaced(List(AssetError.Status(422), AssetError.Status(422)))
        other <- replaced(List(AssetError.Status(500)))
      yield assertTrue(
        once == (Right(()), List(named, named), 2),
        stuck == (Left(AssetError.Status(422)), List(named, named), 2),
        other == (Left(AssetError.Status(500)), List(named), 1),
      )
    ,
    test("http is a release host only on loopback, and a repository is owner and name"):
      AssetRepo.from("example/demo") match
        case Left(_)     => assertTrue(false)
        case Right(repo) =>
          assertTrue(
            AssetBase.from("http://127.0.0.1:9").isRight,
            AssetBase.from("http://localhost:9").isRight,
            AssetBase.from("https://example.test").isRight,
            AssetBase.from("http://example.test") ==
              Left(AssetError.Host("http release host 'example.test' is not loopback")),
            AssetBase.from("not a url") == Left(AssetError.Host("'not a url' is not a release host")),
            repo.owner == "example",
            repo.name == "demo",
            AssetRepo.from("demo") == Left(AssetError.Repository("demo")),
            AssetRepo.from("a/b/c") == Left(AssetError.Repository("a/b/c")),
          )
    ,
    test("an asset upload without a token or a repository is refused before a socket"):
      assertTrue(
        AssetAccess.access(Map.empty) == Left(AssetError.Credentials),
        AssetAccess.access(Map("GITHUB_TOKEN" -> "token")) == Left(AssetError.Credentials),
        AssetAccess.access(Map("GITHUB_REPOSITORY" -> "example/demo")) == Left(AssetError.Credentials),
        AssetAccess.access(Map("GITHUB_TOKEN" -> "", "GITHUB_REPOSITORY" -> "example/demo")) ==
          Left(AssetError.Credentials),
        AssetAccess.access(Map("GITHUB_TOKEN" -> "token", "GITHUB_REPOSITORY" -> "nope")) ==
          Left(AssetError.Repository("nope")),
        AssetAccess.access(Map("GITHUB_TOKEN" -> "token", "GITHUB_REPOSITORY" -> "example/demo")).isRight,
        AssetAccess.bases(Map.empty).isRight,
        AssetAccess.bases(Map("HISTOS_GITHUB" -> "http://example.test")).isLeft,
      ),
  )

  private def replaced(
      faults: List[AssetError]
  ): ZIO[Any, Nothing, (Either[AssetError, Unit], List[Chunk[Byte]], Int)] =
    for
      uploads <- Ref.make(List.empty[Chunk[Byte]])
      deletes <- Ref.make(0)
      queued  <- Ref.make(faults)
      result  <- Recording(uploads, deletes, queued).replace("dist", "app.jar", named).either
      sent    <- uploads.get
      removed <- deletes.get
    yield (result, sent, removed)

  private final class Recording(
      uploads: Ref[List[Chunk[Byte]]],
      deletes: Ref[Int],
      faults: Ref[List[AssetError]],
  ) extends Assets:
    def upload(tag: String, name: String, bytes: Chunk[Byte]): IO[AssetError, Unit] =
      for
        _    <- uploads.update(seen => seen :+ bytes)
        next <- faults.modify:
          case Nil          => (None, Nil)
          case head :: tail => (Some(head), tail)
        _ <- next match
          case None        => ZIO.unit
          case Some(error) => ZIO.fail(error)
      yield ()

    def delete(tag: String, name: String): IO[AssetError, Unit] =
      deletes.update(_ + 1).unit
  end Recording
end AssetLawSpec
