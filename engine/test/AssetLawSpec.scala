package histos.publish

import java.nio.charset.StandardCharsets

import zio.{Chunk, IO, Ref, ZIO}
import zio.test.*

object AssetLawSpec extends ZIOSpecDefault:
  private val named = Chunk.fromArray("jar".getBytes(StandardCharsets.UTF_8))

  private val head  = Gen.alphaNumericChar
  private val tail  = Gen.stringBounded(0, 8)(Gen.oneOf(Gen.alphaNumericChar, Gen.elements('.', '_', '-')))
  private val part  = head.zipWith(tail)((first, rest) => s"$first$rest")
  private val wrong = Gen.oneOf(
    Gen.const(""),
    Gen.elements('.', '_', '-').zipWith(tail)((first, rest) => s"$first$rest"),
    part.map(text => s"$text q"),
  )

  def spec = suite("asset laws")(
    test("a replace retries one 422, and any other status stands"):
      for
        once  <- replaced(List(AssetError.Status(422)))
        stuck <- replaced(List(AssetError.Status(422), AssetError.Status(422)))
        other <- replaced(List(AssetError.Status(500)))
      yield assertTrue(
        once == (Right(()), List(named, named), 2),
        stuck == (Left(AssetError.Status(422)), List(named, named), 2),
        other == (Left(AssetError.Status(500)), List(named), 1),
      )
    ,
    test("a repository is two parts that start with a letter or digit, and nothing else parses"):
      check(part, part, wrong): (owner, name, broken) =>
        val pair = s"$owner/$name"
        assertTrue(
          AssetRepo.from(pair).map(repo => (repo.owner, repo.name)) == Right((owner, name)),
          AssetRepo.from(s"$broken/$name") == Left(AssetError.Repository(s"$broken/$name")),
          AssetRepo.from(s"$owner/$broken") == Left(AssetError.Repository(s"$owner/$broken")),
          AssetRepo.from(owner) == Left(AssetError.Repository(owner)),
          AssetRepo.from(s"$pair/$name") == Left(AssetError.Repository(s"$pair/$name")),
        )
    ,
    test("http is a release host only on loopback, and a remote names its repository"):
      assertTrue(
        AssetBase.from("http://127.0.0.1:9").map(_.root.host) == Right(Some("127.0.0.1")),
        AssetBase.from("http://localhost:9").map(_.root.host) == Right(Some("localhost")),
        AssetBase.from("http://[::1]").isRight,
        AssetBase.from("https://example.test").map(_.root.host) == Right(Some("example.test")),
        AssetBase.from("http://example.test") ==
          Left(AssetError.Host("http release host 'example.test' is not loopback")),
        AssetBase.from("http://0.0.0.0:9") == Left(AssetError.Host("http release host '0.0.0.0' is not loopback")),
        AssetBase.from("http://::1") == Left(AssetError.Host("'http://::1' is not a release host")),
        AssetBase.from("not a url") == Left(AssetError.Host("'not a url' is not a release host")),
        AssetAccess.repositoryOf("https://github.com/early-effect/histos.git").contains("early-effect/histos"),
        AssetAccess.repositoryOf("git@github.com:early-effect/histos.git").contains("early-effect/histos"),
        AssetAccess.repositoryOf("ssh://git@github.com/early-effect/histos").contains("early-effect/histos"),
        AssetAccess.repositoryOf("not-a-remote").isEmpty,
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
        AssetAccess
          .access(Map("GITHUB_TOKEN" -> "token", "GITHUB_REPOSITORY" -> "example/demo"))
          .map(found => (found.repository.owner, found.repository.name, found.token.render)) ==
          Right(("example", "demo", "token")),
      )
    ,
    test("the release hosts are github's unless one is set, and one set alone serves both on the api side"):
      for
        api    <- ZIO.fromEither(AssetBase.from("http://127.0.0.1:9"))
        upload <- ZIO.fromEither(AssetBase.from("http://127.0.0.1:10"))
      yield assertTrue(
        AssetAccess.bases(Map.empty) == Right(AssetAccess.Bases(AssetBase.github, AssetBase.uploads)),
        AssetBase.github.root.host == Some("api.github.com"),
        AssetBase.uploads.root.host == Some("uploads.github.com"),
        AssetAccess.bases(Map("HISTOS_GITHUB" -> "http://127.0.0.1:9")) == Right(AssetAccess.Bases(api, api)),
        AssetAccess.bases(Map("HISTOS_GITHUB_UPLOAD" -> "http://127.0.0.1:10")) ==
          Right(AssetAccess.Bases(AssetBase.github, upload)),
        AssetAccess.bases(
          Map("HISTOS_GITHUB" -> "http://127.0.0.1:9", "HISTOS_GITHUB_UPLOAD" -> "http://127.0.0.1:10")
        ) ==
          Right(AssetAccess.Bases(api, upload)),
        AssetAccess.bases(Map("HISTOS_GITHUB" -> " ")) == Right(AssetAccess.Bases(AssetBase.github, AssetBase.uploads)),
        AssetAccess.bases(Map("HISTOS_GITHUB" -> "http://example.test")) ==
          Left(AssetError.Host("http release host 'example.test' is not loopback")),
        AssetAccess.bases(Map("HISTOS_GITHUB" -> "http://127.0.0.1:9", "HISTOS_GITHUB_UPLOAD" -> "nope")) ==
          Left(AssetError.Host("'nope' is not a release host")),
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
