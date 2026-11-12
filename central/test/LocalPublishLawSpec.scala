package histos.central

import histos.version.{ReleaseVersion, ShipNumber}
import histos.Organization

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, IO, Scope, ZIO}
import zio.test.*

object LocalPublishLawSpec extends ZIOSpecDefault:
  private val ship = ShipNumber("1.0.0")
  private val pk   = Chunk[Byte](0x50.toByte, 0x4b.toByte, 3.toByte, 4.toByte)

  def spec = suite("local publish")(
    test("a zip jar writes the jar, the pom, and three checksum sidecars"):
      placed("jar-ok"): (dir, artifact) =>
        val jar = module(dir).resolve("widget-1.0.0.jar")
        val pom = module(dir).resolve("widget-1.0.0.pom")
        for
          published <- ZIO.serviceWithZIO[Publisher](_.publish(artifact))
          jarBytes  <- bytes(jar)
          pomBytes  <- bytes(pom)
          jarMd5    <- text(module(dir).resolve("widget-1.0.0.jar.md5"))
          jarSha1   <- text(module(dir).resolve("widget-1.0.0.jar.sha1"))
          jarSha256 <- text(module(dir).resolve("widget-1.0.0.jar.sha256"))
          pomSha256 <- text(module(dir).resolve("widget-1.0.0.pom.sha256"))
          sources   <- exists(module(dir).resolve("widget-1.0.0-sources.jar"))
        yield assertTrue(
          published == Published.Local(jar, pom, None, None, Checksum.sha256(pk)),
          jarBytes == pk,
          jarMd5 == Digest.md5(pk) + "\n",
          jarSha1 == Digest.sha1(pk) + "\n",
          jarSha256 == Digest.sha256(pk) + "\n",
          pomSha256 == Digest.sha256(pomBytes) + "\n",
          !sources,
        )
    ,
    test("sources and javadoc are written only when the artifact carries them"):
      placed("extras", sources = Some(pk), javadoc = Some(pk)): (dir, artifact) =>
        val sources = module(dir).resolve("widget-1.0.0-sources.jar")
        val javadoc = module(dir).resolve("widget-1.0.0-javadoc.jar")
        for
          published    <- ZIO.serviceWithZIO[Publisher](_.publish(artifact))
          sourceBytes  <- bytes(sources)
          javadocBytes <- bytes(javadoc)
          sourcesMd5   <- text(module(dir).resolve("widget-1.0.0-sources.jar.md5"))
        yield assertTrue(
          published == Published.Local(
            module(dir).resolve("widget-1.0.0.jar"),
            module(dir).resolve("widget-1.0.0.pom"),
            Some(sources),
            Some(javadoc),
            Checksum.sha256(pk),
          ),
          sourceBytes == pk,
          javadocBytes == pk,
          sourcesMd5 == Digest.md5(pk) + "\n",
        )
    ,
    test("an empty jar, a non-zip, an empty source jar, and a non-zip source jar are refused"):
      val program =
        for
          root      <- ZIO.service[Path]
          dir       <- LawDir.child(root, "refused")
          empty     <- run(dir.resolve("empty"), Chunk.empty, None)
          plain     <- run(dir.resolve("plain"), Chunk[Byte](1, 2), None)
          bare      <- run(dir.resolve("bare-src"), pk, Some(Chunk.empty))
          odd       <- run(dir.resolve("odd-src"), pk, Some(Chunk[Byte](9)))
          emptyLeft <- exists(dir.resolve("empty"))
          plainLeft <- exists(dir.resolve("plain"))
        yield assertTrue(
          empty == Left(PublishError.EmptyJar),
          plain == Left(PublishError.NotAJar),
          bare == Left(PublishError.EmptyJar),
          odd == Left(PublishError.NotAJar),
          !emptyLeft,
          !plainLeft,
        )
      program
    ,
    test("a file is not a repository directory, and a missing directory is created"):
      val program =
        for
          root    <- ZIO.service[Path]
          dir     <- LawDir.child(root, "roots")
          file    <- ZIO.attemptBlocking(Files.write(dir.resolve("not-dir"), Array.emptyByteArray)).orDie
          blocked <- run(file, pk, None)
          missing = dir.resolve("fresh")
          created  <- run(missing, pk, None)
          jarBytes <- bytes(module(missing).resolve("widget-1.0.0.jar"))
        yield assertTrue(
          blocked == Left(PublishError.NotADirectory(file)),
          created == Right(
            Published.Local(
              module(missing).resolve("widget-1.0.0.jar"),
              module(missing).resolve("widget-1.0.0.pom"),
              None,
              None,
              Checksum.sha256(pk),
            )
          ),
          jarBytes == pk,
        )
      program,
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)

  /** A file the law reads back is not there or cannot be read. */
  private final case class Unread(path: Path)

  private def placed(label: String, sources: Option[Chunk[Byte]] = None, javadoc: Option[Chunk[Byte]] = None)(
      use: (Path, Artifact) => ZIO[Publisher, PublishError | Unread, TestResult]
  ): ZIO[Path, ArtifactIdError | PublishError | Unread, TestResult] =
    for
      root   <- ZIO.service[Path]
      dir    <- LawDir.child(root, label)
      made   <- ZIO.fromEither(artifact(pk, sources, javadoc))
      result <- use(dir, made).provide(Publisher.local(dir))
    yield result

  private def run(
      root: Path,
      jar: Chunk[Byte],
      sources: Option[Chunk[Byte]],
  ): ZIO[Any, ArtifactIdError, Either[PublishError, Published]] =
    ZIO
      .fromEither(artifact(jar, sources, None))
      .flatMap: made =>
        ZIO.serviceWithZIO[Publisher](_.publish(made)).either.provide(Publisher.local(root))

  private def artifact(
      jar: Chunk[Byte],
      sources: Option[Chunk[Byte]],
      javadoc: Option[Chunk[Byte]],
  ): Either[ArtifactIdError, Artifact] =
    ArtifactId
      .from("widget")
      .map: name =>
        Artifact(
          Organization("demo"),
          name,
          Revision.Release(ReleaseVersion(ship)),
          jar,
          sources = sources,
          javadoc = javadoc,
        )

  private def module(root: Path): Path =
    root.resolve("demo").resolve("widget").resolve("1.0.0")

  private def bytes(path: Path): IO[Unread, Chunk[Byte]] =
    ZIO.attemptBlocking(Chunk.fromArray(Files.readAllBytes(path))).mapError(_ => Unread(path))

  private def text(path: Path): IO[Unread, String] =
    ZIO.attemptBlocking(Files.readString(path, StandardCharsets.UTF_8)).mapError(_ => Unread(path))

  private def exists(path: Path): IO[Unread, Boolean] =
    ZIO.attemptBlocking(Files.exists(path)).mapError(_ => Unread(path))
end LocalPublishLawSpec
