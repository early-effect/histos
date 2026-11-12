package histos.central

import histos.version.{ReleaseVersion, ShipNumber}
import histos.Organization

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object LocalPublishLawSpec extends ZIOSpecDefault:
  private val ship = ShipNumber("1.0.0")
  private val pk   = Chunk[Byte](0x50.toByte, 0x4b.toByte, 3.toByte, 4.toByte)

  def spec = suite("local publish")(
    test("a zip jar writes the jar, the pom, and three checksum sidecars"):
      placed("jar-ok"): (dir, artifact) =>
        ZIO
          .serviceWithZIO[Publisher](_.publish(artifact))
          .map: published =>
            val jar = module(dir).resolve("widget-1.0.0.jar")
            val pom = module(dir).resolve("widget-1.0.0.pom")
            assertTrue(
              published == Published.Local(jar, pom, None, None, Checksum.sha256(pk)),
              read(jar) == new String(pk.toArray, StandardCharsets.UTF_8),
              Files.isRegularFile(pom),
              sidecar(jar, "md5") == Some(Digest.md5(pk) + "\n"),
              sidecar(jar, "sha1") == Some(Digest.sha1(pk) + "\n"),
              sidecar(jar, "sha256") == Some(Digest.sha256(pk) + "\n"),
              sidecar(pom, "sha256") == Some(Digest.sha256(Chunk.fromArray(Files.readAllBytes(pom))) + "\n"),
              !Files.exists(module(dir).resolve("widget-1.0.0-sources.jar")),
            )
    ,
    test("sources and javadoc are written only when the artifact carries them"):
      placed("extras", sources = Some(pk), javadoc = Some(pk)): (dir, artifact) =>
        ZIO
          .serviceWithZIO[Publisher](_.publish(artifact))
          .map: published =>
            val sources = module(dir).resolve("widget-1.0.0-sources.jar")
            val javadoc = module(dir).resolve("widget-1.0.0-javadoc.jar")
            assertTrue(
              published == Published.Local(
                module(dir).resolve("widget-1.0.0.jar"),
                module(dir).resolve("widget-1.0.0.pom"),
                Some(sources),
                Some(javadoc),
                Checksum.sha256(pk),
              ),
              Files.isRegularFile(sources),
              Files.isRegularFile(javadoc),
              sidecar(sources, "md5") == Some(Digest.md5(pk) + "\n"),
            )
    ,
    test("an empty jar, a non-zip, an empty source jar, and a non-zip source jar are refused"):
      val program =
        for
          root  <- ZIO.service[Path]
          dir   <- LawDir.child(root, "refused")
          empty <- run(dir.resolve("empty"), Chunk.empty, None)
          plain <- run(dir.resolve("plain"), Chunk[Byte](1, 2), None)
          bare  <- run(dir.resolve("bare-src"), pk, Some(Chunk.empty))
          odd   <- run(dir.resolve("odd-src"), pk, Some(Chunk[Byte](9)))
        yield assertTrue(
          empty == Left(PublishError.EmptyJar),
          plain == Left(PublishError.NotAJar),
          bare == Left(PublishError.EmptyJar),
          odd == Left(PublishError.NotAJar),
          !Files.exists(dir.resolve("empty")),
          !Files.exists(dir.resolve("plain")),
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
          created <- run(missing, pk, None)
        yield
          val jar = module(missing).resolve("widget-1.0.0.jar")
          assertTrue(
            blocked == Left(PublishError.NotADirectory(file)),
            created == Right(
              Published.Local(jar, module(missing).resolve("widget-1.0.0.pom"), None, None, Checksum.sha256(pk))
            ),
            Files.isRegularFile(jar),
          )
      program,
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)

  private def placed(label: String, sources: Option[Chunk[Byte]] = None, javadoc: Option[Chunk[Byte]] = None)(
      use: (Path, Artifact) => ZIO[Publisher, PublishError, TestResult]
  ) =
    val program =
      for
        root   <- ZIO.service[Path]
        dir    <- LawDir.child(root, label)
        made   <- ZIO.fromEither(artifact(pk, sources, javadoc))
        result <- use(dir, made).provide(Publisher.local(dir))
      yield result
    program

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

  private def sidecar(path: Path, suffix: String): Option[String] =
    Option(path.getFileName).map: name =>
      Files.readString(path.resolveSibling(name.toString + "." + suffix), StandardCharsets.UTF_8)

  private def read(path: Path): String =
    Files.readString(path, StandardCharsets.UTF_8)
end LocalPublishLawSpec
