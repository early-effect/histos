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
            published match
              case Published.Local(jar, pom, sources, javadoc, sum) =>
                val jarName = "widget-1.0.0.jar"
                assertTrue(
                  sources.isEmpty,
                  javadoc.isEmpty,
                  sum.value == Digest.sha256(pk),
                  Files.isRegularFile(jar),
                  Files.isRegularFile(pom),
                  read(jar) == new String(pk.toArray, StandardCharsets.UTF_8),
                  sidecar(jar, "md5") == Some(Digest.md5(pk) + "\n"),
                  sidecar(jar, "sha1") == Some(Digest.sha1(pk) + "\n"),
                  sidecar(jar, "sha256") == Some(Digest.sha256(pk) + "\n"),
                  sidecar(pom, "sha256") == Some(Digest.sha256(Chunk.fromArray(Files.readAllBytes(pom))) + "\n"),
                  jar.endsWith(jarName),
                  !Files.exists(jar.getParent.resolve("widget-1.0.0-sources.jar")),
                  Files.isDirectory(dir),
                )
              case Published.Snapshot(unique, _) =>
                assertTrue(unique.value == "unused")
    ,
    test("sources and javadoc are written only when the artifact carries them"):
      placed("extras", sources = Some(pk), javadoc = Some(pk)): (_, artifact) =>
        ZIO
          .serviceWithZIO[Publisher](_.publish(artifact))
          .map: published =>
            published match
              case Published.Local(_, _, sources, javadoc, _) =>
                assertTrue(
                  sources.exists(Files.isRegularFile(_)),
                  javadoc.exists(Files.isRegularFile(_)),
                  sources.exists(path => sidecar(path, "md5") == Some(Digest.md5(pk) + "\n")),
                )
              case Published.Snapshot(unique, _) =>
                assertTrue(unique.value == "unused")
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
        yield assertTrue(
          blocked == Left(PublishError.NotADirectory(file)),
          created.map {
            case Published.Local(jar, _, _, _, _) => Files.isRegularFile(jar) && Files.isDirectory(missing)
            case Published.Snapshot(_, _)         => false
          } == Right(true),
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

  private def sidecar(path: Path, suffix: String): Option[String] =
    Option(path.getFileName).map: name =>
      Files.readString(path.resolveSibling(name.toString + "." + suffix), StandardCharsets.UTF_8)

  private def read(path: Path): String =
    Files.readString(path, StandardCharsets.UTF_8)
end LocalPublishLawSpec
