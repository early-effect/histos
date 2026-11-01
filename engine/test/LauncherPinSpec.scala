package histos.publish

import histos.workspace.{IoFailure, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest

import zio.test.*
import zio.ZIO

object LauncherPinSpec extends ZIOSpecDefault:
  private val pointer  = "0.0.2-SNAPSHOT"
  private val revision = "0.0.2-20261101.030257-1"

  def spec = suite("launcher version file")(
    test("a pointer and a timestamp round-trip through comments, blanks, and spaces"):
      val pointerText =
        s"""# channel
           |
           |  histos.version = $pointer
           |""".stripMargin
      val revisionText = s"histos.version=$revision\n"
      val parsedPointer  = LauncherPin.parse(pointerText)
      val parsedRevision = LauncherPin.parse(revisionText)
      assertTrue(
        parsedPointer.map(LauncherPin.render) == Right(s"histos.version=$pointer\n"),
        parsedRevision.map(LauncherPin.render) == Right(revisionText),
        parsedPointer.flatMap(pin => LauncherPin.parse(LauncherPin.render(pin)).map(_.shown)) ==
          parsedPointer.map(_.shown),
        parsedRevision.flatMap(pin => LauncherPin.parse(LauncherPin.render(pin)).map(_.version)) ==
          parsedRevision.map(_.version),
        parsedPointer.map(_.channel) == Right(pointer),
        parsedRevision.map(_.channel) == Right(pointer),
        parsedRevision.map(_.version) == Right(revision),
      )
    ,
    test("a duplicate, unknown, missing, or illegal version fails"):
      assertTrue(
        LauncherPin.parse(s"histos.version=$pointer\nhistos.version=$pointer\n") == Left(LauncherError.Duplicate),
        LauncherPin.parse("other=1\n") == Left(LauncherError.Unknown("other")),
        LauncherPin.parse("# only\n") == Left(LauncherError.Missing),
        LauncherPin.parse("not a field\n") == Left(LauncherError.Line("not a field")),
        LauncherPin.parse("histos.version=0.0.2\n") == Left(LauncherError.Version("0.0.2")),
        LauncherPin.parse("histos.version=0.0.2-20260230.030257-1\n") == Left(
          LauncherError.Version("0.0.2-20260230.030257-1")
        ),
      )
    ,
    test("update writes the timestamp the repository published"):
      ZIO.scoped:
        for
          root  <- Workspace.work("histos-pin")
          repo  <- published(root.resolve("repo"), jarDigest = None)
          cache <- Workspace.createDirectories(root.resolve("cache"))
          _     <- Workspace.writeString(root.resolve(LauncherPin.file), s"histos.version=$pointer\n")
          _     <- TestSystem.putEnv("HISTOS_LAUNCHER_REPO", directoryUri(repo))
          pin   <- LauncherPin.updateAt(root, cache)
          read  <- LauncherPin.read(root)
        yield assertTrue(
          pin.version == revision,
          pin.shown == s"histos.version=$revision",
          read.version == pin.version,
          LauncherPin.render(read) == LauncherPin.render(pin),
        )
    ,
    test("latest writes the pointer and does not fetch"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-latest")
          _    <- Workspace.writeString(root.resolve(LauncherPin.file), s"histos.version=$revision\n")
          pin  <- LauncherPin.latest(root)
          text <- Workspace.readString(root.resolve(LauncherPin.file))
        yield assertTrue(
          pin.version == pointer,
          text == s"histos.version=$pointer\n",
        )
    ,
    test("a wrong published checksum fails and leaves the version file"):
      ZIO.scoped:
        for
          root   <- Workspace.work("histos-pin-bad")
          repo   <- published(root.resolve("repo"), jarDigest = Some("a" * 40))
          cache  <- Workspace.createDirectories(root.resolve("cache"))
          before = s"histos.version=$pointer\n"
          _      <- Workspace.writeString(root.resolve(LauncherPin.file), before)
          _      <- TestSystem.putEnv("HISTOS_LAUNCHER_REPO", directoryUri(repo))
          result <- LauncherPin.updateAt(root, cache).either
          after  <- Workspace.readString(root.resolve(LauncherPin.file))
        yield assertTrue(
          result match
            case Left(_: LauncherError.Resolve) => true
            case _                               => false
          ,
          after == before,
        )
    ,
  )

  private def directoryUri(path: Path): String =
    val uri = path.toAbsolutePath.toUri.toString
    if uri.endsWith("/") then uri else s"$uri/"

  /** A file Maven repository. `jarDigest` replaces the jar's sha1 when the published checksum should be wrong. */
  private def published(root: Path, jarDigest: Option[String]): ZIO[Any, IoFailure, Path] =
    val base     = root.resolve("rocks").resolve("earlyeffect").resolve("histos")
    val snapshot = base.resolve("0.0.2-SNAPSHOT")
    val pom      =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project>
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>rocks.earlyeffect</groupId>
        |  <artifactId>histos</artifactId>
        |  <version>0.0.2-SNAPSHOT</version>
        |</project>
        |""".stripMargin
    val jar = "histos"
    val metadata =
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<metadata modelVersion="1.1.0">
         |  <groupId>rocks.earlyeffect</groupId>
         |  <artifactId>histos</artifactId>
         |  <version>0.0.2-SNAPSHOT</version>
         |  <versioning>
         |    <snapshot>
         |      <timestamp>20261101.030257</timestamp>
         |      <buildNumber>1</buildNumber>
         |    </snapshot>
         |    <lastUpdated>20261101030257</lastUpdated>
         |    <snapshotVersions>
         |      <snapshotVersion>
         |        <extension>jar</extension>
         |        <value>$revision</value>
         |        <updated>20261101030257</updated>
         |      </snapshotVersion>
         |      <snapshotVersion>
         |        <extension>pom</extension>
         |        <value>$revision</value>
         |        <updated>20261101030257</updated>
         |      </snapshotVersion>
         |    </snapshotVersions>
         |  </versioning>
         |</metadata>
         |""".stripMargin
    val listing =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<metadata>
        |  <groupId>rocks.earlyeffect</groupId>
        |  <artifactId>histos</artifactId>
        |  <versioning>
        |    <latest>0.0.2-SNAPSHOT</latest>
        |    <versions>
        |      <version>0.0.2-SNAPSHOT</version>
        |    </versions>
        |  </versioning>
        |</metadata>
        |""".stripMargin
    for
      _ <- Workspace.createDirectories(snapshot)
      _ <- writeHashed(base.resolve("maven-metadata.xml"), listing)
      _ <- writeHashed(snapshot.resolve("maven-metadata.xml"), metadata)
      _ <- writeHashed(snapshot.resolve(s"histos-$revision.pom"), pom)
      _ <- writeHashed(snapshot.resolve(s"histos-$revision.jar"), jar, jarDigest)
    yield root

  private def writeHashed(
      path: Path,
      text: String,
      digest: Option[String] = None,
  ): ZIO[Any, IoFailure, Unit] =
    val bytes = text.getBytes(StandardCharsets.UTF_8)
    Workspace.writeString(path, text) *>
      Workspace.writeString(
        path.resolveSibling(path.getFileName.toString + ".sha1"),
        digest.getOrElse(sha1(bytes)) + "\n",
      )

  private def sha1(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-1").digest(bytes).map("%02x".format(_)).mkString
end LauncherPinSpec
