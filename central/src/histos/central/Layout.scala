package histos.central

import histos.{Developer, License, Scm}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.security.MessageDigest

import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

object Digest:
  def sha256(bytes: Chunk[Byte]): String = digest("SHA-256", bytes)

  def sha1(bytes: Chunk[Byte]): String = digest("SHA-1", bytes)

  def md5(bytes: Chunk[Byte]): String = digest("MD5", bytes)

  private def digest(algorithm: String, bytes: Chunk[Byte]): String =
    MessageDigest.getInstance(algorithm).digest(bytes.toArray).map(byte => f"${byte & 0xff}%02x").mkString
end Digest

private[central] object JarCheck:
  def accept(jar: Chunk[Byte]): Either[PublishError, Chunk[Byte]] =
    jar match
      case Chunk()             => Left(PublishError.EmptyJar)
      case bytes if zip(bytes) => Right(bytes)
      case _                   => Left(PublishError.NotAJar)

  private def zip(bytes: Chunk[Byte]): Boolean =
    bytes.take(2) match
      case first +: second +: Chunk() => first == 0x50.toByte && second == 0x4b.toByte
      case _                          => false
end JarCheck

object Layout:
  def paths(root: Path, artifact: Artifact): Either[PublishError, MavenPaths] =
    val revision = artifact.revision.render
    if revision.isEmpty || revision.contains('/') || revision.contains('\\') || revision.contains("..") then
      Left(PublishError.Escaped(root.resolve(revision)))
    else
      under(root, artifact.group.segments :+ artifact.name.value :+ revision).map: directory =>
        val stem = s"${artifact.name.value}-$revision"
        MavenPaths(
          directory,
          directory.resolve(s"$stem.jar"),
          directory.resolve(s"$stem.pom"),
          artifact.sources.map(_ => directory.resolve(s"$stem-sources.jar")),
          artifact.javadoc.map(_ => directory.resolve(s"$stem-javadoc.jar")),
        )

  def pom(artifact: Artifact): String =
    val document = artifact.document
    val body     = List(
      Some(tag("name", artifact.name.value)),
      document.description.map(text => tag("description", text.value)),
      document.url.map(url => tag("url", url.value)),
      document.license.map(license),
      document.developer.map(developer),
      document.scm.map(scm),
      dependencies(artifact.dependencies),
    ).flatten.mkString("\n")
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<project xmlns="http://maven.apache.org/POM/4.0.0">
       |  <modelVersion>4.0.0</modelVersion>
       |  <groupId>${xml(artifact.group.value)}</groupId>
       |  <artifactId>${xml(artifact.name.value)}</artifactId>
       |  <version>${xml(artifact.revision.render)}</version>
       |  <packaging>jar</packaging>
       |$body
       |</project>
       |""".stripMargin

  private def dependencies(listed: List[MavenDependency]): Option[String] =
    listed match
      case Nil => None
      case _   =>
        val rows = listed
          .map: dependency =>
            s"""    <dependency>
               |      <groupId>${xml(dependency.group.value)}</groupId>
               |      <artifactId>${xml(dependency.name.value)}</artifactId>
               |      <version>${xml(dependency.version.value)}</version>
               |    </dependency>""".stripMargin
          .mkString("\n")
        Some(s"  <dependencies>\n$rows\n  </dependencies>")

  private def license(license: License): String =
    // Central requires the license to be distributed with the repository.
    s"""  <licenses>
       |    <license>
       |      <name>${xml(license.name.value)}</name>
       |      <url>${xml(license.url.value)}</url>
       |      <distribution>repo</distribution>
       |    </license>
       |  </licenses>""".stripMargin

  private def developer(developer: Developer): String =
    s"""  <developers>
       |    <developer>
       |      <id>${xml(developer.id.value)}</id>
       |      <name>${xml(developer.name.value)}</name>
       |    </developer>
       |  </developers>""".stripMargin

  private def scm(scm: Scm): String =
    // Central requires a developer connection. One written connection fills both.
    val connection = xml(scm.connection.value)
    s"""  <scm>
       |    <url>${xml(scm.url.value)}</url>
       |    <connection>$connection</connection>
       |    <developerConnection>$connection</developerConnection>
       |  </scm>""".stripMargin

  private def tag(label: String, text: String): String =
    s"  <$label>${xml(text)}</$label>"

  private def xml(text: String): String =
    text.flatMap:
      case '&'  => "&amp;"
      case '<'  => "&lt;"
      case '>'  => "&gt;"
      case '"'  => "&quot;"
      case '\'' => "&apos;"
      case char => String.valueOf(char)

  private def under(root: Path, segments: List[String]): Either[PublishError, Path] =
    val base   = root.toAbsolutePath.normalize
    val target = segments.foldLeft(base)(_.resolve(_)).normalize
    if target.startsWith(base) then Right(target) else Left(PublishError.Escaped(target))
end Layout
