package histos.central

import histos.Organization

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
          directory.resolve(s"$stem.jar.sha256"),
          directory.resolve(s"$stem.pom.sha256"),
        )

  def pom(artifact: Artifact): String =
    val dependencies = artifact.dependencies match
      case Nil    => ""
      case listed =>
        val rows = listed
          .map: dependency =>
            s"""    <dependency>
               |      <groupId>${dependency.group.value}</groupId>
               |      <artifactId>${dependency.name.value}</artifactId>
               |      <version>${dependency.version.value}</version>
               |    </dependency>""".stripMargin
          .mkString("\n")
        s"""
           |  <dependencies>
           |$rows
           |  </dependencies>""".stripMargin
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<project xmlns="http://maven.apache.org/POM/4.0.0">
       |  <modelVersion>4.0.0</modelVersion>
       |  <groupId>${artifact.group.value}</groupId>
       |  <artifactId>${artifact.name.value}</artifactId>
       |  <version>${artifact.revision.render}</version>
       |  <packaging>jar</packaging>$dependencies
       |</project>
       |""".stripMargin

  private def under(root: Path, segments: List[String]): Either[PublishError, Path] =
    val base   = root.toAbsolutePath.normalize
    val target = segments.foldLeft(base)(_.resolve(_)).normalize
    if target.startsWith(base) then Right(target) else Left(PublishError.Escaped(target))
end Layout
