package histos.central

import histos.Organization

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.security.MessageDigest

import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

/** The version text of a dependency. It is not a Histos revision: another project chose it. */
opaque type DependencyVersion = String

object DependencyVersion:
  given CanEqual[DependencyVersion, DependencyVersion] = CanEqual.derived

  private val Text = """[A-Za-z0-9][A-Za-z0-9._+-]*""".r

  def from(raw: String): Either[PublishError, DependencyVersion] =
    if Text.matches(raw) && !raw.contains("..") then Right(raw) else Left(PublishError.BadVersion(raw))

  extension (version: DependencyVersion) def value: String = version

/** One pom dependency. The artifact id already includes its Scala suffix. */
final case class MavenDependency(group: Organization, name: ArtifactId, version: DependencyVersion)

/** Bytes of one jar, at one publishable revision. */
final case class Artifact(
    group: Organization,
    name: ArtifactId,
    revision: Revision,
    jar: Chunk[Byte],
    dependencies: List[MavenDependency] = Nil,
)

enum PublishIo:
  case Directory, Write, Move

enum PublishError:
  case BadArtifact(raw: String)
  case BadVersion(raw: String)
  case EmptyJar
  case NotAJar
  case NotADirectory(path: Path)
  case Escaped(path: Path)
  case Io(op: PublishIo)

  def message: String = this match
    case BadArtifact(raw)  => s"'$raw' is not an artifact id"
    case BadVersion(raw)   => s"'$raw' is not a dependency version"
    case EmptyJar          => "a published jar is not empty"
    case NotAJar           => "a published jar is a zip"
    case NotADirectory(path) => s"$path is not a directory"
    case Escaped(path)     => s"$path leaves the repository"
    case Io(op)            => s"publish failed while ${op.toString.toLowerCase}"

final case class Published(jar: Path, pom: Path, jarSha256: String)

/** Where one artifact lands. Every path is under the repository root. */
final case class MavenPaths(directory: Path, jar: Path, pom: Path, jarSha256: Path, pomSha256: Path)

object Digest:
  def sha256(bytes: Chunk[Byte]): String =
    val digest = MessageDigest.getInstance("SHA-256")
    digest.digest(bytes.toArray).map(byte => f"${byte & 0xff}%02x").mkString

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
      case Nil => ""
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
    val base = root.toAbsolutePath.normalize
    val target = segments.foldLeft(base)(_.resolve(_)).normalize
    if target.startsWith(base) then Right(target) else Left(PublishError.Escaped(target))

trait Publisher:
  def publish(artifact: Artifact): IO[PublishError, Published]

object Publisher:
  /** A directory in Maven layout. The write is local. Nothing is uploaded. */
  def local(root: Path): ULayer[Publisher] =
    ZLayer.succeed(LocalPublisher(root))

private final class LocalPublisher(root: Path) extends Publisher:
  def publish(artifact: Artifact): IO[PublishError, Published] =
    for
      _      <- directory
      jar    <- accept(artifact.jar)
      paths  <- ZIO.fromEither(Layout.paths(root, artifact))
      pom     = Chunk.fromArray(Layout.pom(artifact).getBytes(java.nio.charset.StandardCharsets.UTF_8))
      digest  = Digest.sha256(jar)
      _      <- place(paths.jar, jar)
      _      <- place(paths.pom, pom)
      _      <- place(paths.jarSha256, text(digest))
      _      <- place(paths.pomSha256, text(Digest.sha256(pom)))
    yield Published(paths.jar, paths.pom, digest)

  private def directory: IO[PublishError, Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) && !Files.isDirectory(root) then Left(PublishError.NotADirectory(root))
      else Right(())
    }.mapError(_ => PublishError.Io(PublishIo.Directory)).flatMap(ZIO.fromEither)

  private def accept(jar: Chunk[Byte]): IO[PublishError, Chunk[Byte]] =
    jar match
      case Chunk() => ZIO.fail(PublishError.EmptyJar)
      case bytes if zip(bytes) => ZIO.succeed(bytes)
      case _ => ZIO.fail(PublishError.NotAJar)

  private def zip(bytes: Chunk[Byte]): Boolean =
    bytes.take(2) match
      case first +: second +: Chunk() => first == 0x50.toByte && second == 0x4b.toByte
      case _                          => false

  private def text(hex: String): Chunk[Byte] =
    Chunk.fromArray(s"$hex\n".getBytes(java.nio.charset.StandardCharsets.UTF_8))

  private def place(path: Path, bytes: Chunk[Byte]): IO[PublishError, Unit] =
    val parent = Option(path.getParent).getOrElse(path)
    val fileName = Option(path.getFileName).map(_.toString).getOrElse("artifact")
    val temp = parent.resolve(s"$fileName.tmp")
    ZIO.acquireReleaseWith(ZIO.succeed(temp))(leftover =>
      ZIO.attemptBlocking(Files.deleteIfExists(leftover)).ignore
    ) { _ =>
      ZIO.attemptBlocking {
        Files.createDirectories(parent)
        Files.write(temp, bytes.toArray)
        try Files.move(temp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch
          case _: AtomicMoveNotSupportedException =>
            Files.move(temp, path, StandardCopyOption.REPLACE_EXISTING)
        ()
      }.mapError(_ => PublishError.Io(PublishIo.Write))
    }
