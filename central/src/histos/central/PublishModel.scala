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

  def from(raw: String): Either[DependencyVersionError, DependencyVersion] =
    if Text.matches(raw) && !raw.contains("..") then Right(raw) else Left(DependencyVersionError.Bad(raw))

  extension (version: DependencyVersion) def value: String = version

enum DependencyVersionError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a dependency version"

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
  case EmptyJar
  case NotAJar
  case NotADirectory(path: Path)
  case Escaped(path: Path)
  case Io(op: PublishIo)
  case Unauthorized
  case NotSnapshot(error: SnapshotRevisionError)
  case Status(code: Int)
  case Transport(reason: TransportFault)
  case Metadata(reason: MetadataFault)

  def message: String = this match
    case EmptyJar            => "a published jar is not empty"
    case NotAJar             => "a published jar is a zip"
    case NotADirectory(path) => s"$path is not a directory"
    case Escaped(path)       => s"$path leaves the repository"
    case Io(op)              => s"publish failed while ${op.toString.toLowerCase}"
    case Unauthorized        => "sonatype rejected the credentials"
    case NotSnapshot(error)  => error.message
    case Status(code)        => s"publish failed with HTTP $code"
    case Transport(reason)   => reason.message
    case Metadata(reason)    => reason.message

/** A directory publish names the files it wrote. A snapshot publish names the unique version it uploaded. */
enum Published:
  case Local(jar: Path, pom: Path, sha256: Checksum)
  case Snapshot(unique: SnapshotVersion, sha256: Checksum)

/** SHA-256 of a published jar, lowercase hex. */
opaque type Checksum = String

object Checksum:
  given CanEqual[Checksum, Checksum] = CanEqual.derived

  def sha256(bytes: Chunk[Byte]): Checksum = Digest.sha256(bytes)

  extension (sum: Checksum) def value: String = sum

/** Where one artifact lands. Every path is under the repository root. */
final case class MavenPaths(directory: Path, jar: Path, pom: Path, jarSha256: Path, pomSha256: Path)

