package histos.central

import histos.{Developer, HttpUrl, License, Organization, PomText, Scm}

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
end DependencyVersion

enum DependencyVersionError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a dependency version"
end DependencyVersionError

/** One pom dependency. The artifact id already includes its Scala suffix. */
final case class MavenDependency(group: Organization, name: ArtifactId, version: DependencyVersion)

/** The pom fields beyond the coordinate. `name` is the artifact id and is not stored here. */
final case class PomDocument(
    description: Option[PomText] = None,
    url: Option[HttpUrl] = None,
    license: Option[License] = None,
    developer: Option[Developer] = None,
    scm: Option[Scm] = None,
)

/** Bytes of one publication. A library also carries a sources jar and a javadoc jar. */
final case class Artifact(
    group: Organization,
    name: ArtifactId,
    revision: Revision,
    jar: Chunk[Byte],
    dependencies: List[MavenDependency] = Nil,
    document: PomDocument = PomDocument(),
    sources: Option[Chunk[Byte]] = None,
    javadoc: Option[Chunk[Byte]] = None,
)

/** Why a publish filesystem call failed. The path stays on `PublishError.Io`. */
enum PublishFault:
  case Stat, Write

  def word: String = this match
    case Stat  => "stat"
    case Write => "write"
end PublishFault

enum PublishError:
  case EmptyJar
  case NotAJar
  case NotADirectory(path: Path)
  case Escaped(path: Path)
  case Io(path: Path, fault: PublishFault)
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
    case Io(path, fault)     => s"could not ${fault.word} $path"
    case Unauthorized        => "sonatype rejected the credentials"
    case NotSnapshot(error)  => error.message
    case Status(code)        => s"publish failed with HTTP $code"
    case Transport(reason)   => reason.message
    case Metadata(reason)    => reason.message
end PublishError

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
end Checksum

/** Where one artifact lands. Sources and javadoc are present only when the artifact has them. */
final case class MavenPaths(
    directory: Path,
    jar: Path,
    pom: Path,
    sources: Option[Path],
    javadoc: Option[Path],
)
