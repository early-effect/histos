package histos.toolchain

import histos.jdk.JdkVersion
import histos.plugin.{ArtifactPath, ArtifactPathError, ContentHash, EntryName}
import histos.workspace.IoFailure
import histos.{Dep, ScalaVersion, ScalaVersionError}

import java.nio.file.Path

/** One role in the host distribution. A missing role is not filled from a project lock. */
enum HostToolRole:
  case Compiler
  case Bridge
  case Interfaces
  case Runtime
  case Support

/** One ordered tool inside the host distribution. `relative` is the path under `bootstrap/compiler`. */
final case class HostToolEntry(
    role: HostToolRole,
    coordinate: Dep,
    digest: ContentHash,
    relative: ArtifactPath,
)

/** The compiler payload packaged with the host. It is not a project dependency. */
final case class HostCompilerManifest(
    version: Int,
    scalaVersion: ScalaVersion,
    tools: List[HostToolEntry],
    jdk: JdkVersion,
    fingerprint: ContentHash,
)

object HostCompilerManifest:
  val Current: Int = 1

/** One tool after the distribution payload was checked and leased. */
final case class MaterializedTool(entry: HostToolEntry, path: Path)

/** The checked compiler, bridge, support, runtime, and JDK for one host session. */
final case class HostCompilerSession(
    manifest: HostCompilerManifest,
    tools: List[MaterializedTool],
    jdkHome: Path,
    jdk: JdkVersion,
    knownStages: List[EntryName],
    fingerprint: ContentHash,
)

enum HostCompilerError:
  case ManifestAbsent
  case EntryAbsent(role: HostToolRole)
  case IncompatibleManifest(found: Int)
  case IncompatibleCompiler(found: ScalaVersion, required: ScalaVersion)
  case ConflictingRole(role: HostToolRole)
  case BadDigest(role: HostToolRole, expected: ContentHash, actual: ContentHash)

  /** The manifest parsed, and its fingerprint does not match the signed entries. No tool jar was hashed. */
  case Fingerprint(recorded: ContentHash, computed: ContentHash)
  case ManifestUnreadable
  case UnsupportedJdk(found: JdkVersion, required: JdkVersion)

  /** `ArtifactPath.from` refused the packaged entry name. This is not a file read. */
  case BadEntry(error: ArtifactPathError)

  /** `ScalaVersion.from` refused the embedded compiler or the compiler coordinate. This is not a file read. */
  case BadRevision(error: ScalaVersionError)
  case Io(failure: IoFailure)

  def message: String = this match
    case ManifestAbsent              => "the host distribution has no compiler manifest"
    case EntryAbsent(role)           => s"the host compiler manifest has no $role"
    case IncompatibleManifest(found) =>
      s"host compiler manifest version $found is not ${HostCompilerManifest.Current}"
    case IncompatibleCompiler(found, required) =>
      s"host compiler ${found.value} does not match ${required.value}"
    case ConflictingRole(role)           => s"host compiler role $role is repeated"
    case BadDigest(role, _, _)           => s"host compiler $role does not match its digest"
    case Fingerprint(_, _)               => "the host compiler manifest fingerprint does not match its entries"
    case ManifestUnreadable              => "the host compiler manifest could not be read"
    case UnsupportedJdk(found, required) =>
      s"JDK ${found.value} does not satisfy ${required.value}"
    case BadEntry(error)    => error.message
    case BadRevision(error) => error.message
    case Io(failure)        => failure.message
end HostCompilerError
