package histos.central

import histos.version.{CiVersion, ManualVersion, ReleaseVersion, SnapshotCoordinate, SnapshotPointer}

/** A revision a repository may store. `<ship>-ci` is written by a local publish. Snapshots refuse it. */
enum Revision:
  case Ci(version: CiVersion)
  case Release(version: ReleaseVersion)
  case Snapshot(coordinate: SnapshotCoordinate)
  case Pointer(pointer: SnapshotPointer)
  case Manual(version: ManualVersion)

  def render: String = this match
    case Ci(version)          => version.render
    case Release(version)     => version.render
    case Snapshot(coordinate) => coordinate.render
    case Pointer(pointer)     => pointer.render
    case Manual(version)      => version.value
end Revision

/** A Maven artifact id. One path segment, not a group. */
opaque type ArtifactId = String

object ArtifactId:
  given CanEqual[ArtifactId, ArtifactId] = CanEqual.derived

  private val Name = """[A-Za-z][A-Za-z0-9_.-]*""".r

  def from(raw: String): Either[ArtifactIdError, ArtifactId] =
    if raw.nonEmpty && !raw.contains("..") && Name.matches(raw) then Right(raw)
    else Left(ArtifactIdError.Bad(raw))

  extension (name: ArtifactId) def value: String = name
end ArtifactId

enum ArtifactIdError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not an artifact id"
end ArtifactIdError
