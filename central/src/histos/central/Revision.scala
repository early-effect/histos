package histos.central

import histos.version.{ManualVersion, ReleaseVersion, SnapshotCoordinate, SnapshotPointer}

/** A revision a repository may store. `<ship>-ci` is not one of these. */
enum Revision:
  case Release(version: ReleaseVersion)
  case Snapshot(coordinate: SnapshotCoordinate)
  case Pointer(pointer: SnapshotPointer)
  case Manual(version: ManualVersion)

  def render: String = this match
    case Release(version)       => version.render
    case Snapshot(coordinate)   => coordinate.render
    case Pointer(pointer)       => pointer.render
    case Manual(version)        => version.value

/** A Maven group. Dotted segments, each starting with a letter. `..` is not a group. */
opaque type GroupId = String

object GroupId:
  given CanEqual[GroupId, GroupId] = CanEqual.derived

  private val Segment = """[A-Za-z][A-Za-z0-9_-]*""".r

  def from(raw: String): Either[PublishError, GroupId] =
    val parts = raw.split("\\.", -1)
    if raw.nonEmpty && parts.forall(Segment.matches) then Right(raw)
    else Left(PublishError.BadGroup(raw))

  extension (group: GroupId)
    def value: String = group
    def segments: List[String] = group.split("\\.", -1).toList

/** A Maven artifact id. One path segment, not a group. */
opaque type ArtifactId = String

object ArtifactId:
  given CanEqual[ArtifactId, ArtifactId] = CanEqual.derived

  private val Name = """[A-Za-z][A-Za-z0-9_.-]*""".r

  def from(raw: String): Either[PublishError, ArtifactId] =
    if raw.nonEmpty && !raw.contains("..") && Name.matches(raw) then Right(raw)
    else Left(PublishError.BadArtifact(raw))

  extension (name: ArtifactId) def value: String = name
