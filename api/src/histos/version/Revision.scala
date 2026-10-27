package histos.version

/** `<ship>-ci`. publishLocal writes it. Snapshots refuse it. */
opaque type CiVersion = ShipNumber

object CiVersion:
  given CanEqual[CiVersion, CiVersion] = CanEqual.derived

  def apply(number: ShipNumber): CiVersion = number

  extension (version: CiVersion)
    def ship: ShipNumber = version
    def render: String   = s"${ship.render}-ci"
end CiVersion

/** The bare ship number, compiled and published by a release session. */
opaque type ReleaseVersion = ShipNumber

object ReleaseVersion:
  given CanEqual[ReleaseVersion, ReleaseVersion] = CanEqual.derived

  def apply(number: ShipNumber): ReleaseVersion = number

  extension (version: ReleaseVersion)
    def ship: ShipNumber = version
    def render: String   = ship.render
end ReleaseVersion

/** `<ship>-SNAPSHOT`. The default branch updates it. It is not a dependency and it is not a pin. */
opaque type SnapshotPointer = ShipNumber

object SnapshotPointer:
  given CanEqual[SnapshotPointer, SnapshotPointer] = CanEqual.derived

  def apply(number: ShipNumber): SnapshotPointer = number

  extension (pointer: SnapshotPointer)
    def ship: ShipNumber = pointer
    def render: String   = s"${ship.render}-SNAPSHOT"
end SnapshotPointer

/** `<ship>-<12 hex>-SNAPSHOT`. The coordinate a registry stores and a catalog pins. */
final case class SnapshotCoordinate(ship: ShipNumber, abbrev: AbbrevSha):
  def render: String = s"${ship.render}-${abbrev.value}-SNAPSHOT"

/** A revision Histos recognizes. `-ci` and the pointer are not pins. */
enum Revision:
  case Ci(version: CiVersion)
  case Snapshot(coordinate: SnapshotCoordinate)
  case Pointer(pointer: SnapshotPointer)
  case Release(version: ReleaseVersion)

object Revision:
  /** Stored snapshots are recognized before the pointer, so `<ship>-<sha>-SNAPSHOT` is not eaten as `<ship>-SNAPSHOT`.
    */
  def parse(raw: String): Either[RevisionError, Revision] =
    if raw.isEmpty then Left(RevisionError.Empty)
    else if raw.endsWith("-SNAPSHOT") then snapshot(raw, raw.stripSuffix("-SNAPSHOT"))
    else if raw.endsWith("-ci") then compileOnly(raw, raw.stripSuffix("-ci"))
    else
      ShipNumber.from(raw) match
        case Right(number) => Right(Revision.Release(ReleaseVersion(number)))
        case Left(_)       => unstored(raw)

  private def snapshot(raw: String, body: String): Either[RevisionError, Revision] =
    ShipNumber.from(body) match
      case Right(number) => Right(Revision.Pointer(SnapshotPointer(number)))
      case Left(_)       =>
        splitSha(body) match
          case Right(ship, abbrev)                  => Right(Revision.Snapshot(SnapshotCoordinate(ship, abbrev)))
          case Left(AbbrevShaError.TooShort(short)) => Left(RevisionError.ShortSha(short))
          case Left(_)                              => Left(RevisionError.NotARevision(raw))

  private def compileOnly(raw: String, body: String): Either[RevisionError, Revision] =
    ShipNumber.from(body) match
      case Right(number) => Right(Revision.Ci(CiVersion(number)))
      case Left(_)       => Left(RevisionError.NotARevision(raw))

  private def unstored(raw: String): Either[RevisionError, Revision] =
    splitSha(raw) match
      case Right(ship, abbrev)                  => Left(RevisionError.Unstored(ship, abbrev))
      case Left(AbbrevShaError.TooShort(short)) => Left(RevisionError.ShortSha(short))
      case Left(_)                              => Left(RevisionError.NotARevision(raw))

  private def splitSha(raw: String): Either[AbbrevShaError, (ShipNumber, AbbrevSha)] =
    val at = raw.lastIndexOf('-')
    if at <= 0 then Left(AbbrevShaError.NotAbbrev(raw))
    else
      ShipNumber.from(raw.substring(0, at)) match
        case Left(_)     => Left(AbbrevShaError.NotAbbrev(raw))
        case Right(ship) => AbbrevSha.from(raw.substring(at + 1)).map(ship -> _)
end Revision

enum RevisionError:
  case Empty
  case ShortSha(raw: String)
  case Unstored(ship: ShipNumber, abbrev: AbbrevSha)
  case NotARevision(raw: String)

  def message: String = this match
    case Empty         => "a revision is not empty"
    case ShortSha(raw) =>
      s"'$raw' is shorter than 12 hex characters. A snapshot sha is the 12-character abbreviation of the commit."
    case Unstored(ship, abbrev) =>
      s"${ship.render}-${abbrev.value} is not stored anywhere. Pin ${ship.render}-${abbrev.value}-SNAPSHOT."
    case NotARevision(raw) => s"'$raw' is not a revision"
end RevisionError

/** What a downstream catalog may pin: a stored snapshot, or a release. */
enum Pin:
  case Snapshot(coordinate: SnapshotCoordinate)
  case Release(version: ReleaseVersion)

  def render: String = this match
    case Snapshot(coordinate) => coordinate.render
    case Release(version)     => version.render
end Pin

object Pin:
  def from(raw: String): Either[PinError, Pin] =
    Revision.parse(raw) match
      case Right(Revision.Snapshot(coordinate))       => Right(Pin.Snapshot(coordinate))
      case Right(Revision.Release(version))           => Right(Pin.Release(version))
      case Right(Revision.Pointer(pointer))           => Left(PinError.Pointer(pointer))
      case Right(Revision.Ci(version))                => Left(PinError.CompileOnly(version))
      case Left(RevisionError.Empty)                  => Left(PinError.Empty)
      case Left(RevisionError.ShortSha(raw))          => Left(PinError.ShortSha(raw))
      case Left(RevisionError.Unstored(ship, abbrev)) => Left(PinError.Unstored(ship, abbrev))
      case Left(RevisionError.NotARevision(raw))      => Left(PinError.NotAPin(raw))

enum PinError:
  case Empty
  case Pointer(pointer: SnapshotPointer)
  case CompileOnly(version: CiVersion)
  case ShortSha(raw: String)
  case Unstored(ship: ShipNumber, abbrev: AbbrevSha)
  case NotAPin(raw: String)

  def message: String = this match
    case Empty                => "a pin is not empty"
    case Pointer(pointer)     => s"${pointer.render} is the snapshot pointer, not a build"
    case CompileOnly(version) => s"${version.render} is the compile version, not a pin"
    case ShortSha(raw)        =>
      s"'$raw' is shorter than 12 hex characters. A snapshot sha is the 12-character abbreviation of the commit."
    case Unstored(ship, abbrev) =>
      s"${ship.render}-${abbrev.value} is not stored anywhere. Pin ${ship.render}-${abbrev.value}-SNAPSHOT."
    case NotAPin(raw) => s"'$raw' is not a pin"
end PinError
