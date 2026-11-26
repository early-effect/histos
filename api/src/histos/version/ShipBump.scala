package histos.version

/** Which component `histos bump` moves. The command's default is [[BumpPart.Patch]]. */
enum BumpPart:
  case Major
  case Minor
  case Patch

  def token: String = this match
    case Major => "major"
    case Minor => "minor"
    case Patch => "patch"
end BumpPart

/** Where Coursier found a stable. A maven repository wins over ivy2 local. */
enum StablePlace:
  case Remote
  case Local

  def word: String = this match
    case Remote => "maven"
    case Local  => "ivy2 local"
end StablePlace

/** One stable `coursier.Versions` listed, and the repository it came from. */
final case class ListedStable(number: ShipNumber, place: StablePlace)

/** What one ship should do. The catalog moves only when a stable has caught `.next`. */
enum ShipMove:
  case Ahead(catalog: ShipNumber, published: ShipNumber, place: StablePlace)
  case Due(published: ShipNumber, place: StablePlace, next: ShipNumber)
  case Unpublished

object ShipBump:
  /** A remote stable wins. Ivy2 local is the answer only when no maven repository listed one. */
  def choose(listed: List[ListedStable]): Option[ListedStable] =
    val remote = listed.filter(_.place == StablePlace.Remote)
    val pool   = if remote.nonEmpty then remote else listed.filter(_.place == StablePlace.Local)
    pool.maxByOption(_.number)

  def move(catalog: ShipNumber, chosen: Option[ListedStable], part: BumpPart): ShipMove =
    chosen match
      case None =>
        ShipMove.Unpublished
      case Some(stable) if Ordering[ShipNumber].lt(stable.number, catalog) =>
        ShipMove.Ahead(catalog, stable.number, stable.place)
      case Some(stable) =>
        ShipMove.Due(stable.number, stable.place, advance(stable.number, part))

  /** The number after `number`. Minor and major reset the smaller components. */
  def advance(number: ShipNumber, part: BumpPart): ShipNumber =
    part match
      case BumpPart.Patch => ShipNumber.trusted(number.major, number.minor, number.patch + 1)
      case BumpPart.Minor => ShipNumber.trusted(number.major, number.minor + 1, 0)
      case BumpPart.Major => ShipNumber.trusted(number.major + 1, 0, 0)

  /** Why `.next` could not be rewritten. */
  enum Rewrite:
    case Missing(ship: ShipName)
    case Shape(ship: ShipName)
    case Unparsed(reason: String)

    def message: String = this match
      case Missing(ship)    => s"""Ship("${ship.value}") has no .next(major, minor, patch) call"""
      case Shape(ship)      => s"""Ship("${ship.value}").next arguments are not three integer literals"""
      case Unparsed(reason) => s"histos.scala did not parse: $reason"
  end Rewrite
end ShipBump
