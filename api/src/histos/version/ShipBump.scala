package histos.version

/** Which component `histos bump` moves. The command's default is [[BumpPart.Patch]]. */
enum BumpPart:
  case Major
  case Minor
  case Patch

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

  enum Rewrite:
    case Missing(ship: ShipName)
    case Shape(ship: ShipName)

    def message: String = this match
      case Missing(ship) => s"""histos.scala has no Ship("${ship.value}").next"""
      case Shape(ship)   => s"""Ship("${ship.value}").next is not three numbers"""
  end Rewrite

  /** Replace the integer literals of `Ship("name").next`. Other whitespace stays. */
  def rewrite(source: String, ship: ShipName, next: ShipNumber): Either[Rewrite, String] =
    val marker = s"""Ship("${ship.value}")"""
    walk(source, marker, ship, 0, Nil).flatMap:
      case Nil   => Left(Rewrite.Missing(ship))
      case spans => Right(substitute(source, spans, next))

  private def walk(
      source: String,
      marker: String,
      ship: ShipName,
      from: Int,
      acc: List[(Int, Int)],
  ): Either[Rewrite, List[(Int, Int)]] =
    val at = source.indexOf(marker, from)
    if at < 0 then Right(acc)
    else
      numbers(source, at + marker.length) match
        case None =>
          Left(Rewrite.Shape(ship))
        case Some(one :: two :: three :: Nil) =>
          walk(source, marker, ship, three._2, acc ++ List(one, two, three))
        case Some(_) =>
          Left(Rewrite.Shape(ship))

  private def substitute(source: String, spans: List[(Int, Int)], next: ShipNumber): String =
    val words = List(next.major.toString, next.minor.toString, next.patch.toString)
    val pairs = spans.grouped(3).toList.flatMap(group => group.zip(words))
    pairs.reverse.foldLeft(source): (text, pair) =>
      val ((start, end), word) = pair
      text.substring(0, start) + word + text.substring(end)

  private def numbers(source: String, at: Int): Option[List[(Int, Int)]] =
    val dot = skip(source, at)
    if !source.startsWith(".next", dot) then None
    else
      val open = skip(source, dot + ".next".length)
      if open >= source.length || source.charAt(open) != '(' then None
      else
        for
          (major, afterMajor) <- readInt(source, open + 1)
          afterComma          <- comma(source, afterMajor)
          (minor, afterMinor) <- readInt(source, afterComma)
          afterComma2         <- comma(source, afterMinor)
          (patch, _)          <- readInt(source, afterComma2)
        yield List(major, minor, patch)

  private def readInt(source: String, at: Int): Option[((Int, Int), Int)] =
    val start = skip(source, at)
    if start >= source.length || !source.charAt(start).isDigit then None
    else
      val end = digits(source, start)
      Some(((start, end), end))

  private def digits(source: String, start: Int): Int =
    def loop(at: Int): Int =
      if at < source.length && source.charAt(at).isDigit then loop(at + 1) else at
    loop(start)

  private def comma(source: String, at: Int): Option[Int] =
    val found = skip(source, at)
    if found < source.length && source.charAt(found) == ',' then Some(found + 1) else None

  private def skip(source: String, at: Int): Int =
    def loop(index: Int): Int =
      if index < source.length && source.charAt(index).isWhitespace then loop(index + 1) else index
    loop(at)
end ShipBump
