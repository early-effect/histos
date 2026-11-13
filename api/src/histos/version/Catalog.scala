package histos.version

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

import histos.{BuildError, HttpUrl, PomText, ProjectName, SourcePos}

/** The catalog identity of a ship. A letter, then letters, digits, `_`, or `-`. Not a project name. */
opaque type ShipName = String

object ShipName:
  given CanEqual[ShipName, ShipName] = CanEqual.derived

  inline def apply(inline raw: String): ShipName = ${
    ShipNameMacro.literal('raw)
  }

  def from(raw: String): Either[ShipNameError, ShipName] =
    raw.headOption match
      case None                           => Left(ShipNameError.Empty)
      case Some(head) if valid(head, raw) => Right(trusted(raw))
      case Some(_)                        => Left(ShipNameError.Bad(raw))

  extension (name: ShipName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): ShipName = raw

  private def valid(head: Char, raw: String): Boolean =
    head.isLetter && raw.forall(char => char.isLetterOrDigit || char == '_' || char == '-')
end ShipName

enum ShipNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a ship name is not empty"
    case Bad(raw) => s"'$raw' is not a ship name"
end ShipNameError

private object ShipNameMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ShipName] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "ShipName(...) takes a literal. Use ShipName.from at runtime.",
          raw,
        )
      case Some(value) =>
        ShipName.from(value) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ ShipName.trusted(${ Expr(value) }) }

/** The launcher and the native client one ship publishes. Absent means this ship publishes no client. */
final case class ShipToolchain(launcher: ProjectName, client: ProjectName)

/** One catalog row. The number is the next release. Sessions derive `-ci`, the snapshot, and the release. Description
  * and url replace the organization's when this ship sets them.
  */
enum Ship:
  case Line(
      shipName: ShipName,
      number: ShipNumber,
      modules: ::[ProjectName],
      described: Option[PomText] = None,
      located: Option[HttpUrl] = None,
      linked: Option[ShipToolchain] = None,
  )

  def name: ShipName = this match
    case line: Line => line.shipName

  def members: ::[ProjectName] = this match
    case line: Line => line.modules

  def description: Option[PomText] = this match
    case line: Line => line.described

  def url: Option[HttpUrl] = this match
    case line: Line => line.located

  def toolchain: Option[ShipToolchain] = this match
    case line: Line => line.linked
end Ship

/** How a build versions its ships. Empty is an application. SNAPSHOT never appears here. */
enum Catalog:
  case Empty
  case Lines(rows: ::[Ship.Line])

  def get(name: ShipName): Option[Ship] =
    ships.find(_.name == name)

  def ships: List[Ship] = this match
    case Empty       => Nil
    case Lines(rows) => rows.map(ship => ship: Ship)
end Catalog

object Catalog:
  val empty: Catalog = Empty

  /** Each row comes with the place the build wrote it, so a refusal names that place. */
  def from(rows: List[(Ship.Line, SourcePos)]): Either[BuildError, Catalog] =
    rows
      .foldLeft[Either[BuildError, List[(Ship.Line, SourcePos)]]](Right(Nil)): (acc, row) =>
        acc.flatMap: seen =>
          val (ship, at) = row
          seen.collectFirst { case (other, first) if other.name == ship.name => first } match
            case Some(first) => Left(BuildError.DuplicateShip(ship.name, first, at))
            case None        =>
              repeated(ship) match
                case Some(member) => Left(BuildError.RepeatedShipMember(ship.name, member, at))
                case None         =>
                  clash(seen.map(_._1), ship) match
                    case Some(member, first) => Left(BuildError.MemberInTwoShips(member, first, ship.name, at))
                    case None                => Right(seen :+ row)
      .map:
        case Nil           => Empty
        case first :: rest => Lines(::(first._1, rest.map(_._1)))

  private def repeated(ship: Ship): Option[ProjectName] =
    ship.members
      .foldLeft[(List[ProjectName], Option[ProjectName])]((Nil, None)):
        case ((_, found @ Some(_)), _) => (Nil, found)
        case ((seen, None), member)    =>
          if seen.contains(member) then (seen, Some(member)) else (member :: seen, None)
      ._2

  private def clash(seen: List[Ship], ship: Ship): Option[(ProjectName, ShipName)] =
    ship.members.foldLeft[Option[(ProjectName, ShipName)]](None): (found, member) =>
      found.orElse:
        seen.collectFirst:
          case other if other.members.contains(member) => (member, other.name)
end Catalog
