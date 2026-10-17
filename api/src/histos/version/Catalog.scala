package histos.version

import histos.ProjectName

/** The catalog identity of a ship. A letter, then letters, digits, `_`, or `-`. Not a project name. */
opaque type ShipName = String

object ShipName:
  given CanEqual[ShipName, ShipName] = CanEqual.derived

  def from(raw: String): Either[ShipNameError, ShipName] =
    raw.headOption match
      case None                           => Left(ShipNameError.Empty)
      case Some(head) if valid(head, raw) => Right(raw)
      case Some(_)                        => Left(ShipNameError.Bad(raw))

  extension (name: ShipName) def value: String = name

  private def valid(head: Char, raw: String): Boolean =
    head.isLetter && raw.forall(char => char.isLetterOrDigit || char == '_' || char == '-')

enum ShipNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty     => "a ship name is not empty"
    case Bad(raw)  => s"'$raw' is not a ship name"

/** One catalog row: the next release number, and the modules that publish it. */
final case class Ship(name: ShipName, number: ShipNumber, members: ::[ProjectName])

/** The ships a build publishes. Empty is an application, not a library catalog. */
final case class Catalog private (ships: List[Ship]):
  def get(name: ShipName): Option[Ship] =
    ships.find(_.name == name)

object Catalog:
  val empty: Catalog = new Catalog(Nil)

  /** The caller proved names are unique and each module is in at most one ship. */
  private[histos] def trusted(ships: List[Ship]): Catalog = new Catalog(ships)

  def from(ships: List[Ship]): Either[CatalogError, Catalog] =
    ships.foldLeft[Either[CatalogError, List[Ship]]](Right(Nil)): (acc, ship) =>
      acc.flatMap: seen =>
        if seen.exists(_.name == ship.name) then Left(CatalogError.DuplicateName(ship.name))
        else
          repeated(ship) match
            case Some(member) => Left(CatalogError.RepeatedMember(ship.name, member))
            case None =>
              clash(seen, ship) match
                case Some(member, first) => Left(CatalogError.InTwoShips(member, first, ship.name))
                case None                => Right(seen :+ ship)
    .map(checked => new Catalog(checked))

  private def repeated(ship: Ship): Option[ProjectName] =
    ship.members.foldLeft[(List[ProjectName], Option[ProjectName])]((Nil, None)):
      case ((_, found @ Some(_)), _) => (Nil, found)
      case ((seen, None), member)    =>
        if seen.contains(member) then (seen, Some(member)) else (member :: seen, None)
    ._2

  private def clash(seen: List[Ship], ship: Ship): Option[(ProjectName, ShipName)] =
    ship.members.foldLeft[Option[(ProjectName, ShipName)]](None): (found, member) =>
      found.orElse:
        seen.collectFirst:
          case other if other.members.contains(member) => (member, other.name)

enum CatalogError:
  case DuplicateName(name: ShipName)
  case RepeatedMember(ship: ShipName, member: ProjectName)
  case InTwoShips(member: ProjectName, first: ShipName, second: ShipName)

  def message: String = this match
    case DuplicateName(name) => s"ship '${name.value}' is declared twice"
    case RepeatedMember(ship, member) =>
      s"ship '${ship.value}' lists '${member.value}' twice"
    case InTwoShips(member, first, second) =>
      s"'${member.value}' is in ship '${first.value}' and ship '${second.value}'"
