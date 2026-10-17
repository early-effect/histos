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

/** One catalog row. A line derives coordinates from a ship number. A manual row publishes the version it states. */
enum Ship:
  case Line(shipName: ShipName, number: ShipNumber, modules: ::[ProjectName])
  case Manual(shipName: ShipName, version: ManualVersion, modules: ::[ProjectName])

  def name: ShipName = this match
    case Line(shipName, _, _)   => shipName
    case Manual(shipName, _, _) => shipName

  def members: ::[ProjectName] = this match
    case Line(_, _, modules)   => modules
    case Manual(_, _, modules) => modules

/** How a build versions its ships. The two schemes are not mixed. Empty is an application. */
enum Catalog:
  case Empty
  case Lines(rows: ::[Ship.Line])
  case Manual(rows: ::[Ship.Manual])

  def get(name: ShipName): Option[Ship] =
    ships.find(_.name == name)

  def ships: List[Ship] = this match
    case Empty         => Nil
    case Lines(rows)   => rows.map(ship => ship: Ship)
    case Manual(rows)  => rows.map(ship => ship: Ship)

object Catalog:
  val empty: Catalog = Empty

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
    .flatMap(assemble)

  private def assemble(ships: List[Ship]): Either[CatalogError, Catalog] =
    val lines = ships.collect { case line: Ship.Line => line }
    val manual = ships.collect { case row: Ship.Manual => row }
    (lines, manual) match
      case (Nil, Nil)             => Right(Empty)
      case (first :: rest, Nil)   => Right(Lines(::(first, rest)))
      case (Nil, first :: rest)   => Right(Manual(::(first, rest)))
      case _                      => Left(CatalogError.MixedScheme)

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
  case MixedScheme

  def message: String = this match
    case DuplicateName(name) => s"ship '${name.value}' is declared twice"
    case RepeatedMember(ship, member) =>
      s"ship '${ship.value}' lists '${member.value}' twice"
    case InTwoShips(member, first, second) =>
      s"'${member.value}' is in ship '${first.value}' and ship '${second.value}'"
    case MixedScheme =>
      "a catalog is either early-effect ships or manual versions, and this one is both"
