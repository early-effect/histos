package histos.version

import _root_.scala.annotation.publicInBinary

/** The next release: `major.minor.patch`, with no leading zeros. `01.2.3` is not `1.2.3`. */
final case class ShipNumber private (major: Int, minor: Int, patch: Int):
  def render: String = s"$major.$minor.$patch"

object ShipNumber:
  given CanEqual[ShipNumber, ShipNumber] = CanEqual.derived

  /** A literal (`ShipNumber("1.4.2")`), rejected at compile time when it is not a ship number. */
  inline def apply(inline raw: String): ShipNumber = ${ ShipNumberMacro.literal('raw) }

  def from(raw: String): Either[ShipNumberError, ShipNumber] =
    if raw.isEmpty then Left(ShipNumberError.Empty)
    else
      raw.split("\\.", -1) match
        case Array(major, minor, patch) =>
          (component(major), component(minor), component(patch)) match
            case (Some(parsedMajor), Some(parsedMinor), Some(parsedPatch)) =>
              Right(trusted(parsedMajor, parsedMinor, parsedPatch))
            case _ => Left(ShipNumberError.NotAShip(raw))
        case _ => Left(ShipNumberError.NotAShip(raw))

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(major: Int, minor: Int, patch: Int): ShipNumber =
    new ShipNumber(major, minor, patch)

  private def component(raw: String): Option[Int] =
    if raw.isEmpty then None
    else if raw.length > 1 && raw.charAt(0) == '0' then None
    else if raw.length > 10 || !raw.forall(_.isDigit) then None
    else raw.toLongOption.filter(value => value >= 0 && value <= Int.MaxValue).map(_.toInt)
end ShipNumber

enum ShipNumberError:
  case Empty
  case NotAShip(raw: String)

  def message: String = this match
    case Empty         => "a ship number is not empty"
    case NotAShip(raw) => s"'$raw' is not a ship number. A ship number is major.minor.patch with no leading zeros."
end ShipNumberError
