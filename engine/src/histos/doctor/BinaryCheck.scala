package histos.doctor

import histos.version.{BumpPart, ShipName, ShipNumber}

import java.nio.file.Path

/** The binary difference between a previous release and the classes this build just compiled. Previous is the first
  * argument, matching the direction a published jar is compared with a new class directory.
  */
object BinaryCheck:
  enum Difference:
    case None
    case Break(part: BumpPart, count: Int)

  /** On a `0.x` line any problem is a minor bump. On `1.x` and later a missing class is major and any other problem is
    * minor.
    */
  def part(number: ShipNumber, missingClass: Boolean): BumpPart =
    if number.major == 0 then BumpPart.Minor
    else if missingClass then BumpPart.Major
    else BumpPart.Minor

  def compare(previous: Path, current: Path, number: ShipNumber): Difference =
    val problems =
      new com.typesafe.tools.mima.lib.MiMaLib(Nil).collectProblems(previous.toFile, current.toFile, Nil)
    if problems.isEmpty then Difference.None
    else
      val missing = problems.exists(problem => problem.getClass.getSimpleName.contains("MissingClass"))
      Difference.Break(part(number, missing), problems.size)

  /** The catalog number already includes the component a break requires. */
  def covers(previous: ShipNumber, catalog: ShipNumber, part: BumpPart): Boolean =
    part match
      case BumpPart.Major => catalog.major > previous.major
      case BumpPart.Minor =>
        catalog.major > previous.major || (catalog.major == previous.major && catalog.minor > previous.minor)
      case BumpPart.Patch => Ordering[ShipNumber].gt(catalog, previous)

  def outcome(ship: ShipName, number: ShipNumber, difference: Difference): Outcome =
    difference match
      case Difference.None =>
        Outcome.Ok(CheckId.trusted("ship.binary"), s"${ship.value} ${number.render} matches the previous release")
      case Difference.Break(bump, _) =>
        Outcome.Found(histos.version.ShipDiagnostic.BinaryBump(ship, number, bump.token))
end BinaryCheck
