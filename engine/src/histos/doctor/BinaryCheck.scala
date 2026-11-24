package histos.doctor

import histos.version.{BumpPart, ShipNumber}

/** Early-semver bump policy for independently collected backward binary compatibility evidence. */
object BinaryCheck:
  enum Difference:
    case None
    case Break(part: BumpPart, count: Int)

  /** On a `0.x` line any problem is a minor bump. On `1.x` and later a missing class is major and any other problem is
    * minor.
    */
  def part(number: ShipNumber, problems: ::[CompatibilityProblem]): BumpPart =
    val missing = problems.exists:
      case CompatibilityProblem.MissingClass(_)                              => true
      case CompatibilityProblem.Template(_) | CompatibilityProblem.Member(_) => false
    if number.major == 0 then BumpPart.Minor
    else if missing then BumpPart.Major
    else BumpPart.Minor

  def difference(number: ShipNumber, result: CompatibilityResult): Difference =
    result match
      case CompatibilityResult.Compatible             => Difference.None
      case CompatibilityResult.Incompatible(problems) => Difference.Break(part(number, problems), problems.size)

  /** The catalog number already includes the component a break requires. */
  def covers(previous: ShipNumber, catalog: ShipNumber, part: BumpPart): Boolean =
    part match
      case BumpPart.Major => catalog.major > previous.major
      case BumpPart.Minor =>
        catalog.major > previous.major || (catalog.major == previous.major && catalog.minor > previous.minor)
      case BumpPart.Patch => Ordering[ShipNumber].gt(catalog, previous)
end BinaryCheck
