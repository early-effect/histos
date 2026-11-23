package histos

/** A workflow fan-out axis. The empty set is not a value of this enum. */
enum Axis:
  case Platform, ScalaVersion, Module

/** The empty set is one aggregate job. Module/Platform axes partition indivisible test groups; Native groups may
  * coalesce overlapping module rows. ScalaVersion is refused by workflow planning until test selection supports a
  * version filter. No unsupported CLI syntax is generated.
  */
final case class Splits private (axes: Set[Axis])

object Splits:
  val aggregate: Splits = new Splits(Set.empty)

  def of(axis: Axis, axes: Axis*): Splits =
    new Splits((axis :: axes.toList).toSet)
end Splits
