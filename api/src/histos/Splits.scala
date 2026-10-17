package histos

/** A workflow fan-out axis. The empty set is not a value of this enum. */
enum Axis:
  case Platform, ScalaVersion, Module

/**
 * How CI fans out. The empty set is Aggregate, one `./histos test` job, and it is the default.
 * A non-empty set is a matrix of those axes. There is no second renderer.
 */
final case class Splits private (axes: Set[Axis])

object Splits:
  val aggregate: Splits = new Splits(Set.empty)

  def of(axis: Axis, axes: Axis*): Splits =
    new Splits((axis :: axes.toList).toSet)
