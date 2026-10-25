package histos.toolchain

/** Numeric version order. A missing component is zero. A `+` build or a `-`
  * pre-release does not invert the numbers in front of it.
  */
object VersionOrder:
  def compare(left: String, right: String): Int =
    byComponent(
      components(left, stripJavaOne = false),
      components(right, stripJavaOne = false)
    )

  def atLeast(found: String, floor: String, stripJavaOne: Boolean): Boolean =
    byComponent(
      components(found, stripJavaOne),
      components(floor, stripJavaOne)
    ) >= 0

  private def components(raw: String, stripJavaOne: Boolean): List[Int] =
    val body = if stripJavaOne && raw.startsWith("1.") then raw.drop(2) else raw
    val digits = """\d+""".r.findAllIn(body).flatMap(_.toIntOption).toList
    if digits.isEmpty then List(0) else digits

  private def byComponent(left: List[Int], right: List[Int]): Int =
    val width = math.max(left.length, right.length)
    left
      .padTo(width, 0)
      .zip(right.padTo(width, 0))
      .collectFirst {
        case (found, required) if found != required => found.compare(required)
      }
      .getOrElse(0)
