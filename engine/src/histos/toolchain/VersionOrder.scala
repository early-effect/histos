package histos.toolchain

/** Numeric version order. A missing component is zero. A `-` pre-release sorts below its release and above the release
  * before it. A `+` build sorts above the same version without one.
  */
object VersionOrder:
  def compare(left: String, right: String): Int =
    order(parse(left, stripJavaOne = false), parse(right, stripJavaOne = false))

  def atLeast(found: String, floor: String, stripJavaOne: Boolean): Boolean =
    order(parse(found, stripJavaOne), parse(floor, stripJavaOne)) >= 0

  /** The digit runs before the first `-` or `+`, after a `-` up to the `+`, and after the `+`. */
  private final case class Parsed(release: List[Int], pre: Option[List[Int]], build: List[Int])

  private def parse(raw: String, stripJavaOne: Boolean): Parsed =
    val body           = if stripJavaOne && raw.startsWith("1.") then raw.drop(2) else raw
    val (core, build)  = body.span(_ != '+')
    val (release, pre) = core.span(_ != '-')
    Parsed(numbers(release), Option.when(pre.nonEmpty)(numbers(pre)), numbers(build))

  private def numbers(text: String): List[Int] =
    """\d+""".r.findAllIn(text).flatMap(_.toIntOption).toList

  private def order(left: Parsed, right: Parsed): Int =
    val pre = (left.pre, right.pre) match
      case (None, None)               => 0
      case (None, Some(_))            => 1
      case (Some(_), None)            => -1
      case (Some(found), Some(other)) => byComponent(found, other)
    List(byComponent(left.release, right.release), pre, byComponent(left.build, right.build))
      .find(_ != 0)
      .getOrElse(0)

  private def byComponent(left: List[Int], right: List[Int]): Int =
    val width = math.max(left.length, right.length)
    left
      .padTo(width, 0)
      .zip(right.padTo(width, 0))
      .collectFirst {
        case (found, required) if found != required => found.compare(required)
      }
      .getOrElse(0)
end VersionOrder
