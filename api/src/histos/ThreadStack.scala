package histos

import _root_.scala.annotation.publicInBinary

/** The stack every thread of a Native binary gets, such as `8m`: a byte count, or a count of `k` or `m`, from `64k` to
  * `1024m`. Histos hands it to the binary under `histos run` and `histos test`.
  */
opaque type ThreadStack = String

object ThreadStack:
  given CanEqual[ThreadStack, ThreadStack] = CanEqual.derived

  private val Shape = """(\d+)([km]?)""".r
  private val Least = 64L * 1024
  private val Most  = 1024L * 1024 * 1024

  /** A literal (`ThreadStack("16m")`), refused at compile time when [[from]] refuses it. */
  inline def apply(inline raw: String): ThreadStack = ${ ThreadStackMacro.literal('raw) }

  /** What a Native module gets when it names none, the Linux default. A ZIO run loop frame in a Histos link is about
    * 5.5KB, and ZIO nests 300 of them before it trampolines, so the 1MB macOS gives a thread overflows.
    */
  val default: ThreadStack = trusted("8m")

  def from(raw: String): Either[ThreadStackError, ThreadStack] =
    raw match
      case ""                 => Left(ThreadStackError.Empty)
      case Shape(count, unit) =>
        count.toLongOption.filter(_ <= Most).map(_ * scale(unit)) match
          case Some(bytes) if Least <= bytes && bytes <= Most => Right(trusted(raw))
          case _                                              => Left(ThreadStackError.OutOfRange(raw))
      case _ => Left(ThreadStackError.Malformed(raw))

  private def scale(unit: String): Long =
    unit match
      case "k" => 1024L
      case "m" => 1024L * 1024
      case _   => 1L

  extension (stack: ThreadStack) def value: String = stack

  @publicInBinary private[histos] def trusted(raw: String): ThreadStack = raw
end ThreadStack

enum ThreadStackError:
  case Empty
  case Malformed(raw: String)
  case OutOfRange(raw: String)

  def message: String = this match
    case Empty           => "a thread stack is not empty"
    case Malformed(raw)  => s"'$raw' is not a thread stack: a byte count, or a count of k or m, such as 8m"
    case OutOfRange(raw) => s"'$raw' is outside the 64k to 1024m a thread stack can be"
end ThreadStackError
