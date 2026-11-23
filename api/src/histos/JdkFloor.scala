package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** Minimum JDK major a runner installs, distinct from an installed JDK version. */
opaque type JdkFloor = Int

object JdkFloor:
  given CanEqual[JdkFloor, JdkFloor] = CanEqual.derived

  inline def apply(inline raw: Int): JdkFloor = ${ JdkFloorMacro.literal('raw) }

  def from(raw: Int): Either[JdkFloorError, JdkFloor] =
    if raw > 0 then Right(trusted(raw)) else Left(JdkFloorError.NotPositive(raw))

  extension (floor: JdkFloor) def value: Int = floor

  @publicInBinary private[histos] def trusted(raw: Int): JdkFloor = raw
end JdkFloor

enum JdkFloorError:
  case NotPositive(raw: Int)

  def message: String = this match
    case NotPositive(raw) => s"$raw is not a JDK major"
end JdkFloorError

private object JdkFloorMacro:
  def literal(raw: Expr[Int])(using Quotes): Expr[JdkFloor] =
    import quotes.reflect.report
    raw.value match
      case None        => report.errorAndAbort("JdkFloor(...) takes a literal. Use JdkFloor.from at runtime.", raw)
      case Some(major) =>
        JdkFloor.from(major) match
          case Left(error) => report.errorAndAbort(error.message, raw)
          case Right(_)    => '{ JdkFloor.trusted(${ Expr(major) }) }
