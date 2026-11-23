package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** One JVM system property a test child receives, such as `chekhov.browsers=firefox`. The name and the value are both
  * part of the value. A space, or a missing `=`, is not a property.
  */
opaque type SystemProperty = String

object SystemProperty:
  given CanEqual[SystemProperty, SystemProperty] = CanEqual.derived

  private val Shape = """[A-Za-z][A-Za-z0-9._-]*=[^\s]*""".r

  /** A literal (`SystemProperty("chekhov.browsers=firefox")`), refused at compile time when [[from]] refuses it. */
  inline def apply(inline raw: String): SystemProperty = ${ SystemPropertyMacro.literal('raw) }

  def from(raw: String): Either[SystemPropertyError, SystemProperty] =
    if raw.isEmpty then Left(SystemPropertyError.Empty)
    else if Shape.matches(raw) then Right(trusted(raw))
    else Left(SystemPropertyError.Malformed(raw))

  extension (property: SystemProperty)
    def value: String = property

    /** The argument a JVM child receives. */
    def argument: String = s"-D$property"
  end extension

  @publicInBinary private[histos] def trusted(raw: String): SystemProperty = raw
end SystemProperty

enum SystemPropertyError:
  case Empty
  case Malformed(raw: String)

  def message: String = this match
    case Empty          => "a system property is not empty"
    case Malformed(raw) => s"'$raw' is not a system property (name=value, with no spaces)"
end SystemPropertyError

private object SystemPropertyMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[SystemProperty] =
    import quotes.reflect.report
    raw.value match
      case None =>
        report.errorAndAbort("SystemProperty(...) takes a literal. Use SystemProperty.from at runtime.", raw)
      case Some(text) =>
        SystemProperty.from(text) match
          case Left(error) => report.errorAndAbort(error.message, raw)
          case Right(_)    => '{ SystemProperty.trusted(${ Expr(text) }) }
