package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** One VM option, never a main class, classpath selector, or program argument. */
opaque type JvmFlag = String

object JvmFlag:
  given CanEqual[JvmFlag, JvmFlag] = CanEqual.derived

  inline def apply(inline raw: String): JvmFlag = ${ JvmFlagMacro.literal('raw) }

  def from(raw: String): Either[JvmFlagError, JvmFlag] =
    if raw.isEmpty then Left(JvmFlagError.Empty)
    else if raw.exists(_.isWhitespace) || raw.exists(_.isControl) then Left(JvmFlagError.Malformed(raw))
    else if raw.startsWith("-Xbootclasspath") then Left(JvmFlagError.Malformed(raw))
    else if raw.startsWith("-X") || raw.startsWith("-ea") || raw.startsWith("-da") || raw.startsWith("--add-opens=") ||
      raw.startsWith("--add-exports=") || raw.startsWith("--enable-native-access=")
    then Right(trusted(raw))
    else Left(JvmFlagError.Malformed(raw))

  extension (flag: JvmFlag) def value: String = flag

  @publicInBinary private[histos] def trusted(raw: String): JvmFlag = raw
end JvmFlag

enum JvmFlagError:
  case Empty
  case Malformed(raw: String)

  def message: String = this match
    case Empty          => "a JVM flag is not empty"
    case Malformed(raw) => s"'$raw' is not a standalone JVM flag; properties use SystemProperty"
end JvmFlagError

/** The launcher puts these before the bootstrap main. Application arguments remain separate. */
final case class JvmOptions(flags: List[JvmFlag] = Nil, properties: List[SystemProperty] = Nil):
  def arguments: List[String] = flags.map(_.value) ++ properties.map(_.argument)

object JvmOptions:
  val empty: JvmOptions = JvmOptions()

private object JvmFlagMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[JvmFlag] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("JvmFlag(...) takes a literal. Use JvmFlag.from at runtime.", raw)
      case Some(text) =>
        JvmFlag.from(text) match
          case Left(error) => report.errorAndAbort(error.message, raw)
          case Right(_)    => '{ JvmFlag.trusted(${ Expr(text) }) }
