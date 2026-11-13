package histos.publish

import scala.annotation.publicInBinary
import scala.quoted.*

import zio.json.*

/** A ship, an aggregate, or a module named on `publish`. Empty and spaced tokens are refused before the graph is read.
  */
opaque type PublishToken = String

object PublishToken:
  given CanEqual[PublishToken, PublishToken] = CanEqual.derived

  given JsonCodec[PublishToken] =
    JsonCodec.string.transformOrFail(
      raw => from(raw).left.map(_.message),
      _.value,
    )

  enum Error:
    case Empty
    case Spaced(text: String)

    def message: String = this match
      case Empty        => "a publish token is empty"
      case Spaced(text) => s"publish token '$text' contains whitespace"
  end Error

  inline def apply(inline raw: String): PublishToken = ${ PublishTokenMacro.literal('raw) }

  def from(raw: String): Either[Error, PublishToken] =
    if raw.isEmpty then Left(Error.Empty)
    else if raw.exists(_.isWhitespace) then Left(Error.Spaced(raw))
    else Right(trusted(raw))

  extension (token: PublishToken) def value: String = token

  @publicInBinary private[histos] def trusted(raw: String): PublishToken = raw
end PublishToken

private object PublishTokenMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[PublishToken] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("PublishToken(...) takes a literal. Use PublishToken.from at runtime.", raw)
      case Some(text) =>
        PublishToken.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ PublishToken.trusted(${ Expr(text) }) }
