package histos.central

import java.util.UUID

import scala.annotation.publicInBinary
import scala.quoted.*

import zio.json.*

/** The id an upload body named, or the one `publish promote` and `publish drop` take. The ends are trimmed. An empty id
  * or whitespace inside it is refused.
  */
opaque type DeploymentId = String

object DeploymentId:
  given CanEqual[DeploymentId, DeploymentId] = CanEqual.derived

  given JsonCodec[DeploymentId] =
    JsonCodec.string.transformOrFail(
      raw => from(raw).left.map(_.message),
      _.value,
    )

  enum Error:
    case Empty
    case Spaced(text: String)

    def message: String = this match
      case Empty        => "a deployment id is empty"
      case Spaced(text) => s"deployment id '$text' contains whitespace"
  end Error

  inline def apply(inline raw: String): DeploymentId = ${ DeploymentIdMacro.literal('raw) }

  def from(raw: String): Either[Error, DeploymentId] =
    val text = raw.trim
    if text.isEmpty then Left(Error.Empty)
    else if text.exists(_.isWhitespace) then Left(Error.Spaced(text))
    else Right(trusted(text))

  /** A UUID's text is never empty and has no whitespace. */
  def of(uuid: UUID): DeploymentId = uuid.toString

  extension (id: DeploymentId) def value: String = id

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[central] def trusted(raw: String): DeploymentId = raw
end DeploymentId

private object DeploymentIdMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[DeploymentId] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("DeploymentId(...) takes a literal. Use DeploymentId.from at runtime.", raw)
      case Some(text) =>
        DeploymentId.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(id) => '{ DeploymentId.trusted(${ Expr(id.value) }) }
