package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*
import _root_.scala.util.Try

import java.net.URI

/** An absolute HTTP or HTTPS URI written as publication metadata, not a network capability. */
opaque type MetadataUri = String

object MetadataUri:
  given CanEqual[MetadataUri, MetadataUri] = CanEqual.derived

  inline def apply(inline raw: String): MetadataUri = ${ MetadataUriMacro.literal('raw) }

  def from(raw: String): Either[MetadataUriError, MetadataUri] =
    if raw.exists(char => char.isWhitespace || char.isControl) then Left(MetadataUriError.Bad(raw))
    else
      Try(new URI(raw)).toEither.left
        .map(_ => MetadataUriError.Bad(raw))
        .flatMap: uri =>
          val scheme =
            Option(uri.getScheme).exists(value => value.equalsIgnoreCase("http") || value.equalsIgnoreCase("https"))
          val host = Option(uri.getHost).exists(_.nonEmpty)
          if scheme && host && uri.getPort >= -1 && uri.getPort <= 65535 then Right(trusted(raw))
          else Left(MetadataUriError.Bad(raw))

  extension (uri: MetadataUri) def value: String = uri

  @publicInBinary private[histos] def trusted(raw: String): MetadataUri = raw
end MetadataUri

enum MetadataUriError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not an absolute HTTP or HTTPS metadata URI"
end MetadataUriError

private object MetadataUriMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[MetadataUri] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort("MetadataUri(...) takes a literal. Use MetadataUri.from at runtime.", raw)
      case Some(text) =>
        MetadataUri.from(text) match
          case Left(error) => quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_)    => '{ MetadataUri.trusted(${ Expr(text) }) }
