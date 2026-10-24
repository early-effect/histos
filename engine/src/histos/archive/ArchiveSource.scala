package histos.archive

import histos.jdk.Sha256
import histos.toolchain.HostPlatform

import java.net.URI
import java.nio.file.Path
import scala.util.Try
import scala.annotation.publicInBinary
import scala.quoted.*
import zio.{IO, ZIO}

opaque type HttpsUrl = String

object HttpsUrl:
  given CanEqual[HttpsUrl, HttpsUrl] = CanEqual.derived

  def from(raw: String): Either[HttpsUrlError, HttpsUrl] =
    for
      uri <- UriText.parse(raw).left.map(_ => HttpsUrlError.Malformed(raw))
      _ <- Either.cond(
        Option(uri.getScheme).contains("https") && Option(uri.getHost)
          .exists(_.nonEmpty) &&
          Option(uri.getRawUserInfo).isEmpty && Option(
            uri.getRawFragment
          ).isEmpty &&
          (uri.getPort == -1 || (uri.getPort > 0 && uri.getPort <= 65535)) &&
          Option(uri.getRawAuthority).forall(authority =>
            !authority.endsWith(":") && !authority.contains('%')
          ),
        (),
        HttpsUrlError.Invalid(raw)
      )
    yield raw

  extension (url: HttpsUrl) def value: String = url

enum HttpsUrlError:
  case Malformed(raw: String)
  case Invalid(raw: String)

  def message: String = this match
    case Malformed(raw) => s"malformed HTTPS archive URL: $raw"
    case Invalid(raw)   => s"invalid HTTPS archive URL: $raw"

opaque type FileUrl = String

object FileUrl:
  given CanEqual[FileUrl, FileUrl] = CanEqual.derived
  inline def apply(inline raw: String): FileUrl = ${
    FileUrlLiteral.literal('raw)
  }

  def from(raw: String): Either[ArchiveSourceError, FileUrl] =
    for
      uri <- UriText.parse(raw).left.map(_ => ArchiveSourceError.Malformed(raw))
      _ <- Either.cond(
        Option(uri.getScheme).contains("file") && !uri.isOpaque && Option(
          uri.getRawPath
        ).exists(_.startsWith("/")) && Option(uri.getRawAuthority)
          .forall(_.isEmpty) && Option(uri.getRawQuery).isEmpty && Option(
          uri.getRawFragment
        ).isEmpty && !raw.startsWith("file:////") &&
          !"(?i)%(00|0[1-9a-f]|1[0-9a-f]|7f)".r.findFirstIn(raw).nonEmpty,
        (),
        ArchiveSourceError.Invalid(raw)
      )
    yield
      if raw.startsWith("file://") then raw
      else "file://" + raw.drop("file:".length)

  extension (url: FileUrl)
    def value: String = url
    def path: IO[ArchiveSourceError, Path] =
      ZIO
        .attemptBlocking(Path.of(URI(url)))
        .mapError(_ => ArchiveSourceError.PlatformPath(url))
        .flatMap(path =>
          if path.isAbsolute then ZIO.succeed(path)
          else ZIO.fail(ArchiveSourceError.PlatformPath(url))
        )

  @publicInBinary private[archive] def trusted(raw: String): FileUrl = raw

private object FileUrlLiteral:
  def literal(raw: Expr[String])(using Quotes): Expr[FileUrl] = raw.value match
    case None =>
      quotes.reflect.report.errorAndAbort(
        "FileUrl takes a literal. Use FileUrl.from at runtime.",
        raw
      )
    case Some(value) =>
      FileUrl.from(value) match
        case Left(error) =>
          quotes.reflect.report.errorAndAbort(error.message, raw)
        case Right(validated) =>
          '{ FileUrl.trusted(${ Expr(validated.value) }) }

enum ArchiveSource:
  case Local(path: Path)
  case File(url: FileUrl)
  case Remote(url: HttpsUrl)

  def render: String = this match
    case Local(path) => path.toUri.toASCIIString
    case Remote(url) => url.value
    case File(url)   => url.value

object ArchiveSource:
  def from(raw: String): Either[ArchiveSourceError, ArchiveSource] =
    if raw.startsWith("https:") then
      HttpsUrl
        .from(raw)
        .left
        .map(ArchiveSourceError.Https(_))
        .map(ArchiveSource.Remote(_))
    else FileUrl.from(raw).map(ArchiveSource.File(_))

enum ArchiveSourceError:
  case Malformed(raw: String)
  case Invalid(raw: String)
  case Https(error: HttpsUrlError)
  case PlatformPath(raw: String)

  def message: String = this match
    case Malformed(raw) => s"malformed archive source URI: $raw"
    case Invalid(raw)   =>
      s"archive source is not an absolute file or HTTPS URL: $raw"
    case Https(error)      => error.message
    case PlatformPath(raw) =>
      s"file archive URI cannot be materialized on this host: $raw"

final case class ArchivePin(source: ArchiveSource, sha256: Sha256)
final case class HostArchive(host: HostPlatform, pin: ArchivePin)

private enum UriTextError:
  case Malformed

private object UriText:
  def parse(raw: String): Either[UriTextError, URI] =
    if raw.isEmpty || raw.exists(character =>
        character <= ' ' || character >= 127
      )
    then Left(UriTextError.Malformed)
    else
      Try(URI(raw)).toEither.left
        .map(_ => UriTextError.Malformed)
        .flatMap: uri =>
          Either.cond(
            List(Option(uri.getRawPath), Option(uri.getRawQuery)).flatten
              .forall(value => !value.contains('[') && !value.contains(']')),
            uri,
            UriTextError.Malformed
          )
