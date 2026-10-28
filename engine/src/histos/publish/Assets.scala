package histos.publish

import histos.central.TransportFault

import scala.annotation.tailrec

import heddle.client.Client
import heddle.http.{Body, MediaType, Method, QueryParams, Request, Response, Scheme, Url}
import heddle.http.header.Headers

import zio.{Chunk, Config, IO, ZIO, ZLayer}

/** Why an asset upload did not start, or did not finish. The token is not in the message. */
enum AssetError:
  case Credentials
  case Host(detail: String)
  case Repository(raw: String)
  case UnknownShip(name: String)
  case Duplicate(name: String)
  case Transport(fault: TransportFault)
  case Unauthorized
  case Status(code: Int)
  case Missing(tag: String)
  case Report

  def message: String = this match
    case Credentials       => "GITHUB_TOKEN and GITHUB_REPOSITORY are required"
    case Host(detail)      => detail
    case Repository(raw)   => s"'$raw' is not a repository"
    case UnknownShip(name) => s"ship '$name' is not in the catalog"
    case Duplicate(name)   => s"ship '$name' is named twice"
    case Transport(fault)  => AssetError.transport(fault)
    case Unauthorized      => "the release host rejected the credentials"
    case Status(code)      => s"the release host failed with HTTP $code"
    case Missing(tag)      => s"the release $tag was not found"
    case Report            => "the release did not name an upload"
end AssetError

object AssetError:
  private def transport(fault: TransportFault): String = fault match
    case TransportFault.Target(raw)                => s"could not reach the release host: '$raw' is not a target"
    case TransportFault.Connect(host, port)        => s"could not connect to the release host at $host:$port"
    case TransportFault.ConnectTimeout(host, port) => s"the release host at $host:$port did not accept a connection"
    case TransportFault.Tls(host, port)            => s"tls failed with the release host at $host:$port"
    case TransportFault.ReadTimeout(host, port)    => s"the release host at $host:$port stopped answering"
    case TransportFault.Io(host, port)             => s"the release host at $host:$port closed the exchange"
    case TransportFault.Protocol(host, port)       => s"the release host at $host:$port sent an invalid response"
    case TransportFault.Pool(host, port)           => s"no free connection to the release host at $host:$port"
    case TransportFault.Trust                      => "the release host trust material is unusable"
    case TransportFault.Body                       => "the release host response body failed"

/** `https` anywhere. `http` only on loopback. Unset is GitHub. */
opaque type AssetBase = Url

object AssetBase:
  val github: AssetBase =
    Url(heddle.http.Path.root, scheme = Some(Scheme.Https), host = Some("api.github.com"))

  val uploads: AssetBase =
    Url(heddle.http.Path.root, scheme = Some(Scheme.Https), host = Some("uploads.github.com"))

  def from(raw: String): Either[AssetError, AssetBase] =
    histos.central.PortalBase.from(raw) match
      case Right(base)                                           => Right(base.root)
      case Left(histos.central.PortalBase.Error.Malformed(text)) =>
        Left(AssetError.Host(s"'$text' is not a release host"))
      case Left(histos.central.PortalBase.Error.Insecure(host)) =>
        Left(AssetError.Host(s"http release host '$host' is not loopback"))

  extension (base: AssetBase) def root: Url = base
end AssetBase

/** `owner/name`. Not a git remote. */
opaque type AssetRepo = String

object AssetRepo:
  def from(raw: String): Either[AssetError, AssetRepo] =
    raw.split("/", -1) match
      case Array(owner, name) if part(owner) && part(name) => Right(raw)
      case _                                               => Left(AssetError.Repository(raw))

  extension (repo: AssetRepo)
    def owner: String =
      val at = repo.indexOf('/')
      if at < 0 then repo else repo.substring(0, at)

    def name: String =
      val at = repo.indexOf('/')
      if at < 0 then "" else repo.substring(at + 1)
  end extension

  private def part(raw: String): Boolean =
    raw.nonEmpty &&
      raw.charAt(0).isLetterOrDigit &&
      raw.forall(char => char.isLetterOrDigit || char == '.' || char == '_' || char == '-')
end AssetRepo

/** The bearer token. `toString` stays redacted. */
opaque type AssetToken = Config.Secret

object AssetToken:
  def apply(raw: String): AssetToken =
    Config.Secret(Chunk.fromArray(raw.toCharArray))

  extension (token: AssetToken) def render: String = new String(token.value.toArray)
end AssetToken

/** Upload one file to the release for a tag. The layer does not read the environment. */
trait Assets:
  def upload(tag: String, name: String, bytes: Chunk[Byte]): IO[AssetError, Unit]

object Assets:
  def layer(
      api: AssetBase,
      uploads: AssetBase,
      token: AssetToken,
      repository: AssetRepo,
  ): ZLayer[Any, AssetError, Assets] =
    val http =
      ZLayer.succeed(Client.Config.default) >>>
        Client.layer.mapError(error => AssetError.Transport(TransportFault.from(error)))
    http >>> ZLayer.fromFunction((client: Client) => AssetClient(api, uploads, token, repository, client))

private final class AssetClient(
    api: AssetBase,
    uploads: AssetBase,
    token: AssetToken,
    repository: AssetRepo,
    client: Client,
) extends Assets:
  private val bearer = "Bearer " + token.render

  def upload(tag: String, name: String, bytes: Chunk[Byte]): IO[AssetError, Unit] =
    lookup(tag).flatMap(id => post(id, name, bytes))

  private def lookup(tag: String): IO[AssetError, String] =
    val url = api.root / "repos" / repository.owner / repository.name / "releases" / "tags" / tag
    exchange(Method.GET, url, Body.empty).flatMap: response =>
      response.status.code match
        case 200       => named(response)
        case 404       => ZIO.fail(AssetError.Missing(tag))
        case 401 | 403 => ZIO.fail(AssetError.Unauthorized)
        case code      => ZIO.fail(AssetError.Status(code))

  /** The id is the `/releases/<id>/` segment of `upload_url`. The post uses the configured upload host. */
  private def named(response: Response): IO[AssetError, String] =
    response.body.strict match
      case Some(bytes) =>
        val body = new String(bytes.toArray, java.nio.charset.StandardCharsets.UTF_8)
        releaseId(body) match
          case Some(id) => ZIO.succeed(id)
          case None     => ZIO.fail(AssetError.Report)
      case None => ZIO.fail(AssetError.Report)

  private def post(id: String, name: String, bytes: Chunk[Byte]): IO[AssetError, Unit] =
    val url = (uploads.root / "repos" / repository.owner / repository.name / "releases" / id / "assets")
      .copy(query = QueryParams.of("name" -> name))
    val body = Body.fromBytes(bytes, Some(MediaType.OctetStream))
    exchange(Method.POST, url, body).flatMap: response =>
      response.status.code match
        case 201       => ZIO.unit
        case 401 | 403 => ZIO.fail(AssetError.Unauthorized)
        case code      => ZIO.fail(AssetError.Status(code))

  private def exchange(method: Method, url: Url, body: Body): IO[AssetError, Response] =
    val headers = Headers.empty
      .add("Authorization", bearer)
      .add("User-Agent", "histos")
      .add("Accept", "application/vnd.github+json")
    client
      .batched(Request(method, url, headers, body))
      .mapError(error => AssetError.Transport(TransportFault.from(error)))

  /** The first `"upload_url"` that is a key. A mention inside another string is skipped. */
  private def releaseId(body: String): Option[String] =
    field(body, "upload_url").flatMap(idAt)

  @tailrec
  private def field(body: String, name: String, from: Int = 0): Option[String] =
    val key = s""""$name""""
    val at  = body.indexOf(key, from)
    if at < 0 then None
    else
      quoted(body.substring(at + key.length)) match
        case None   => field(body, name, at + key.length)
        case parsed => parsed

  private def quoted(afterKey: String): Option[String] =
    val colon = afterKey.dropWhile(_.isWhitespace)
    if !colon.startsWith(":") then None
    else
      val opened = colon.drop(1).dropWhile(_.isWhitespace)
      if !opened.startsWith("\"") then None
      else
        val rest = opened.drop(1)
        val end  = rest.indexOf('"')
        if end < 0 then None else Some(rest.substring(0, end))

  private def idAt(url: String): Option[String] =
    val marker = "/releases/"
    val at     = url.indexOf(marker)
    if at < 0 then None
    else
      val rest = url.substring(at + marker.length)
      val id   = rest.takeWhile(_.isDigit)
      if id.nonEmpty && rest.drop(id.length).startsWith("/") then Some(id) else None
end AssetClient
