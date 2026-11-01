package histos.central

import java.nio.charset.StandardCharsets

import scala.annotation.tailrec

import heddle.client.Client
import heddle.http.{Body, FormField, Method, Path, QueryParams, Request, Response, Scheme, Url}
import heddle.http.header.{BasicCredentials, Headers}

import zio.{Chunk, Config, Console, Duration, IO, ZIO, ZLayer, durationInt}

/** Hold polls until `VALIDATED`. Automatic polls until `PUBLISHED`. */
enum ReleaseMode:
  case Hold, Automatic

/** One Central deployment state. The wire tokens stay in the client. */
enum DeploymentState:
  case Pending, Validating, Validated, Publishing, Published, Failed

/** The id an upload body named. Empty or any whitespace is refused. */
opaque type DeploymentId = String

object DeploymentId:
  given CanEqual[DeploymentId, DeploymentId] = CanEqual.derived

  def from(raw: String): Either[PortalError, DeploymentId] =
    val text = raw.trim
    if text.isEmpty || text.exists(_.isWhitespace) then Left(PortalError.Unnamed)
    else Right(text)

  extension (id: DeploymentId) def value: String = id
end DeploymentId

/** Where a release deploy goes. `https` anywhere, `http` only on loopback. */
opaque type PortalBase = Url

object PortalBase:
  given CanEqual[PortalBase, PortalBase] = CanEqual.derived

  enum Error:
    case Malformed(raw: String)
    case Insecure(host: String)

    def message: String = this match
      case Malformed(raw) => s"'$raw' is not a portal"
      case Insecure(host) => s"http portal '$host' is not loopback"
  end Error

  val central: PortalBase =
    trusted(
      Url(
        Path.root,
        scheme = Some(Scheme.Https),
        host = Some("central.sonatype.com"),
      )
    )

  def from(raw: String): Either[Error, PortalBase] =
    val trimmed =
      raw.trim match
        case text if text.endsWith("/") => text.dropRight(1)
        case text                       => text
    Url.decode(trimmed) match
      case Left(_)    => Left(Error.Malformed(raw))
      case Right(url) =>
        (url.scheme, url.host) match
          case (Some(Scheme.Https), Some(_))                     => Right(url)
          case (Some(Scheme.Http), Some(host)) if loopback(host) => Right(url)
          case (Some(Scheme.Http), Some(host))                   => Left(Error.Insecure(host))
          case _                                                 => Left(Error.Malformed(raw))

  extension (base: PortalBase) def root: Url = base

  private[central] def trusted(url: Url): PortalBase = url

  private def loopback(host: String): Boolean =
    host == "localhost" || host == "127.0.0.1" || host == "[::1]" || host == "::1"
end PortalBase

/** The deployment an upload reached, and the state the mode was waiting for. */
final case class Deployment(id: DeploymentId, state: DeploymentState)

/** Why a portal call failed. The response body is not retained. */
enum PortalError:
  case Unauthorized
  case Status(code: Int)
  case Transport(fault: TransportFault)
  case Unnamed
  case Report
  case Failed(id: DeploymentId)

  def message: String = this match
    case Unauthorized     => "the portal rejected the credentials"
    case Status(code)     => s"the portal failed with HTTP $code"
    case Transport(fault) => PortalError.transport(fault)
    case Unnamed          => "the portal upload did not name a deployment"
    case Report           => "the portal status did not name a deployment state"
    case Failed(id)       => s"the portal deployment ${id.value} failed"
end PortalError

object PortalError:
  private def transport(fault: TransportFault): String = fault match
    case TransportFault.Target(raw)                => s"could not reach the portal: '$raw' is not a target"
    case TransportFault.Connect(host, port)        => s"could not connect to the portal at $host:$port"
    case TransportFault.ConnectTimeout(host, port) => s"the portal at $host:$port did not accept a connection"
    case TransportFault.Tls(host, port)            => s"tls failed with the portal at $host:$port"
    case TransportFault.ReadTimeout(host, port)    => s"the portal at $host:$port stopped answering"
    case TransportFault.Io(host, port)             => s"the portal at $host:$port closed the exchange"
    case TransportFault.Protocol(host, port)       => s"the portal at $host:$port sent an invalid response"
    case TransportFault.Pool(host, port)           => s"no free connection to the portal at $host:$port"
    case TransportFault.Trust                      => "the portal trust material is unusable"
    case TransportFault.Body                       => "the portal response body failed"

/** Upload, promote, and drop against one portal base. The layer does not read the environment. */
trait Portal:
  def upload(bundle: Chunk[Byte], mode: ReleaseMode): IO[PortalError, Deployment]
  def promote(id: DeploymentId): IO[PortalError, Unit]
  def drop(id: DeploymentId): IO[PortalError, Unit]

object Portal:
  private[central] val interval: Duration = 1.second

  /** Quiet status reads between progress lines. The read stays at `interval` so a terminal state is not delayed. */
  private[central] val reportEvery: Int = 30

  private[central] enum PollChange:
    case Same, Changed

  private[central] enum PollNote:
    case Say, Wait

  private[central] def beat(quiet: Int, change: PollChange): (PollNote, Int) =
    change match
      case PollChange.Changed                          => (PollNote.Say, 0)
      case PollChange.Same if quiet + 1 >= reportEvery => (PollNote.Say, 0)
      case PollChange.Same                             => (PollNote.Wait, quiet + 1)

  def layer(base: PortalBase, credentials: RepositorySecret): ZLayer[Any, PortalError, Portal] =
    val http =
      ZLayer.succeed(Client.Config.default) >>>
        Client.layer.mapError(error => PortalError.Transport(TransportFault.from(error)))
    http >>> ZLayer.fromFunction((client: Client) => PortalClient(base, credentials, client))
end Portal

private final class PortalClient(base: PortalBase, credentials: RepositorySecret, client: Client) extends Portal:
  private val token =
    "Bearer " + BasicCredentials.render(credentials.user.value, new String(credentials.password.value.toArray))

  def upload(bundle: Chunk[Byte], mode: ReleaseMode): IO[PortalError, Deployment] =
    for
      id    <- posted(bundle, mode)
      state <- until(id, ready(mode))
    yield Deployment(id, state)

  def promote(id: DeploymentId): IO[PortalError, Unit] =
    completed(Method.POST, id)

  def drop(id: DeploymentId): IO[PortalError, Unit] =
    completed(Method.DELETE, id)

  private def posted(bundle: Chunk[Byte], mode: ReleaseMode): IO[PortalError, DeploymentId] =
    val url = (base.root / "api" / "v1" / "publisher" / "upload")
      .copy(query = QueryParams.of("publishingType" -> publishingType(mode)))
    val field = FormField.Binary("bundle", bundle, filename = Some("bundle.zip"))
    Body.multipart(Chunk.single(field)).flatMap { body =>
      exchange(Method.POST, url, body).flatMap(accepted)
    }

  private def until(id: DeploymentId, ready: DeploymentState): IO[PortalError, DeploymentState] =
    poll(id, ready, None, 0)

  private def poll(
      id: DeploymentId,
      ready: DeploymentState,
      previous: Option[DeploymentState],
      quiet: Int,
  ): IO[PortalError, DeploymentState] =
    state(id).flatMap { found =>
      val change = previous match
        case Some(state) if state == found => Portal.PollChange.Same
        case _                             => Portal.PollChange.Changed
      val (note, next) = Portal.beat(quiet, change)
      val line         = note match
        case Portal.PollNote.Say  => Console.printLine(progress(id, found, ready)).orElseSucceed(())
        case Portal.PollNote.Wait => ZIO.unit
      line *>
        (if found == DeploymentState.Failed then ZIO.fail(PortalError.Failed(id))
         else if found == ready then ZIO.succeed(found)
         else ZIO.sleep(Portal.interval) *> poll(id, ready, Some(found), next))
    }

  private def progress(id: DeploymentId, found: DeploymentState, ready: DeploymentState): String =
    val token = wire(found)
    if found == DeploymentState.Failed || found == ready then s"portal ${id.value} is $token"
    else s"portal ${id.value} is $token, waiting for ${wire(ready)}"

  private def completed(method: Method, id: DeploymentId): IO[PortalError, Unit] =
    exchange(method, base.root / "api" / "v1" / "publisher" / "deployment" / id.value, Body.empty).flatMap { response =>
      response.status.code match
        case 204       => ZIO.unit
        case 401 | 403 => ZIO.fail(PortalError.Unauthorized)
        case code      => ZIO.fail(PortalError.Status(code))
    }

  private def accepted(response: Response): IO[PortalError, DeploymentId] =
    response.status.code match
      case 201       => named(response)
      case 401 | 403 => ZIO.fail(PortalError.Unauthorized)
      case code      => ZIO.fail(PortalError.Status(code))

  private def named(response: Response): IO[PortalError, DeploymentId] =
    response.body.strict match
      case Some(bytes) => ZIO.fromEither(DeploymentId.from(new String(bytes.toArray, StandardCharsets.UTF_8)))
      case None        => ZIO.fail(PortalError.Unnamed)

  private def state(id: DeploymentId): IO[PortalError, DeploymentState] =
    val url = (base.root / "api" / "v1" / "publisher" / "status")
      .copy(query = QueryParams.of("id" -> id.value))
    exchange(Method.POST, url, Body.empty).flatMap { response =>
      response.status.code match
        case 200       => reported(response)
        case 401 | 403 => ZIO.fail(PortalError.Unauthorized)
        case code      => ZIO.fail(PortalError.Status(code))
    }

  private def reported(response: Response): IO[PortalError, DeploymentState] =
    response.body.strict match
      case Some(bytes) =>
        stateOf(new String(bytes.toArray, StandardCharsets.UTF_8)) match
          case Some(found) => ZIO.succeed(found)
          case None        => ZIO.fail(PortalError.Report)
      case None => ZIO.fail(PortalError.Report)

  private def exchange(method: Method, url: Url, body: Body): IO[PortalError, Response] =
    val headers = Headers.empty.add("Authorization", token).add("User-Agent", "histos")
    client
      .batched(Request(method, url, headers, body))
      .mapError(error => PortalError.Transport(TransportFault.from(error)))

  private def publishingType(mode: ReleaseMode): String =
    mode match
      case ReleaseMode.Hold      => "USER_MANAGED"
      case ReleaseMode.Automatic => "AUTOMATIC"

  private def ready(mode: ReleaseMode): DeploymentState =
    mode match
      case ReleaseMode.Hold      => DeploymentState.Validated
      case ReleaseMode.Automatic => DeploymentState.Published

  /** The first `"deploymentState"` that is a key. A mention inside another string is skipped. */
  @tailrec
  private def stateOf(body: String, from: Int = 0): Option[DeploymentState] =
    val key = "\"deploymentState\""
    val at  = body.indexOf(key, from)
    if at < 0 then None
    else
      valueAt(body.substring(at + key.length)) match
        case None   => stateOf(body, at + key.length)
        case parsed => parsed

  private def valueAt(afterKey: String): Option[DeploymentState] =
    val colon = afterKey.dropWhile(_.isWhitespace)
    if !colon.startsWith(":") then None
    else
      val quoted = colon.drop(1).dropWhile(_.isWhitespace)
      if !quoted.startsWith("\"") then None
      else
        val rest = quoted.drop(1)
        val end  = rest.indexOf('"')
        if end < 0 then None else parseState(rest.substring(0, end))

  private def parseState(raw: String): Option[DeploymentState] =
    raw match
      case "PENDING"    => Some(DeploymentState.Pending)
      case "VALIDATING" => Some(DeploymentState.Validating)
      case "VALIDATED"  => Some(DeploymentState.Validated)
      case "PUBLISHING" => Some(DeploymentState.Publishing)
      case "PUBLISHED"  => Some(DeploymentState.Published)
      case "FAILED"     => Some(DeploymentState.Failed)
      case _            => None

  private def wire(state: DeploymentState): String =
    state match
      case DeploymentState.Pending    => "PENDING"
      case DeploymentState.Validating => "VALIDATING"
      case DeploymentState.Validated  => "VALIDATED"
      case DeploymentState.Publishing => "PUBLISHING"
      case DeploymentState.Published  => "PUBLISHED"
      case DeploymentState.Failed     => "FAILED"
end PortalClient
