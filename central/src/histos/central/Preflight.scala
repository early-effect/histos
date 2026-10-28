package histos.central

import java.nio.charset.StandardCharsets

import heddle.client.Client
import heddle.http.{Body, Method, Path, QueryParams, Request, Response, Scheme, Url}
import heddle.http.header.Headers

import zio.{Chunk, IO, ZIO, ZLayer}

/** A signing fingerprint: 16, 40, or 64 hex digits. Whitespace and a leading `0x` are ignored. */
opaque type Fingerprint = String

object Fingerprint:
  given CanEqual[Fingerprint, Fingerprint] = CanEqual.derived

  def from(raw: String): Either[PreflightError, Fingerprint] =
    val text = raw.filter(ch => !ch.isWhitespace)
    val hex  =
      if text.length >= 2 && text.substring(0, 2).equalsIgnoreCase("0x") then text.substring(2)
      else text
    if (hex.length == 16 || hex.length == 40 || hex.length == 64) && hex.forall(isHex) then Right(hex.toUpperCase)
    else Left(PreflightError.BadFingerprint)

  extension (fingerprint: Fingerprint) def value: String = fingerprint

  private def isHex(char: Char): Boolean =
    char.isDigit || (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F')
end Fingerprint

/** Why a release did not upload. Response bodies are not retained. */
enum PreflightError:
  case Portal(error: PortalBase.Error)
  case BadFingerprint
  case Coordinate(gav: Gav)
  case Published(gav: Gav)
  case Repository(gav: Gav, code: Int)
  case RepositoryDown(gav: Gav, fault: TransportFault)
  case KeyMissing(fingerprint: Fingerprint)
  case KeyStatus(code: Int)
  case KeyDown(fault: TransportFault)
  case Client(fault: TransportFault)
  case Upload(error: PortalError)

  def message: String = this match
    case Portal(error)              => error.message
    case BadFingerprint             => "the signing key is not a fingerprint"
    case Coordinate(gav)            => s"${gav.render} is not a Maven path"
    case Published(gav)             => s"${gav.render} is already on Central"
    case Repository(gav, code)      => s"repo1 did not answer for ${gav.render} (HTTP $code)"
    case RepositoryDown(gav, fault) => s"could not reach repo1 for ${gav.render} (${PreflightError.place(fault)})"
    case KeyMissing(fingerprint)    => s"keyserver.ubuntu.com has no key ${fingerprint.value}"
    case KeyStatus(code)            => s"keyserver.ubuntu.com failed with HTTP $code"
    case KeyDown(fault)             => s"could not reach keyserver.ubuntu.com (${PreflightError.place(fault)})"
    case Client(fault)              => s"could not open the preflight client (${PreflightError.place(fault)})"
    case Upload(error)              => error.message
end PreflightError

object PreflightError:
  private def place(fault: TransportFault): String = fault match
    case TransportFault.Target(raw)                => s"'$raw' is not a target"
    case TransportFault.Connect(host, port)        => s"no connection to $host:$port"
    case TransportFault.ConnectTimeout(host, port) => s"$host:$port did not accept a connection"
    case TransportFault.Tls(host, port)            => s"tls failed at $host:$port"
    case TransportFault.ReadTimeout(host, port)    => s"$host:$port stopped answering"
    case TransportFault.Io(host, port)             => s"$host:$port closed the exchange"
    case TransportFault.Protocol(host, port)       => s"$host:$port sent an invalid response"
    case TransportFault.Pool(host, port)           => s"no free connection to $host:$port"
    case TransportFault.Trust                      => "trust material is unusable"
    case TransportFault.Body                       => "the response body failed"

/** Repo1 and the signing key, before an upload to Central. A rehearsal portal skips both reads. */
object Preflight:
  /** An https origin, or http on loopback. `repo1` includes `/maven2`. */
  opaque type Registry = Url

  object Registry:
    given CanEqual[Registry, Registry] = CanEqual.derived

    val repo1: Registry =
      trusted(
        Url(
          Path(Chunk("maven2")),
          scheme = Some(Scheme.Https),
          host = Some("repo1.maven.org"),
        )
      )

    val keyserver: Registry =
      trusted(
        Url(
          Path.root,
          scheme = Some(Scheme.Https),
          host = Some("keyserver.ubuntu.com"),
        )
      )

    def from(raw: String): Either[PortalBase.Error, Registry] =
      PortalBase.from(raw).map(base => trusted(base.root))

    extension (registry: Registry) def root: Url = registry

    private def trusted(url: Url): Registry = url
  end Registry

  // The extension lives on the nested opaque type. Calls in this object do not see it otherwise.
  import Registry.root

  final case class Hosts(repo1: Registry, keyserver: Registry)

  object Hosts:
    val central: Hosts = Hosts(Registry.repo1, Registry.keyserver)

  private val Agent = Headers.empty.add("User-Agent", "histos")
  private val Armor = "-----BEGIN PGP PUBLIC KEY BLOCK-----"

  /** Blank or unset `HISTOS_PORTAL` is Central. A set value is one portal base. */
  def portal(env: Map[String, String]): Either[PortalBase.Error, PortalBase] =
    env.get("HISTOS_PORTAL") match
      case None      => Right(PortalBase.central)
      case Some(raw) =>
        val text = raw.trim
        if text.isEmpty then Right(PortalBase.central) else PortalBase.from(text)

  def release[R](
      env: Map[String, String],
      gavs: Chunk[Gav],
      fingerprint: Fingerprint,
      upload: ZIO[R, PortalError, Deployment],
      hosts: Hosts = Hosts.central,
  ): ZIO[R, PreflightError, Deployment] =
    portal(env) match
      case Left(error) => ZIO.fail(PreflightError.Portal(error))
      case Right(base) =>
        val gate =
          if base == PortalBase.central then reads(gavs, fingerprint, hosts)
          else ZIO.unit
        gate *> upload.mapError(PreflightError.Upload(_))

  private def reads(gavs: Chunk[Gav], fingerprint: Fingerprint, hosts: Hosts): IO[PreflightError, Unit] =
    ZIO.fromEither(located(gavs)).flatMap { paths =>
      ZIO
        .serviceWithZIO[Client] { client =>
          ZIO.foreachDiscard(paths) { pair =>
            val (gav, path) = pair
            head(client, hosts.repo1, gav, path)
          } *> lookup(client, hosts.keyserver, fingerprint)
        }
        .provide(clientLayer)
    }

  private def clientLayer: ZLayer[Any, PreflightError, Client] =
    ZLayer.succeed(Client.Config.default) >>>
      Client.layer.mapError(error => PreflightError.Client(TransportFault.from(error)))

  private def located(gavs: Chunk[Gav]): Either[PreflightError, Chunk[(Gav, String)]] =
    gavs.foldLeft[Either[PreflightError, Chunk[(Gav, String)]]](Right(Chunk.empty)):
      case (left @ Left(_), _) => left
      case (Right(acc), gav)   => relative(gav).map(path => acc :+ (gav, path))

  private def relative(gav: Gav): Either[PreflightError, String] =
    if escaped(gav.group) || escaped(gav.artifact) || escaped(gav.version) then Left(PreflightError.Coordinate(gav))
    else Right(s"${gav.directory}/${gav.artifact}-${gav.version}.pom")

  private def escaped(raw: String): Boolean =
    raw.isEmpty || raw.contains('/') || raw.contains('\\') || raw.contains("..") || raw == "."

  /** 404 is absent. A 2xx is already public. Any other status refuses the upload. */
  private def head(client: Client, base: Registry, gav: Gav, path: String): IO[PreflightError, Unit] =
    client
      .batched(Request(Method.HEAD, under(base.root, path), Agent, Body.empty))
      .mapError(error => PreflightError.RepositoryDown(gav, TransportFault.from(error)))
      .flatMap { response =>
        response.status.code match
          case 404                               => ZIO.unit
          case code if code >= 200 && code < 300 => ZIO.fail(PreflightError.Published(gav))
          case code                              => ZIO.fail(PreflightError.Repository(gav, code))
      }

  /** HKP `op=get`. A 200 whose body is not a public key is a miss. Histos does not upload the key. */
  private def lookup(client: Client, base: Registry, fingerprint: Fingerprint): IO[PreflightError, Unit] =
    val url = (base.root / "pks" / "lookup").copy(
      query = QueryParams.of("op" -> "get", "search" -> s"0x${fingerprint.value}")
    )
    client
      .batched(Request(Method.GET, url, Agent, Body.empty))
      .mapError(error => PreflightError.KeyDown(TransportFault.from(error)))
      .flatMap { response =>
        response.status.code match
          case 200 if armored(response) => ZIO.unit
          case 200 | 404                => ZIO.fail(PreflightError.KeyMissing(fingerprint))
          case code                     => ZIO.fail(PreflightError.KeyStatus(code))
      }

  private def armored(response: Response): Boolean =
    response.body.strict match
      case Some(bytes) => new String(bytes.toArray, StandardCharsets.UTF_8).contains(Armor)
      case None        => false

  private def under(base: Url, relative: String): Url =
    relative.split('/').foldLeft(base) { (url, segment) =>
      if segment.isEmpty then url else url / segment
    }
end Preflight
