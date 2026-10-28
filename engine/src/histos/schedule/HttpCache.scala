package histos.schedule

import heddle.client.{Client, ClientError}
import heddle.http.{Body, Method, Request, Response, Url}

import zio.{Chunk, IO, URIO, ZIO}

/** Bazel HTTP cache. `/cas/{sha256}` holds the output bytes. `/{namespace}/ac/{action}` holds the ActionResult that
  * names them. bazel-remote ignores an instance prefix on the blob and keeps the action cache per instance, so only the
  * index is prefixed. The blob is written first: a crash leaves an unreferenced blob, not an index entry for a missing
  * one. A lookup walks the role's prefixes. Only a 404 tries the next one.
  */
object HttpCache:
  def make(origin: Url, mode: RemoteMode, namespace: CacheNamespace): URIO[Client, RemoteCache] =
    ZIO.serviceWith[Client](client => new Cache(client, origin, mode, namespace))

  private[schedule] def exchange(error: ClientError): CacheExchange = error match
    case ClientError.InvalidTarget(_, _)            => CacheExchange.Target
    case ClientError.Connect(authority, _)          => CacheExchange.Connect(authority.host, authority.port)
    case ClientError.ConnectTimeout(authority)      => CacheExchange.ConnectTimeout(authority.host, authority.port)
    case ClientError.Tls(authority, _)              => CacheExchange.Tls(authority.host, authority.port)
    case ClientError.ReadTimeout(authority)         => CacheExchange.ReadTimeout(authority.host, authority.port)
    case ClientError.Io(authority, _)               => CacheExchange.Io(authority.host, authority.port)
    case ClientError.Protocol(authority, _)         => CacheExchange.Protocol(authority.host, authority.port)
    case ClientError.PoolExhausted(authority)       => CacheExchange.Pool(authority.host, authority.port)
    case ClientError.NotAnEventStream(authority, _) => CacheExchange.Protocol(authority.host, authority.port)
    case ClientError.InvalidTrust(_)                => CacheExchange.Trust
    case ClientError.BodyFailed(_)                  => CacheExchange.Body

  private final class Cache(client: Client, origin: Url, val mode: RemoteMode, namespace: CacheNamespace)
      extends RemoteCache:
    def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]] =
      mode match
        case RemoteMode.Off                     => ZIO.none
        case RemoteMode.Read | RemoteMode.Write => lookup(namespace.reads, action)

    def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit] =
      mode match
        case RemoteMode.Off   => ZIO.fail(RemoteError.Off)
        case RemoteMode.Read  => ZIO.fail(RemoteError.ReadOnly)
        case RemoteMode.Write =>
          val output = Digest.ofBytes(bytes)
          put(origin / "cas" / output.value, bytes) *>
            put(index(namespace, action), ActionResult.encode(output, bytes.length))

    private def lookup(rest: List[CacheNamespace], action: Digest): IO[RemoteError, Option[Chunk[Byte]]] =
      rest match
        case Nil               => ZIO.none
        case candidate :: more =>
          get(index(candidate, action)).flatMap:
            case None       => lookup(more, action)
            case Some(body) => materialize(action, body)

    private def materialize(action: Digest, body: Chunk[Byte]): IO[RemoteError, Option[Chunk[Byte]]] =
      ActionResult.decode(body) match
        case None          => ZIO.fail(RemoteError.Index(action))
        case Some(pointer) =>
          get(origin / "cas" / pointer.output.value).flatMap:
            case None        => ZIO.fail(RemoteError.Dangling(action, pointer.output))
            case Some(bytes) =>
              if bytes.length.toLong == pointer.size && Digest.ofBytes(bytes) == pointer.output then ZIO.some(bytes)
              else ZIO.fail(RemoteError.Mismatch(action, pointer.output))

    private def index(role: CacheNamespace, action: Digest): Url =
      role.segments.foldLeft(origin)(_ / _) / "ac" / action.value

    private def get(url: Url): IO[RemoteError, Option[Chunk[Byte]]] =
      send(Request.get(url)).flatMap: response =>
        response.status.code match
          case 200  => payload(response).map(Some(_))
          case 404  => ZIO.none
          case code => ZIO.fail(RemoteError.Status(code))

    private def put(url: Url, bytes: Chunk[Byte]): IO[RemoteError, Unit] =
      send(Request(Method.PUT, url, body = Body.fromBytes(bytes))).flatMap: response =>
        response.status.code match
          case 200 | 204 => ZIO.unit
          case code      => ZIO.fail(RemoteError.Status(code))

    private def send(request: Request): IO[RemoteError, Response] =
      client.batched(request).mapError(error => RemoteError.Exchange(exchange(error)))

    private def payload(response: Response): IO[RemoteError, Chunk[Byte]] =
      response.body.collect.mapError(_ => RemoteError.Exchange(CacheExchange.Body))
  end Cache
end HttpCache
