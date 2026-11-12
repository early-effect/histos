package histos.schedule

import heddle.client.Client
import heddle.http.Url

import zio.{Scope, System, UIO, ZEnvironment, ZIO, ZLayer}

/** `HISTOS_CACHE_URI` picks the client. Unset or blank is [[RemoteCache.unconfigured]] and opens nothing. `http` and
  * `https` are the heddle cache. `grpc` is plaintext and `grpcs` is TLS. `HISTOS_CACHE_MODE` is `read` unless it is
  * `write`. `HISTOS_CACHE_NAMESPACE` is the existing namespace, and unset means `main`. A bad scheme, mode, namespace,
  * or address fails. It does not fall through to off.
  */
object CacheConfig:
  val UriKey: String  = "HISTOS_CACHE_URI"
  val ModeKey: String = "HISTOS_CACHE_MODE"

  enum Target:
    case Http(url: Url)
    case Grpc(host: String, port: Int, tls: Boolean)

  final case class Selected(target: Target, mode: RemoteMode, namespace: CacheNamespace)

  /** The process environment. Tests pass a map to [[decide]], [[open]], and [[promote]] instead. */
  def processEnv: UIO[Map[String, String]] =
    ZIO.withSystem(System.SystemLive)(System.envs.orElseSucceed(Map.empty))

  def decide(env: Map[String, String]): Either[RemoteError, Option[Selected]] =
    blank(env.get(UriKey)) match
      case None =>
        for
          _ <- modeOf(env)
          _ <- namespaceOf(env)
        yield None
      case Some(uri) =>
        for
          target    <- target(uri)
          mode      <- modeOf(env)
          namespace <- namespaceOf(env)
        yield Some(Selected(target, mode, namespace))

  def open(env: Map[String, String]): ZIO[Scope, RemoteError, RemoteCache] =
    ZIO
      .fromEither(decide(env))
      .flatMap:
        case None                                              => ZIO.succeed(RemoteCache.unconfigured)
        case Some(Selected(Target.Http(url), mode, namespace)) =>
          client.flatMap(http => HttpCache.make(url, mode, namespace).provideEnvironment(ZEnvironment(http)))
        case Some(Selected(grpc: Target.Grpc, mode, namespace)) =>
          GrpcCache.open(grpc, GrpcChannel.Idle, mode, namespace)

  /** Copy one HTTP index onto another. gRPC has no action-cache list, so promote names that limit and does not open a
    * second port. Unset stays off.
    */
  def promote(env: Map[String, String], from: CacheNamespace, to: CacheNamespace): ZIO[Scope, RemoteError, Unit] =
    ZIO
      .fromEither(decide(env))
      .flatMap:
        case None                                       => ZIO.fail(RemoteError.Off)
        case Some(Selected(Target.Grpc(_, _, _), _, _)) => ZIO.fail(RemoteError.HttpOnly("cache promote"))
        case Some(Selected(Target.Http(url), _, _))     =>
          client.flatMap(http => HttpCache.copy(url, from, to).provideEnvironment(ZEnvironment(http)))

  private def client: ZIO[Scope, RemoteError, Client] =
    (ZLayer.succeed(Client.Config.default) >>> Client.layer).build
      .mapError(error => RemoteError.Exchange(HttpCache.exchange(error)))
      .map(_.get[Client])

  private def modeOf(env: Map[String, String]): Either[RemoteError, RemoteMode] =
    blank(env.get(ModeKey)) match
      case None          => Right(RemoteMode.Read)
      case Some("read")  => Right(RemoteMode.Read)
      case Some("write") => Right(RemoteMode.Write)
      case Some(raw)     => Left(RemoteError.Mode(raw))

  private def namespaceOf(env: Map[String, String]): Either[RemoteError, CacheNamespace] =
    blank(env.get(CacheNamespace.Key)) match
      case None      => Right(CacheNamespace.Main)
      case Some(raw) => CacheNamespace.from(raw).left.map(RemoteError.Namespace(_))

  private def target(raw: String): Either[RemoteError, Target] =
    val sep = raw.indexOf("://")
    if sep <= 0 then Left(RemoteError.Address(raw))
    else
      val scheme = raw.substring(0, sep)
      val rest   = raw.substring(sep + 3)
      scheme match
        case "http" | "https" => http(raw)
        case "grpc"           => grpc(rest, tls = false, raw)
        case "grpcs"          => grpc(rest, tls = true, raw)
        case other            => Left(RemoteError.Scheme(other))

  private def http(raw: String): Either[RemoteError, Target] =
    Url.decode(raw) match
      case Left(_)    => Left(RemoteError.Address(raw))
      case Right(url) =>
        url.host match
          case Some(_) => Right(Target.Http(url))
          case None    => Left(RemoteError.Address(raw))

  private def grpc(rest: String, tls: Boolean, raw: String): Either[RemoteError, Target] =
    if rest.isEmpty || rest.exists(ch => ch == '/' || ch == '?' || ch == '#' || ch == '@') then
      Left(RemoteError.Address(raw))
    else
      rest.split(":", -1) match
        case Array(host, portText) if host.nonEmpty =>
          portText.toIntOption match
            case Some(port) if port > 0 && port <= 65535 => Right(Target.Grpc(host, port, tls))
            case _                                       => Left(RemoteError.Address(raw))
        case _ => Left(RemoteError.Address(raw))

  private def blank(raw: Option[String]): Option[String] =
    raw.map(_.trim).filter(_.nonEmpty)
end CacheConfig
