package histos.schedule

import zio.{Chunk, IO, ULayer, ZLayer}

/** `Off` is the process default. A configured cache is read-only until it is opened for write.
  */
enum RemoteMode:
  case Off
  case Read
  case Write

/** Why an exchange with the remote cache produced no response. The case never carries a throwable or its message. */
enum CacheExchange:
  case Target
  case Connect(host: String, port: Int)
  case ConnectTimeout(host: String, port: Int)
  case Tls(host: String, port: Int)
  case ReadTimeout(host: String, port: Int)
  case Io(host: String, port: Int)
  case Protocol(host: String, port: Int)
  case Pool(host: String, port: Int)
  case Trust
  case Body

  def message: String = this match
    case Target                     => "the remote cache target is not a url"
    case Connect(host, port)        => s"could not connect to the remote cache at $host:$port"
    case ConnectTimeout(host, port) => s"the remote cache at $host:$port did not accept a connection"
    case Tls(host, port)            => s"tls failed with the remote cache at $host:$port"
    case ReadTimeout(host, port)    => s"the remote cache at $host:$port stopped answering"
    case Io(host, port)             => s"the remote cache at $host:$port closed the exchange"
    case Protocol(host, port)       => s"the remote cache at $host:$port sent an invalid response"
    case Pool(host, port)           => s"no free connection to the remote cache at $host:$port"
    case Trust                      => "the remote cache trust material is unusable"
    case Body                       => "the remote cache response body failed"
end CacheExchange

enum RemoteError:
  case Off
  case ReadOnly
  case Dangling(action: Digest, output: Digest)
  case Text(action: Digest)
  case Exchange(fault: CacheExchange)
  case Status(code: Int)
  case Index(action: Digest)
  case Mismatch(action: Digest, output: Digest)

  def message: String = this match
    case Off                      => "remote cache is not configured"
    case ReadOnly                 => "remote cache is read-only"
    case Dangling(action, output) =>
      s"remote cache action ${action.value} names missing output ${output.value}"
    case Text(action)             => s"remote cache output for ${action.value} is not utf-8"
    case Exchange(fault)          => fault.message
    case Status(code)             => s"remote cache answered $code"
    case Index(action)            => s"remote cache action ${action.value} is not an action result"
    case Mismatch(action, output) =>
      s"remote cache action ${action.value} returned bytes that are not ${output.value}"
end RemoteError

/** Action index plus a content-addressed store. `find` reads both. `store` writes both, and refuses unless the mode is
  * write. `purge` deletes this cache's index entries and leaves every blob.
  */
trait RemoteCache:
  def mode: RemoteMode
  def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]]
  def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit]

  /** Delete this cache's index entries and leave every blob. `Off` and `Read` refuse. */
  def purge: IO[RemoteError, Unit]
end RemoteCache

object RemoteCache:
  val off: ULayer[RemoteCache] = ZLayer.succeed(unconfigured)

  val unconfigured: RemoteCache = OffCache
end RemoteCache

private object OffCache extends RemoteCache:
  val mode: RemoteMode                                                 = RemoteMode.Off
  def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]]       = zio.ZIO.none
  def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit] =
    zio.ZIO.fail(RemoteError.Off)
  def purge: IO[RemoteError, Unit] =
    zio.ZIO.fail(RemoteError.Off)
