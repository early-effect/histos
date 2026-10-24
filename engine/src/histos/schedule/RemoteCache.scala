package histos.schedule

import zio.{Chunk, IO, ULayer, ZLayer}

/** `Off` is the process default. A configured cache is read-only until it is
  * opened for write.
  */
enum RemoteMode:
  case Off
  case Read
  case Write

enum RemoteError:
  case Off
  case ReadOnly
  case Dangling(action: Digest, output: Digest)
  case Text(action: Digest)

  def message: String = this match
    case Off                      => "remote cache is not configured"
    case ReadOnly                 => "remote cache is read-only"
    case Dangling(action, output) =>
      s"remote cache action ${action.value} names missing output ${output.value}"
    case Text(action) => s"remote cache output for ${action.value} is not utf-8"

/** Action index plus a content-addressed store. `find` reads both. `store`
  * writes both, and refuses unless the mode is write.
  */
trait RemoteCache:
  def mode: RemoteMode
  def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]]
  def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit]

object RemoteCache:
  val off: ULayer[RemoteCache] = ZLayer.succeed(unconfigured)

  val unconfigured: RemoteCache = OffCache

private object OffCache extends RemoteCache:
  val mode: RemoteMode = RemoteMode.Off
  def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]] = zio.ZIO.none
  def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit] =
    zio.ZIO.fail(RemoteError.Off)
