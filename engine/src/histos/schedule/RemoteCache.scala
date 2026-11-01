package histos.schedule

import zio.{Chunk, IO, ULayer, ZLayer}

/** `Off` is the process default. A configured cache is read-only until it is opened for write.
  */
enum RemoteMode:
  case Off
  case Read
  case Write

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
