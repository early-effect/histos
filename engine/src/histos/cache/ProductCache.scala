package histos.cache

import histos.jdk.Sha256
import histos.schedule.{CacheError, Digest, RemoteCache, RemoteError, RemoteMiss, RemoteMode}
import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ZIO}

/** The local blob of a compile, link, or package, read and written through [[RemoteCache.find]] and
  * [[RemoteCache.store]]. `find` checks the download against its digest before these bytes are installed.
  */
object ProductCache:
  /** Put a remote hit into the local store. A timeout is a miss. A mismatched download fails. `Off` does nothing. */
  def stage(remote: RemoteCache, store: ContentStore, key: Sha256): IO[RemoteError | CacheError, Unit] =
    remote.mode match
      case RemoteMode.Off                     => ZIO.unit
      case RemoteMode.Read | RemoteMode.Write =>
        id(key).flatMap: digest =>
          RemoteMiss
            .unanswered(remote.find(digest))(None)
            .flatMap:
              case None        => ZIO.unit
              case Some(bytes) =>
                zio.Clock.ClockLive.instant.flatMap(now => store.put(digest, bytes, now))

  /** Upload the local blob when this cache writes. A timeout is skipped. A missing local blob is not an upload. */
  def publish(remote: RemoteCache, store: ContentStore, key: Sha256): IO[RemoteError | CacheError, Unit] =
    remote.mode match
      case RemoteMode.Write =>
        id(key).flatMap: digest =>
          store
            .read(digest)
            .flatMap:
              case None        => ZIO.unit
              case Some(bytes) => RemoteMiss.unanswered(remote.store(digest, bytes))(())
      case RemoteMode.Off | RemoteMode.Read => ZIO.unit

  /** A local or remote hit is written to `dest` and `pack` does not run. A miss runs `pack`, stores those bytes, and
    * uploads them when the cache writes.
    */
  def materialize[E](
      remote: RemoteCache,
      store: ContentStore,
      key: Sha256,
      dest: Path,
      pack: IO[E, Chunk[Byte]],
  ): IO[E | RemoteError | CacheError, Chunk[Byte]] =
    for
      _      <- stage(remote, store, key)
      digest <- id(key)
      found  <- store.read(digest)
      bytes  <- found match
        case Some(hit) => place(dest, hit).as(hit)
        case None      =>
          pack.flatMap: packed =>
            zio.Clock.ClockLive.instant.flatMap(now => store.put(digest, packed, now)) *>
              publish(remote, store, key).as(packed)
    yield bytes

  private def id(key: Sha256): IO[CacheError, Digest] =
    Digest.from(key.value) match
      case Some(found) => ZIO.succeed(found)
      case None        => ZIO.fail(CacheError.Io(IoFailure(Path.of(key.value), IoFault.Read)))

  private def place(dest: Path, bytes: Chunk[Byte]): IO[CacheError, Unit] =
    ZIO
      .attemptBlocking {
        Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(dest, bytes.toArray)
        ()
      }
      .mapError(_ => CacheError.Io(IoFailure(dest, IoFault.Write)))
end ProductCache
