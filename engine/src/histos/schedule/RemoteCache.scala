package histos.schedule

import zio.{Chunk, IO, Ref, UIO, ULayer, ZIO, ZLayer}

/** `Off` is the process default. A configured cache is read-only until it is opened for write. */
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
    case Off                       => "remote cache is not configured"
    case ReadOnly                  => "remote cache is read-only"
    case Dangling(action, output)  => s"remote cache action ${action.value} names missing output ${output.value}"
    case Text(action)              => s"remote cache output for ${action.value} is not utf-8"

/** Action index plus a content-addressed store. `find` reads both. `store` writes both, and refuses unless the mode is write. */
trait RemoteCache:
  def mode: RemoteMode
  def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]]
  def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit]

object RemoteCache:
  val off: ULayer[RemoteCache] = ZLayer.succeed(unconfigured)

  val unconfigured: RemoteCache = OffCache

  def memory(mode: RemoteMode): UIO[Memory] =
    Ref.make(RemoteState.empty).map(state => Memory(mode, state))

private object OffCache extends RemoteCache:
  val mode: RemoteMode = RemoteMode.Off
  def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]] = ZIO.none
  def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit] = ZIO.fail(RemoteError.Off)

private final case class RemoteState(actions: Map[Digest, Digest], blobs: Map[Digest, Chunk[Byte]])

private object RemoteState:
  val empty: RemoteState = RemoteState(Map.empty, Map.empty)

/** In-process store. Tests plant entries. There is no network client. */
final class Memory private[schedule] (val mode: RemoteMode, state: Ref[RemoteState]) extends RemoteCache:
  def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]] =
    state.get.flatMap: current =>
      current.actions.get(action) match
        case None => ZIO.none
        case Some(output) =>
          current.blobs.get(output) match
            case Some(bytes) => ZIO.some(bytes)
            case None        => ZIO.fail(RemoteError.Dangling(action, output))

  def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit] =
    mode match
      case RemoteMode.Off   => ZIO.fail(RemoteError.Off)
      case RemoteMode.Read  => ZIO.fail(RemoteError.ReadOnly)
      case RemoteMode.Write =>
        val output = Digest.ofBytes(bytes)
        state.update(current => current.copy(actions = current.actions + (action -> output), blobs = current.blobs + (output -> bytes)))

  def plant(action: Digest, bytes: Chunk[Byte]): UIO[Unit] =
    val output = Digest.ofBytes(bytes)
    state.update(current => current.copy(actions = current.actions + (action -> output), blobs = current.blobs + (output -> bytes)))

  def plantDangling(action: Digest): UIO[Digest] =
    val output = Digest.ofBytes(Chunk.single(1.toByte))
    state.update(current => current.copy(actions = current.actions + (action -> output))).as(output)

  def holds(action: Digest): UIO[Boolean] =
    state.get.map(_.actions.contains(action))
