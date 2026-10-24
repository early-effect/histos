package histos.schedule

import zio.{Chunk, Ref, UIO, ZIO}

private final case class RemoteState(
    actions: Map[Digest, Digest],
    blobs: Map[Digest, Chunk[Byte]]
)

private final class TestRemoteCache(
    val mode: RemoteMode,
    state: Ref[RemoteState]
) extends RemoteCache:
  def find(action: Digest): zio.IO[RemoteError, Option[Chunk[Byte]]] =
    state.get.flatMap: current =>
      current.actions.get(action) match
        case None         => ZIO.none
        case Some(output) =>
          current.blobs.get(output) match
            case Some(bytes) => ZIO.some(bytes)
            case None        => ZIO.fail(RemoteError.Dangling(action, output))

  def store(action: Digest, bytes: Chunk[Byte]): zio.IO[RemoteError, Unit] =
    mode match
      case RemoteMode.Off   => ZIO.fail(RemoteError.Off)
      case RemoteMode.Read  => ZIO.fail(RemoteError.ReadOnly)
      case RemoteMode.Write =>
        val output = Digest.ofBytes(bytes)
        state.update(current =>
          current.copy(
            actions = current.actions + (action -> output),
            blobs = current.blobs + (output -> bytes)
          )
        )

  def plant(action: Digest, bytes: Chunk[Byte]): UIO[Unit] =
    val output = Digest.ofBytes(bytes)
    state.update(current =>
      current.copy(
        actions = current.actions + (action -> output),
        blobs = current.blobs + (output -> bytes)
      )
    )

  def plantDangling(action: Digest): UIO[Digest] =
    val output = Digest.ofBytes(Chunk.single(1.toByte))
    state
      .update(current =>
        current.copy(actions = current.actions + (action -> output))
      )
      .as(output)

  def holds(action: Digest): UIO[Boolean] =
    state.get.map(_.actions.contains(action))

object TestRemoteCache:
  def memory(mode: RemoteMode): UIO[TestRemoteCache] =
    Ref
      .make(RemoteState(Map.empty, Map.empty))
      .map(state => new TestRemoteCache(mode, state))
