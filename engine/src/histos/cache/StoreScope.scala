package histos.cache

import zio.{Chunk, Exit, Ref, Scope, UIO, ZIO}

/** One reservation for a cache key.
  *
  * `read` restores the published bytes and its release publishes nothing. `write` restores, runs the body, and the
  * release publishes the body's bytes only when the key is still free. The first successful release wins. A later write
  * does not replace it. A read that finishes first does not take the key.
  */
trait StoreScope:
  def read(key: String): ZIO[Scope, Nothing, Chunk[Byte]]

  def write(key: String)(body: Chunk[Byte] => UIO[Chunk[Byte]]): ZIO[Scope, Nothing, Chunk[Byte]]

  def published(key: String): UIO[Option[Chunk[Byte]]]
end StoreScope

object StoreScope:
  def memory: UIO[StoreScope] =
    Ref.make(Map.empty[String, Chunk[Byte]]).map(Memory(_))

private final class Memory(slots: Ref[Map[String, Chunk[Byte]]]) extends StoreScope:
  def read(key: String): ZIO[Scope, Nothing, Chunk[Byte]] =
    ZIO.acquireRelease(current(key).map(_.getOrElse(Chunk.empty)))(_ => ZIO.unit)

  def write(key: String)(body: Chunk[Byte] => UIO[Chunk[Byte]]): ZIO[Scope, Nothing, Chunk[Byte]] =
    current(key).flatMap:
      case Some(existing) =>
        ZIO.acquireRelease(ZIO.succeed(existing))(_ => ZIO.unit) *> body(existing).as(existing)
      case None =>
        ZIO
          .acquireReleaseExit(Ref.make[Option[Chunk[Byte]]](None))(commit(key))
          .flatMap: held =>
            body(Chunk.empty).tap(bytes => held.set(Some(bytes)))

  def published(key: String): UIO[Option[Chunk[Byte]]] =
    current(key)

  private def current(key: String): UIO[Option[Chunk[Byte]]] =
    slots.get.map(_.get(key))

  private def commit(
      key: String
  )(held: Ref[Option[Chunk[Byte]]], exit: Exit[Any, Any]): UIO[Unit] =
    exit match
      case Exit.Success(_) =>
        held.get.flatMap:
          case None        => ZIO.unit
          case Some(bytes) =>
            slots.update: map =>
              if map.contains(key) then map else map.updated(key, bytes)
      case Exit.Failure(_) => ZIO.unit
end Memory
