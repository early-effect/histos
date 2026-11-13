package histos.cache

import zio.{Ref, UIO, URIO, ZIO, ZLayer}

/** Starts of the Scala.js or Scala Native linker. A restored output does not tick. */
final case class LinkerStarts(count: Ref[Int]):
  def tick: UIO[Unit] = count.update(_ + 1)
  def get: UIO[Int]   = count.get

object LinkerStarts:
  val live: ZLayer[Any, Nothing, LinkerStarts] =
    ZLayer.fromZIO(Ref.make(0).map(LinkerStarts(_)))

  def tick: URIO[LinkerStarts, Unit] =
    ZIO.serviceWithZIO[LinkerStarts](_.tick)
end LinkerStarts
