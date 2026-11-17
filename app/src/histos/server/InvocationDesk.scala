package histos.server

import histos.plugin.{
  GenerationPluginEvent,
  InvocationContext,
  InvocationId,
  InvocationPluginEvent,
  PluginEventError,
  PluginEvents,
}

import zio.{IO, Queue, Ref, Scope, UIO, URIO, ZIO}

/** One retained generation. Construction runs once. Each caller gets its own invocation queue. */
final class InvocationDesk(
    built: Ref[Int],
    closed: Ref[Boolean],
    generation: Queue[GenerationPluginEvent],
):
  def constructions: UIO[Int] = built.get

  def generationEvents: UIO[List[GenerationPluginEvent]] =
    generation.takeAll.map(_.toList)

  /** Built once. The context and the queue belong to the lease that calls it. */
  def observe(
      context: InvocationContext,
      events: Queue[InvocationPluginEvent],
      live: Ref[Boolean],
  ): PluginEvents.Invocation =
    new PluginEvents.Invocation:
      def emit(event: InvocationPluginEvent): IO[PluginEventError, Unit] =
        write(context.id, live, events, event)

  def open(context: InvocationContext): URIO[Scope, InvocationLease] =
    for
      _      <- built.update(count => if count == 0 then 1 else count)
      events <- Queue.unbounded[InvocationPluginEvent]
      live   <- Ref.make(true)
      lease = InvocationLease(context, events, live, this)
      _ <- ZIO.addFinalizer(lease.close)
    yield lease

  def emitGeneration(event: GenerationPluginEvent): IO[PluginEventError, Unit] =
    ZIO.uninterruptible {
      closed.get.flatMap: down =>
        if down then ZIO.fail(PluginEventError.Closed)
        else generation.offer(event).unit
    }

  def close: UIO[Unit] = closed.set(true)

  private def write(
      id: InvocationId,
      live: Ref[Boolean],
      events: Queue[InvocationPluginEvent],
      event: InvocationPluginEvent,
  ): IO[PluginEventError, Unit] =
    ZIO.uninterruptible {
      ZIO.succeed(id).flatMap { _ =>
        live.get.zip(closed.get).flatMap { (open, down) =>
          if !open || down then ZIO.fail(PluginEventError.Closed)
          else events.offer(event).unit
        }
      }
    }
end InvocationDesk

object InvocationDesk:
  def make: UIO[InvocationDesk] =
    for
      built      <- Ref.make(0)
      closed     <- Ref.make(false)
      generation <- Queue.unbounded[GenerationPluginEvent]
    yield InvocationDesk(built, closed, generation)

final case class InvocationLease(
    context: InvocationContext,
    events: Queue[InvocationPluginEvent],
    live: Ref[Boolean],
    desk: InvocationDesk,
):
  def invocation: PluginEvents.Invocation = desk.observe(context, events, live)

  def heard: UIO[List[InvocationPluginEvent]] = events.takeAll.map(_.toList)

  def close: UIO[Unit] = live.set(false)
end InvocationLease
