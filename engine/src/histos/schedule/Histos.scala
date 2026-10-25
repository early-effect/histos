package histos.schedule

import histos.step.{
  Need,
  Step,
  StepId,
  StepInfo,
  StepKind,
  StepVisitor,
  Stored,
  StoredError
}

import zio.{
  Cause,
  Chunk,
  Duration,
  Exit,
  FiberRef,
  NonEmptyChunk,
  Promise,
  Queue,
  Ref,
  Scope,
  Semaphore,
  UIO,
  URIO,
  ZIO,
  ZLayer
}
import zio.stream.ZStream

enum Outcome[+E, +A]:
  case Done(value: A)
  case Failed(failures: NonEmptyChunk[StepFailure[E]])

enum StepFailure[+E]:
  case Failed(id: StepId, cause: Cause[E])
  case Skipped(id: StepId, upstream: NonEmptyChunk[StepId])

enum StepEvent[+E]:
  case Started(id: StepId)
  case Finished(id: StepId, source: Source, took: Duration)
  case Stopped(failure: StepFailure[E])

enum Source:
  case Ran, LocalHit, RemoteHit

final case class Running[+E, +A](
    events: ZStream[Any, Nothing, StepEvent[E]],
    outcome: UIO[Outcome[E, A]]
)

trait Histos:
  def start[R, E, A](goal: Step[R, E, A]): URIO[R & Scope, Running[E, A]]

  /** The fiber running a step records where its value came from. */
  private[schedule] def note(source: Source): UIO[Unit]

object Histos:
  val live: ZLayer[Parallelism, Nothing, Histos] =
    ZLayer.scoped[Parallelism]:
      for
        parallelism <- ZIO.service[Parallelism]
        source <- FiberRef.make(Source.Ran)
      yield HistosLive(parallelism, source)

  /** Scheduler actions carry text. The codec is the cache's text form. */
  given Stored[String] with
    def encode(value: String): Chunk[Byte] =
      Chunk.fromArray(value.getBytes(java.nio.charset.StandardCharsets.UTF_8))

    def decode(bytes: Chunk[Byte]): Either[StoredError, String] =
      Right(new String(bytes.toArray, java.nio.charset.StandardCharsets.UTF_8))

    def outputs(value: String): Chunk[histos.step.OutputFile] = Chunk.empty

private final class HistosLive(
    parallelism: Parallelism,
    source: FiberRef[Source]
) extends Histos:
  private[schedule] def note(origin: Source): UIO[Unit] =
    source.set(origin)

  def start[R, E, A](goal: Step[R, E, A]): URIO[R & Scope, Running[E, A]] =
    for
      cpu <- Semaphore.make(parallelism.cpu.max(1))
      io <- Semaphore.make(parallelism.io.max(1))
      process <- Semaphore.make(parallelism.process.max(1))
      limits = Limits(cpu, io, process)
      gates <- Ref.make(Map.empty[StepId, Promise[StepFailure[E], Chunk[Byte]]])
      events <- Queue.unbounded[StepEvent[E]]
      done <- Promise.make[Nothing, Outcome[E, A]]
      _ <- eval(goal, limits, gates, events)
        .foldCauseZIO(
          cause =>
            if cause.isInterrupted then ZIO.failCause(cause)
            else fail(goal, cause, done),
          value => done.succeed(Outcome.Done(value))
        )
        .ensuring(events.shutdown)
        .forkScoped
    yield Running(ZStream.fromQueue(events), done.await)

  private def fail[E, A](
      goal: Step[?, E, A],
      cause: Cause[StepFailure[E]],
      done: Promise[Nothing, Outcome[E, A]]
  ): UIO[Unit] =
    val failure = cause.failureOption match
      case Some(step) => step
      case None       =>
        StepFailure.Failed(
          identityOf(goal).getOrElse(StepId.Work("goal")),
          Cause.empty
        )
    done.succeed(Outcome.Failed(NonEmptyChunk(failure))).unit

  private def eval[R, E, A](
      step: Step[R, E, A],
      limits: Limits,
      gates: Ref[Map[StepId, Promise[StepFailure[E], Chunk[Byte]]]],
      events: Queue[StepEvent[E]]
  ): ZIO[R, StepFailure[E], A] =
    step.visit(new StepVisitor[R, E, [X] =>> ZIO[R, StepFailure[E], X]]:
      def onLeaf[B, K <: StepKind](
          leaf: Step.Leaf[R, E, B, K]
      ): ZIO[R, StepFailure[E], B] =
        leaf.stored match
          case Some(codec) =>
            shared(leaf.info.id, codec, runLeaf(leaf, limits, events), gates)
          case None => runLeaf(leaf, limits, events)
      def onZip[B, C, K <: StepKind](
          left: Step.Of[R, E, B, K],
          right: Step.Of[R, E, C, K]
      ): ZIO[R, StepFailure[E], (B, C)] =
        for
          leftFiber <- eval(left, limits, gates, events).exit.fork
          rightFiber <- eval(right, limits, gates, events).exit.fork
          first <- leftFiber.join
          second <- rightFiber.join
          result <- (first, second) match
            case (Exit.Success(value), Exit.Success(other)) =>
              ZIO.succeed((value, other))
            case (Exit.Failure(cause), _) => ZIO.failCause(cause)
            case (_, Exit.Failure(cause)) => ZIO.failCause(cause)
        yield result
      def onMap[B, C, K <: StepKind](
          inner: Step.Of[R, E, B, K],
          f: B => C
      ): ZIO[R, StepFailure[E], C] =
        eval(inner, limits, gates, events).map(f)
      def onThen[B, C](
          chain: Step.Then[R, E, B, C]
      ): ZIO[R, StepFailure[E], C] =
        shared(
          chain.info.id,
          chain.storedOut,
          runThen(chain, limits, gates, events),
          gates
        ))

  private def runThen[R, E, A, B](
      step: Step.Then[R, E, A, B],
      limits: Limits,
      gates: Ref[Map[StepId, Promise[StepFailure[E], Chunk[Byte]]]],
      events: Queue[StepEvent[E]]
  ): ZIO[R, StepFailure[E], B] =
    eval(step.upstream, limits, gates, events).foldCauseZIO(
      cause =>
        val upstream =
          identityOf(step.upstream).getOrElse(StepId.Work("upstream"))
        val skipped = StepFailure.Skipped(step.info.id, NonEmptyChunk(upstream))
        events.offer(StepEvent.Stopped(skipped)).ignore *> ZIO.failCause(cause)
      ,
      value =>
        val started = java.lang.System.nanoTime()
        source.set(Source.Ran) *>
          events.offer(StepEvent.Started(step.info.id)).ignore *>
          limited(step.info, limits)(
            step
              .next(value)
              .mapError(error =>
                StepFailure.Failed(step.info.id, Cause.fail(error))
              )
          ).tapBoth(
            failure => events.offer(StepEvent.Stopped(failure)).ignore,
            _ =>
              source.get.flatMap: origin =>
                events
                  .offer(
                    StepEvent.Finished(
                      step.info.id,
                      origin,
                      Duration.fromNanos(java.lang.System.nanoTime() - started)
                    )
                  )
                  .ignore
          )
    )

  private def runLeaf[R, E, A](
      leaf: Step.Leaf[R, E, A, ?],
      limits: Limits,
      events: Queue[StepEvent[E]]
  ): ZIO[R, StepFailure[E], A] =
    val started = java.lang.System.nanoTime()
    source.set(Source.Ran) *>
      events.offer(StepEvent.Started(leaf.info.id)).ignore *>
      limited(leaf.info, limits)(
        leaf.run.mapError(error =>
          StepFailure.Failed(leaf.info.id, Cause.fail(error))
        )
      ).tapBoth(
        failure => events.offer(StepEvent.Stopped(failure)).ignore,
        _ =>
          source.get.flatMap: origin =>
            events
              .offer(
                StepEvent.Finished(
                  leaf.info.id,
                  origin,
                  Duration.fromNanos(java.lang.System.nanoTime() - started)
                )
              )
              .ignore
      )

  private def shared[R, E, A](
      id: StepId,
      codec: Stored[A],
      body: ZIO[R, StepFailure[E], A],
      gates: Ref[Map[StepId, Promise[StepFailure[E], Chunk[Byte]]]]
  ): ZIO[R, StepFailure[E], A] =
    Promise
      .make[StepFailure[E], Chunk[Byte]]
      .flatMap: fresh =>
        gates
          .modify { map =>
            map.get(id) match
              case Some(existing) => (Left(existing), map)
              case None           => (Right(fresh), map.updated(id, fresh))
          }
          .flatMap:
            case Left(existing) =>
              existing.await.flatMap: bytes =>
                codec.decode(bytes) match
                  case Right(value) => ZIO.succeed(value)
                  case Left(_)      =>
                    ZIO.dieMessage("a stored step value did not decode")
            case Right(mine) =>
              body.foldCauseZIO(
                cause =>
                  mine.complete(Exit.failCause(cause)) *> ZIO.failCause(cause),
                value => mine.succeed(codec.encode(value)).as(value)
              )

  private def limited[R, E, A](info: StepInfo, limits: Limits)(
      body: ZIO[R, E, A]
  ): ZIO[R, E, A] =
    val order = List(Need.Cpu, Need.Io, Need.Process).filter(need =>
      info.needs.contains(need) || (info.needs.isEmpty && need == Need.Cpu)
    )
    order.foldLeft(body)((effect, need) => limits.of(need).withPermit(effect))

  private def identityOf[R, E, A](step: Step[R, E, A]): Option[StepId] =
    step.visit(new StepVisitor[R, E, [X] =>> Option[StepId]]:
      def onLeaf[B, K <: StepKind](
          leaf: Step.Leaf[R, E, B, K]
      ): Option[StepId] = Some(leaf.info.id)
      def onZip[B, C, K <: StepKind](
          left: Step.Of[R, E, B, K],
          right: Step.Of[R, E, C, K]
      ): Option[StepId] = None
      def onMap[B, C, K <: StepKind](
          inner: Step.Of[R, E, B, K],
          f: B => C
      ): Option[StepId] = identityOf(inner)
      def onThen[B, C](chain: Step.Then[R, E, B, C]): Option[StepId] =
        Some(chain.info.id))

private final case class Limits(
    cpu: Semaphore,
    io: Semaphore,
    process: Semaphore
):
  def of(need: Need): Semaphore =
    need match
      case Need.Cpu     => cpu
      case Need.Io      => io
      case Need.Process => process
