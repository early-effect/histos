package histos.bsp

import java.io.OutputStream

import zio.{IO, Promise, Queue, Scope, UIO, ZIO}

/** One fiber writes the stream. Every other fiber offers a payload and waits until that frame is written. */
final class FrameQueue private (submit: String => IO[Frame.FrameError, Unit]):
  def offer(payload: String): IO[Frame.FrameError, Unit] = submit(payload)

object FrameQueue:
  /** One writer for `out`, for the life of this scope.
    *
    * The fiber is a daemon. A scope child is interrupted before the finalizer that asks it to finish the frame it
    * already holds, and an offer waiting on that frame would hang.
    */
  def open(out: OutputStream): ZIO[Scope, Nothing, FrameQueue] =
    ZIO
      .acquireRelease {
        Queue.unbounded[Option[Pending]].flatMap { queue =>
          writeAll(out, queue).forkDaemon.map(fiber => (queue, fiber))
        }
      } { (queue, fiber) =>
        queue.offer(None) *> fiber.join
      }
      .map { (queue, _) =>
        new FrameQueue(payload => submit(queue, payload))
      }

  private def submit(queue: Queue[Option[Pending]], payload: String): IO[Frame.FrameError, Unit] =
    ZIO.uninterruptibleMask { restore =>
      Promise.make[Frame.FrameError, Unit].flatMap { done =>
        queue.offer(Some(Pending(payload, done))).flatMap {
          case true  => restore(done.await)
          case false => ZIO.fail(Frame.FrameError.Io)
        }
      }
    }

  private def writeAll(out: OutputStream, queue: Queue[Option[Pending]]): UIO[Unit] =
    def step: UIO[Unit] =
      queue.take.flatMap {
        case None          => ZIO.unit
        case Some(pending) =>
          Frame
            .write(out, pending.payload)
            .foldZIO(
              error => pending.done.fail(error).unit,
              _ => pending.done.succeed(()).unit,
            ) *> step
      }
    step

  private final case class Pending(payload: String, done: Promise[Frame.FrameError, Unit])
end FrameQueue
