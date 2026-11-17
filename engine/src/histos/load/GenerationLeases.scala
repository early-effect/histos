package histos.load

import histos.plugin.PluginRunError

import zio.{Exit, Ref, Scope, UIO, ZIO, ZLayer}

/** One candidate generation. A discarded candidate closes now. A retired generation closes after the last lease. */
private[load] final class GenerationLeases(
    scope: Scope.Closeable,
    state: Ref[GenerationLeases.State],
):
  /** Service acquires register here. A task lease is what keeps them alive after retire. */
  def generation: Scope = scope

  /** One uninterruptible region. An interrupt between the increment and the finalizer would leak the count. */
  def lease: ZIO[Scope, PluginRunError, Unit] =
    ZIO.uninterruptible:
      state
        .modify { current =>
          if current.closed then (Left(PluginRunError.Closed), current)
          else (Right(()), current.copy(leases = current.leases + 1))
        }
        .flatMap:
          case Left(error) => ZIO.fail(error)
          case Right(_)    => ZIO.addFinalizer(release).unit

  def use[R, E, A](effect: ZIO[R & Scope, E, A]): ZIO[R, E, A] =
    scope.extend(effect)

  /** Close now. A later retire does not close again. */
  def discard: UIO[Unit] =
    finish(force = true)

  /** Close once no lease remains. */
  def retire: UIO[Unit] =
    state
      .modify { current =>
        val next    = current.copy(retired = true)
        val closing = !current.closed && current.leases == 0
        (closing, if closing then next.copy(closed = true) else next)
      }
      .flatMap { closing =>
        ZIO.when(closing)(scope.close(Exit.unit)).unit
      }

  private def release: UIO[Unit] =
    state
      .modify { current =>
        val left    = math.max(0, current.leases - 1)
        val next    = current.copy(leases = left)
        val closing = current.retired && left == 0 && !current.closed
        (closing, if closing then next.copy(closed = true) else next)
      }
      .flatMap { closing =>
        ZIO.when(closing)(scope.close(Exit.unit)).unit
      }

  private def finish(force: Boolean): UIO[Unit] =
    state
      .modify { current =>
        val closing = force && !current.closed
        (closing, current.copy(retired = true, closed = current.closed || closing, leases = 0))
      }
      .flatMap { closing =>
        ZIO.when(closing)(scope.close(Exit.unit)).unit
      }
end GenerationLeases

private[load] object GenerationLeases:
  final case class State(leases: Int, retired: Boolean, closed: Boolean)

  def make: UIO[GenerationLeases] =
    for
      scope <- Scope.make
      state <- Ref.make(State(0, retired = false, closed = false))
    yield GenerationLeases(scope, state)

  def layer: ZLayer[Any, Nothing, GenerationLeases] =
    ZLayer.fromZIO(make)
end GenerationLeases
