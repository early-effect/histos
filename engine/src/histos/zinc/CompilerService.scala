package histos.zinc

import histos.jdk.Sha256
import histos.schedule.Parallelism

import zio.{Fiber, IO, Promise, Queue, Ref, Scope, Tag, UIO, ZIO, ZLayer}
import zio.stream.ZStream

/** Scala 3 and Scala 2.13 compilers. Each series owns its pool. A queue is the stream of compile work.
  */
private[histos] trait Compilers:
  def compile(
      request: CompileRequest,
      artifacts: CompilerArtifacts,
      extra: List[String],
      hear: CompileProblem => Unit,
      digest: Sha256,
      admitted: UIO[Unit],
  ): IO[CompileError, CompileOutput]

  def openings: UIO[Int]

  /** Close the compilers. The next compile opens another pool. */
  def drop: UIO[Unit]
end Compilers

private[histos] object Compilers:
  /** Routes a compile to the series service. The two pools do not share a queue. */
  val route: ZLayer[Scala3Compilers & Scala213Compilers, Nothing, Compilers] =
    ZLayer.fromFunction { (scala3: Scala3Compilers, scala213: Scala213Compilers) =>
      new Compilers:
        def compile(
            request: CompileRequest,
            artifacts: CompilerArtifacts,
            extra: List[String],
            hear: CompileProblem => Unit,
            digest: Sha256,
            admitted: UIO[Unit],
        ): IO[CompileError, CompileOutput] =
          ScalaSeries.of(request.toolchain.version) match
            case Some(ScalaSeries.Scala3) =>
              scala3.offer(request, artifacts, extra, hear, digest, admitted)
            case Some(ScalaSeries.Scala2(13)) =>
              scala213.offer(request, artifacts, extra, hear, digest, admitted)
            case _ => ZIO.fail(CompileError.UnsupportedScala(request.toolchain.version))

        def openings: UIO[Int] =
          scala3.openings.zipWith(scala213.openings)(_ + _)

        def drop: UIO[Unit] = scala3.drop *> scala213.drop
    }

  /** One pool per series, alive for the `Scope` that builds this layer. */
  def live(width: Int): ZLayer[Scope, Nothing, Compilers] =
    (Scala3Compilers.layer(width) ++ Scala213Compilers.layer(width)) >>> route

  def live: ZLayer[Scope, Nothing, Compilers] =
    live(Parallelism.default.cpu)

  /** Opens compilers for `effect` and closes them when it ends. */
  def provide[R, E, A](width: Int)(effect: ZIO[R & Compilers, E, A]): ZIO[R, E, A] =
    ZIO.scoped(
      live(width).build.flatMap { env =>
        effect.provideSomeLayer(ZLayer.succeed(env.get[Compilers]))
      }
    )
end Compilers

/** The Scala 3 pool and its work stream. */
private[histos] trait Scala3Compilers:
  def offer(
      request: CompileRequest,
      artifacts: CompilerArtifacts,
      extra: List[String],
      hear: CompileProblem => Unit,
      digest: Sha256,
      admitted: UIO[Unit],
  ): IO[CompileError, CompileOutput]
  def openings: UIO[Int]
  def drop: UIO[Unit]

private[histos] object Scala3Compilers:
  def layer(width: Int): ZLayer[Scope, Nothing, Scala3Compilers] =
    SeriesCompilers.layer(width, _.asScala3)

/** The Scala 2.13 pool and its work stream. */
private[histos] trait Scala213Compilers:
  def offer(
      request: CompileRequest,
      artifacts: CompilerArtifacts,
      extra: List[String],
      hear: CompileProblem => Unit,
      digest: Sha256,
      admitted: UIO[Unit],
  ): IO[CompileError, CompileOutput]
  def openings: UIO[Int]
  def drop: UIO[Unit]

private[histos] object Scala213Compilers:
  def layer(width: Int): ZLayer[Scope, Nothing, Scala213Compilers] =
    SeriesCompilers.layer(width, _.asScala213)

private[zinc] object SeriesCompilers:
  def layer[A: Tag](width: Int, adapt: SeriesCompilers => A): ZLayer[Scope, Nothing, A] =
    ZLayer.scoped {
      for
        current <- Ref.make[Option[Generation]](None)
        _       <- ZIO.addFinalizer(
          current.get.flatMap {
            case Some(generation) => generation.close
            case None             => ZIO.unit
          }
        )
      yield adapt(SeriesCompilers(width, current))
    }

/** One series. The queue and the pool open on the first offer. `drop` retires them and leaves the series empty. */
private[zinc] final class SeriesCompilers(
    width: Int,
    current: Ref[Option[Generation]],
):
  def offer(
      request: CompileRequest,
      artifacts: CompilerArtifacts,
      extra: List[String],
      hear: CompileProblem => Unit,
      digest: Sha256,
      admitted: UIO[Unit],
  ): IO[CompileError, CompileOutput] =
    for
      cancelled <- Ref.make(false)
      running   <- Promise.make[Nothing, Fiber[CompileError, CompileOutput]]
      reply     <- Promise.make[CompileError, CompileOutput]
      work = CompileWork(request, artifacts, extra, hear, digest, admitted, reply, running, cancelled)
      result <- submit(work)
    yield result

  def openings: UIO[Int] =
    current.get.flatMap {
      case Some(generation) => generation.pool.openings
      case None             => ZIO.succeed(0)
    }

  /** Retire a live generation. The next offer opens the next one. An empty series stays empty. */
  def drop: UIO[Unit] =
    current
      .modify {
        case Some(old) => (Some(old), None)
        case None      => (None, None)
      }
      .flatMap(ZIO.foreachDiscard(_)(_.retire))

  def asScala3: Scala3Compilers =
    val self = this
    new Scala3Compilers:
      def offer(
          request: CompileRequest,
          artifacts: CompilerArtifacts,
          extra: List[String],
          hear: CompileProblem => Unit,
          digest: Sha256,
          admitted: UIO[Unit],
      )            = self.offer(request, artifacts, extra, hear, digest, admitted)
      def openings = self.openings
      def drop     = self.drop

  def asScala213: Scala213Compilers =
    val self = this
    new Scala213Compilers:
      def offer(
          request: CompileRequest,
          artifacts: CompilerArtifacts,
          extra: List[String],
          hear: CompileProblem => Unit,
          digest: Sha256,
          admitted: UIO[Unit],
      )            = self.offer(request, artifacts, extra, hear, digest, admitted)
      def openings = self.openings
      def drop     = self.drop

  private def submit(work: CompileWork): IO[CompileError, CompileOutput] =
    generation.flatMap(gen => claim(gen, work, again = true))

  /** The first offer builds the queue and the pool. A raced open keeps the generation that won the ref. */
  private def generation: UIO[Generation] =
    current.get.flatMap {
      case Some(existing) => ZIO.succeed(existing)
      case None           =>
        Generation.open(width).flatMap { created =>
          current
            .modify {
              case Some(existing) => ((existing, Some(created)), Some(existing))
              case None           => ((created, None), Some(created))
            }
            .flatMap { (winner, spare) =>
              ZIO.foreachDiscard(spare)(_.close).as(winner)
            }
        }
    }

  /** The generation this compile claimed stays open until the compile finishes, even after `drop`. */
  private def claim(
      gen: Generation,
      work: CompileWork,
      again: Boolean,
  ): IO[CompileError, CompileOutput] =
    gen.tryAcquire.flatMap { held =>
      if held then
        gen.queue
          .offer(work)
          .flatMap { accepted =>
            if accepted then await(work)
            else if again then generation.flatMap(now => claim(now, work, again = false))
            else stopped
          }
          .ensuring(gen.release)
      else if again then generation.flatMap(now => claim(now, work, again = false))
      else stopped
    }

  private def stopped: IO[CompileError, Nothing] =
    ZIO.fail(CompileError.Failed(List(CompileDiagnostic.fromCompiler("the compiler service stopped"))))

  private def await(work: CompileWork): IO[CompileError, CompileOutput] =
    work.reply.await.onInterrupt(
      work.cancelled.set(true) *>
        work.running.poll.flatMap {
          case Some(get) => get.flatMap(_.interrupt).ignore
          case None      => ZIO.unit
        }
    )
end SeriesCompilers

/** The pool, the queue, and the stream that pulls work off the queue. */
private final class Generation(
    scope: Scope.Closeable,
    val queue: Queue[CompileWork],
    val pool: CompilerPool,
    uses: Ref[Generation.Uses],
):
  def tryAcquire: UIO[Boolean] =
    uses.modify {
      case Generation.Uses.Live(n)    => (true, Generation.Uses.Live(n + 1))
      case Generation.Uses.Retired(n) => (true, Generation.Uses.Retired(n + 1))
      case Generation.Uses.Closed     => (false, Generation.Uses.Closed)
    }

  def release: UIO[Unit] =
    uses
      .modify {
        case Generation.Uses.Live(n) if n > 0    => (false, Generation.Uses.Live(n - 1))
        case Generation.Uses.Retired(n) if n > 1 => (false, Generation.Uses.Retired(n - 1))
        case Generation.Uses.Retired(_)          => (true, Generation.Uses.Closed)
        case other                               => (false, other)
      }
      .flatMap(stop => ZIO.when(stop)(shutdown).unit)

  /** New work uses the next generation. This one closes when the last compile on it finishes. */
  def retire: UIO[Unit] =
    uses
      .modify {
        case Generation.Uses.Live(0) => (true, Generation.Uses.Closed)
        case Generation.Uses.Live(n) => (false, Generation.Uses.Retired(n))
        case other                   => (false, other)
      }
      .flatMap(stop => ZIO.when(stop)(shutdown).unit)

  def close: UIO[Unit] =
    uses
      .modify {
        case Generation.Uses.Closed => (false, Generation.Uses.Closed)
        case _                      => (true, Generation.Uses.Closed)
      }
      .flatMap(stop => ZIO.when(stop)(shutdown).unit)

  private def shutdown: UIO[Unit] =
    queue.shutdown *> scope.close(zio.Exit.unit).ignore
end Generation

private object Generation:
  enum Uses:
    case Live(n: Int)
    case Retired(n: Int)
    case Closed

  def open(width: Int): UIO[Generation] =
    for
      scope <- Scope.make
      queue <- Queue.unbounded[CompileWork]
      pool  <- scope.extend(CompilerPool.make(width))
      uses  <- Ref.make[Uses](Uses.Live(0))
      _     <- ZStream
        .fromQueue(queue)
        .mapZIOParUnordered(width)(work => Generation.run(pool, work))
        .runDrain
        .forkIn(scope)
    yield Generation(scope, queue, pool, uses)

  private def run(pool: CompilerPool, work: CompileWork): UIO[Unit] =
    ZIO.ifZIO(work.cancelled.get)(
      work.reply.interrupt.unit,
      pool
        .compile(work.request, work.artifacts, work.extra, work.hear, work.digest, work.admitted)
        .fork
        .flatMap { fiber =>
          work.running.succeed(fiber) *>
            fiber.join.foldCauseZIO(
              cause => work.reply.failCause(cause).ignore,
              out => work.reply.succeed(out).ignore,
            )
        },
    )
end Generation

private final class CompileWork(
    val request: CompileRequest,
    val artifacts: CompilerArtifacts,
    val extra: List[String],
    val hear: CompileProblem => Unit,
    val digest: Sha256,
    val admitted: UIO[Unit],
    val reply: Promise[CompileError, CompileOutput],
    val running: Promise[Nothing, Fiber[CompileError, CompileOutput]],
    val cancelled: Ref[Boolean],
)
