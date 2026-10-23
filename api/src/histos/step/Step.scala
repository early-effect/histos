package histos.step

import zio.ZIO

type Step[-R, +E, +A] = Step.Of[R, E, A, StepKind]
type LocalStep[-R, +E, +A] = Step.Of[R, E, A, StepKind.Local]
type CachedStep[-R, +E, +A] = Step.Of[R, E, A, StepKind.Cached]
type HermeticStep[-R, +E, +A] = Step.Of[R, E, A, StepKind.Hermetic]

object Step:
  /** One step. `K` is what the cache may do with it. */
  sealed trait Of[-R, +E, +A, +K <: StepKind]:
    def mirror: RuntimeKind

    def @@[R1 <: R, E1 >: E, K2 <: StepKind](aspect: StepAspect[R1, E1, K2])(using Fits[K, K2]): Of[R1, E1, A, K]

    /** The weakest kind that still describes both sides. */
    def zip[R1 <: R, E1 >: E, B, K1 >: K <: StepKind](that: Of[R1, E1, B, K1]): Of[R1, E1, (A, B), K1] =
      Zipped(this, that)

    /** A pure derivation. It keeps the kind and is never stored. */
    def map[B](f: A => B): Of[R, E, B, K] = Mapped(this, f)

  final case class Leaf[-R, +E, A, +K <: StepKind](
      info: StepInfo,
      kind: RuntimeKind,
      stored: Option[Stored[A]],
      run: ZIO[R, E, A],
  ) extends Of[R, E, A, K]:
    def mirror: RuntimeKind = kind

    def @@[R1 <: R, E1 >: E, K2 <: StepKind](aspect: StepAspect[R1, E1, K2])(using Fits[K, K2]): Of[R1, E1, A, K] =
      aspect(this)

  final case class Zipped[-R, +E, A, B, +K <: StepKind](left: Of[R, E, A, K], right: Of[R, E, B, K])
      extends Of[R, E, (A, B), K]:
    def mirror: RuntimeKind = RuntimeKind.join(left.mirror, right.mirror)

    def @@[R1 <: R, E1 >: E, K2 <: StepKind](aspect: StepAspect[R1, E1, K2])(using Fits[K, K2]): Of[R1, E1, (A, B), K] =
      Zipped(left @@ aspect, right @@ aspect)

  final case class Mapped[-R, +E, A, B, +K <: StepKind](inner: Of[R, E, A, K], f: A => B) extends Of[R, E, B, K]:
    def mirror: RuntimeKind = inner.mirror

    def @@[R1 <: R, E1 >: E, K2 <: StepKind](aspect: StepAspect[R1, E1, K2])(using Fits[K, K2]): Of[R1, E1, B, K] =
      Mapped(inner @@ aspect, f)

  /** The upstream value is hashed. The continuation is the hermetic work. */
  final case class Then[-R, +E, A, B](
      upstream: Of[R, E, A, StepKind],
      info: StepInfo,
      storedIn: Stored[A],
      storedOut: Stored[B],
      next: A => ZIO[R, E, B],
  ) extends Of[R, E, B, StepKind.Hermetic]:
    def mirror: RuntimeKind = RuntimeKind.Hermetic

    def @@[R1 <: R, E1 >: E, K2 <: StepKind](aspect: StepAspect[R1, E1, K2])(using
        Fits[StepKind.Hermetic, K2]
    ): Of[R1, E1, B, StepKind.Hermetic] =
      Then(
        upstream,
        info,
        storedIn,
        storedOut,
        value => aspect(Leaf(info, RuntimeKind.Hermetic, Some(storedOut), next(value))).run,
      )

  /** Each constructor pins the output type first, so `R` and `E` come from the effect. */
  def local[A]: MakeLocal[A] = MakeLocal()

  def cached[A: Stored]: MakeCached[A] = MakeCached()

  def hermetic[A: Stored]: MakeHermetic[A] = MakeHermetic()

  /** Run the algebra. Caching and scheduling are the executor's job. */
  def run[R, E, A](step: Of[R, E, A, StepKind]): ZIO[R, E, A] =
    step match
      case Leaf(_, _, _, effect)         => effect
      case Zipped(left, right)           => run(left).zip(run(right))
      case Mapped(inner, f)              => run(inner).map(f)
      case Then(upstream, _, _, _, next) => run(upstream).flatMap(next)

  private[step] def promote[R, E, A](step: Of[R, E, A, StepKind.Cached]): Of[R, E, A, StepKind.Hermetic] =
    step match
      case Leaf(info, _, stored, effect) => Leaf(info, RuntimeKind.Hermetic, stored, effect)
      case Zipped(left, right)           => Zipped(promote(left), promote(right))
      case Mapped(inner, f)              => Mapped(promote(inner), f)
      case done: Then[r, e, a, b]        => done

  final class MakeLocal[A]:
    def apply[R, E](info: StepInfo)(effect: ZIO[R, E, A]): LocalStep[R, E, A] =
      Leaf(info, RuntimeKind.Local, None, effect)

  final class MakeCached[A](using Stored[A]):
    def apply[R, E](info: StepInfo)(effect: ZIO[R, E, A]): CachedStep[R, E, A] =
      Leaf(info, RuntimeKind.Cached, Some(summon[Stored[A]]), effect)

  final class MakeHermetic[A](using Stored[A]):
    def apply[R, E](info: StepInfo)(effect: ZIO[R, E, A]): HermeticStep[R, E, A] =
      Leaf(info, RuntimeKind.Hermetic, Some(summon[Stored[A]]), effect)

  extension [R, E, A, K <: StepKind](upstream: Of[R, E, A, K])
    /** The key hashes the upstream value, so any kind may feed a hermetic step when its value has a codec. */
    def andThenHermetic[B: Stored](info: StepInfo)(using Stored[A]): AndThen[R, E, A, B] =
      AndThen(upstream, info, summon[Stored[A]], summon[Stored[B]])

  extension [R, E, A](step: CachedStep[R, E, A])
    def sandboxed: HermeticStep[R, E, A] = promote(step)

  final class AndThen[R, E, A, B](
      upstream: Of[R, E, A, StepKind],
      info: StepInfo,
      storedIn: Stored[A],
      storedOut: Stored[B],
  ):
    def apply(next: A => ZIO[R, E, B]): HermeticStep[R, E, B] =
      Then(upstream, info, storedIn, storedOut, next)
