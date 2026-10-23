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

    def visit[R1 <: R, E1 >: E, F[+_]](visitor: StepVisitor[R1, E1, F]): F[A]

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

    def visit[R1 <: R, E1 >: E, F[+_]](visitor: StepVisitor[R1, E1, F]): F[A] = visitor.onLeaf(this)

    def @@[R1 <: R, E1 >: E, K2 <: StepKind](aspect: StepAspect[R1, E1, K2])(using Fits[K, K2]): Of[R1, E1, A, K] =
      aspect(this)

  final case class Zipped[-R, +E, A, B, +K <: StepKind](left: Of[R, E, A, K], right: Of[R, E, B, K])
      extends Of[R, E, (A, B), K]:
    def mirror: RuntimeKind = RuntimeKind.join(left.mirror, right.mirror)

    def visit[R1 <: R, E1 >: E, F[+_]](visitor: StepVisitor[R1, E1, F]): F[(A, B)] = visitor.onZip(left, right)

    def @@[R1 <: R, E1 >: E, K2 <: StepKind](aspect: StepAspect[R1, E1, K2])(using Fits[K, K2]): Of[R1, E1, (A, B), K] =
      Zipped(left @@ aspect, right @@ aspect)

  final case class Mapped[-R, +E, A, B, +K <: StepKind](inner: Of[R, E, A, K], f: A => B) extends Of[R, E, B, K]:
    def mirror: RuntimeKind = inner.mirror

    def visit[R1 <: R, E1 >: E, F[+_]](visitor: StepVisitor[R1, E1, F]): F[B] = visitor.onMap(inner, f)

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

    def visit[R1 <: R, E1 >: E, F[+_]](visitor: StepVisitor[R1, E1, F]): F[B] = visitor.onThen(this)

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
    step.visit(new StepVisitor[R, E, [X] =>> ZIO[R, E, X]]:
      def onLeaf[B, K <: StepKind](leaf: Leaf[R, E, B, K]): ZIO[R, E, B] = leaf.run
      def onZip[B, C, K <: StepKind](left: Of[R, E, B, K], right: Of[R, E, C, K]): ZIO[R, E, (B, C)] =
        run(left).zip(run(right))
      def onMap[B, C, K <: StepKind](inner: Of[R, E, B, K], f: B => C): ZIO[R, E, C] = run(inner).map(f)
      def onThen[B, C](step: Then[R, E, B, C]): ZIO[R, E, C] = run(step.upstream).flatMap(step.next)
    )

  private[step] def promote[R, E, A](step: Of[R, E, A, StepKind.Cached]): Of[R, E, A, StepKind.Hermetic] =
    step.visit(new StepVisitor[R, E, [X] =>> Of[R, E, X, StepKind.Hermetic]]:
      def onLeaf[B, K <: StepKind](leaf: Leaf[R, E, B, K]): Of[R, E, B, StepKind.Hermetic] =
        Leaf(leaf.info, RuntimeKind.Hermetic, leaf.stored, leaf.run)
      def onZip[B, C, K <: StepKind](left: Of[R, E, B, K], right: Of[R, E, C, K]): Of[R, E, (B, C), StepKind.Hermetic] =
        Zipped(left.visit(this), right.visit(this))
      def onMap[B, C, K <: StepKind](inner: Of[R, E, B, K], f: B => C): Of[R, E, C, StepKind.Hermetic] =
        Mapped(inner.visit(this), f)
      def onThen[B, C](chain: Then[R, E, B, C]): Of[R, E, C, StepKind.Hermetic] = chain
    )

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

trait StepVisitor[R, E, F[+_]]:
  def onLeaf[A, K <: StepKind](leaf: Step.Leaf[R, E, A, K]): F[A]
  def onZip[A, B, K <: StepKind](left: Step.Of[R, E, A, K], right: Step.Of[R, E, B, K]): F[(A, B)]
  def onMap[A, B, K <: StepKind](inner: Step.Of[R, E, A, K], f: A => B): F[B]
  def onThen[A, B](step: Step.Then[R, E, A, B]): F[B]
