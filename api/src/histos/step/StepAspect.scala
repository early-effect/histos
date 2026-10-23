package histos.step

import zio.{ZIO, ZIOAspect}

/** A decoration. `K` is the least kind it accepts, and it is contravariant, so a wider aspect cannot hide a narrower one. */
trait StepAspect[-R, +E, -K <: StepKind]:
  def apply[R1 <: R, E1 >: E, A, K1 <: StepKind](step: Step.Leaf[R1, E1, A, K1]): Step.Leaf[R1, E1, A, K1]

  /** The composite accepts only the steps both sides accept. */
  def @@[R1 <: R, E1 >: E, K2 <: StepKind](that: StepAspect[R1, E1, K2]): StepAspect[R1, E1, K & K2] =
    StepAspect.compose(this, that)

object StepAspect:
  val identity: StepAspect[Any, Nothing, StepKind] = new StepAspect[Any, Nothing, StepKind]:
    def apply[R1 <: Any, E1 >: Nothing, A, K1 <: StepKind](step: Step.Leaf[R1, E1, A, K1]): Step.Leaf[R1, E1, A, K1] =
      step

  /** Wrap the effect. The aspect's error joins the step's error at `@@`. */
  def lift[R, E](aspect: ZIOAspect[Nothing, R, E, Any, Nothing, Any]): StepAspect[R, E, StepKind] =
    new StepAspect[R, E, StepKind]:
      def apply[R1 <: R, E1 >: E, A, K1 <: StepKind](step: Step.Leaf[R1, E1, A, K1]): Step.Leaf[R1, E1, A, K1] =
        step.copy(run = aspect(step.run))

  val traced: StepAspect[Any, Nothing, StepKind] = new StepAspect[Any, Nothing, StepKind]:
    def apply[R1 <: Any, E1 >: Nothing, A, K1 <: StepKind](step: Step.Leaf[R1, E1, A, K1]): Step.Leaf[R1, E1, A, K1] =
      step.copy(run = ZIO.logSpan(step.info.id.toString)(step.run))

  val timed: StepAspect[Any, Nothing, StepKind] =
    lift(new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any]:
      def apply[R >: Nothing <: Any, E >: Nothing <: Any, A >: Nothing <: Any](zio: ZIO[R, E, A])(using
          _root_.zio.Trace
      ): ZIO[R, E, A] =
        zio.timed.map((_, value) => value)
    )

  private[step] def compose[R, E, K <: StepKind, K2 <: StepKind](
      first: StepAspect[R, E, K],
      second: StepAspect[R, E, K2],
  ): StepAspect[R, E, K & K2] =
    new StepAspect[R, E, K & K2]:
      def apply[R1 <: R, E1 >: E, A, K1 <: StepKind](step: Step.Leaf[R1, E1, A, K1]): Step.Leaf[R1, E1, A, K1] =
        second(first(step))
