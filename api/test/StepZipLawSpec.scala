package histos.step

import _root_.scala.compiletime.testing.typeCheckErrors

import zio.*
import zio.test.*

object StepZipLawSpec extends ZIOSpecDefault:
  private val info = StepInfo(StepId.Work("law"), Chunk.empty, Set.empty)
  private val next = StepInfo(StepId.Work("next"), Chunk.empty, Set.empty)

  given Stored[Int] with
    def encode(value: Int): Chunk[Byte]                      = Chunk.fromArray(value.toString.getBytes("UTF-8"))
    def decode(bytes: Chunk[Byte]): Either[StoredError, Int] =
      String(bytes.toArray, "UTF-8").toIntOption.toRight(StoredError.Malformed)
    def outputs(value: Int): Chunk[OutputFile] = Chunk.empty

  def spec = suite("step zip laws")(
    test("a cached pair stays cached, a hermetic pair stays hermetic, and hermetic with cached is cached"):
      val cached    = Step.cached[Int](info)(ZIO.succeed(2))
      val hermetic  = Step.hermetic[Int](info)(ZIO.succeed(4))
      val sameCache = cached.zip(Step.cached[Int](info)(ZIO.succeed(3)))
      val sameBox   = hermetic.zip(Step.hermetic[Int](info)(ZIO.succeed(5)))
      val weaker    = hermetic.zip(cached)
      val promoted  = sameCache.sandboxed
      for
        cachedValue   <- Step.run(sameCache)
        hermeticValue <- Step.run(sameBox)
        weakerValue   <- Step.run(weaker)
        promotedValue <- Step.run(promoted)
      yield assertTrue(
        sameCache.mirror == RuntimeKind.Cached,
        sameBox.mirror == RuntimeKind.Hermetic,
        weaker.mirror == RuntimeKind.Cached,
        promoted.mirror == RuntimeKind.Hermetic,
        cachedValue == (2, 3),
        hermeticValue == (4, 5),
        weakerValue == (4, 2),
        promotedValue == (2, 3),
      )
    ,
    test("identity, trace, and time keep the value, and a composed aspect visits every leaf it decorates"):
      val marked = StepAspect.traced @@ StepAspect.timed
      for
        hits    <- Ref.make(0)
        plain   <- Step.run(Step.local[Int](info)(ZIO.succeed(7)) @@ StepAspect.identity)
        spanned <- Step.run(Step.local[Int](info)(ZIO.succeed(7)) @@ marked)
        once    <- Step.run(Step.local[Int](info)(ZIO.succeed(2)) @@ counting(hits))
        twice   <- Step.run(
          Step.cached[Int](info)(ZIO.succeed(2)).zip(Step.cached[Int](info)(ZIO.succeed(3))) @@ counting(hits)
        )
        mapped  <- Step.run(Step.cached[Int](info)(ZIO.succeed(2)).map(_ + 1) @@ counting(hits))
        chained <- Step.run(
          Step
            .cached[Int](info)(ZIO.succeed(2))
            .andThenHermetic[Int](next)(value => ZIO.succeed(value + 1)) @@ counting(hits)
        )
        seen <- hits.get
      yield assertTrue(
        plain == 7,
        spanned == 7,
        once == 2,
        twice == (2, 3),
        mapped == 3,
        chained == 3,
        seen == 1 + 2 + 1 + 1,
      )
    ,
    test("the intersection of cached and remote fits a hermetic step and refuses a cached one"):
      assertTrue(
        typeCheckErrors("""import histos.step.*
import zio.{Chunk, ZIO}
val info = StepInfo(StepId.Work("law"), Chunk.empty, Set.empty)
given Stored[Int] with
  def encode(value: Int): Chunk[Byte] = Chunk.empty
  def decode(bytes: Chunk[Byte]): Either[StoredError, Int] = Right(0)
  def outputs(value: Int): Chunk[OutputFile] = Chunk.empty
Step.hermetic[Int](info)(ZIO.succeed(1)) @@ (StepAspect.cached @@ StepAspect.remote)
""").isEmpty,
        typeCheckErrors("""import histos.step.*
import zio.{Chunk, ZIO}
val info = StepInfo(StepId.Work("law"), Chunk.empty, Set.empty)
given Stored[Int] with
  def encode(value: Int): Chunk[Byte] = Chunk.empty
  def decode(bytes: Chunk[Byte]): Either[StoredError, Int] = Right(0)
  def outputs(value: Int): Chunk[OutputFile] = Chunk.empty
Step.cached[Int](info)(ZIO.succeed(1)) @@ (StepAspect.cached @@ StepAspect.remote)
""").nonEmpty,
      ),
  )

  private def counting(hits: Ref[Int]): StepAspect[Any, Nothing, StepKind] =
    StepAspect.lift(new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any]:
      def apply[R >: Nothing <: Any, E >: Nothing <: Any, A >: Nothing <: Any](
          zio: ZIO[R, E, A]
      )(using _root_.zio.Trace): ZIO[R, E, A] =
        hits.update(_ + 1) *> zio)
end StepZipLawSpec
