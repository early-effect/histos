package histos.step

import _root_.scala.compiletime.testing.typeCheckErrors

import zio.*
import zio.test.*

object StepLawSpec extends ZIOSpecDefault:
  private val info = StepInfo(StepId.Work("law"), Chunk.empty, Set.empty)

  given Stored[Int] with
    def encode(value: Int): Chunk[Byte]                      = Chunk.fromArray(value.toString.getBytes("UTF-8"))
    def decode(bytes: Chunk[Byte]): Either[StoredError, Int] =
      String(bytes.toArray, "UTF-8").toIntOption.toRight(StoredError.Malformed)
    def outputs(value: Int): Chunk[OutputFile] = Chunk.empty

  def spec = suite("step laws")(
    test("joining two runtime kinds is the weakest kind that still describes both"):
      val joined = List(
        (RuntimeKind.Local, RuntimeKind.Local, RuntimeKind.Local),
        (RuntimeKind.Local, RuntimeKind.Cached, RuntimeKind.Mixed),
        (RuntimeKind.Local, RuntimeKind.Hermetic, RuntimeKind.Mixed),
        (RuntimeKind.Local, RuntimeKind.Mixed, RuntimeKind.Mixed),
        (RuntimeKind.Cached, RuntimeKind.Local, RuntimeKind.Mixed),
        (RuntimeKind.Cached, RuntimeKind.Cached, RuntimeKind.Cached),
        (RuntimeKind.Cached, RuntimeKind.Hermetic, RuntimeKind.Cached),
        (RuntimeKind.Cached, RuntimeKind.Mixed, RuntimeKind.Mixed),
        (RuntimeKind.Hermetic, RuntimeKind.Local, RuntimeKind.Mixed),
        (RuntimeKind.Hermetic, RuntimeKind.Cached, RuntimeKind.Cached),
        (RuntimeKind.Hermetic, RuntimeKind.Hermetic, RuntimeKind.Hermetic),
        (RuntimeKind.Hermetic, RuntimeKind.Mixed, RuntimeKind.Mixed),
        (RuntimeKind.Mixed, RuntimeKind.Local, RuntimeKind.Mixed),
        (RuntimeKind.Mixed, RuntimeKind.Cached, RuntimeKind.Mixed),
        (RuntimeKind.Mixed, RuntimeKind.Hermetic, RuntimeKind.Mixed),
        (RuntimeKind.Mixed, RuntimeKind.Mixed, RuntimeKind.Mixed),
      )
      assertTrue(joined.filterNot((left, right, expected) => RuntimeKind.join(left, right) == expected) == Nil)
    ,
    test("map keeps the mirror, zip joins it, and run visits both"):
      val left  = Step.local[Int](info)(ZIO.succeed(2))
      val right = Step.local[Int](info)(ZIO.succeed(3))
      val both  = left.zip(right).map((a, b) => a + b)
      for value <- Step.run(both)
      yield assertTrue(left.mirror == RuntimeKind.Local, both.mirror == RuntimeKind.Local, value == 5)
    ,
    test("a cached step becomes hermetic when sandboxed, and andThenHermetic is hermetic"):
      val cached  = Step.cached[Int](info)(ZIO.succeed(4)).map(_ + 1)
      val boxed   = cached.sandboxed
      val chained = Step
        .local[Int](info)(ZIO.succeed(4))
        .andThenHermetic[Int](StepInfo(StepId.Work("next"), Chunk.empty, Set.empty))(value => ZIO.succeed(value + 1))
      for
        sand  <- Step.run(boxed)
        chain <- Step.run(chained)
      yield assertTrue(
        cached.mirror == RuntimeKind.Cached,
        boxed.mirror == RuntimeKind.Hermetic,
        sand == 5,
        chained.mirror == RuntimeKind.Hermetic,
        chain == 5,
      )
    ,
    test("a local step accepts identity and refuses a hermetic or cached aspect"):
      assertTrue(
        typeCheckErrors("""import histos.step.*
import zio.Chunk
import zio.ZIO
val info = StepInfo(StepId.Work("law"), Chunk.empty, Set.empty)
Step.local[Int](info)(ZIO.succeed(1)) @@ StepAspect.identity
""").isEmpty,
        typeCheckErrors("""import histos.step.*
import zio.Chunk
import zio.ZIO
val info = StepInfo(StepId.Work("law"), Chunk.empty, Set.empty)
Step.local[Int](info)(ZIO.succeed(1)) @@ StepAspect.remote
""").nonEmpty,
        typeCheckErrors("""import histos.step.*
import zio.Chunk
import zio.ZIO
val info = StepInfo(StepId.Work("law"), Chunk.empty, Set.empty)
Step.local[Int](info)(ZIO.succeed(1)) @@ StepAspect.cached
""").nonEmpty,
        typeCheckErrors("""import histos.step.*
import zio.{Chunk, ZIO}
val info = StepInfo(StepId.Work("law"), Chunk.empty, Set.empty)
given Stored[Int] with
  def encode(value: Int): Chunk[Byte] = Chunk.empty
  def decode(bytes: Chunk[Byte]): Either[StoredError, Int] = Right(0)
  def outputs(value: Int): Chunk[OutputFile] = Chunk.empty
Step.hermetic[Int](info)(ZIO.succeed(1)) @@ StepAspect.remote
""").isEmpty,
        typeCheckErrors("""import histos.step.*
import zio.{Chunk, ZIO}
val info = StepInfo(StepId.Work("law"), Chunk.empty, Set.empty)
given Stored[Int] with
  def encode(value: Int): Chunk[Byte] = Chunk.empty
  def decode(bytes: Chunk[Byte]): Either[StoredError, Int] = Right(0)
  def outputs(value: Int): Chunk[OutputFile] = Chunk.empty
Step.hermetic[Int](info)(ZIO.succeed(1)) @@ StepAspect.cached
""").isEmpty,
      ),
  )
end StepLawSpec
