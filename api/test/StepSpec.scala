package histos.step

import histos.Platform

import scala.compiletime.testing.typeCheckErrors

import zio.{Chunk, ZIO, ZIOAspect}
import zio.test.*

object StepSpec extends ZIOSpecDefault:
  private val core: histos.ProjectName =
    histos.ProjectName.from("core") match
      case Right(name) => name
      case Left(_)     =>
        histos.ProjectName.from("core") match
          case Right(name) => name
          case Left(_)     => core

  private val info = StepInfo(
    StepId.Cell(core, histos.ScalaVersion("3.10.0"), Platform.Jvm, Phase.Compile),
    Chunk.empty,
    Set(Need.Cpu),
  )

  private val remote: StepAspect[Any, Nothing, StepKind.Hermetic] =
    new StepAspect[Any, Nothing, StepKind.Hermetic]:
      def apply[R1 <: Any, E1 >: Nothing, A, K1 <: StepKind](step: Step.Leaf[R1, E1, A, K1]): Step.Leaf[R1, E1, A, K1] =
        step

  private val cached: StepAspect[Any, Nothing, StepKind.Cached] =
    new StepAspect[Any, Nothing, StepKind.Cached]:
      def apply[R1 <: Any, E1 >: Nothing, A, K1 <: StepKind](step: Step.Leaf[R1, E1, A, K1]): Step.Leaf[R1, E1, A, K1] =
        step

  private enum Extra:
    case Boom

  private val failing: StepAspect[Any, Extra, StepKind] =
    new StepAspect[Any, Extra, StepKind]:
      def apply[R1 <: Any, E1 >: Extra, A, K1 <: StepKind](step: Step.Leaf[R1, E1, A, K1]): Step.Leaf[R1, E1, A, K1] =
        step.copy(run = step.run *> ZIO.fail(Extra.Boom))

  private given Stored[Int] with
    def encode(value: Int): Chunk[Byte] = Chunk.fromArray(value.toString.getBytes)
    def decode(bytes: Chunk[Byte]): Either[StoredError, Int] =
      new String(bytes.toArray).toIntOption match
        case Some(value) => Right(value)
        case None        => Left(StoredError.Malformed)
    def outputs(value: Int): Chunk[OutputFile] = Chunk.empty

  private final class Opaque

  private val localStep = Step.local[Int](info)(ZIO.succeed(1))
  private val cachedStep = Step.cached[Int](info)(ZIO.succeed(1))
  private val hermeticStep = Step.hermetic[Int](info)(ZIO.succeed(1))
  private val opaqueEffect: ZIO[Any, Nothing, Opaque] = ZIO.succeed(new Opaque())
  private val intEffect: ZIO[Any, Nothing, Int] = ZIO.succeed(1)

  private inline def messages(inline code: String): List[String] =
    typeCheckErrors(code).map(_.message).toList

  private val algebra = suite("step algebra")(
    test("a step runs, and map keeps the value derivation off the cache"):
      val step = Step.local[Int](info)(ZIO.succeed(1)).map(_ + 1)
      for
        value <- Step.run(step)
      yield assertTrue(value == 2, step.mirror == RuntimeKind.Local)
    ,
    test("zip of two hermetic steps stays hermetic, and hermetic with local is mixed"):
      val compile = Step.hermetic[Int](info)(ZIO.succeed(1))
      val link = Step.hermetic[Int](info)(ZIO.succeed(2))
      val publish = Step.local[Int](info)(ZIO.succeed(3))
      val same = compile.zip(link)
      val mixed = compile.zip(publish)
      for
        pair <- Step.run(same)
        both <- Step.run(mixed)
      yield assertTrue(pair == ((1, 2)), both == ((1, 3)), same.mirror == RuntimeKind.Hermetic, mixed.mirror == RuntimeKind.Mixed)
    ,
    test("a local value with a codec feeds a hermetic step"):
      val fetched = Step.local[Int](info)(ZIO.succeed(4))
      val compiled = fetched.andThenHermetic[Int](info)(sources => ZIO.succeed(sources + 1))
      for
        value <- Step.run(compiled)
      yield assertTrue(value == 5, compiled.mirror == RuntimeKind.Hermetic)
    ,
    test("an aspect error joins the step error"):
      val step = Step.local[Int](info)(ZIO.succeed(1)) @@ failing
      Step.run(step).either.map(result => assertTrue(result == Left(Extra.Boom)))
    ,
    test("sandboxed lifts a cached step so a hermetic aspect applies"):
      val testStep = Step.cached[Int](info)(ZIO.succeed(1)).sandboxed @@ remote
      for
        value <- Step.run(testStep)
      yield assertTrue(value == 1, testStep.mirror == RuntimeKind.Hermetic)
    ,
    test("lift keeps a ZIO aspect on the effect"):
      val mark = new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any]:
        def apply[R >: Nothing <: Any, E >: Nothing <: Any, A >: Nothing <: Any](
            zio: ZIO[R, E, A]
        )(using _root_.zio.Trace): ZIO[R, E, A] =
          zio.map(value => value)
      val step = Step.local[Int](info)(ZIO.succeed(7)) @@ StepAspect.lift(mark) @@ StepAspect.timed
      for
        value <- Step.run(step)
      yield assertTrue(value == 7)
    ,
    test("a remote aspect is rejected on a local step, a cached step, a mapped cached step, and a mixed zip"):
      val local = messages("localStep @@ remote")
      val testStep = messages("cachedStep @@ remote")
      val mapped = messages("cachedStep.map(_ + 1) @@ remote")
      val mixed = messages("hermeticStep.zip(localStep) @@ remote")
      assertTrue(
        local.exists(_.contains("StepKind.Local")),
        testStep.exists(_.contains("StepKind.Cached")),
        mapped.exists(_.contains("StepKind.Cached")),
        mixed.exists(_.contains("StepKind.Mixed")),
      )
    ,
    test("a value without a codec cannot feed a hermetic step or be cached"):
      val fed = messages("Step.local[Opaque](info)(opaqueEffect).andThenHermetic[Int](info)(_ => intEffect)")
      val stored = messages("Step.cached[Opaque](info)(opaqueEffect)")
      assertTrue(
        fed.exists(_.contains("has no Stored codec")),
        stored.exists(_.contains("has no Stored codec")),
      )
    ,
    test("composing aspects intersects their kinds"):
      val onCompile = messages("hermeticStep @@ (remote @@ StepAspect.timed)")
      val onTest = messages("cachedStep @@ (StepAspect.timed @@ remote)")
      val widened = messages("val anyKind: StepAspect[Any, Nothing, StepKind] = remote; hermeticStep @@ anyKind")
      assertTrue(onCompile.isEmpty, onTest.exists(_.contains("StepKind.Cached")), widened.nonEmpty)
    ,
  )

  def spec = algebra
