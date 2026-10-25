package histos.schedule

import histos.step.{Need, Step, StepId, StepInfo, Stored, StoredError}

import zio.{Chunk, Promise, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object HistosSpec extends ZIOSpecDefault:
  private val infoA = StepInfo(StepId.Work("a"), Chunk.empty, Set(Need.Cpu))
  private val infoB = StepInfo(StepId.Work("b"), Chunk.empty, Set(Need.Cpu))

  private given Stored[Int] with
    def encode(value: Int): Chunk[Byte]                      = Chunk.fromArray(value.toString.getBytes)
    def decode(bytes: Chunk[Byte]): Either[StoredError, Int] =
      new String(bytes.toArray).toIntOption match
        case Some(value) => Right(value)
        case None        => Left(StoredError.Malformed)
    def outputs(value: Int): Chunk[histos.step.OutputFile] = Chunk.empty

  private val one = ZLayer.succeed(Parallelism(cpu = 1, io = 1, process = 1)) >>> Histos.live

  private val executor = suite("Histos")(
    test("a cpu limit of one does not run two steps at once"):
      def step(id: String, entered: Ref[Int], release: Promise[Nothing, Unit]) =
        Step.local[Unit](StepInfo(StepId.Work(id), Chunk.empty, Set(Need.Cpu)))(
          entered.updateAndGet(_ + 1) *> release.await
        )
      ZIO.scoped:
        for
          entered <- Ref.make(0)
          release <- Promise.make[Nothing, Unit]
          running <- ZIO.serviceWithZIO[Histos](_.start(step("a", entered, release).zip(step("b", entered, release))))
          held    <- entered.get.repeatUntil(_ == 1)
          _       <- release.succeed(())
          outcome <- running.outcome
          after   <- entered.get
        yield assertTrue(held == 1, after == 2, outcome == Outcome.Done(((), ())))
    ,
    test("a failed upstream does not run its dependent"):
      ZIO.scoped:
        for
          ran <- Ref.make(false)
          upstream  = Step.local[Int](infoA)(ZIO.fail("boom"))
          dependent = upstream.andThenHermetic[Int](infoB)(_ => ran.set(true) *> ZIO.succeed(1))
          running <- ZIO.serviceWithZIO[Histos](_.start(dependent))
          outcome <- running.outcome
          did     <- ran.get
        yield
          val failed = outcome match
            case _: Outcome.Failed[?, ?] => true
            case _: Outcome.Done[?, ?]   => false
          assertTrue(!did, failed)
    ,
    (test("events stay readable after the run completes"):
      ZIO.scoped:
        for
          running <- ZIO.serviceWithZIO[Histos](
            _.start(Step.local[Unit](infoA)(ZIO.unit))
          )
          _     <- running.outcome
          found <- running.events.take(2).runCollect.timeout(2.seconds)
        yield
          val readable = found match
            case Some(events) =>
              events.exists:
                case StepEvent.Finished(_, Source.Ran, _) => true
                case _                                    => false
            case None => false
          assertTrue(readable)
    ) @@ TestAspect.withLiveClock,
  ).provide(one)

  def spec = executor
end HistosSpec
