package histos.schedule

import histos.step.{Need, Step, StepId, StepInfo, Stored, StoredError}

import java.nio.charset.StandardCharsets

import zio.{Chunk, Duration, NonEmptyChunk, Ref, ZIO, ZLayer, durationInt}
import zio.test.*

object StepEventSpec extends ZIOSpecDefault:
  private val infoA = StepInfo(StepId.Work("a"), Chunk.empty, Set(Need.Cpu))
  private val infoB = StepInfo(StepId.Work("b"), Chunk.empty, Set(Need.Cpu))

  private given Stored[Int] with
    def encode(value: Int): Chunk[Byte] =
      Chunk.fromArray(value.toString.getBytes(StandardCharsets.UTF_8))
    def decode(bytes: Chunk[Byte]): Either[StoredError, Int] =
      new String(bytes.toArray, StandardCharsets.UTF_8).toIntOption match
        case Some(value) => Right(value)
        case None        => Left(StoredError.Malformed)
    def outputs(value: Int): Chunk[histos.step.OutputFile] = Chunk.empty

  private val opened =
    ZLayer.succeed(Parallelism(cpu = 1, io = 1, process = 1)) >>> Histos.live

  private def action(id: String, deps: ActionId*): Action =
    Action(
      ActionId.trusted(id),
      List(Input(InputName.trusted("src"), InputValue.Text(id))),
      deps.toList,
    )

  private def oneFinish[E](
      events: Chunk[StepEvent[E]],
      id: StepId,
      source: Source,
  ): Boolean =
    events.toList match
      case StepEvent.Started(started) ::
          StepEvent.Finished(done, origin, took) :: Nil =>
        started == id && done == id && origin == source &&
        took.compareTo(Duration.Zero) >= 0
      case _ => false

  private def watch[E](
      runner: Histos,
      scheduler: Scheduler,
      root: ActionId,
      actions: List[Action],
      local: (Action, String) => zio.IO[E, String],
      count: Int,
  ) =
    for
      step <- scheduler.planned(root, actions, local)
      pair <- ZIO.scoped:
        runner
          .start(step)
          .flatMap: running =>
            for
              outcome <- running.outcome
              events  <- running.events.take(count).runCollect
            yield (outcome, events)
    yield pair

  def spec = suite("step events")(
    test("a ran step records Source.Ran and its duration"):
      for
        calls  <- Ref.make(0)
        runner <- ZIO.service[Histos]
        node      = action("a")
        scheduler = Scheduler(
          runner,
          ActionCache.miss,
          RemoteCache.unconfigured,
        )
        pair <- watch(
          runner,
          scheduler,
          node.id,
          List(node),
          (_, _) => calls.update(_ + 1).as("ran"),
          2,
        )
        (outcome, events) = pair
        did <- calls.get
      yield assertTrue(
        outcome == Outcome.Done("ran"),
        did == 1,
        oneFinish(events, StepId.Work("a"), Source.Ran),
      )
    ,
    test("a local cache hit is Source.LocalHit and does not run"):
      val hit: ActionCache = new ActionCache:
        def get(digest: Digest)                = ZIO.some("from-local")
        def put(digest: Digest, value: String) = ZIO.unit
      for
        calls  <- Ref.make(0)
        runner <- ZIO.service[Histos]
        node      = action("a")
        scheduler = Scheduler(runner, hit, RemoteCache.unconfigured)
        pair <- watch(
          runner,
          scheduler,
          node.id,
          List(node),
          (_, _) => calls.update(_ + 1).as("ran"),
          2,
        )
        (outcome, events) = pair
        did <- calls.get
      yield assertTrue(
        outcome == Outcome.Done("from-local"),
        did == 0,
        oneFinish(events, StepId.Work("a"), Source.LocalHit),
      )
    ,
    test("a remote cache hit is Source.RemoteHit and does not run"):
      for
        calls  <- Ref.make(0)
        runner <- ZIO.service[Histos]
        node = action("a")
        remote <- TestRemoteCache.memory(RemoteMode.Read)
        _      <- remote.plant(
          Digest.of(node, Map.empty),
          Chunk.fromArray("from-remote".getBytes(StandardCharsets.UTF_8)),
        )
        scheduler = Scheduler(runner, ActionCache.miss, remote)
        pair <- watch(
          runner,
          scheduler,
          node.id,
          List(node),
          (_, _) => calls.update(_ + 1).as("ran"),
          2,
        )
        (outcome, events) = pair
        did <- calls.get
      yield assertTrue(
        outcome == Outcome.Done("from-remote"),
        did == 0,
        oneFinish(events, StepId.Work("a"), Source.RemoteHit),
      )
    ,
    test("a dependent served from the local cache is Source.LocalHit"):
      for
        calls  <- Ref.make(0)
        gets   <- Ref.make(0)
        runner <- ZIO.service[Histos]
        cache: ActionCache = new ActionCache:
          def get(digest: Digest) =
            gets
              .modify(n => (n, n + 1))
              .flatMap: seen =>
                if seen == 0 then ZIO.none else ZIO.some("from-cache")
          def put(digest: Digest, value: String) = ZIO.unit
        upstream  = action("b")
        root      = action("a", upstream.id)
        scheduler = Scheduler(runner, cache, RemoteCache.unconfigured)
        pair <- watch(
          runner,
          scheduler,
          root.id,
          List(root, upstream),
          (node, _) => calls.update(_ + 1).as(node.id.value),
          4,
        )
        (outcome, events) = pair
        did <- calls.get
      yield
        val sourced = events.toList match
          case StepEvent.Started(first) ::
              StepEvent.Finished(firstDone, Source.Ran, firstTook) ::
              StepEvent.Started(second) ::
              StepEvent.Finished(secondDone, Source.LocalHit, secondTook) ::
              Nil =>
            first == StepId.Work("b") && firstDone == first &&
            second == StepId.Work("a") && secondDone == second &&
            firstTook.compareTo(Duration.Zero) >= 0 &&
            secondTook.compareTo(Duration.Zero) >= 0
          case _ => false
        assertTrue(outcome == Outcome.Done("from-cache"), did == 1, sourced)
    ,
    test("an upstream failure stops the dependent as skipped"):
      ZIO.scoped:
        for
          ran    <- Ref.make(false)
          runner <- ZIO.service[Histos]
          upstream  = Step.local[Int](infoA)(ZIO.fail("boom"))
          dependent = upstream.andThenHermetic[Int](infoB)(_ => ran.set(true) *> ZIO.succeed(1))
          running <- runner.start(dependent)
          outcome <- running.outcome
          events  <- running.events.take(3).runCollect
          did     <- ran.get
        yield
          val skipped = events.toList match
            case StepEvent.Started(started) ::
                StepEvent.Stopped(StepFailure.Failed(failed, _)) ::
                StepEvent.Stopped(
                  StepFailure.Skipped(id, upstreamId)
                ) :: Nil =>
              started == infoA.id && failed == infoA.id && id == infoB.id &&
              upstreamId == NonEmptyChunk(infoA.id)
            case _ => false
          val failed = outcome match
            case _: Outcome.Failed[?, ?] => true
            case _: Outcome.Done[?, ?]   => false
          assertTrue(!did, failed, skipped),
  ).provide(opened) @@ TestAspect.withLiveClock @@ TestAspect.timeout(
    30.seconds
  )
end StepEventSpec
