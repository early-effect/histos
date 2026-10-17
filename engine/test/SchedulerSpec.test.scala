package histos.schedule

import histos.task.{TaskError, TaskKind}

import zio.{IO, Promise, Ref, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object SchedulerSpec extends ZIOSpecDefault:
  private def aid(raw: String): ActionId = ActionId.trusted(raw)

  private val a = aid("a")
  private val b = aid("b")
  private val c = aid("c")
  private val d = aid("d")

  private def action(id: ActionId, deps: ActionId*): Action =
    Action(id, TaskKind.Inspect, List(Input("src", id.value)), deps.toList)

  private val localOk: Action => IO[TaskError, String] =
    action => ZIO.succeed(action.id.value)

  private def execute(root: ActionId, actions: List[Action], local: Action => IO[TaskError, String] = localOk) =
    ZIO.serviceWithZIO[Scheduler](_.execute(root, actions, local)).provide(Scheduler.live, ZLayer.succeed(ActionCache.miss))

  private val schedule = suite("scheduler")(
    test("a miss runs the action after its dependencies, and only those"):
      for
        seen <- Ref.make(List.empty[String])
        out  <- execute(
                  a,
                  List(action(a, b), action(b, c), action(c), action(d)),
                  action => seen.update(action.id.value :: _) *> ZIO.succeed(action.id.value),
                )
        ran <- seen.get
      yield assertTrue(out == "a" && ran.reverse == List("c", "b", "a")),
    test("a shared dependency runs once"):
      for
        count <- Ref.make(Map.empty[String, Int])
        _     <- execute(
                   a,
                   List(action(a, b, c), action(b, d), action(c, d), action(d)),
                   action => count.update(m => m + (action.id.value -> (m.getOrElse(action.id.value, 0) + 1))) *> ZIO.succeed("ok"),
                 )
        ran <- count.get
      yield assertTrue(ran.getOrElse("d", 0) == 1 && ran.getOrElse("a", 0) == 1),
    test("independent dependencies overlap"):
      (for
        startedB <- Promise.make[Nothing, Unit]
        startedC <- Promise.make[Nothing, Unit]
        out <-
          ZIO.serviceWithZIO[Scheduler](
            _.execute(
              a,
              List(action(a, b, c), action(b), action(c)),
              action =>
                if action.id == b then startedB.succeed(()) *> startedC.await.as("b")
                else if action.id == c then startedC.succeed(()) *> startedB.await.as("c")
                else ZIO.succeed("a"),
            )
          ).timeoutFail(ScheduleError.UnknownAction(a))(5.seconds)
      yield assertTrue(out == "a")
    ).provide(Scheduler.live, ZLayer.succeed(ActionCache.miss)),
    test("a cycle runs nothing"):
      for
        ran <- Ref.make(false)
        out <- execute(a, List(action(a, b), action(b, a)), _ => ran.set(true) *> ZIO.succeed("x")).either
        did <- ran.get
      yield assertTrue:
        out match
          case Left(ScheduleError.Cycle(path)) => !did && path.map(_.value).contains("a")
          case _                                => false
    ,
    test("interrupting the run interrupts the local action"):
      for
        started <- Promise.make[Nothing, Unit]
        fiber   <- execute(a, List(action(a)), _ => started.succeed(()) *> ZIO.never).fork
        _       <- started.await
        _       <- fiber.interrupt
        exit    <- fiber.await
      yield assertTrue(exit.isInterrupted),
    test("a cache hit does not run locally"):
      val hit = new ActionCache:
        def get(digest: Digest): UIO[Option[String]] = ZIO.some("cached")
      (for
        called <- Ref.make(false)
        out <- ZIO.serviceWithZIO[Scheduler](_.execute(a, List(action(a)), _ => called.set(true) *> ZIO.succeed("local"))).either
        did <- called.get
      yield assertTrue(out == Right("cached") && !did)
    ).provide(Scheduler.live, ZLayer.succeed(hit)),
    test("changing an input changes the digest"):
      val first  = action(a)
      val second = first.copy(inputs = List(Input("src", "other")))
      assertTrue(Digest.of(first, Map.empty) != Digest.of(second, Map.empty)),
  )

  def spec = schedule @@ TestAspect.withLiveClock @@ TestAspect.timeout(15.seconds)
