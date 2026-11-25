package histos.plugin

import histos.doctor.Noted
import histos.lock.{LockContext, LockDecision, LockStrategy, LockStrategyError}
import histos.suite.HistosSpec

import _root_.scala.compiletime.testing.typeChecks

import zio.{Exit, Promise, Ref, Scope, ZIO, ZLayer}
import zio.test.*

object BuildServicesSpec extends HistosSpec:
  private def exactLabel(
      plan: Plan[NoExtraServices & ReviewLabel, LabelSetupError]
  ): Plan[NoExtraServices & ReviewLabel, LabelSetupError] = plan

  private def exactTasks(
      plan: Plan[NoExtraServices & ReviewLabel & ReviewTasks, Nothing]
  ): Plan[NoExtraServices & ReviewLabel & ReviewTasks, Nothing] = plan

  private def known(error: ServiceActivationError): String = error match
    case ServiceActivationError.Setup(diagnosis)              => diagnosis.summary
    case ServiceActivationError.MissingCapability(key, phase) => s"${key.value} $phase"
    case ServiceActivationError.BadKey(raw)                   => raw
    case ServiceActivationError.DuplicateService(key)         => key.value
    case ServiceActivationError.DuplicateReplacement(slot)    => slot.toString

  private inline val withLabel =
    """
    import histos.plugin.*
    import zio.ZLayer
    trait ReviewLabel
    final class Tasks(label: ReviewLabel) extends HistosPlugin:
      val id = PluginId("reviewed-locks")
      def contribute = Contribution(Nil, Nil, Nil)
      def run(request: RunRequest) = zio.ZIO.succeed(PluginOutput("", Nil))
    object Tasks:
      val layer: ZLayer[ReviewLabel, Nothing, Tasks] = ZLayer.fromFunction(Tasks(_))
    val wired = BuildServices.default.add[ReviewLabel](ZLayer.succeed[ReviewLabel](new ReviewLabel {})).tasks[Tasks](Tasks.layer)
    """

  private inline val withoutLabel =
    """
    import histos.plugin.*
    import zio.ZLayer
    trait ReviewLabel
    final class Tasks(label: ReviewLabel) extends HistosPlugin:
      val id = PluginId("reviewed-locks")
      def contribute = Contribution(Nil, Nil, Nil)
      def run(request: RunRequest) = zio.ZIO.succeed(PluginOutput("", Nil))
    object Tasks:
      val layer: ZLayer[ReviewLabel, Nothing, Tasks] = ZLayer.fromFunction(Tasks(_))
    val missing = BuildServices.default.tasks[Tasks](Tasks.layer)
    """

  private inline val earlyLoading =
    """
    import histos.plugin.*
    val early = StrategyServices.default.withPluginLoading(PluginLoading.exactOrRebuild)
    """

  private inline val lateLoading =
    """
    import histos.plugin.*
    val late = BuildServices.default.withPluginLoading(PluginLoading.exactOrRebuild)
    """

  private inline val invocationLayer =
    """
    import histos.plugin.*
    import zio.{ZIO, ZLayer}
    trait ReviewLabel
    val layer: ZLayer[InvocationContext, Nothing, ReviewLabel] =
      ZLayer.fromZIO(ZIO.serviceWith[InvocationContext](_ => new ReviewLabel {}))
    """

  private inline val invocationRegistration =
    """
    import histos.plugin.*
    import zio.{ZIO, ZLayer}
    trait ReviewLabel
    val layer: ZLayer[InvocationContext, Nothing, ReviewLabel] =
      ZLayer.fromZIO(ZIO.serviceWith[InvocationContext](_ => new ReviewLabel {}))
    val added = BuildServices.default.add[ReviewLabel](layer)
    """

  private inline val invocationEvents =
    """
    import histos.plugin.*
    import zio.{ZIO, ZLayer}
    trait ReviewLabel
    val layer: ZLayer[PluginEvents.Invocation, Nothing, ReviewLabel] =
      ZLayer.fromZIO(ZIO.serviceWith[PluginEvents.Invocation](_ => new ReviewLabel {}))
    val added = BuildServices.default.add[ReviewLabel](layer)
    """

  private inline val invocationEventsLayer =
    """
    import histos.plugin.*
    import zio.{ZIO, ZLayer}
    trait ReviewLabel
    val layer: ZLayer[PluginEvents.Invocation, Nothing, ReviewLabel] =
      ZLayer.fromZIO(ZIO.serviceWith[PluginEvents.Invocation](_ => new ReviewLabel {}))
    """

  private inline val intersectionTag =
    """
    import zio.Tag
    trait ReviewLabel
    trait Marker
    given Tag[ReviewLabel & Marker] = Tag[ReviewLabel & Marker]
    """

  private inline val intersectionKey =
    """
    import histos.plugin.*
    import zio.{Tag, ZLayer}
    trait ReviewLabel
    trait Marker
    given Tag[ReviewLabel & Marker] = Tag[ReviewLabel & Marker]
    val layer: ZLayer[Any, Nothing, ReviewLabel & Marker] =
      ZLayer.succeed[ReviewLabel & Marker](new ReviewLabel with Marker {})
    val added = BuildServices.default.add[ReviewLabel & Marker](layer)
    """

  private inline val oneCapability =
    """
    import histos.plugin.*
    import zio.{Tag, ZLayer}
    trait ReviewLabel
    given Tag[ReviewLabel] = Tag[ReviewLabel]
    val layer: ZLayer[Any, Nothing, ReviewLabel] = ZLayer.succeed[ReviewLabel](new ReviewLabel {})
    val added = BuildServices.default.add[ReviewLabel](layer)
    """

  def spec = suite("build services")(
    test("CheckedReviewLabel infers LabelSetupError without an ascription") {
      val checked = exactLabel(BuildServices.default.add[ReviewLabel](CheckedReviewLabel.layer("reviewed")))
      val tasks   = exactTasks(
        BuildServices.default.add[ReviewLabel](ReviewLabel.layer("reviewed")).tasks[ReviewTasks](ReviewTasks.layer)
      )
      assertTrue((checked eq checked), (tasks eq tasks), known(ServiceActivationError.BadKey("nope")) == "nope")
    },
    test(
      "a missing service, a late loading override, an invocation requirement, and an intersection key do not compile"
    ) {
      assertTrue(
        typeChecks(withLabel),
        !typeChecks(withoutLabel),
        typeChecks(earlyLoading),
        !typeChecks(lateLoading),
        typeChecks(invocationLayer),
        !typeChecks(invocationRegistration),
        typeChecks(invocationEventsLayer),
        !typeChecks(invocationEvents),
        typeChecks(oneCapability),
        typeChecks(intersectionTag),
        !typeChecks(intersectionKey),
      )
    },
    test("a duplicate service is refused before its layer runs") {
      ZIO.scoped:
        for
          count <- Ref.make(0)
          layer = ZLayer.fromZIO:
            count.update(_ + 1) *> ZIO.succeed[ReviewLabel](LiveReviewLabel("reviewed"))
          exit <- activate(BuildServices.default.add[ReviewLabel](layer).add[ReviewLabel](layer).activate).exit
          seen <- count.get
          duplicate = failed(exit) match
            case Some(ServiceActivationError.DuplicateService(_)) => true
            case _                                                => false
        yield assertTrue(duplicate, seen == 0)
    },
    test("a second withLocks is refused before its layer runs") {
      ZIO.scoped:
        for
          count <- Ref.make(0)
          layer = ZLayer.fromZIO:
            count.update(_ + 1) *> ZIO.succeed[LockStrategy](Ambient.locks)
          exit <- activate(
            BuildServices.default.withLocks(layer).withLocks(layer).activate
          ).exit
          seen <- count.get
          slot = failed(exit) match
            case Some(ServiceActivationError.DuplicateReplacement(CoreSlot.Locks)) => true
            case _                                                                 => false
        yield assertTrue(slot, seen == 0)
    },
    test("a second loading strategy is refused before its layer runs") {
      ZIO.scoped:
        for
          count <- Ref.make(0)
          layer = ZLayer.fromZIO:
            count.update(_ + 1) *> ZIO.succeed[PluginLoadingStrategy](Loading)
          exit <- activateStrategy(
            StrategyServices.default.withPluginLoading(layer).withPluginLoading(layer).activate
          ).exit
          seen <- count.get
          slot = failed(exit) match
            case Some(ServiceActivationError.DuplicateReplacement(CoreSlot.PluginLoading)) => true
            case _                                                                         => false
        yield assertTrue(slot, seen == 0)
    },
    test("withLocks keeps a concrete strategy under the lock interface") {
      val asked = AskedLocks()
      val layer = ZLayer.succeed(asked)
      ZIO.scoped:
        for
          built    <- activate(BuildServices.default.withLocks(layer).activate)
          strategy <- activateStrategy(StrategyServices.default.withLocks(layer).activate)
        yield assertTrue((built.locks eq asked), strategy.locks.contains(asked))
    },
    test("an empty label keeps the previous generation and surfaces the diagnosis") {
      val replacement = AskedLocks()
      val previous    = BuildServices.default.withLocks(ZLayer.succeed[LockStrategy](replacement))
      val failing     = BuildServices.default.add[ReviewLabel](CheckedReviewLabel.layer(""))
      ZIO.scoped:
        for
          held     <- Promise.make[Nothing, Unit]
          _        <- ZIO.addFinalizer(held.succeed(()))
          acquired <- Ref.make(0)
          kept     <- activate(previous.activate)
          exit     <- ZIO.scoped(activate(failing.activate).exit)
          released = ZLayer.scoped[Any]:
            ZIO.acquireRelease(acquired.update(_ + 1))(_ => acquired.update(_ - 1)) *>
              ZIO.fail(LabelSetupError.Empty)
          releaseExit <- ZIO.scoped(activate(BuildServices.default.add[ReviewLabel](released).activate).exit)
          open        <- held.isDone
          still       <- acquired.get
          noted = failed(exit) match
            case Some(LabelSetupError.Empty) => Some(LabelSetupError.Empty.diagnosis)
            case _                           => None
          releasedNoted = failed(releaseExit) match
            case Some(LabelSetupError.Empty) => true
            case _                           => false
        yield assertTrue(
          !open,
          kept.locks eq replacement,
          still == 0,
          releasedNoted,
          noted.contains(LabelSetupError.Empty.diagnosis),
          LabelSetupError.Empty.diagnosis.summary == "Review label is empty",
        )
    },
    test("add and tasks of one capability construct one instance") {
      ZIO.scoped:
        for
          made <- Ref.make[List[ActivationEcho]](Nil)
          good = ZLayer.fromZIO:
            val echo = ActivationEcho("once")
            made.update(echo :: _) *> ZIO.succeed(echo)
          built <- activate(
            BuildServices.default
              .add[ActivationEcho](good)
              .tasks[ActivationEcho](ZLayer.fail(EchoSetup.Second))
              .activate
          )
          seen  <- made.get
          again <- Ref.make[List[ActivationEcho]](Nil)
          taskLayer = ZLayer.fromZIO:
            val echo = ActivationEcho("task")
            again.update(echo :: _) *> ZIO.succeed(echo)
          reversed <- activate(
            BuildServices.default
              .tasks[ActivationEcho](taskLayer)
              .add[ActivationEcho](ZLayer.fail(EchoSetup.Second))
              .activate
          )
          seenAgain <- again.get
        yield
          val forward = built.tasks match
            case (echo: ActivationEcho) :: Nil => seen.length == 1 && seen.contains(echo) && echo.token == "once"
            case _                             => false
          val backward = reversed.tasks match
            case (echo: ActivationEcho) :: Nil =>
              seenAgain.length == 1 && seenAgain.contains(echo) && echo.token == "task"
            case _ => false
          assertTrue(forward, backward)
    },
    test("a concrete layer is registered under the capability interface") {
      ZIO.scoped:
        for
          count <- Ref.make(0)
          layer = ZLayer.fromZIO:
            count.update(_ + 1) *> ZIO.succeed(LiveReviewLabel("reviewed"))
          built <- activate(
            BuildServices.default.add[ReviewLabel](layer).tasks[ActivationSeen](ActivationSeen.layer).activate
          )
          seen <- count.get
          text = built.tasks match
            case (found: ActivationSeen) :: Nil => Some(found.text)
            case _                              => None
        yield assertTrue(seen == 1, text.contains("reviewed"))
    },
    test("a service acquired during activation stays until the generation scope closes") {
      for
        open   <- Ref.make(0)
        during <- ZIO.scoped:
          activate:
            val layer = ZLayer.scoped[Any]:
              ZIO.acquireRelease(open.update(_ + 1))(_ => open.update(_ - 1)) *>
                ZIO.succeed[ReviewLabel](LiveReviewLabel("reviewed"))
            BuildServices.default.add[ReviewLabel](layer).activate *> open.get
        after <- open.get
      yield assertTrue(during == 1, after == 0)
    },
    test("interrupting setup runs finalizers and leaves the generation scope open") {
      ZIO.scoped:
        for
          held   <- Promise.make[Nothing, Unit]
          _      <- ZIO.addFinalizer(held.succeed(()))
          opened <- Promise.make[Nothing, Unit]
          closed <- Promise.make[Nothing, Unit]
          layer = ZLayer.scoped[Any]:
            ZIO.acquireRelease(opened.succeed(()))(_ => closed.succeed(())) *>
              ZIO.never *> ZIO.succeed[ReviewLabel](LiveReviewLabel("reviewed"))
          fiber <- activate(BuildServices.default.add[ReviewLabel](layer).activate).fork
          _     <- opened.await
          _     <- fiber.interrupt
          _     <- closed.await
          open  <- held.isDone
        yield assertTrue(!open)
    },
  )

  private def failed[E](exit: Exit[E, ?]): Option[E] =
    exit match
      case Exit.Failure(cause) => cause.failureOption
      case Exit.Success(_)     => None

  private def activate[E, A](
      effect: ZIO[GenerationAmbient & CoreServices & Scope, E, A]
  ): ZIO[Scope, E, A] =
    ZIO.serviceWithZIO[Scope]: scope =>
      effect.provide(
        ZLayer.succeed[BuildContext](Ambient.context),
        ZLayer.succeed[PluginEvents.Generation](Ambient.events),
        ZLayer.succeed[LockStrategy](Ambient.locks),
        PluginLoading.exactOrRebuild,
        ZLayer.succeed(scope),
      )

  private def activateStrategy[E, A](
      effect: ZIO[GenerationAmbient & Scope, E, A]
  ): ZIO[Scope, E, A] =
    ZIO.serviceWithZIO[Scope]: scope =>
      effect.provide(
        ZLayer.succeed[BuildContext](Ambient.context),
        ZLayer.succeed[PluginEvents.Generation](Ambient.events),
        ZLayer.succeed(scope),
      )

  private object Ambient:
    private inline val digest = "0000000000000000000000000000000000000000000000000000000000000000"

    val context: BuildContext = new BuildContext:
      def generation: GenerationId  = GenerationId(digest)
      def repository: String        = "repo"
      def declarations: ContentHash = ContentHash(digest)
      def host: ApiDigest           = ApiDigest(digest)

    val events: PluginEvents.Generation = new PluginEvents.Generation:
      def emit(event: GenerationPluginEvent): zio.IO[PluginEventError, Unit] = ZIO.unit

    val locks: LockStrategy = new LockStrategy:
      def decide(context: LockContext): zio.IO[LockStrategyError, LockDecision] =
        ZIO.succeed(LockDecision.UsePinned)
  end Ambient

  private object Loading extends PluginLoadingStrategy:
    def plan(context: PluginLoadContext): zio.IO[PluginStrategyError, PluginLoadPlan] =
      ZIO.succeed(PluginLoadPlan.UseExactBinary)
end BuildServicesSpec

final class AskedLocks extends LockStrategy:
  def decide(context: LockContext): zio.IO[LockStrategyError, LockDecision] =
    ZIO.succeed(LockDecision.Frozen)

final class ActivationEcho(val token: String) extends HistosPlugin:
  val id = PluginId("activation-echo")

  def contribute: Contribution = Contribution(Nil, Nil, Nil)

  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput(token, Nil))
end ActivationEcho

enum EchoSetup extends ServiceSetupError:
  case Second

  def diagnosis: Noted = this match
    case Second =>
      Noted.error(
        "echo.setup",
        "The task layer ran",
        "A service and a task for one capability share one instance.",
        Noted.steps("Register the capability once."),
      )
end EchoSetup

final class ActivationSeen(val text: String) extends HistosPlugin:
  val id = PluginId("activation-seen")

  def contribute: Contribution = Contribution(Nil, Nil, Nil)

  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput(text, Nil))
end ActivationSeen

object ActivationSeen:
  def fromLabel(label: ReviewLabel): ActivationSeen = ActivationSeen(label.value)

  val layer: ZLayer[ReviewLabel, Nothing, ActivationSeen] =
    ZLayer.fromFunction(fromLabel)
end ActivationSeen
