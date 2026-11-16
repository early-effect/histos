package histos.plugin

import histos.doctor.Noted
import histos.lock.LockStrategy

import zio.{Scope, Tag, ZEnvironment, ZIO, ZLayer}

/** Stable generation data. No command, caller, or consent channel. */
trait BuildContext:
  def generation: GenerationId
  def repository: String
  def declarations: ContentHash
  def host: ApiDigest

/** Marker for a plan that has registered no extra service. */
sealed trait NoExtraServices

object NoExtraServices:
  private[plugin] case object Marker extends NoExtraServices
  private[plugin] def marker: NoExtraServices = Marker

type GenerationAmbient = BuildContext & PluginEvents.Generation
type CoreServices      = LockStrategy & PluginLoadingStrategy

/** A plugin's typed setup failure. It is not an exception. */
trait ServiceSetupError:
  def diagnosis: Noted

enum ActivationPhase:
  case Strategy
  case Main

enum CoreSlot:
  case Locks
  case PluginLoading

enum ServiceActivationError:
  case Setup(diagnosis: Noted)
  case MissingCapability(key: ServiceKey, phase: ActivationPhase)
  case BadKey(raw: String)
  case DuplicateService(key: ServiceKey)
  case DuplicateReplacement(slot: CoreSlot)

  def message: String = this match
    case Setup(diagnosis)              => diagnosis.summary
    case MissingCapability(key, phase) => s"capability ${key.value} is missing at $phase"
    case BadKey(raw)                   => s"'$raw' is not a service key"
    case DuplicateService(key)         => s"service ${key.value} is registered twice"
    case DuplicateReplacement(slot)    => s"$slot is replaced twice"
end ServiceActivationError

/** Host-facing result. User services stay in the generation environment that built them. `locks` is the replacement, or
  * the core strategy when the plan did not call `.withLocks`.
  */
final case class ActivatedBuildServices(locks: LockStrategy, tasks: List[HistosPlugin])

/** The associated-type plan is [[Plan]]. This trait is what [[histos.HistosBuild.services]] returns. */
trait BuildServices:
  type Extra
  type Error <: ServiceSetupError

object BuildServices:
  def default: Plan[NoExtraServices, Nothing] = Plan.empty

final class AddPartiallyApplied[S, E <: ServiceSetupError, A](plan: Plan[S, E])(using Tag[A]):
  def apply(layer: ZLayer[GenerationAmbient & CoreServices & S, Nothing, A]): Plan[S & A, E] =
    Plan.added(plan, layer)

  def apply[E1 <: ServiceSetupError](
      layer: ZLayer[GenerationAmbient & CoreServices & S, E1, A]
  )(using DummyImplicit): Plan[S & A, E | E1] =
    Plan.addedE(plan, layer)
end AddPartiallyApplied

final class TasksPartiallyApplied[S, E <: ServiceSetupError, P <: HistosPlugin](plan: Plan[S, E])(using Tag[P]):
  def apply(layer: ZLayer[GenerationAmbient & CoreServices & S, Nothing, P]): Plan[S & P, E] =
    Plan.task(plan, layer)

  def apply[E1 <: ServiceSetupError](
      layer: ZLayer[GenerationAmbient & CoreServices & S, E1, P]
  )(using DummyImplicit): Plan[S & P, E | E1] =
    Plan.taskE(plan, layer)
end TasksPartiallyApplied

/** `S` is the registered capability intersection. Each capability keeps its own [[zio.Tag]]. An intersection tag is not
  * summoned.
  */
sealed abstract class Plan[S, E <: ServiceSetupError] extends BuildServices:
  type Extra = S
  type Error = E

  def withLocks(layer: ZLayer[GenerationAmbient, Nothing, LockStrategy]): Plan[S, E] =
    Plan.locks(this, layer)

  def withLocks[E1 <: ServiceSetupError](layer: ZLayer[GenerationAmbient, E1, LockStrategy])(using
      DummyImplicit
  ): Plan[S, E | E1] =
    Plan.locksE(this, layer)

  def add[A: Tag]: AddPartiallyApplied[S, E, A] =
    AddPartiallyApplied(this)

  def tasks[P <: HistosPlugin: Tag]: TasksPartiallyApplied[S, E, P] =
    TasksPartiallyApplied(this)

  /** Builds registered layers in the caller scope. A duplicate is refused before any layer runs. */
  def activate: ZIO[
    GenerationAmbient & CoreServices & Scope,
    ServiceActivationError | E,
    ActivatedBuildServices,
  ] =
    Plan.refuse(recorded) match
      case Some(error) => ZIO.fail(error)
      case None        =>
        ZIO.serviceWithZIO[LockStrategy]: core =>
          materialize.flatMap: chain =>
            chain.locks match
              case Nil                => ZIO.succeed(ActivatedBuildServices(core, chain.tasks))
              case replacement :: Nil => ZIO.succeed(ActivatedBuildServices(replacement, chain.tasks))
              case _                  => ZIO.fail(ServiceActivationError.DuplicateReplacement(CoreSlot.Locks))

  protected def recorded: List[Plan.Slot]

  protected def materialize: ZIO[
    GenerationAmbient & CoreServices & Scope,
    ServiceActivationError | E,
    Plan.Chain[S],
  ]
end Plan

object Plan:
  private[plugin] enum Slot:
    case Lock
    case Service(tag: Tag[?])
    case Task(tag: Tag[?])

  private[plugin] final case class Chain[S](
      environment: ZEnvironment[S],
      locks: List[LockStrategy],
      tasks: List[HistosPlugin],
  )

  private[plugin] def empty: Plan[NoExtraServices, Nothing] = Empty()

  private[plugin] def locks[S, E <: ServiceSetupError](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient, Nothing, LockStrategy],
  ): Plan[S, E] =
    Locks(prev, layer)

  private[plugin] def locksE[S, E <: ServiceSetupError, E1 <: ServiceSetupError](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient, E1, LockStrategy],
  ): Plan[S, E | E1] =
    LocksE(prev, layer)

  private[plugin] def added[S, E <: ServiceSetupError, A: Tag](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient & CoreServices & S, Nothing, A],
  ): Plan[S & A, E] =
    Added(prev, layer)

  private[plugin] def addedE[S, E <: ServiceSetupError, A: Tag, E1 <: ServiceSetupError](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient & CoreServices & S, E1, A],
  ): Plan[S & A, E | E1] =
    AddedE(prev, layer)

  private[plugin] def task[S, E <: ServiceSetupError, P <: HistosPlugin: Tag](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient & CoreServices & S, Nothing, P],
  ): Plan[S & P, E] =
    Tasked(prev, layer)

  private[plugin] def taskE[S, E <: ServiceSetupError, P <: HistosPlugin: Tag, E1 <: ServiceSetupError](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient & CoreServices & S, E1, P],
  ): Plan[S & P, E | E1] =
    TaskedE(prev, layer)

  private[plugin] def refuse(slots: List[Slot]): Option[ServiceActivationError] =
    val locks = slots.count:
      case Slot.Lock => true
      case _         => false
    if locks > 1 then Some(ServiceActivationError.DuplicateReplacement(CoreSlot.Locks))
    else
      coreConflict(slots) match
        case found @ Some(_) => found
        case None            =>
          repeated(slots.collect { case Slot.Service(tag) => tag }) match
            case Some(tag) => Some(duplicate(tag))
            case None      =>
              repeated(slots.collect { case Slot.Task(tag) => tag }) match
                case Some(tag) => Some(duplicate(tag))
                case None      => None

  private def repeated(tags: List[Tag[?]]): Option[Tag[?]] =
    tags
      .foldLeft((List.empty[Tag[?]], Option.empty[Tag[?]])):
        case ((_, found @ Some(_)), _)                            => (Nil, found)
        case ((seen, None), tag) if seen.exists(_.tag == tag.tag) => (seen, Some(tag))
        case ((seen, None), tag)                                  => (tag :: seen, None)
      ._2

  private def coreConflict(slots: List[Slot]): Option[ServiceActivationError] =
    val tags = slots.collect { case Slot.Service(tag) => tag.tag }
    if tags.exists(_ == summon[Tag[LockStrategy]].tag) then
      Some(ServiceActivationError.DuplicateReplacement(CoreSlot.Locks))
    else if tags.exists(_ == summon[Tag[PluginLoadingStrategy]].tag) then
      Some(ServiceActivationError.DuplicateReplacement(CoreSlot.PluginLoading))
    else None

  private def duplicate(tag: Tag[?]): ServiceActivationError =
    ServiceKey.from(tag.tag.toString) match
      case Right(key) => ServiceActivationError.DuplicateService(key)
      case Left(_)    => ServiceActivationError.BadKey(tag.tag.toString)

  private def missing(tag: Tag[?], phase: ActivationPhase): ServiceActivationError =
    ServiceKey.from(tag.tag.toString) match
      case Right(key) => ServiceActivationError.MissingCapability(key, phase)
      case Left(_)    => ServiceActivationError.BadKey(tag.tag.toString)

  private def provide[S, A: Tag, E](
      prior: ZEnvironment[S],
      layer: ZLayer[GenerationAmbient & CoreServices & S, E, A],
  ): ZIO[GenerationAmbient & CoreServices & Scope, ServiceActivationError | E, A] =
    layer.build
      .provideSomeEnvironment[GenerationAmbient & CoreServices & Scope](base => base.unionAll(prior))
      .flatMap: built =>
        built.getDynamic[A] match
          case Some(value) => ZIO.succeed(value)
          case None        =>
            ZIO.fail(missing(summon[Tag[A]], ActivationPhase.Main))

  private final class Empty(using Tag[NoExtraServices]) extends Plan[NoExtraServices, Nothing]:
    protected def recorded: List[Slot] = Nil

    protected def materialize: ZIO[
      GenerationAmbient & CoreServices & Scope,
      ServiceActivationError | Nothing,
      Chain[NoExtraServices],
    ] =
      ZIO.succeed(Chain(ZEnvironment[NoExtraServices](NoExtraServices.marker), Nil, Nil))
  end Empty

  private final class Locks[S, E <: ServiceSetupError](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient, Nothing, LockStrategy],
  ) extends Plan[S, E]:
    protected def recorded: List[Slot] = Slot.Lock :: prev.recorded

    protected def materialize: ZIO[GenerationAmbient & CoreServices & Scope, ServiceActivationError | E, Chain[S]] =
      for
        prior    <- prev.materialize
        built    <- layer.build
        strategy <- built.getDynamic[LockStrategy] match
          case Some(value) => ZIO.succeed(value)
          case None        =>
            ZIO.fail(ServiceActivationError.MissingCapability(ServiceKey("lock-strategy"), ActivationPhase.Main))
      yield prior.copy(locks = prior.locks :+ strategy)
  end Locks

  private final class LocksE[S, E <: ServiceSetupError, E1 <: ServiceSetupError](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient, E1, LockStrategy],
  ) extends Plan[S, E | E1]:
    protected def recorded: List[Slot] = Slot.Lock :: prev.recorded

    protected def materialize: ZIO[
      GenerationAmbient & CoreServices & Scope,
      ServiceActivationError | E | E1,
      Chain[S],
    ] =
      for
        prior    <- prev.materialize
        built    <- layer.build
        strategy <- built.getDynamic[LockStrategy] match
          case Some(value) => ZIO.succeed(value)
          case None        =>
            ZIO.fail(ServiceActivationError.MissingCapability(ServiceKey("lock-strategy"), ActivationPhase.Main))
      yield prior.copy(locks = prior.locks :+ strategy)
  end LocksE

  private final class Added[S, E <: ServiceSetupError, A](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient & CoreServices & S, Nothing, A],
  )(using Tag[A])
      extends Plan[S & A, E]:
    protected def recorded: List[Slot] = Slot.Service(summon[Tag[A]]) :: prev.recorded

    protected def materialize: ZIO[
      GenerationAmbient & CoreServices & Scope,
      ServiceActivationError | E,
      Chain[S & A],
    ] =
      for
        prior <- prev.materialize
        value <- provide(prior.environment, layer)
      yield Chain(prior.environment.add(value), prior.locks, prior.tasks)
  end Added

  private final class AddedE[S, E <: ServiceSetupError, A, E1 <: ServiceSetupError](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient & CoreServices & S, E1, A],
  )(using Tag[A])
      extends Plan[S & A, E | E1]:
    protected def recorded: List[Slot] = Slot.Service(summon[Tag[A]]) :: prev.recorded

    protected def materialize: ZIO[
      GenerationAmbient & CoreServices & Scope,
      ServiceActivationError | E | E1,
      Chain[S & A],
    ] =
      for
        prior <- prev.materialize
        value <- provide(prior.environment, layer)
      yield Chain(prior.environment.add(value), prior.locks, prior.tasks)
  end AddedE

  private final class Tasked[S, E <: ServiceSetupError, P <: HistosPlugin](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient & CoreServices & S, Nothing, P],
  )(using Tag[P])
      extends Plan[S & P, E]:
    protected def recorded: List[Slot] = Slot.Task(summon[Tag[P]]) :: prev.recorded

    protected def materialize: ZIO[
      GenerationAmbient & CoreServices & Scope,
      ServiceActivationError | E,
      Chain[S & P],
    ] =
      for
        prior <- prev.materialize
        value <- provide(prior.environment, layer)
      yield Chain(prior.environment.add(value), prior.locks, prior.tasks :+ value)
  end Tasked

  private final class TaskedE[S, E <: ServiceSetupError, P <: HistosPlugin, E1 <: ServiceSetupError](
      prev: Plan[S, E],
      layer: ZLayer[GenerationAmbient & CoreServices & S, E1, P],
  )(using Tag[P])
      extends Plan[S & P, E | E1]:
    protected def recorded: List[Slot] = Slot.Task(summon[Tag[P]]) :: prev.recorded

    protected def materialize: ZIO[
      GenerationAmbient & CoreServices & Scope,
      ServiceActivationError | E | E1,
      Chain[S & P],
    ] =
      for
        prior <- prev.materialize
        value <- provide(prior.environment, layer)
      yield Chain(prior.environment.add(value), prior.locks, prior.tasks :+ value)
  end TaskedE
end Plan

/** Early lock and loading services. There is no late main equivalent of plugin loading. */
trait StrategyServices:
  type Error <: ServiceSetupError

trait HistosStrategies:
  def services: StrategyServices

final case class ActivatedStrategies(
    locks: Option[LockStrategy],
    loading: Option[PluginLoadingStrategy],
)

sealed abstract class StrategyPlan[E <: ServiceSetupError] extends StrategyServices:
  type Error = E

  def withLocks(layer: ZLayer[GenerationAmbient, Nothing, LockStrategy]): StrategyPlan[E] =
    StrategyPlan.locks(this, layer)

  def withLocks[E1 <: ServiceSetupError](layer: ZLayer[GenerationAmbient, E1, LockStrategy])(using
      DummyImplicit
  ): StrategyPlan[E | E1] =
    StrategyPlan.locksE(this, layer)

  def withPluginLoading(layer: ZLayer[GenerationAmbient, Nothing, PluginLoadingStrategy]): StrategyPlan[E] =
    StrategyPlan.loading(this, layer)

  def withPluginLoading[E1 <: ServiceSetupError](
      layer: ZLayer[GenerationAmbient, E1, PluginLoadingStrategy]
  )(using DummyImplicit): StrategyPlan[E | E1] =
    StrategyPlan.loadingE(this, layer)

  /** A second replacement is refused before the new layer runs. */
  def activate: ZIO[GenerationAmbient & Scope, ServiceActivationError | E, ActivatedStrategies] =
    StrategyPlan.refuse(recorded) match
      case Some(error) => ZIO.fail(error)
      case None        => materialize

  protected def recorded: List[StrategyPlan.Slot]

  protected def materialize: ZIO[GenerationAmbient & Scope, ServiceActivationError | E, ActivatedStrategies]
end StrategyPlan

object StrategyServices:
  def default: StrategyPlan[Nothing] = StrategyPlan.empty

object StrategyPlan:
  private[plugin] enum Slot:
    case Lock
    case Loading

  private[plugin] def empty: StrategyPlan[Nothing] = Empty()

  private def locks[E <: ServiceSetupError](
      prev: StrategyPlan[E],
      layer: ZLayer[GenerationAmbient, Nothing, LockStrategy],
  ): StrategyPlan[E] =
    Locks(prev, layer)

  private def locksE[E <: ServiceSetupError, E1 <: ServiceSetupError](
      prev: StrategyPlan[E],
      layer: ZLayer[GenerationAmbient, E1, LockStrategy],
  ): StrategyPlan[E | E1] =
    LocksE(prev, layer)

  private def loading[E <: ServiceSetupError](
      prev: StrategyPlan[E],
      layer: ZLayer[GenerationAmbient, Nothing, PluginLoadingStrategy],
  ): StrategyPlan[E] =
    Loading(prev, layer)

  private def loadingE[E <: ServiceSetupError, E1 <: ServiceSetupError](
      prev: StrategyPlan[E],
      layer: ZLayer[GenerationAmbient, E1, PluginLoadingStrategy],
  ): StrategyPlan[E | E1] =
    LoadingE(prev, layer)

  private def refuse(slots: List[Slot]): Option[ServiceActivationError] =
    val locks = slots.count(_ == Slot.Lock)
    val loads = slots.count(_ == Slot.Loading)
    if locks > 1 then Some(ServiceActivationError.DuplicateReplacement(CoreSlot.Locks))
    else if loads > 1 then Some(ServiceActivationError.DuplicateReplacement(CoreSlot.PluginLoading))
    else None

  private def one[A: Tag](
      built: ZEnvironment[A],
      key: ServiceKey,
  ): ZIO[Any, ServiceActivationError, A] =
    built.getDynamic[A] match
      case Some(value) => ZIO.succeed(value)
      case None        => ZIO.fail(ServiceActivationError.MissingCapability(key, ActivationPhase.Strategy))

  private final class Empty extends StrategyPlan[Nothing]:
    protected def recorded: List[Slot] = Nil

    protected def materialize: ZIO[GenerationAmbient & Scope, ServiceActivationError, ActivatedStrategies] =
      ZIO.succeed(ActivatedStrategies(None, None))
  end Empty

  private final class Locks[E <: ServiceSetupError](
      prev: StrategyPlan[E],
      layer: ZLayer[GenerationAmbient, Nothing, LockStrategy],
  ) extends StrategyPlan[E]:
    protected def recorded: List[Slot] = Slot.Lock :: prev.recorded

    protected def materialize: ZIO[GenerationAmbient & Scope, ServiceActivationError | E, ActivatedStrategies] =
      for
        earlier  <- prev.materialize
        built    <- layer.build
        strategy <- one(built, ServiceKey("lock-strategy"))
      yield earlier.copy(locks = Some(strategy))
  end Locks

  private final class LocksE[E <: ServiceSetupError, E1 <: ServiceSetupError](
      prev: StrategyPlan[E],
      layer: ZLayer[GenerationAmbient, E1, LockStrategy],
  ) extends StrategyPlan[E | E1]:
    protected def recorded: List[Slot] = Slot.Lock :: prev.recorded

    protected def materialize: ZIO[GenerationAmbient & Scope, ServiceActivationError | E | E1, ActivatedStrategies] =
      for
        earlier  <- prev.materialize
        built    <- layer.build
        strategy <- one(built, ServiceKey("lock-strategy"))
      yield earlier.copy(locks = Some(strategy))
  end LocksE

  private final class Loading[E <: ServiceSetupError](
      prev: StrategyPlan[E],
      layer: ZLayer[GenerationAmbient, Nothing, PluginLoadingStrategy],
  ) extends StrategyPlan[E]:
    protected def recorded: List[Slot] = Slot.Loading :: prev.recorded

    protected def materialize: ZIO[GenerationAmbient & Scope, ServiceActivationError | E, ActivatedStrategies] =
      for
        earlier  <- prev.materialize
        built    <- layer.build
        strategy <- one(built, ServiceKey("plugin-loading"))
      yield earlier.copy(loading = Some(strategy))
  end Loading

  private final class LoadingE[E <: ServiceSetupError, E1 <: ServiceSetupError](
      prev: StrategyPlan[E],
      layer: ZLayer[GenerationAmbient, E1, PluginLoadingStrategy],
  ) extends StrategyPlan[E | E1]:
    protected def recorded: List[Slot] = Slot.Loading :: prev.recorded

    protected def materialize: ZIO[GenerationAmbient & Scope, ServiceActivationError | E | E1, ActivatedStrategies] =
      for
        earlier  <- prev.materialize
        built    <- layer.build
        strategy <- one(built, ServiceKey("plugin-loading"))
      yield earlier.copy(loading = Some(strategy))
  end LoadingE
end StrategyPlan
