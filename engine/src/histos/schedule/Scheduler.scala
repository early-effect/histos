package histos.schedule

import histos.step.{
  Input as StepInput,
  InputName as StepInputName,
  InputValue as StepInputValue,
  Need as StepNeed,
  Step,
  StepId,
  StepInfo
}

import Histos.given

import zio.*

enum ScheduleError[+E]:
  case UnknownAction(id: ActionId)
  case DuplicateAction(id: ActionId)
  case MissingDependency(action: ActionId, missing: ActionId)
  case Cycle(path: List[ActionId])
  case DuplicateInput(action: ActionId, name: InputName)
  case Failed(action: ActionId, error: E)
  case Cache(error: CacheError)
  case Remote(error: RemoteError)

  def message: String = this match
    case UnknownAction(id)   => s"no action '${id.value}'"
    case DuplicateAction(id) => s"action '${id.value}' is declared twice"
    case MissingDependency(action, missing) =>
      s"'${action.value}' depends on '${missing.value}', which is not in the plan"
    case Cycle(path) => s"action cycle ${path.map(_.value).mkString(" -> ")}"
    case DuplicateInput(action, name) =>
      s"'${action.value}' declares input '${name.value}' twice"
    case Failed(action, error) => s"'${action.value}' failed: $error"
    case Cache(error)          => error.message
    case Remote(error)         => error.message

/** One executor and the caches for this run. The caches are the caller's, so a
  * test passes its own.
  */
final case class Scheduler(
    runner: Histos,
    cache: ActionCache,
    remote: RemoteCache
):
  /** Run `root` after its dependencies. A remote hit is the stored output. A
    * miss runs `local`. A write cache uploads only a sendable action.
    */
  def execute[E](
      root: ActionId,
      actions: List[Action],
      local: (Action, String) => IO[E, String],
      verdict: Action => Verdict = Sendable.ofAction
  ): IO[ScheduleError[E], String] =
    ZIO
      .fromEither(prepare(root, actions))
      .flatMap: ready =>
        run(root, ready, local, verdict)

  /** Cache policy for one action. The caller already ordered its upstream
    * steps.
    */
  def cached[E](
      action: Action,
      digest: Digest,
      local: IO[E, String],
      verdict: Action => Verdict = Sendable.ofAction
  ): IO[ScheduleError[E], String] =
    perform(action, digest, (_, _) => local, verdict, "")

  private final case class Ready(
      byId: Map[ActionId, Action],
      order: List[ActionId],
      digests: Map[ActionId, Digest]
  )

  private def prepare(
      root: ActionId,
      actions: List[Action]
  ): Either[ScheduleError[Nothing], Ready] =
    for
      byId <- index(actions)
      _ <- byId.get(root).toRight(ScheduleError.UnknownAction(root))
      _ <- inputs(byId)
      order <- closure(root, byId)
      hashed <- hashes(order, byId)
    yield Ready(byId, order, hashed)

  private def index(
      actions: List[Action]
  ): Either[ScheduleError[Nothing], Map[ActionId, Action]] =
    actions.foldLeft[Either[ScheduleError[Nothing], Map[ActionId, Action]]](
      Right(Map.empty)
    ): (acc, action) =>
      acc.flatMap: seen =>
        if seen.contains(action.id) then
          Left(ScheduleError.DuplicateAction(action.id))
        else Right(seen + (action.id -> action))

  private def inputs(
      byId: Map[ActionId, Action]
  ): Either[ScheduleError[Nothing], Unit] =
    byId.values.foldLeft[Either[ScheduleError[Nothing], Unit]](Right(())):
      (acc, action) =>
        acc.flatMap: _ =>
          val names = action.inputs.map(_.name.value)
          names
            .foldLeft[(List[String], Option[String])]((Nil, None)):
              case ((_, Some(name)), _) => (Nil, Some(name))
              case ((seen, None), name) =>
                if seen.contains(name) then (seen, Some(name))
                else (name :: seen, None)
            ._2 match
            case Some(name) =>
              Left(
                ScheduleError.DuplicateInput(action.id, InputName.trusted(name))
              )
            case None => Right(())

  private def closure(
      root: ActionId,
      byId: Map[ActionId, Action]
  ): Either[ScheduleError[Nothing], List[ActionId]] =
    def walk(
        id: ActionId,
        path: List[ActionId],
        done: Set[ActionId]
    ): Either[ScheduleError[Nothing], (List[ActionId], Set[ActionId])] =
      if done.contains(id) then Right((Nil, done))
      else if path.contains(id) then
        Left(ScheduleError.Cycle((id :: path).reverse.dropWhile(_ != id)))
      else
        byId.get(id) match
          case None =>
            path match
              case parent :: _ =>
                Left(ScheduleError.MissingDependency(parent, id))
              case Nil => Left(ScheduleError.UnknownAction(id))
          case Some(action) =>
            val next = action.dependencies.foldLeft[Either[ScheduleError[
              Nothing
            ], (List[ActionId], Set[ActionId])]](
              Right((Nil, done))
            ): (acc, dep) =>
              acc.flatMap: (soFar, seen) =>
                walk(dep, id :: path, seen).map: (more, after) =>
                  (soFar ++ more, after)
            next.map: (deps, seen) =>
              (deps :+ id, seen + id)

    walk(root, Nil, Set.empty).map(_._1)

  private def hashes(
      order: List[ActionId],
      byId: Map[ActionId, Action]
  ): Either[ScheduleError[Nothing], Map[ActionId, Digest]] =
    order.foldLeft[Either[ScheduleError[Nothing], Map[ActionId, Digest]]](
      Right(Map.empty)
    ): (acc, id) =>
      acc.flatMap: soFar =>
        byId.get(id) match
          case None         => Left(ScheduleError.UnknownAction(id))
          case Some(action) =>
            action.dependencies.find(dep => !soFar.contains(dep)) match
              case Some(dep) =>
                Left(ScheduleError.MissingDependency(action.id, dep))
              case None => Right(soFar + (id -> Digest.of(action, soFar)))

  private def run[E](
      root: ActionId,
      ready: Ready,
      local: (Action, String) => IO[E, String],
      verdict: Action => Verdict
  ): IO[ScheduleError[E], String] =
    for
      step <- build(root, ready, local, verdict)
      outcome <- ZIO.scoped(runner.start(step).flatMap(_.outcome))
      result <- outcome match
        case Outcome.Done(value)      => ZIO.succeed(value)
        case Outcome.Failed(failures) => recover(failures)
    yield result

  private def recover[E](
      failures: NonEmptyChunk[StepFailure[ScheduleError[E]]]
  ): IO[ScheduleError[E], Nothing] =
    failures.head match
      case StepFailure.Failed(_, cause) => ZIO.failCause(cause)
      case StepFailure.Skipped(id, _)   =>
        val key = id match
          case StepId.Work(text) => text
          case other             => other.toString
        ZIO.fail(ScheduleError.UnknownAction(ActionId.trusted(key)))

  private def build[E](
      id: ActionId,
      ready: Ready,
      local: (Action, String) => IO[E, String],
      verdict: Action => Verdict
  ): IO[ScheduleError[E], histos.step.HermeticStep[Any, ScheduleError[
    E
  ], String]] =
    for
      action <- ZIO.fromEither(
        ready.byId.get(id).toRight(ScheduleError.UnknownAction(id))
      )
      digest <- ZIO.fromEither(
        ready.digests.get(id).toRight(ScheduleError.UnknownAction(id))
      )
      body = perform(action, digest, local, verdict, "")
      info = stepInfo(action, digest)
      step = Step.hermetic[String](info)(body)
      wired <- action.dependencies match
        case Nil          => ZIO.succeed(step)
        case head :: tail =>
          ZIO
            .foreach(head :: tail)(dep => build(dep, ready, local, verdict))
            .map: ups =>
              val upstream = ups match
                case only :: Nil   => only
                case first :: rest =>
                  rest
                    .foldLeft[histos.step.Step[Any, ScheduleError[E], String]](
                      first
                    ): (acc, next) =>
                      acc.zip(next).map((left, right) => join(left, right))
                case Nil => step.map(_ => "")
              upstream.andThenHermetic[String](info)(value =>
                perform(action, digest, local, verdict, value)
              )
    yield wired

  private def join(left: String, right: String): String =
    if left.isEmpty then right
    else if right.isEmpty then left
    else s"$left\n$right"

  private def stepInfo(action: Action, digest: Digest): StepInfo =
    StepInfo(
      StepId.Work(action.id.value),
      Chunk(
        StepInput(
          StepInputName(digest.value),
          StepInputValue.Digest(digest.value)
        )
      ),
      Set(StepNeed.Cpu)
    )

  private def perform[E](
      action: Action,
      digest: Digest,
      local: (Action, String) => IO[E, String],
      verdict: Action => Verdict,
      upstream: String
  ): IO[ScheduleError[E], String] =
    for
      remoteHit <- ScheduleAspects.remote(remote, action, digest)
      out <- remoteHit match
        case Some(value) => ZIO.succeed(value)
        case None        =>
          ScheduleAspects
            .cached(cache, action, digest)
            .flatMap:
              case Some(value) => ZIO.succeed(value)
              case None        =>
                miss(action, digest, cache, remote, local, verdict, upstream)
    yield out

  /** `Policy.Core` stores every local success. A plugin stores only a sendable
    * result. A stayed plugin result is returned and not written.
    */
  private def miss[E](
      action: Action,
      digest: Digest,
      cache: ActionCache,
      remote: RemoteCache,
      local: (Action, String) => IO[E, String],
      verdict: Action => Verdict,
      upstream: String
  ): IO[ScheduleError[E], String] =
    local(action, upstream)
      .mapError(ScheduleError.Failed(action.id, _))
      .flatMap: value =>
        val decided = verdict(action)
        val store = action.policy match
          case Policy.Core               => true
          case Policy.Plugin(_, _, _, _) =>
            decided match
              case Verdict.Send    => true
              case Verdict.Stay(_) => false
        if store then
          cache.put(digest, value).mapError(ScheduleError.Cache(_)) *>
            ScheduleAspects.upload(remote, decided, digest, value).as(value)
        else ZIO.succeed(value)

end Scheduler

object Scheduler:
  val live: ZLayer[ActionCache & RemoteCache, Nothing, Scheduler] =
    val histos = Parallelism.live >>> Histos.live
    (histos ++ ZLayer.service[ActionCache] ++ ZLayer
      .service[RemoteCache]) >>> ZLayer.fromFunction(Scheduler.apply)
