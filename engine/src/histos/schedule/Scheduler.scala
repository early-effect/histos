package histos.schedule

import java.nio.ByteBuffer
import java.nio.charset.{CharsetDecoder, CodingErrorAction, StandardCharsets}

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
    case UnknownAction(id)              => s"no action '${id.value}'"
    case DuplicateAction(id)            => s"action '${id.value}' is declared twice"
    case MissingDependency(action, missing) =>
      s"'${action.value}' depends on '${missing.value}', which is not in the plan"
    case Cycle(path)                    => s"action cycle ${path.map(_.value).mkString(" -> ")}"
    case DuplicateInput(action, name)   => s"'${action.value}' declares input '${name.value}' twice"
    case Failed(action, _)              => s"'${action.value}' failed"
    case Cache(error)                   => error.message
    case Remote(error)                  => error.message

trait Scheduler:
  /** Run `root` after its dependencies. A remote hit is the stored output. A miss runs `local`. A write cache uploads only a sendable action. */
  def execute[E](
      root: ActionId,
      actions: List[Action],
      local: Action => IO[E, String],
      verdict: Action => Verdict = Sendable.ofAction,
  ): ZIO[ActionCache & RemoteCache, ScheduleError[E], String]

object Scheduler:
  val live: ULayer[Scheduler] = ZLayer.succeed(SchedulerLive)

private object SchedulerLive extends Scheduler:
  def execute[E](
      root: ActionId,
      actions: List[Action],
      local: Action => IO[E, String],
      verdict: Action => Verdict,
  ): ZIO[ActionCache & RemoteCache, ScheduleError[E], String] =
    ZIO.serviceWithZIO[ActionCache]: cache =>
      ZIO.serviceWithZIO[RemoteCache]: remote =>
        ZIO.fromEither(prepare(root, actions)).flatMap: ready =>
          run(root, ready, cache, remote, local, verdict)

  private final case class Ready(byId: Map[ActionId, Action], order: List[ActionId], digests: Map[ActionId, Digest])

  private def prepare(root: ActionId, actions: List[Action]): Either[ScheduleError[Nothing], Ready] =
    for
      byId   <- index(actions)
      _      <- byId.get(root).toRight(ScheduleError.UnknownAction(root))
      _      <- inputs(byId)
      order  <- closure(root, byId)
      hashed <- hashes(order, byId)
    yield Ready(byId, order, hashed)

  private def index(actions: List[Action]): Either[ScheduleError[Nothing], Map[ActionId, Action]] =
    actions.foldLeft[Either[ScheduleError[Nothing], Map[ActionId, Action]]](Right(Map.empty)): (acc, action) =>
      acc.flatMap: seen =>
        if seen.contains(action.id) then Left(ScheduleError.DuplicateAction(action.id))
        else Right(seen + (action.id -> action))

  private def inputs(byId: Map[ActionId, Action]): Either[ScheduleError[Nothing], Unit] =
    byId.values.foldLeft[Either[ScheduleError[Nothing], Unit]](Right(())): (acc, action) =>
      acc.flatMap: _ =>
        val names = action.inputs.map(_.name.value)
        names.foldLeft[(List[String], Option[String])]((Nil, None)):
          case ((_, Some(name)), _) => (Nil, Some(name))
          case ((seen, None), name) =>
            if seen.contains(name) then (seen, Some(name)) else (name :: seen, None)
        ._2 match
          case Some(name) => Left(ScheduleError.DuplicateInput(action.id, InputName.trusted(name)))
          case None       => Right(())

  private def closure(root: ActionId, byId: Map[ActionId, Action]): Either[ScheduleError[Nothing], List[ActionId]] =
    def walk(
        id: ActionId,
        path: List[ActionId],
        done: Set[ActionId],
    ): Either[ScheduleError[Nothing], (List[ActionId], Set[ActionId])] =
      if done.contains(id) then Right((Nil, done))
      else if path.contains(id) then Left(ScheduleError.Cycle((id :: path).reverse.dropWhile(_ != id)))
      else
        byId.get(id) match
          case None =>
            path match
              case parent :: _ => Left(ScheduleError.MissingDependency(parent, id))
              case Nil         => Left(ScheduleError.UnknownAction(id))
          case Some(action) =>
            val next = action.dependencies.foldLeft[Either[ScheduleError[Nothing], (List[ActionId], Set[ActionId])]](
              Right((Nil, done))
            ): (acc, dep) =>
              acc.flatMap: (soFar, seen) =>
                walk(dep, id :: path, seen).map: (more, after) =>
                  (soFar ++ more, after)
            next.map: (deps, seen) =>
              (deps :+ id, seen + id)

    walk(root, Nil, Set.empty).map(_._1)

  private def hashes(order: List[ActionId], byId: Map[ActionId, Action]): Either[ScheduleError[Nothing], Map[ActionId, Digest]] =
    order.foldLeft[Either[ScheduleError[Nothing], Map[ActionId, Digest]]](Right(Map.empty)): (acc, id) =>
      acc.flatMap: soFar =>
        byId.get(id) match
          case None         => Left(ScheduleError.UnknownAction(id))
          case Some(action) =>
            action.dependencies.find(dep => !soFar.contains(dep)) match
              case Some(dep) => Left(ScheduleError.MissingDependency(action.id, dep))
              case None      => Right(soFar + (id -> Digest.of(action, soFar)))

  private def run[E](
      root: ActionId,
      ready: Ready,
      cache: ActionCache,
      remote: RemoteCache,
      local: Action => IO[E, String],
      verdict: Action => Verdict,
  ): IO[ScheduleError[E], String] =
    for
      gates <- ZIO.foreach(ready.order)(id => Promise.make[ScheduleError[E], String].map(id -> _)).map(_.toMap)
      _     <- ZIO.foreachParDiscard(ready.order)(id => publish(id, ready, gates, cache, remote, local, verdict))
      out   <- gate(gates, root).flatMap(_.await)
    yield out

  private def publish[E](
      id: ActionId,
      ready: Ready,
      gates: Map[ActionId, Promise[ScheduleError[E], String]],
      cache: ActionCache,
      remote: RemoteCache,
      local: Action => IO[E, String],
      verdict: Action => Verdict,
  ): IO[ScheduleError[E], Unit] =
    val compute =
      for
        action <- ZIO.fromEither(ready.byId.get(id).toRight(ScheduleError.UnknownAction(id)))
        _      <- ZIO.foreachParDiscard(action.dependencies)(dep => gate(gates, dep).flatMap(_.await))
        digest <- ZIO.fromEither(ready.digests.get(id).toRight(ScheduleError.UnknownAction(id)))
        remoteHit <- lookup(remote, digest)
        out <- remoteHit match
                 case Some(value) => ZIO.succeed(value)
                 case None =>
                   cache.get(digest).mapError(ScheduleError.Cache(_)).flatMap:
                     case Some(value) => ZIO.succeed(value)
                     case None =>
                       local(action).mapError(ScheduleError.Failed(id, _)).flatMap: value =>
                         cache.put(digest, value).mapError(ScheduleError.Cache(_)) *>
                           upload(remote, verdict(action), digest, value).as(value)
      yield out
    compute.exit.flatMap: exit =>
      val resumed = exit match
        case Exit.Success(value) => ZIO.succeed(value)
        case Exit.Failure(cause) => ZIO.failCause(cause)
      gate(gates, id).flatMap(_.done(exit)) *> resumed
    .unit

  private def lookup(remote: RemoteCache, digest: Digest): IO[ScheduleError[Nothing], Option[String]] =
    remote.mode match
      case RemoteMode.Off => ZIO.none
      case RemoteMode.Read | RemoteMode.Write =>
        remote.find(digest).mapError(ScheduleError.Remote(_)).flatMap:
          case None        => ZIO.none
          case Some(bytes) => decode(digest, bytes).mapError(ScheduleError.Remote(_)).map(Some(_))

  private def upload(remote: RemoteCache, verdict: Verdict, digest: Digest, value: String): IO[ScheduleError[Nothing], Unit] =
    (remote.mode, verdict) match
      case (RemoteMode.Write, Verdict.Send) =>
        val bytes = Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8))
        remote.store(digest, bytes).mapError(ScheduleError.Remote(_))
      case _ => ZIO.unit

  private def decode(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, String] =
    ZIO.attempt(decoder.decode(ByteBuffer.wrap(bytes.toArray)).toString).mapError(_ => RemoteError.Text(action))

  private def decoder: CharsetDecoder =
    StandardCharsets.UTF_8.newDecoder
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)

  private def gate[E](
      gates: Map[ActionId, Promise[ScheduleError[E], String]],
      id: ActionId,
  ): IO[ScheduleError[E], Promise[ScheduleError[E], String]] =
    ZIO.fromEither(gates.get(id).toRight(ScheduleError.UnknownAction(id)))
end SchedulerLive
