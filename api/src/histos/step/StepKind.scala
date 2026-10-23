package histos.step

/** What the cache may do with a step. `Hermetic` is a `Cached` step whose upstreams are hashed by content. */
sealed trait StepKind

object StepKind:
  /** A zip of kinds the cache cannot treat as one. */
  sealed trait Mixed extends StepKind

  /** Never cached: run, publish, open plugin tasks. */
  sealed trait Local extends Mixed

  /** The local action cache may serve it: test. */
  sealed trait Cached extends Mixed

  /** Local and remote caches may serve it: compile, link, package. */
  sealed trait Hermetic extends Cached

/** The kind a runner can match. The phantom and this value are set by the same constructor. */
enum RuntimeKind:
  case Local, Cached, Hermetic, Mixed

object RuntimeKind:
  def join(left: RuntimeKind, right: RuntimeKind): RuntimeKind =
    (left, right) match
      case (Mixed, _) | (_, Mixed)         => Mixed
      case (Local, Local)                  => Local
      case (Local, _) | (_, Local)         => Mixed
      case (Cached, Cached)                => Cached
      case (Hermetic, Hermetic)            => Hermetic
      case (Cached, Hermetic) | (Hermetic, Cached) => Cached
