package histos.schedule

import histos.task.TaskKind

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import zio.{IO, UIO, ZIO}

/** One declared input. The name is part of the cache key. */
final case class Input(name: String, value: String)

opaque type ActionId = String

object ActionId:
  given CanEqual[ActionId, ActionId] = CanEqual.derived

  def from(raw: String): Either[ActionIdError, ActionId] =
    if raw.isEmpty then Left(ActionIdError.Empty) else Right(raw)

  /** A non-empty literal the tests already validated. Not a parser. */
  private[schedule] def trusted(raw: String): ActionId = raw

  extension (id: ActionId) def value: String = id

enum ActionIdError:
  case Empty

/** A task node the scheduler can run. The effect is not stored here. */
final case class Action(
    id: ActionId,
    kind: TaskKind,
    inputs: List[Input],
    dependencies: List[ActionId],
)

/** Identity of an action and of the actions it depends on. The cache layer stores bytes under this key. */
opaque type Digest = String

object Digest:
  given CanEqual[Digest, Digest] = CanEqual.derived

  def from(raw: String): Option[Digest] =
    if raw.length == 64 && raw.forall(isHex) then Some(raw) else None

  def of(action: Action, dependencyDigests: Map[ActionId, Digest]): Digest =
    val inputs = action.inputs.sortBy(_.name).map(input => s"${input.name}=${input.value}").mkString("\n")
    val deps = action.dependencies
      .map(id => s"${id.value}=${dependencyDigests.getOrElse(id, "")}")
      .sorted
      .mkString("\n")
    val text = s"${action.kind}\n${action.id.value}\n$inputs\n$deps"
    val bytes = MessageDigest.getInstance("SHA-256").digest(text.getBytes(StandardCharsets.UTF_8))
    bytes.map(b => f"${b & 0xff}%02x").mkString

  extension (digest: Digest) def value: String = digest

  private def isHex(c: Char): Boolean =
    c.isDigit || (c >= 'a' && c <= 'f')

/** Lookup and store. A miss is `None`, and the scheduler runs the action locally, then `put`s the text. */
trait ActionCache:
  def get(digest: Digest): IO[CacheError, Option[String]]
  def put(digest: Digest, value: String): IO[CacheError, Unit] = ZIO.unit

object ActionCache:
  val miss: ActionCache = _ => ZIO.none
end ActionCache
