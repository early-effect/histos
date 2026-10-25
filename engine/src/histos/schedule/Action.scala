package histos.schedule

import histos.jdk.Sha256
import histos.plugin.{Need, RepoPath, Seal}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest

import zio.{Chunk, IO, UIO, ZIO}

/** A cache-key name. Empty is not a name. */
opaque type InputName = String

object InputName:
  given CanEqual[InputName, InputName] = CanEqual.derived

  def from(raw: String): Either[InputNameError, InputName] =
    if raw.isEmpty then Left(InputNameError.Empty) else Right(raw)

  /** A non-empty literal the caller already checked. */
  private[histos] def trusted(raw: String): InputName = raw

  extension (name: InputName) def value: String = name
end InputName

enum InputNameError:
  case Empty

/** What an input contributes to the key. Text stays text. A hash is not a stringly digest.
  */
enum InputValue:
  case Text(text: String)
  case Hash(sha: Sha256)

  def canonical: String = this match
    case Text(text) => s"text $text"
    case Hash(sha)  => s"hash ${sha.value}"
end InputValue

/** One declared input. The name is part of the cache key. */
final case class Input(name: InputName, value: InputValue)

opaque type ActionId = String

object ActionId:
  given CanEqual[ActionId, ActionId] = CanEqual.derived

  def from(raw: String): Either[ActionIdError, ActionId] =
    if raw.isEmpty then Left(ActionIdError.Empty) else Right(raw)

  /** A non-empty literal the tests already validated. Not a parser. */
  private[schedule] def trusted(raw: String): ActionId = raw

  extension (id: ActionId) def value: String = id
end ActionId

enum ActionIdError:
  case Empty

/** Whether the scheduler may send the action. A plugin hit also names the repo and the declared output files. Those two
  * are not part of the digest. An empty output list stores the task text and does not check a file.
  */
enum Policy:
  case Core
  case Plugin(
      seal: Seal,
      needs: Set[Need],
      root: Option[Path] = None,
      outputs: List[RepoPath] = Nil,
  )

/** A task node the scheduler can run. The effect is not stored here. `Core` keeps compile keys stable.
  */
final case class Action(
    id: ActionId,
    inputs: List[Input],
    dependencies: List[ActionId],
    policy: Policy = Policy.Core,
)

/** Identity of an action and of the actions it depends on. The cache layer stores bytes under this key.
  */
opaque type Digest = String

object Digest:
  given CanEqual[Digest, Digest] = CanEqual.derived

  def from(raw: String): Option[Digest] =
    if raw.length == 64 && raw.forall(isHex) then Some(raw) else None

  def ofBytes(bytes: Chunk[Byte]): Digest =
    Sha256.of(bytes).value

  def of(action: Action, dependencyDigests: Map[ActionId, Digest]): Digest =
    val inputs = action.inputs
      .sortBy(_.name.value)
      .map(input => s"${input.name.value}=${input.value.canonical}")
      .mkString("\n")
    val deps = action.dependencies
      .map(id => s"${id.value}=${dependencyDigests.get(id).fold("<missing>")(_.value)}")
      .sorted
      .mkString("\n")
    val body = s"${action.id.value}\n$inputs\n$deps"
    val text = action.policy match
      case Policy.Core                      => body
      case Policy.Plugin(seal, needs, _, _) =>
        val listed = needs.toList.map(_.toString).sorted.mkString(",")
        val need   = if listed.isEmpty then "" else s" $listed"
        s"$body\nplugin $seal$need"
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(text.getBytes(StandardCharsets.UTF_8))
    bytes.map(b => f"${b & 0xff}%02x").mkString

  extension (digest: Digest) def value: String = digest

  private def isHex(c: Char): Boolean =
    c.isDigit || (c >= 'a' && c <= 'f')
end Digest

/** Lookup and store. A miss is `None`, and the scheduler runs the action locally, then `put`s the text.
  */
trait ActionCache:
  def get(digest: Digest): IO[CacheError, Option[String]]
  def put(digest: Digest, value: String): IO[CacheError, Unit]

object ActionCache:
  val miss: ActionCache = new ActionCache:
    def get(digest: Digest): IO[CacheError, Option[String]]      = ZIO.none
    def put(digest: Digest, value: String): IO[CacheError, Unit] = ZIO.unit
