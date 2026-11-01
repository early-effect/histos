package histos.schedule

import histos.jdk.Sha256
import histos.plugin.{Need, RepoPath, Seal}

import java.nio.file.Path

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

  /** A digest this process just computed. */
  private[histos] def trusted(raw: String): Digest = raw

  extension (digest: Digest) def value: String = digest

  private def isHex(c: Char): Boolean =
    c.isDigit || (c >= 'a' && c <= 'f')
end Digest
