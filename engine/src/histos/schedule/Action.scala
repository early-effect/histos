package histos.schedule

import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.{Need, RepoPath, Seal}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest

import zio.{Chunk, IO, UIO, ZIO}

/** Hash of an action. The digest value stays data. */
object ActionDigest:
  def ofBytes(bytes: Chunk[Byte]): Digest =
    Digest.trusted(Sha256Digest.of(bytes).value)

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
    Digest.trusted(bytes.map(b => f"${b & 0xff}%02x").mkString)
end ActionDigest

/** Lookup and store. A miss is `None`, and the scheduler runs the action locally, then `put`s the text.
  */
trait ActionCache:
  def get(digest: Digest): IO[CacheError, Option[String]]
  def put(digest: Digest, value: String): IO[CacheError, Unit]

object ActionCache:
  val miss: ActionCache = new ActionCache:
    def get(digest: Digest): IO[CacheError, Option[String]]      = ZIO.none
    def put(digest: Digest, value: String): IO[CacheError, Unit] = ZIO.unit
