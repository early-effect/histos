package histos.node

import histos.archive.{ArchivePin, HostArchive}
import histos.toolchain.{
  HostPlatform,
  ToolArchiveError,
  ToolLock,
  ToolLockError
}

import zio.{Chunk, NonEmptyChunk}

enum NodePinError:
  case BadMajor(raw: String)
  case BadVersion(error: NodeVersionError)
  case MajorDisagrees(declared: NodeMajor, found: NodeMajor)
  def message: String = this match
    case BadMajor(raw) =>
      s"Node lock major is not a bounded positive integer: $raw"
    case BadVersion(error)               => error.message
    case MajorDisagrees(declared, found) =>
      s"Node lock major ${declared.value} disagrees with ${found.value}"

enum NodeLockError:
  case Schema(error: ToolLockError[NodePinError])
  def message: String = this match
    case Schema(ToolLockError.Pin(error)) => error.message
    case Schema(error)                    => error.schemaMessage

final case class NodeLock private (tools: ToolLock[NodeVersion]):
  def major: NodeMajor = tools.pin.major
  def version: NodeVersion = tools.pin
  def archives: NonEmptyChunk[HostArchive] = tools.archives
  def archive(host: HostPlatform): Either[ToolArchiveError, ArchivePin] =
    tools.archive(host)

object NodeLock:
  private val codec = ToolLock.Codec[NodeVersion, NodePinError](
    (majorRaw, versionRaw) =>
      for
        _ <- Either.cond(
          majorRaw.matches("[+]?[0-9]+"),
          (),
          NodePinError.BadMajor(majorRaw)
        )
        major <- majorRaw.toIntOption
          .flatMap(value => NodeMajor.from(value).toOption)
          .toRight(NodePinError.BadMajor(majorRaw))
        version <- NodeVersion
          .from(versionRaw)
          .left
          .map(NodePinError.BadVersion(_))
        _ <- Either.cond(
          version.major == major,
          (),
          NodePinError.MajorDisagrees(major, version.major)
        )
      yield version,
    version => (version.major.value.toString, version.value)
  )

  def from(
      version: NodeVersion,
      entries: Chunk[HostArchive]
  ): Either[ToolLockError[Nothing], NodeLock] =
    ToolLock.from(version, entries).map(NodeLock(_))

  def parse(text: String): Either[NodeLockError, NodeLock] =
    ToolLock
      .parse(text, codec)
      .map(NodeLock(_))
      .left
      .map(NodeLockError.Schema(_))

  def render(lock: NodeLock): String = ToolLock.render(lock.tools, codec)
