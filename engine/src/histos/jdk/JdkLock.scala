package histos.jdk

import histos.archive.{ArchivePin, HostArchive}
import histos.toolchain.{
  HostPlatform,
  ToolArchiveError,
  ToolLock,
  ToolLockError
}

import java.security.MessageDigest
import zio.{Chunk, NonEmptyChunk}

opaque type Sha256 = String

object Sha256:
  given CanEqual[Sha256, Sha256] = CanEqual.derived

  def from(raw: String): Either[Sha256Error, Sha256] =
    val hex = raw.trim.toLowerCase(java.util.Locale.ROOT)
    if hex.length == 64 && hex.forall(isHex) then Right(hex)
    else Left(Sha256Error.Bad(raw))

  def of(bytes: Chunk[Byte]): Sha256 = fromDigest(
    MessageDigest.getInstance("SHA-256").digest(bytes.toArray)
  )

  private[histos] def fromDigest(bytes: Array[Byte]): Sha256 =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  extension (sha: Sha256) def value: String = sha

  private def isHex(c: Char): Boolean =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

enum Sha256Error:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"sha256 is not 64 hex digits: $raw"

enum JdkPinError:
  case BadMajor(raw: String)
  case BadVersion(error: VersionError)
  case MajorDisagrees(declared: JdkMajor, found: JdkMajor)
  def message: String = this match
    case BadMajor(raw) =>
      s"JDK lock major is not a bounded positive integer: $raw"
    case BadVersion(error)               => error.message
    case MajorDisagrees(declared, found) =>
      s"JDK lock major ${declared.value} disagrees with ${found.value}"

enum LockError:
  case Schema(error: ToolLockError[JdkPinError])
  def message: String = this match
    case Schema(ToolLockError.Pin(error)) => error.message
    case Schema(error)                    => error.schemaMessage

final case class JdkLock private (tools: ToolLock[JdkVersion]):
  def major: JdkMajor = tools.pin.major
  def version: JdkVersion = tools.pin
  def archives: NonEmptyChunk[HostArchive] = tools.archives
  def archive(host: HostPlatform): Either[ToolArchiveError, ArchivePin] =
    tools.archive(host)

object JdkLock:
  private val codec = ToolLock.Codec[JdkVersion, JdkPinError](
    (majorRaw, versionRaw) =>
      for
        _ <- Either.cond(
          majorRaw.matches("[+]?[0-9]+"),
          (),
          JdkPinError.BadMajor(majorRaw)
        )
        major <- majorRaw.toIntOption
          .flatMap(value => JdkMajor.from(value).toOption)
          .toRight(JdkPinError.BadMajor(majorRaw))
        version <- JdkVersion
          .from(versionRaw)
          .left
          .map(JdkPinError.BadVersion(_))
        _ <- Either.cond(
          version.major == major,
          (),
          JdkPinError.MajorDisagrees(major, version.major)
        )
      yield version,
    version => (version.major.value.toString, version.value)
  )

  def from(
      version: JdkVersion,
      entries: Chunk[HostArchive]
  ): Either[ToolLockError[Nothing], JdkLock] =
    ToolLock.from(version, entries).map(JdkLock(_))

  def parse(text: String): Either[LockError, JdkLock] =
    ToolLock.parse(text, codec).map(JdkLock(_)).left.map(LockError.Schema(_))

  def render(lock: JdkLock): String = ToolLock.render(lock.tools, codec)
