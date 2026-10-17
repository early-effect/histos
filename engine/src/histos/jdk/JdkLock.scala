package histos.jdk

import java.nio.file.Path
import java.security.MessageDigest

import zio.Chunk

opaque type Sha256 = String

object Sha256:
  given CanEqual[Sha256, Sha256] = CanEqual.derived

  def from(raw: String): Either[Sha256Error, Sha256] =
    val hex = raw.trim.toLowerCase(java.util.Locale.ROOT)
    if hex.length == 64 && hex.forall(isHex) then Right(hex) else Left(Sha256Error.Bad(raw))

  def of(bytes: Chunk[Byte]): Sha256 = fromDigest(MessageDigest.getInstance("SHA-256").digest(bytes.toArray))

  private[jdk] def fromDigest(bytes: Array[Byte]): Sha256 =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  extension (sha: Sha256) def value: String = sha

  private def isHex(c: Char): Boolean =
    c.isDigit || (c >= 'a' && c <= 'f')

enum Sha256Error:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"sha256 is not 64 hex digits: $raw"

opaque type HttpsUrl = String

object HttpsUrl:
  given CanEqual[HttpsUrl, HttpsUrl] = CanEqual.derived

  def from(raw: String): Either[LockError, HttpsUrl] =
    if raw.startsWith("https://") && raw.length > "https://".length && !raw.exists(_.isWhitespace) then Right(raw)
    else Left(LockError.BadUrl(raw))

  extension (url: HttpsUrl) def value: String = url

enum ArchiveSource:
  case Local(path: Path)
  case Remote(url: HttpsUrl)

enum LockField:
  case Major, Version, Url, Sha256

object LockField:
  def name(field: LockField): String =
    field match
      case Major   => "major"
      case Version => "version"
      case Url     => "url"
      case Sha256  => "sha256"

enum LockError:
  case Empty
  case BadLine(line: String)
  case UnknownField(name: String)
  case Duplicate(field: LockField)
  case Missing(field: LockField)
  case BadMajor(raw: String)
  case BadVersion(error: VersionError)
  case BadUrl(raw: String)
  case BadSha256(error: Sha256Error)
  case MajorDisagrees(declared: JdkMajor, found: JdkMajor)

  def message: String = this match
    case Empty                         => "jdk lock is empty"
    case BadLine(line)                 => s"jdk lock line is not a field: $line"
    case UnknownField(name)            => s"jdk lock field is unknown: $name"
    case Duplicate(field)              => s"jdk lock repeats ${LockField.name(field)}"
    case Missing(field)                => s"jdk lock is missing ${LockField.name(field)}"
    case BadMajor(raw)                 => s"jdk lock major is not a positive integer: $raw"
    case BadVersion(error)             => error.message
    case BadUrl(raw)                   => s"jdk lock url is not a file or https url: $raw"
    case BadSha256(error)              => error.message
    case MajorDisagrees(declared, found) =>
      s"jdk lock major ${declared.value} does not match version major ${found.value}"

final case class JdkLock(major: JdkMajor, version: JdkVersion, source: ArchiveSource, sha256: Sha256)

object JdkLock:
  def parse(text: String): Either[LockError, JdkLock] =
    val lines = text.linesIterator.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#")).toList
    if lines.isEmpty then Left(LockError.Empty)
    else lines.foldLeft[Either[LockError, Acc]](Right(Acc()))(step).flatMap(build)

  def render(lock: JdkLock): String =
    s"""major ${lock.major.value}
       |version ${lock.version.value}
       |url ${url(lock.source)}
       |sha256 ${lock.sha256.value}
       |""".stripMargin

  def url(source: ArchiveSource): String =
    source match
      case ArchiveSource.Local(path) => path.toUri.toASCIIString
      case ArchiveSource.Remote(url) => url.value

  private final case class Acc(
      major: Option[String] = None,
      version: Option[String] = None,
      url: Option[String] = None,
      sha: Option[String] = None,
  )

  private def step(acc: Either[LockError, Acc], line: String): Either[LockError, Acc] =
    acc.flatMap: state =>
      split(line) match
        case Left(error)           => Left(error)
        case Right((field, value)) => put(state, field, value)

  private def split(line: String): Either[LockError, (LockField, String)] =
    val space = line.indexOf(' ')
    if space <= 0 || space == line.length - 1 then Left(LockError.BadLine(line))
    else
      val name  = line.substring(0, space)
      val value = line.substring(space + 1).trim
      if value.isEmpty then Left(LockError.BadLine(line))
      else named(name).map(_ -> value)

  private def named(name: String): Either[LockError, LockField] =
    name match
      case "major"   => Right(LockField.Major)
      case "version" => Right(LockField.Version)
      case "url"     => Right(LockField.Url)
      case "sha256"  => Right(LockField.Sha256)
      case other     => Left(LockError.UnknownField(other))

  private def put(acc: Acc, field: LockField, value: String): Either[LockError, Acc] =
    def once(slot: Option[String], set: String => Acc): Either[LockError, Acc] =
      slot match
        case Some(_) => Left(LockError.Duplicate(field))
        case None    => Right(set(value))
    field match
      case LockField.Major   => once(acc.major, raw => acc.copy(major = Some(raw)))
      case LockField.Version => once(acc.version, raw => acc.copy(version = Some(raw)))
      case LockField.Url     => once(acc.url, raw => acc.copy(url = Some(raw)))
      case LockField.Sha256  => once(acc.sha, raw => acc.copy(sha = Some(raw)))

  private def build(acc: Acc): Either[LockError, JdkLock] =
    for
      majorRaw   <- acc.major.toRight(LockError.Missing(LockField.Major))
      versionRaw <- acc.version.toRight(LockError.Missing(LockField.Version))
      urlRaw     <- acc.url.toRight(LockError.Missing(LockField.Url))
      shaRaw     <- acc.sha.toRight(LockError.Missing(LockField.Sha256))
      major      <- parseMajor(majorRaw)
      version    <- JdkVersion.from(versionRaw).left.map(LockError.BadVersion(_))
      _          <- Either.cond(version.major == major, (), LockError.MajorDisagrees(major, version.major))
      source     <- parseSource(urlRaw)
      sha        <- Sha256.from(shaRaw).left.map(LockError.BadSha256(_))
    yield JdkLock(major, version, source, sha)

  private def parseMajor(raw: String): Either[LockError, JdkMajor] =
    raw.toIntOption match
      case Some(n) => JdkMajor.from(n).left.map(_ => LockError.BadMajor(raw))
      case None    => Left(LockError.BadMajor(raw))

  private def parseSource(raw: String): Either[LockError, ArchiveSource] =
    if raw.startsWith("https://") then HttpsUrl.from(raw).map(ArchiveSource.Remote(_))
    else if raw.startsWith("file:") then parseFile(raw)
    else Left(LockError.BadUrl(raw))

  private def parseFile(raw: String): Either[LockError, ArchiveSource] =
    try
      val uri = java.net.URI(raw)
      if uri.getScheme != "file" then Left(LockError.BadUrl(raw))
      else
        val path = Path.of(uri)
        if path.isAbsolute then Right(ArchiveSource.Local(path)) else Left(LockError.BadUrl(raw))
    catch
      case _: java.net.URISyntaxException => Left(LockError.BadUrl(raw))
      case _: IllegalArgumentException    => Left(LockError.BadUrl(raw))
