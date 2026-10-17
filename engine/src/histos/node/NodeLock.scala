package histos.node

import histos.jdk.{ArchiveSource, HttpsUrl, Sha256, Sha256Error}

import java.nio.file.Path

enum NodeLockField:
  case Major, Version, Url, Sha256

object NodeLockField:
  def name(field: NodeLockField): String =
    field match
      case Major   => "major"
      case Version => "version"
      case Url     => "url"
      case Sha256  => "sha256"

enum NodeLockError:
  case Empty
  case BadLine(line: String)
  case UnknownField(name: String)
  case Duplicate(field: NodeLockField)
  case Missing(field: NodeLockField)
  case BadMajor(raw: String)
  case BadVersion(error: NodeVersionError)
  case BadUrl(raw: String)
  case BadSha256(error: Sha256Error)
  case MajorDisagrees(declared: NodeMajor, found: NodeMajor)

  def message: String = this match
    case Empty            => "node lock is empty"
    case BadLine(line)    => s"node lock line is not a field: $line"
    case UnknownField(name) => s"node lock field is unknown: $name"
    case Duplicate(field) => s"node lock repeats ${NodeLockField.name(field)}"
    case Missing(field)   => s"node lock is missing ${NodeLockField.name(field)}"
    case BadMajor(raw)    => s"node lock major is not a positive integer: $raw"
    case BadVersion(error) => error.message
    case BadUrl(raw)      => s"node lock url is not a file or https url: $raw"
    case BadSha256(error) => error.message
    case MajorDisagrees(declared, found) =>
      s"node lock major ${declared.value} does not match version major ${found.value}"

final case class NodeLock(major: NodeMajor, version: NodeVersion, source: ArchiveSource, sha256: Sha256)

object NodeLock:
  def parse(text: String): Either[NodeLockError, NodeLock] =
    val lines = text.linesIterator.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#")).toList
    if lines.isEmpty then Left(NodeLockError.Empty)
    else lines.foldLeft[Either[NodeLockError, Acc]](Right(Acc()))(step).flatMap(build)

  def render(lock: NodeLock): String =
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

  private def step(acc: Either[NodeLockError, Acc], line: String): Either[NodeLockError, Acc] =
    acc.flatMap: state =>
      split(line) match
        case Left(error)           => Left(error)
        case Right((field, value)) => put(state, field, value)

  private def split(line: String): Either[NodeLockError, (NodeLockField, String)] =
    val space = line.indexOf(' ')
    if space <= 0 || space == line.length - 1 then Left(NodeLockError.BadLine(line))
    else
      val name  = line.substring(0, space)
      val value = line.substring(space + 1).trim
      if value.isEmpty then Left(NodeLockError.BadLine(line))
      else named(name).map(_ -> value)

  private def named(name: String): Either[NodeLockError, NodeLockField] =
    name match
      case "major"   => Right(NodeLockField.Major)
      case "version" => Right(NodeLockField.Version)
      case "url"     => Right(NodeLockField.Url)
      case "sha256"  => Right(NodeLockField.Sha256)
      case other     => Left(NodeLockError.UnknownField(other))

  private def put(acc: Acc, field: NodeLockField, value: String): Either[NodeLockError, Acc] =
    def once(slot: Option[String], set: String => Acc): Either[NodeLockError, Acc] =
      slot match
        case Some(_) => Left(NodeLockError.Duplicate(field))
        case None    => Right(set(value))
    field match
      case NodeLockField.Major   => once(acc.major, raw => acc.copy(major = Some(raw)))
      case NodeLockField.Version => once(acc.version, raw => acc.copy(version = Some(raw)))
      case NodeLockField.Url     => once(acc.url, raw => acc.copy(url = Some(raw)))
      case NodeLockField.Sha256  => once(acc.sha, raw => acc.copy(sha = Some(raw)))

  private def build(acc: Acc): Either[NodeLockError, NodeLock] =
    for
      majorRaw   <- acc.major.toRight(NodeLockError.Missing(NodeLockField.Major))
      versionRaw <- acc.version.toRight(NodeLockError.Missing(NodeLockField.Version))
      urlRaw     <- acc.url.toRight(NodeLockError.Missing(NodeLockField.Url))
      shaRaw     <- acc.sha.toRight(NodeLockError.Missing(NodeLockField.Sha256))
      major      <- parseMajor(majorRaw)
      version    <- NodeVersion.from(versionRaw).left.map(NodeLockError.BadVersion(_))
      _          <- Either.cond(version.major == major, (), NodeLockError.MajorDisagrees(major, version.major))
      source     <- parseSource(urlRaw)
      sha        <- Sha256.from(shaRaw).left.map(NodeLockError.BadSha256(_))
    yield NodeLock(major, version, source, sha)

  private def parseMajor(raw: String): Either[NodeLockError, NodeMajor] =
    raw.toIntOption match
      case Some(n) => NodeMajor.from(n).left.map(_ => NodeLockError.BadMajor(raw))
      case None    => Left(NodeLockError.BadMajor(raw))

  private def parseSource(raw: String): Either[NodeLockError, ArchiveSource] =
    if raw.startsWith("https://") then HttpsUrl.from(raw).left.map(_ => NodeLockError.BadUrl(raw)).map(ArchiveSource.Remote(_))
    else if raw.startsWith("file:") then parseFile(raw)
    else Left(NodeLockError.BadUrl(raw))

  private def parseFile(raw: String): Either[NodeLockError, ArchiveSource] =
    try
      val uri = java.net.URI(raw)
      if uri.getScheme != "file" then Left(NodeLockError.BadUrl(raw))
      else
        val path = Path.of(uri)
        if path.isAbsolute then Right(ArchiveSource.Local(path)) else Left(NodeLockError.BadUrl(raw))
    catch
      case _: java.net.URISyntaxException => Left(NodeLockError.BadUrl(raw))
      case _: IllegalArgumentException    => Left(NodeLockError.BadUrl(raw))
