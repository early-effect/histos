package histos.toolchain

import histos.archive.{
  ArchivePin,
  ArchiveSource,
  ArchiveSourceError,
  HostArchive
}
import histos.jdk.{Sha256, Sha256Error}

import zio.{Chunk, NonEmptyChunk}

final case class ToolLock[T] private (
    pin: T,
    archives: NonEmptyChunk[HostArchive]
):
  def archive(host: HostPlatform): Either[ToolArchiveError, ArchivePin] =
    archives
      .find(_.host == host)
      .map(_.pin)
      .toRight(ToolArchiveError.NoArchive(host))

enum ToolArchiveError:
  case NoArchive(host: HostPlatform)
  def message: String = this match
    case NoArchive(host) =>
      s"runtime lock has no archive for ${host.description}"

enum ToolLockField:
  case Major, Version, Archive
  def name: String = this match
    case Major   => "major"
    case Version => "version"
    case Archive => "archive"

enum ToolLockError[+E]:
  case Empty
  case BadLine(line: String)
  case UnknownField(name: String)
  case Duplicate(field: ToolLockField)
  case Missing(field: ToolLockField)
  case Pin(error: E)
  case EmptyArchives
  case Host(error: HostPlatformError)
  case DuplicateHost(host: HostPlatform)
  case Source(error: ArchiveSourceError)
  case BadSha256(error: Sha256Error)

  def schemaMessage: String = this match
    case Empty               => "runtime lock is empty"
    case BadLine(line)       => s"malformed runtime lock row: $line"
    case UnknownField(name)  => s"unknown runtime lock field: $name"
    case Duplicate(field)    => s"runtime lock repeats ${field.name}"
    case Missing(field)      => s"runtime lock is missing ${field.name}"
    case Pin(_)              => "runtime lock pin is invalid"
    case EmptyArchives       => "runtime lock has no archive rows"
    case Host(error)         => error.message
    case DuplicateHost(host) => s"runtime lock repeats host ${host.description}"
    case Source(error)       => error.message
    case BadSha256(error)    => error.message

object ToolLock:
  final case class Codec[T, E](
      parse: (String, String) => Either[E, T],
      fields: T => (String, String)
  )

  private final case class Rows(
      fields: Map[ToolLockField, String] = Map.empty,
      archives: Chunk[HostArchive] = Chunk.empty
  )

  def from[T](
      pin: T,
      entries: Chunk[HostArchive]
  ): Either[ToolLockError[Nothing], ToolLock[T]] =
    for
      nonempty <- NonEmptyChunk
        .fromChunk(entries)
        .toRight(ToolLockError.EmptyArchives)
      _ <- entries.foldLeft[Either[ToolLockError[Nothing], Set[HostPlatform]]](
        Right(Set.empty)
      ): (acc, row) =>
        acc.flatMap: seen =>
          for
            _ <- row.host.token.toRight(
              ToolLockError.Host(
                HostPlatformError.Unknown(row.host.description)
              )
            )
            _ <- Either.cond(
              !seen.contains(row.host),
              (),
              ToolLockError.DuplicateHost(row.host)
            )
          yield seen + row.host
    yield ToolLock(pin, nonempty)

  def parse[T, E](
      text: String,
      codec: Codec[T, E]
  ): Either[ToolLockError[E], ToolLock[T]] =
    val lines = text
      .split("\n", -1)
      .iterator
      .map(stripLine)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .toList
    if lines.isEmpty then Left(ToolLockError.Empty)
    else
      for
        rows <- lines.foldLeft[Either[ToolLockError[E], Rows]](Right(Rows()))(
          (acc, line) => acc.flatMap(row(_, line))
        )
        major <- rows.fields
          .get(ToolLockField.Major)
          .toRight(ToolLockError.Missing(ToolLockField.Major))
        version <- rows.fields
          .get(ToolLockField.Version)
          .toRight(ToolLockError.Missing(ToolLockField.Version))
        pin <- codec.parse(major, version).left.map(ToolLockError.Pin(_))
        lock <- from(pin, rows.archives)
      yield lock

  def render[T, E](lock: ToolLock[T], codec: Codec[T, E]): String =
    val (major, version) = codec.fields(lock.pin)
    val archiveRows = lock.archives
      .map(row =>
        s"archive ${row.host.description} ${row.pin.source.render} ${row.pin.sha256.value}"
      )
      .mkString("\n")
    s"major $major\nversion $version\n$archiveRows\n"

  private def stripLine(line: String): String =
    def whitespace(character: Char): Boolean =
      character == ' ' || character == '\t' || character == '\r'
    line.dropWhile(whitespace).reverse.dropWhile(whitespace).reverse

  private def row(
      rows: Rows,
      line: String
  ): Either[ToolLockError[Nothing], Rows] =
    if line.exists(character => character < ' ' && character != '\t') then
      Left(ToolLockError.BadLine(line))
    else
      line.split("[ \\t]+", -1).toList match
        case "major" :: value :: Nil => header(rows, ToolLockField.Major, value)
        case "version" :: value :: Nil =>
          header(rows, ToolLockField.Version, value)
        case "archive" :: hostRaw :: sourceRaw :: shaRaw :: Nil =>
          for
            host <- HostPlatform.from(hostRaw).left.map(ToolLockError.Host(_))
            _ <- Either.cond(
              !rows.archives.exists(_.host == host),
              (),
              ToolLockError.DuplicateHost(host)
            )
            source <- ArchiveSource
              .from(sourceRaw)
              .left
              .map(ToolLockError.Source(_))
            sha <- Sha256.from(shaRaw).left.map(ToolLockError.BadSha256(_))
          yield rows.copy(archives =
            rows.archives :+ HostArchive(host, ArchivePin(source, sha))
          )
        case name :: _ if !Set("major", "version", "archive").contains(name) =>
          Left(ToolLockError.UnknownField(name))
        case _ => Left(ToolLockError.BadLine(line))

  private def header(
      rows: Rows,
      field: ToolLockField,
      value: String
  ): Either[ToolLockError[Nothing], Rows] =
    Either.cond(
      !rows.fields.contains(field),
      rows.copy(fields = rows.fields.updated(field, value)),
      ToolLockError.Duplicate(field)
    )
