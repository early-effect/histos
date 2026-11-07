package histos.coverage

import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** One instrumented statement. `ignored` is `$COVERAGE-OFF$` and is not a rate. */
final case class Statement(
    id: Int,
    source: String,
    packageName: String,
    className: String,
    classType: String,
    fullClassName: String,
    method: String,
    start: Int,
    end: Int,
    line: Int,
    symbol: String,
    tree: String,
    branch: Boolean,
    ignored: Boolean,
    description: String,
)

enum CoverageError:
  case BadVersion(found: String)
  case Truncated
  case BadNumber(field: String, raw: String)
  case BadBoolean(field: String, raw: String)
  case BadRegex(kind: ExcludeKind, raw: String)
  case MissingInventory(path: Path)

  def message: String = this match
    case BadVersion(found)      => s"coverage format is not 3.0: $found"
    case Truncated              => "coverage statement is cut off"
    case BadNumber(field, raw)  => s"coverage $field is not an integer: $raw"
    case BadBoolean(field, raw) => s"coverage $field is not true or false: $raw"
    case BadRegex(kind, raw)    => s"coverage exclude ${kind.token} is not a regex: $raw"
    case MissingInventory(path) => s"coverage inventory is missing in $path"
end CoverageError

/** Format 3.0, the line protocol the Scala 3 compiler and the Scala 2 plugin both write. */
object Inventory:
  val Header: String = "# Coverage data, format version: 3.0"

  def parse(text: String): Either[CoverageError, List[Statement]] =
    val lines = normalize(text)
    lines match
      case Nil           => Left(CoverageError.BadVersion(""))
      case first :: rest =>
        if first != Header then Left(CoverageError.BadVersion(first))
        else blocks(rest.dropWhile(_.startsWith("#")), Nil)

  private def blocks(
      lines: List[String],
      acc: List[Statement],
  ): Either[CoverageError, List[Statement]] =
    lines.dropWhile(_.isEmpty) match
      case Nil  => Right(acc.reverse)
      case rest =>
        split(rest, Nil) match
          case None               => Left(CoverageError.Truncated)
          case Some((body, tail)) =>
            one(body) match
              case Left(error) => Left(error)
              case Right(stmt) => blocks(tail, stmt :: acc)

  private def split(
      lines: List[String],
      seen: List[String],
  ): Option[(List[String], List[String])] =
    lines match
      case Nil          => None
      case "\f" :: tail => Some((seen.reverse, tail))
      case line :: tail => split(tail, line :: seen)

  private def one(lines: List[String]): Either[CoverageError, Statement] =
    lines match
      case id :: source :: pkg :: cls :: kind :: full :: method :: start :: end :: line :: symbol :: tree ::
          branch :: count :: ignored :: desc =>
        for
          _        <- integer("count", count)
          idN      <- integer("id", id)
          startN   <- integer("start", start)
          endN     <- integer("end", end)
          lineN    <- integer("line", line)
          branchB  <- flag("branch", branch)
          ignoredB <- flag("ignored", ignored)
        yield Statement(
          idN,
          source,
          pkg,
          cls,
          kind,
          full,
          method,
          startN,
          endN,
          lineN,
          symbol,
          tree,
          branchB,
          ignoredB,
          desc.mkString("\n"),
        )
      case _ => Left(CoverageError.Truncated)

  private def integer(field: String, raw: String): Either[CoverageError, Int] =
    raw.toIntOption match
      case Some(n) => Right(n)
      case None    => Left(CoverageError.BadNumber(field, raw))

  private def flag(field: String, raw: String): Either[CoverageError, Boolean] =
    raw match
      case "true"  => Right(true)
      case "false" => Right(false)
      case _       => Left(CoverageError.BadBoolean(field, raw))

  private def normalize(text: String): List[String] =
    text.replace("\r\n", "\n").replace("\r", "\n").split("\n", -1).toList
end Inventory

/** One id per line. A blank line is skipped. The compiler writes these after the tests run. */
object Measurements:
  val Prefix: String = "scoverage.measurements."

  /** Drop measurement files in one classes directory. The inventory stays. A missing directory is nothing to drop. */
  def forget(dir: Path): IO[IoFailure, Unit] =
    ZIO
      .attemptBlocking(Files.isDirectory(dir))
      .mapError(_ => IoFailure(dir, IoFault.Stat))
      .flatMap:
        case false => ZIO.unit
        case true  =>
          ZIO.scoped:
            for
              stream <- ZIO.acquireRelease(
                ZIO.attemptBlocking(Files.list(dir)).mapError(_ => IoFailure(dir, IoFault.Walk))
              )(open => ZIO.attemptBlocking(open.close()).ignore)
              paths <- ZIO
                .attemptBlocking(
                  stream.iterator.asScala.filter(path => fileName(path).exists(_.startsWith(Prefix))).toList
                )
                .mapError(_ => IoFailure(dir, IoFault.Walk))
              _ <- ZIO.foreachDiscard(paths)(Workspace.delete)
            yield ()

  private def fileName(path: Path): Option[String] =
    Option(path.getFileName).map(_.toString)

  def parse(text: String): Either[CoverageError, Set[Int]] =
    val lines = text.replace("\r\n", "\n").replace("\r", "\n").split("\n", -1).toList.filter(_.nonEmpty)
    def read(rest: List[String], acc: Set[Int]): Either[CoverageError, Set[Int]] =
      rest match
        case Nil         => Right(acc)
        case raw :: tail =>
          raw.toIntOption match
            case Some(id) => read(tail, acc + id)
            case None     => Left(CoverageError.BadNumber("measurement", raw))
    read(lines, Set.empty)
end Measurements
