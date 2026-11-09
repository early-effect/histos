package histos.format

import histos.workspace.Workspace
import histos.ScalafmtConf

import java.nio.file.Path

import scala.annotation.tailrec

import zio.{IO, ZIO}

/** A scalafmt release Histos can lock: `major.minor.patch`, with an optional pre-release suffix, at 3.0.0 or newer. */
opaque type ScalafmtVersion = String

object ScalafmtVersion:
  given CanEqual[ScalafmtVersion, ScalafmtVersion] = CanEqual.derived

  private val Shape = """(\d+)\.(\d+)\.(\d+)(-[0-9A-Za-z.]+)?""".r

  def from(config: Path, raw: String): Either[ScalafmtConfError, ScalafmtVersion] =
    raw match
      case Shape(major, _, _, _) =>
        if major.toIntOption.exists(_ >= 3) then Right(raw) else Left(ScalafmtConfError.Unsupported(config, raw))
      case _ => Left(ScalafmtConfError.BadVersion(config, raw))

  extension (version: ScalafmtVersion) def value: String = version

  /** The version the declared configuration names. The error path is relative to the repository. */
  def of(repo: Path, conf: ScalafmtConf): IO[ScalafmtConfError, ScalafmtVersion] =
    val shown = Path.of(conf.relative)
    Workspace
      .readIfPresent(repo.resolve(conf.relative))
      .mapError(ScalafmtConfError.Unreadable(_))
      .flatMap:
        case None       => ZIO.fail(ScalafmtConfError.Missing(shown))
        case Some(text) =>
          ScalafmtConfText.version(text) match
            case None      => ZIO.fail(ScalafmtConfError.NoVersion(shown))
            case Some(raw) => ZIO.fromEither(from(shown, raw))
end ScalafmtVersion

/** The top-level `version` of a HOCON scalafmt configuration. The last top-level line wins, as in HOCON. A `version`
  * inside an object, a string, or a comment is not the scalafmt version. Includes are not followed.
  */
object ScalafmtConfText:
  private val Line = """\s*version\s*[=:]\s*(.*?)\s*""".r

  def version(text: String): Option[String] =
    text.linesIterator
      .foldLeft((0, Option.empty[String])) { case ((depth, found), line) =>
        val code = uncommented(line)
        val next = code match
          case Line(value) if depth == 0 => Some(unquote(value))
          case _                         => found
        (depth + braces(code), next)
      }
      ._2

  private def uncommented(line: String): String =
    @tailrec def end(at: Int, quoted: Boolean): Int =
      if at >= line.length then at
      else
        line.charAt(at) match
          case '"'                                         => end(at + 1, !quoted)
          case '#' if !quoted                              => at
          case '/' if !quoted && line.startsWith("//", at) => at
          case _                                           => end(at + 1, quoted)
    line.take(end(0, quoted = false))

  private def braces(code: String): Int =
    code
      .foldLeft((false, 0)) { case ((quoted, delta), char) =>
        char match
          case '"'            => (!quoted, delta)
          case '{' if !quoted => (quoted, delta + 1)
          case '}' if !quoted => (quoted, delta - 1)
          case _              => (quoted, delta)
      }
      ._2

  private def unquote(value: String): String =
    if value.length >= 2 && value.startsWith("\"") && value.endsWith("\"") then value.substring(1, value.length - 1)
    else value
end ScalafmtConfText
