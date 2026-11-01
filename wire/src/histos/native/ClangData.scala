package histos.native

import histos.ClangMajor
import java.nio.file.{Path}

/** The version token `clang --version` reported, such as `21.0.0`. The major is part of it. */
final case class ClangVersion private (value: String, major: ClangMajor)

object ClangVersion:
  given CanEqual[ClangVersion, ClangVersion] = CanEqual.derived

  private val Token = """(\d+)\.\d+\.\d+([.\-+][0-9A-Za-z.]+)?""".r

  def from(raw: String): Either[ClangText, ClangVersion] =
    raw match
      case Token(major, _) =>
        major.toIntOption match
          case Some(n) =>
            ClangMajor.from(n) match
              case Right(pinned) => Right(ClangVersion(raw, pinned))
              case Left(_)       => Left(ClangText.BadVersion(raw))
          case None => Left(ClangText.BadVersion(raw))
      case _ => Left(ClangText.BadVersion(raw))

  private[native] def trusted(text: String, major: ClangMajor): ClangVersion =
    ClangVersion(text, major)
end ClangVersion

/** The `Target:` line, such as `arm64-apple-darwin27.0.0`. */
opaque type ClangTarget = String

object ClangTarget:
  given CanEqual[ClangTarget, ClangTarget] = CanEqual.derived

  def from(raw: String): Either[ClangText, ClangTarget] =
    if raw.nonEmpty && !raw.exists(_.isWhitespace) then Right(raw) else Left(ClangText.NoTarget)

  extension (target: ClangTarget) def value: String = target
end ClangTarget

enum ClangText:
  case Empty
  case NoVersion
  case BadVersion(raw: String)
  case NoTarget

  def message: String = this match
    case Empty           => "clang --version said nothing"
    case NoVersion       => "clang --version did not report a version"
    case BadVersion(raw) => s"not a clang version: $raw"
    case NoTarget        => "clang --version did not report a target"
end ClangText

enum ClangMiss:
  case Absent
  case NotOnPath(wanted: ClangMajor)
  case BadPath(entry: String)
  case NoClangPP(clang: Path)
  case Unreadable(path: Path)
  case Status(path: Path, code: Int)
  case Text(path: Path, error: ClangText)
  case WrongMajor(found: ClangVersion, wanted: ClangMajor, clang: Path)
  case Mismatched(clang: ClangVersion, clangpp: ClangVersion)

  def message: String = this match
    case Absent =>
      "clang is not on PATH"
    case NotOnPath(wanted) =>
      s"clang is not on PATH. Install clang ${wanted.value}"
    case BadPath(entry) =>
      s"'$entry' is not a path"
    case NoClangPP(clang) =>
      s"clang++ is not beside $clang"
    case Unreadable(path) =>
      s"could not read $path"
    case Status(path, code) =>
      s"$path --version exited $code"
    case Text(path, error) =>
      s"$path --version: ${error.message}"
    case WrongMajor(found, wanted, clang) =>
      s"clang ${found.value} at $clang is not major ${wanted.value}. Install clang ${wanted.value}"
    case Mismatched(clang, clangpp) =>
      s"clang ${clang.value} and clang++ ${clangpp.value} are not the same compiler"
end ClangMiss
