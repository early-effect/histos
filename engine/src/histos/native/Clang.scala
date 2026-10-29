package histos.native

import histos.ClangMajor

import histos.process.{ChildEvent, Process}
import histos.workspace.Workspace

import java.io.IOException
import java.nio.file.{Files, InvalidPathException, Path}

import scala.annotation.tailrec

import zio.{IO, ZIO}

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

final case class ClangReport(version: ClangVersion, target: ClangTarget)

object ClangReport:
  private val VersionWord  = " version "
  private val TargetPrefix = "Target: "

  def parse(text: String): Either[ClangText, ClangReport] =
    val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    if lines.isEmpty then Left(ClangText.Empty)
    else
      for
        version <- versionOf(lines)
        target  <- targetOf(lines)
      yield ClangReport(version, target)

  private def versionOf(lines: List[String]): Either[ClangText, ClangVersion] =
    lines.collectFirst:
      case line if line.contains(VersionWord) =>
        val after = line.substring(line.indexOf(VersionWord) + VersionWord.length)
        after.takeWhile(c => !c.isWhitespace)
    match
      case None        => Left(ClangText.NoVersion)
      case Some("")    => Left(ClangText.NoVersion)
      case Some(token) => ClangVersion.from(token)

  private def targetOf(lines: List[String]): Either[ClangText, ClangTarget] =
    lines.collectFirst:
      case line if line.startsWith(TargetPrefix) => line.drop(TargetPrefix.length).trim
    match
      case None      => Left(ClangText.NoTarget)
      case Some(raw) => ClangTarget.from(raw)
end ClangReport

/** The clang and clang++ that agreed on one version. They are a pair, not two searches. */
final case class Clang(clang: Path, clangpp: Path, version: ClangVersion, target: ClangTarget)

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

object ClangProbe:
  /** The clang on `PATH`, with no major check. */
  def found(env: Map[String, String]): ZIO[Process, ClangMiss, Clang] =
    binary(env, "clang").flatMap:
      case None        => ZIO.fail(ClangMiss.Absent)
      case Some(clang) =>
        cxxBeside(clang) match
          case None          => ZIO.fail(ClangMiss.NoClangPP(clang))
          case Some(clangpp) =>
            regular(clangpp).flatMap:
              case false => ZIO.fail(ClangMiss.NoClangPP(clang))
              case true  =>
                for
                  cc  <- report(clang)
                  cxx <- report(clangpp)
                  _   <-
                    if cc.version != cxx.version then ZIO.fail(ClangMiss.Mismatched(cc.version, cxx.version))
                    else ZIO.unit
                yield Clang(clang, clangpp, cc.version, cc.target)

  def resolve(wanted: ClangMajor, env: Map[String, String]): ZIO[Process, ClangMiss, Clang] =
    found(env)
      .flatMap: clang =>
        if clang.version.major == wanted then ZIO.succeed(clang)
        else ZIO.fail(ClangMiss.WrongMajor(clang.version, wanted, clang.clang))
      .mapError:
        case ClangMiss.Absent => ClangMiss.NotOnPath(wanted)
        case other            => other

  private def report(binary: Path): ZIO[Process, ClangMiss, ClangReport] =
    ZIO.scoped:
      for
        work   <- Workspace.work("histos-clang-version").mapError(_ => ClangMiss.Unreadable(binary))
        events <- ZIO
          .serviceWithZIO[Process](
            _.command(List(binary.toString, "--version"), work, Map.empty).runCollect
          )
          .mapError(_ => ClangMiss.Unreadable(binary))
        code <- ZIO
          .fromOption(events.collect { case ChildEvent.Exited(exit) => exit }.lastOption)
          .orElseFail(ClangMiss.Unreadable(binary))
        text = events
          .collect {
            case ChildEvent.Out(line) => line
            case ChildEvent.Err(line) => line
          }
          .mkString("\n")
        parsed <-
          if code.value == 0 then ZIO.fromEither(ClangReport.parse(text)).mapError(ClangMiss.Text(binary, _))
          else ZIO.fail(ClangMiss.Status(binary, code.value))
      yield parsed

  private def binary(env: Map[String, String], name: String): IO[ClangMiss, Option[Path]] =
    checked(scan(entries(env), name))

  private def regular(path: Path): IO[ClangMiss, Boolean] =
    checked(isRegular(path))

  private def checked[A](body: => Either[ClangMiss, A]): IO[ClangMiss, A] =
    ZIO.attemptBlocking(body).orDie.flatMap(ZIO.fromEither)

  private def entries(env: Map[String, String]): List[String] =
    env
      .get("PATH")
      .filter(_.nonEmpty)
      .toList
      .flatMap(_.split(java.io.File.pathSeparator).toList.filter(_.nonEmpty))

  @tailrec
  private def scan(rest: List[String], name: String): Either[ClangMiss, Option[Path]] =
    rest match
      case Nil           => Right(None)
      case entry :: more =>
        look(entry, name) match
          case Right(None) => scan(more, name)
          case located     => located

  private def look(entry: String, name: String): Either[ClangMiss, Option[Path]] =
    pathOf(entry).flatMap: dir =>
      val file = dir.resolve(name)
      isRegular(file).map(hit => if hit then Some(file) else None)

  private def pathOf(entry: String): Either[ClangMiss, Path] =
    try Right(Path.of(entry))
    catch case _: InvalidPathException => Left(ClangMiss.BadPath(entry))

  private def isRegular(path: Path): Either[ClangMiss, Boolean] =
    try Right(Files.isRegularFile(path))
    catch case _: IOException => Left(ClangMiss.Unreadable(path))

  private def cxxBeside(clang: Path): Option[Path] =
    Option(clang.getFileName).flatMap: file =>
      val cxx = if file.toString.endsWith(".exe") then "clang++.exe" else "clang++"
      Option(clang.getParent).map(_.resolve(cxx))
end ClangProbe
