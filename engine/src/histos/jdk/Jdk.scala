package histos.jdk

import java.nio.file.{Files, Path}

import histos.toolchain.{RuntimeFiles, VersionOrder}

import zio.{IO, ZIO}

opaque type JdkMajor = Int

object JdkMajor:
  given CanEqual[JdkMajor, JdkMajor] = CanEqual.derived

  def from(raw: Int): Either[JdkError, JdkMajor] =
    if raw > 0 then Right(raw) else Left(JdkError.BadMajor(raw))

  /** A positive major the caller already checked. */
  private[histos] def trusted(raw: Int): JdkMajor = raw

  extension (major: JdkMajor) def value: Int = major
end JdkMajor

/** A release-file version. The major is part of the value, so reading it cannot fail.
  */
final case class JdkVersion private (value: String, major: JdkMajor):
  def atLeast(floor: JdkVersion): Boolean =
    VersionOrder.atLeast(value, floor.value, stripJavaOne = true)

/** `JAVA_RUNTIME_VERSION` from the same `release` file. The build number after `+` is part of it. When that line is
  * absent, this is [[JdkVersion.value]].
  */
opaque type JdkBuild = String

object JdkBuild:
  given CanEqual[JdkBuild, JdkBuild] = CanEqual.derived

  def fromRelease(text: String, version: JdkVersion): JdkBuild =
    quoted(text, "JAVA_RUNTIME_VERSION")
      .filter(buildText)
      .getOrElse(version.value)

  extension (build: JdkBuild) def value: String = build

  private def quoted(text: String, key: String): Option[String] =
    val prefix = s"""$key=""""
    text.linesIterator
      .map(_.trim)
      .collectFirst:
        case row
            if row.startsWith(prefix) && row.endsWith("\"") &&
              row.length > prefix.length + 1 =>
          row.substring(prefix.length, row.length - 1)

  private def buildText(raw: String): Boolean =
    raw.nonEmpty && Character.isDigit(raw.codePointAt(0)) && raw.forall(char =>
      char.isLetterOrDigit || "+-._".contains(char)
    )
end JdkBuild

/** The version the floor compares, and the build fingerprint beside it. */
final case class JdkRelease(version: JdkVersion, build: JdkBuild)

object JdkVersion:
  def from(raw: String): Either[VersionError, JdkVersion] =
    majorOf(raw) match
      case Some(major) if charset(raw) => Right(JdkVersion(raw, major))
      case _                           => Left(VersionError.Bad(raw))

  def fromRelease(text: String): Either[ReleaseError, JdkVersion] =
    val key = "JAVA_VERSION=\""
    val at  = text.indexOf(key)
    if at < 0 then Left(ReleaseError.MissingKey)
    else
      val rest = text.substring(at + key.length)
      val end  = rest.indexOf('"')
      if end < 0 then Left(ReleaseError.Unclosed)
      else
        val raw = rest.substring(0, end)
        from(raw).left.map(_ => ReleaseError.BadVersion(raw))

  /** A version the caller already checked. The major has to be that version's major.
    */
  private[histos] def trusted(text: String, major: JdkMajor): JdkVersion =
    JdkVersion(text, major)

  private def majorOf(raw: String): Option[JdkMajor] =
    val body   = if raw.startsWith("1.") then raw.drop(2) else raw
    val digits = body.takeWhile(_.isDigit)
    digits.toIntOption.flatMap(n => JdkMajor.from(n).toOption)

  private def charset(raw: String): Boolean =
    raw.matches("[0-9][A-Za-z0-9.+_-]*")
end JdkVersion

enum VersionError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"not a JDK version: $raw"
end VersionError

enum ReleaseError:
  case NoFile
  case MissingKey
  case Unclosed
  case BadVersion(raw: String)

  def message: String = this match
    case NoFile          => "no release file"
    case MissingKey      => "release file has no JAVA_VERSION"
    case Unclosed        => "release file has an unclosed JAVA_VERSION"
    case BadVersion(raw) => s"release file has a bad JAVA_VERSION: $raw"
end ReleaseError

enum Reject:
  case Absent
  case NoJavac
  case UnreadableVersion(error: ReleaseError)

  def message: String = this match
    case Absent                   => "not a JDK home"
    case NoJavac                  => "no javac"
    case UnreadableVersion(error) => error.message
end Reject

final case class JdkHome(
    path: Path,
    hasJavac: Boolean,
    release: Either[ReleaseError, JdkRelease],
)

enum JdkError:
  case BadMajor(raw: Int)
  case Io(failure: histos.workspace.IoFailure)

  def message: String = this match
    case BadMajor(raw) => s"$raw is not a JDK major"
    case Io(failure)   => failure.message
end JdkError

object Jdk:
  def home(dir: Path): IO[JdkError, JdkHome] =
    for
      javac <- ZIO
        .attemptBlocking(
          Files.isRegularFile(dir.resolve("bin/javac")) || Files.isRegularFile(
            dir.resolve("bin/javac.exe")
          )
        )
        .mapError(_ =>
          JdkError.Io(
            histos.workspace.IoFailure(dir, histos.workspace.IoFault.Stat)
          )
        )
      text <- RuntimeFiles
        .readIfPresent(dir.resolve("release"))
        .mapError(JdkError.Io(_))
    yield JdkHome(dir, javac, text.fold(Left(ReleaseError.NoFile))(readRelease))

  private def readRelease(text: String): Either[ReleaseError, JdkRelease] =
    JdkVersion.fromRelease(text).map { version =>
      JdkRelease(version, JdkBuild.fromRelease(text, version))
    }
end Jdk
