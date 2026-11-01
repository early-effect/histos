package histos.jdk

import java.nio.file.{Files, Path}

import histos.toolchain.RuntimeFiles

import zio.{IO, ZIO}

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

final case class JdkHome(
    path: Path,
    hasJavac: Boolean,
    release: Either[ReleaseError, JdkRelease],
)

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
