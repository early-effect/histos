package histos.testrun

import java.nio.file.{Files, Path}

import scala.util.control.NonFatal

import histos.{Platform, ProjectName, ScalaVersion}
import histos.workspace.{IoFailure, IoFault, Workspace}

import zio.{IO, UIO, ZIO}

/** A stored success could not be written or removed. A missing record is not one of these: [[QuickRecord.hit]] treats
  * that as a miss.
  */
enum QuickError:
  case Save(failure: IoFailure)
  case Clear(failure: IoFailure)

  def message: String = this match
    case Save(failure)  => failure.message
    case Clear(failure) => failure.message
end QuickError

/** A stored success. A missing record, or a different digest, is not a success. An unreadable record is a miss. A
  * failed save or clear is [[QuickError]].
  */
object QuickRecord:
  def hit(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String,
      digest: String,
  ): UIO[Boolean] =
    ZIO
      .attemptBlocking(read(file(repo, module, version, platform, suite)))
      .orElseSucceed(None)
      .map(_.contains(digest))

  def save(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String,
      digest: String,
  ): IO[QuickError, Unit] =
    val path = file(repo, module, version, platform, suite)
    // Unlink first. A mode that forbids reading still lets the owner replace
    // the record when the parent directory is writable, which is how a miss
    // stores the next success.
    ZIO
      .attemptBlocking(Files.deleteIfExists(path))
      .mapError(_ => QuickError.Save(IoFailure(path, IoFault.Delete))) *>
      Workspace
        .writeString(path, s"success $digest\n")
        .mapError(QuickError.Save(_))

  def clear(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String,
  ): IO[QuickError, Unit] =
    val path = file(repo, module, version, platform, suite)
    ZIO
      .attemptBlocking(Files.deleteIfExists(path))
      .mapError(_ => QuickError.Clear(IoFailure(path, IoFault.Delete)))
      .unit

  private[testrun] def file(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String,
  ): Path =
    val base = repo
      .resolve(".histos")
      .resolve("test-quick")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)
    suite.split('.').filter(_.nonEmpty).foldLeft(base)(_.resolve(_))

  private def read(path: Path): Option[String] =
    if !Files.isRegularFile(path) then None
    else
      try
        Files
          .readString(path)
          .linesIterator
          .map(_.trim)
          .find(_.nonEmpty)
          .flatMap:
            case text if text.startsWith("success ") =>
              Some(text.stripPrefix("success ").trim)
            case _ => None
      catch case NonFatal(_) => None
end QuickRecord
