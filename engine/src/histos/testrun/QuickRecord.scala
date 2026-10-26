package histos.testrun

import java.io.IOException
import java.nio.file.{AccessDeniedException, Files, NoSuchFileException, Path}

import histos.jdk.{Sha256, Sha256Error}
import histos.{Platform, ProjectName, ScalaVersion}
import histos.workspace.{IoFailure, IoFault, Workspace}

import zio.{IO, ZIO}

/** A stored success could not be written, read, or removed, or the text was not a success digest. A missing record is
  * not one of these: [[QuickRecord.hit]] treats that as a miss.
  */
enum QuickError:
  case Save(failure: IoFailure)
  case Clear(failure: IoFailure)
  case Read(failure: IoFailure)
  case Shape(path: Path)
  case Hash(path: Path, error: Sha256Error)
  case Suite(error: SuiteNameError)

  def message: String = this match
    case Save(failure)     => failure.message
    case Clear(failure)    => failure.message
    case Read(failure)     => failure.message
    case Shape(path)       => s"quick record $path is not a stored success"
    case Hash(path, error) => s"quick record $path ${error.message}"
    case Suite(error)      => error.message
end QuickError

/** A stored success. A missing record, or a different digest, is not a success. An unreadable record, or a record that
  * is not `success` plus a sha256, is [[QuickError]]. A failed save or clear is [[QuickError]].
  */
object QuickRecord:
  def hit(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: SuiteName,
      digest: Sha256,
  ): IO[QuickError, Boolean] =
    load(file(repo, module, version, platform, suite)).map:
      case Some(stored) => stored == digest
      case None         => false

  def save(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: SuiteName,
      digest: Sha256,
  ): IO[QuickError, Unit] =
    val path = file(repo, module, version, platform, suite)
    // Unlink first. A mode that forbids reading still lets the owner replace
    // the record when the parent directory is writable, which is how a miss
    // stores the next success.
    ZIO
      .attemptBlocking(Files.deleteIfExists(path))
      .mapError(_ => QuickError.Save(IoFailure(path, IoFault.Delete))) *>
      Workspace
        .writeString(path, s"success ${digest.value}\n")
        .mapError(QuickError.Save(_))

  def clear(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: SuiteName,
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
      suite: SuiteName,
  ): Path =
    val base = repo
      .resolve(".histos")
      .resolve("test-quick")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)
    suite.value.split('.').filter(_.nonEmpty).foldLeft(base)(_.resolve(_))

  /** `None` is a missing file. A present file that cannot be read, or is not `success` plus a sha256, fails. */
  private def load(path: Path): IO[QuickError, Option[Sha256]] =
    ZIO
      .attemptBlocking(Files.readString(path))
      .foldZIO(
        {
          case _: NoSuchFileException =>
            ZIO.succeed(None)
          case _: AccessDeniedException =>
            ZIO.fail(QuickError.Read(IoFailure(path, IoFault.Read)))
          case _: IOException =>
            ZIO.fail(QuickError.Read(IoFailure(path, IoFault.Read)))
          case defect =>
            ZIO.die(defect)
        },
        text => parse(path, text).map(Some(_)),
      )

  private def parse(path: Path, text: String): IO[QuickError, Sha256] =
    text.linesIterator.map(_.trim).find(_.nonEmpty) match
      case Some(line) if line.startsWith("success ") =>
        val raw = line.stripPrefix("success ").trim
        ZIO.fromEither(Sha256.from(raw)).mapError(QuickError.Hash(path, _))
      case _ =>
        ZIO.fail(QuickError.Shape(path))
end QuickRecord
