package histos.testrun

import java.nio.file.{Files, Path}

import scala.util.control.NonFatal

import histos.{Platform, ProjectName, ScalaVersion}
import histos.workspace.Workspace

import zio.UIO
import zio.ZIO

/** A stored success. A missing record, or a different digest, is not a success.
  */
object QuickRecord:
  def hit(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String,
      digest: String
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
      digest: String
  ): UIO[Unit] =
    Workspace
      .writeString(
        file(repo, module, version, platform, suite),
        s"success $digest\n"
      )
      .ignore

  def clear(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String
  ): UIO[Unit] =
    ZIO
      .attemptBlocking(
        Files.deleteIfExists(file(repo, module, version, platform, suite))
      )
      .ignore

  private[testrun] def file(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String
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
