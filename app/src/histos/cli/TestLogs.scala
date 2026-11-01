package histos.cli

import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import zio.{UIO, ZIO}

/** One plain log per cell, under `.histos/tests`. `clean` deletes that directory.
  */
object TestLogs:
  def file(repo: Path, module: ProjectName, version: ScalaVersion, platform: Platform): Path =
    repo
      .resolve(".histos")
      .resolve("tests")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(s"${platform.token}.log")

  def append(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      line: String,
  ): UIO[Unit] =
    val dest = file(repo, module, version, platform)
    ZIO.attemptBlocking {
      Files.createDirectories(dest.getParent)
      Files.writeString(
        dest,
        line + "\n",
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND,
      )
    }.ignore
end TestLogs
