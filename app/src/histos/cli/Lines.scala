package histos.cli

import histos.testrun.TestedCell
import histos.zinc.CompiledCell
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

/** The lines a compile or a test prints. The daemon and the local command use the same text. */
object Lines:
  def compiled(repo: Path, cell: CompiledCell): String =
    s"${cell.module.value} ${cell.version.value} ${token(cell.platform)} ${cell.classFiles} ${relative(repo, cell.classes)}"

  def begin(module: ProjectName, version: ScalaVersion, platform: Platform): String =
    s"${module.value} ${version.value} ${token(platform)}"

  def tested(cell: TestedCell): String =
    s"${cell.module.value} ${cell.version.value} ${token(cell.platform)} ${cell.status.label}"

  def cached(module: ProjectName, version: ScalaVersion, platform: Platform, suite: String): String =
    s"${module.value} ${version.value} ${token(platform)} $suite cached"

  private def token(platform: Platform): String =
    platform match
      case Platform.Jvm    => "jvm"
      case Platform.Js     => "js"
      case Platform.Native => "native"

  def relative(repo: Path, path: Path): String =
    val base = repo.toAbsolutePath.normalize
    val full = path.toAbsolutePath.normalize
    (if full.startsWith(base) then base.relativize(full) else full).toString.replace('\\', '/')
end Lines
