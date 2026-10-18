package histos.cli

import histos.testrun.TestedCell
import histos.zinc.CompiledCell
import histos.{ProjectName, ScalaVersion}

import java.nio.file.Path

/** The lines a compile or a test prints. The daemon and the local command use the same text. */
object Lines:
  def compiled(repo: Path, cell: CompiledCell): String =
    s"${cell.module.value} ${cell.version.value} jvm ${cell.classFiles} ${relative(repo, cell.classes)}"

  def begin(module: ProjectName, version: ScalaVersion): String =
    s"${module.value} ${version.value} jvm"

  def tested(cell: TestedCell): String =
    s"${cell.module.value} ${cell.version.value} jvm ${cell.status.label}"

  def relative(repo: Path, path: Path): String =
    val base = repo.toAbsolutePath.normalize
    val full = path.toAbsolutePath.normalize
    (if full.startsWith(base) then base.relativize(full) else full).toString.replace('\\', '/')
end Lines
