package histos.assembly

import histos.Module

import java.nio.file.Path

/** Where `histos assembly` writes one module. An asset wins. Otherwise the build output path. */
object AssemblyPath:
  def relative(module: Module): String =
    module.asset match
      case Some(file) => file.value
      case None       => s"target/assembly/${module.name.value}.jar"

  def output(repo: Path, module: Module): Path =
    repo.resolve(relative(module))
end AssemblyPath
