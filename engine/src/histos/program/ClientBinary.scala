package histos.program

import histos.{Module, Platform, ProjectGraph}

import java.nio.file.Path

/** The native client the launcher execs from `dist/histos-client`.
  */
object ClientBinary:
  val moduleName: String = "client"
  val fileName: String   = "histos-client"

  def destination(repo: Path): Path =
    repo.resolve("dist").resolve(fileName)

  /** A native module named `client` that has a main. The install and snapshot client matrix exist only then.
    */
  def present(graph: ProjectGraph): Boolean =
    graph.projects.exists:
      case module: Module =>
        module.name.value == moduleName &&
        module.platforms.all.contains(Platform.Native) &&
        module.application.nonEmpty
      case _ => false
end ClientBinary
