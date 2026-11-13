package histos.program

import java.nio.file.Path

/** The native client the launcher execs from `dist/histos-client`.
  */
object ClientBinary:
  val moduleName: String = "client"
  val fileName: String   = "histos-client"

  def destination(repo: Path): Path =
    repo.resolve("dist").resolve(fileName)
end ClientBinary
