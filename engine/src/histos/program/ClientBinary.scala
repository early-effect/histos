package histos.program

import java.nio.file.Path

/** The native client binary, `histos-client`. `Program.install` copies it to `dist/histos-client`. A toolchain publish
  * links it under `dist/clients` and installs that file beside the jar.
  */
object ClientBinary:
  val moduleName: String = "client"
  val fileName: String   = "histos-client"

  def destination(repo: Path): Path =
    repo.resolve("dist").resolve(fileName)
end ClientBinary
