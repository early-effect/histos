package histos.program

import java.nio.file.Path

/** The native client binary, `histos-client`. `Program.install` copies it to `dist/histos-client`. A published
  * toolchain does not ship that binary. `toolchain client` links it beside the installed jar.
  */
object ClientBinary:
  val moduleName: String = "client"
  val fileName: String   = "histos-client"

  def destination(repo: Path): Path =
    repo.resolve("dist").resolve(fileName)
end ClientBinary
