package histos.jdk

/** Histos runs on macOS and Linux. The Windows tokens stay so a later port has a type to fill. */
object HostSupport:
  val refused: String =
    "Histos runs on macOS and Linux. On Windows, use WSL. A native Windows build may come later."
