package histos.publish

import histos.jdk.Sha256

import java.nio.charset.StandardCharsets

/** The sha256 of `install/histos` packed into the launcher jar. The bootstrap wrapper installs `shim-<sha256>`. */
object PublishedShim:
  val entry: String    = "META-INF/histos/shim.sha256"
  val fileName: String = "install/histos"

  def asset(sha: Sha256): String = s"shim-${sha.value}"

  /** Left when this jar has no digest, or the resource is not 64 hex digits. */
  def load: Either[ScriptError, Sha256] =
    val loader = classOf[PublishedShim.type].getClassLoader
    Option(loader.getResourceAsStream(entry)) match
      case None         => Left(ScriptError.Shim)
      case Some(stream) =>
        try
          val text = String(stream.readAllBytes, StandardCharsets.UTF_8).trim
          Sha256.from(text).left.map(_ => ScriptError.Shim)
        catch case _: java.io.IOException => Left(ScriptError.Shim)
        finally stream.close()
end PublishedShim
