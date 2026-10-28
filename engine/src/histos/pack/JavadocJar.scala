package histos.pack

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{Chunk, IO}

/** The javadoc jar Central requires. Real scaladoc is a later product, so the jar holds a README. */
object JavadocJar:
  val Entry: String = "README"
  val Text: String  = "This jar is not scaladoc.\n"

  def write(destination: Path): IO[JarError, Jarred] =
    val bytes = Chunk.fromArray(Text.getBytes(StandardCharsets.UTF_8))
    ClassJar.writeBytes(List(Entry -> bytes), destination)
end JavadocJar
