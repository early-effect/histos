package histos.pack

import java.nio.charset.StandardCharsets

import zio.{Chunk, IO}

/** The javadoc file Central requires. It is one page, not a scaladoc site. */
object JavadocStub:
  val Entry: String = "index.html"

  val Page: Chunk[Byte] =
    Chunk.fromArray(
      """<!DOCTYPE html>
        |<html>
        |<head><title>javadoc</title></head>
        |<body><p>Sources are in the sources jar.</p></body>
        |</html>
        |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )

  def write(destination: java.nio.file.Path): IO[JarError, Jarred] =
    ClassJar.writeBytes(List(Entry -> Page), destination)
end JavadocStub
