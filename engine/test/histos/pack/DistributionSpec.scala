package histos.pack

import histos.load.BuildClasspath
import histos.publish.PublishedShim
import histos.suite.HistosSpec
import histos.workspace.Workspace
import histos.ToolchainIdentity

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{Chunk, ZIO}
import zio.test.*

object DistributionSpec extends HistosSpec:
  def spec = suite("a packed classpath")(
    test("is a distribution, drops ide data, and keeps the application identity") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-dist-spec").orDieWith(error => new java.io.IOException(error.message))
          app = root.resolve("app.jar")
          api = root.resolve("api.jar")
          dep = root.resolve("dep.jar")
          out = root.resolve("histos.jar")
          _ <- ClassJar
            .writeBytes(
              List(
                "histos/cli/Main.class"                  -> bytes(1, 2, 3),
                "META-INF/semanticdb/a.scala.semanticdb" -> bytes(4),
                "META-INF/TEST.SF"                       -> bytes(5),
                ToolchainIdentity.entry                  -> Chunk.fromArray(applicationIdentity),
                PublishedShim.entry                      -> Chunk.fromArray(applicationShim),
              ),
              app,
            )
            .orDieWith(error => new java.io.IOException(error.message))
          _ <- ClassJar
            .writeBytes(List("histos/Graph.class" -> bytes(6, 7)), api)
            .orDieWith(error => new java.io.IOException(error.message))
          _ <- ClassJar
            .writeBytes(
              List(
                "third/Lib.class"       -> bytes(8),
                ToolchainIdentity.entry -> Chunk.fromArray(dependencyIdentity),
                PublishedShim.entry     -> Chunk.fromArray(dependencyShim),
              ),
              dep,
            )
            .orDieWith(error => new java.io.IOException(error.message))
          _           <- Distribution.write(app, List(dep, api), api, out)
          distributed <- BuildClasspath.distribution(out)
          names       <- entries(out)
          nested      <- entry(out, BuildClasspath.Entry)
          identity    <- entry(out, ToolchainIdentity.entry)
          shim        <- entry(out, PublishedShim.entry)
          original    <- ZIO.attemptBlocking(Files.readAllBytes(api)).orDie
        yield assertTrue(
          distributed,
          names.contains("histos/cli/Main.class"),
          names.contains("third/Lib.class"),
          names.contains(BuildClasspath.Entry),
          !names.exists(ClassJar.semanticdb),
          !names.exists(_.endsWith(".SF")),
          nested.sameElements(original),
          identity.sameElements(applicationIdentity),
          shim.sameElements(applicationShim),
        )
      }
    }
  )

  private val applicationIdentity = utf8(
    """{"coordinate":"app","commit":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","dirty":false}"""
  )
  private val dependencyIdentity = utf8(
    """{"coordinate":"dep","commit":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","dirty":true}"""
  )
  private val applicationShim = utf8("a" * 64 + "\n")
  private val dependencyShim  = utf8("b" * 64 + "\n")

  private def utf8(text: String): Array[Byte] =
    text.getBytes(StandardCharsets.UTF_8)

  private def bytes(values: Int*): Chunk[Byte] =
    Chunk.fromArray(values.map(_.toByte).toArray)

  private def entries(jar: Path): ZIO[Any, Throwable, List[String]] =
    ZIO.attemptBlocking {
      val zip = new ZipFile(jar.toFile)
      try zip.entries.asScala.map(_.getName).toList
      finally zip.close()
    }

  private def entry(jar: Path, name: String): ZIO[Any, Throwable, Array[Byte]] =
    ZIO.attemptBlocking {
      val zip = new ZipFile(jar.toFile)
      try
        Option(zip.getEntry(name)) match
          case None       => Array.emptyByteArray
          case Some(item) =>
            val input = zip.getInputStream(item)
            try input.readAllBytes()
            finally input.close()
      finally zip.close()
    }
end DistributionSpec
