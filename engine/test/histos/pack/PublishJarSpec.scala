package histos.pack

import histos.workspace.Workspace

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.ZIO
import zio.test.*

object PublishJarSpec extends ZIOSpecDefault:
  def spec = suite("a published jar")(
    test("the javadoc jar is one page") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-javadoc-stub").orDieWith(error => new java.io.IOException(error.message))
          jar = root.resolve("stub-javadoc.jar")
          written <- JavadocStub.write(jar).orDieWith(error => new java.io.IOException(error.message))
          size    <- ZIO.attemptBlocking(Files.size(jar)).orDie
          names   <- entries(jar)
          page    <- entry(jar, JavadocStub.Entry)
        yield assertTrue(
          written.entries == 1,
          size < 4096,
          names == List(JavadocStub.Entry),
          new String(page, StandardCharsets.UTF_8).contains("Sources are in the sources jar."),
          !names.exists(_.contains("package-list")),
        )
      }
    },
    test("semanticdb stays out of the class jar") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-class-jar").orDieWith(error => new java.io.IOException(error.message))
          classes = root.resolve("classes")
          _ <- ZIO.attemptBlocking {
            Files.createDirectories(classes.resolve("histos/cli"))
            Files.createDirectories(classes.resolve("META-INF/semanticdb"))
            Files.write(classes.resolve("histos/cli/Main.class"), Array[Byte](1, 2, 3))
            Files.write(classes.resolve("META-INF/semanticdb/Main.scala.semanticdb"), Array[Byte](4))
          }.orDie
          jar = root.resolve("classes.jar")
          _ <- ClassJar
            .write(classes, Nil, jar, drop = ClassJar.semanticdb)
            .orDieWith(error => new java.io.IOException(error.message))
          names <- entries(jar)
        yield assertTrue(
          names.contains("histos/cli/Main.class"),
          !names.exists(ClassJar.semanticdb),
          !names.exists(_.startsWith("bootstrap/")),
        )
      }
    },
  )

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
end PublishJarSpec
