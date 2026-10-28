package histos.pack

import histos.jdk.Sha256
import histos.zinc.{CompileRequest, CompileToolchain, GraphCompile, GraphCompileError, JvmCompile, ScalacFlags}

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object JarSpec extends ZIOSpecDefault:
  private val keys = suite("package key")(
    test("a trailing comment keeps the package key, and a method body moves it"):
      val comment = Sha256.of(Chunk.fromArray("comment".getBytes))
      val body    = Sha256.of(Chunk.fromArray("body".getBytes))
      val kept    = PackageKey.of(List(comment))
      assertTrue(
        kept == PackageKey.of(List(comment)),
        kept != PackageKey.of(List(body)),
        kept != PackageKey.of(List(comment, body)),
      )
  )

  private val jar = suite("class jar")(
    test("class files are entries, and an empty directory is refused"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _ <- ZIO.attempt {
            Files.createDirectories(classes.resolve("demo"))
            Files.write(
              classes.resolve("demo").resolve("Hello.class"),
              Array[Byte](0xca.toByte, 0xfe.toByte, 0xba.toByte, 0xbe.toByte),
            )
            Files.write(
              classes.resolve("demo").resolve("Other.class"),
              Array[Byte](1, 2, 3),
            )
          }
          packed <- ClassJar.write(
            classes,
            root.resolve("out").resolve("lib.jar"),
          )
          names <- ZIO.attempt {
            val zip = new ZipFile(packed.path.toFile)
            try zip.entries.asScala.map(_.getName).toList.sorted
            finally zip.close()
          }
          _     <- ZIO.attempt(Files.createDirectories(root.resolve("empty")))
          empty <- ClassJar
            .write(root.resolve("empty"), root.resolve("no.jar"))
            .either
        yield assertTrue(
          packed.entries == 2,
          names == List("demo/Hello.class", "demo/Other.class"),
          empty == Left(JarError.Empty(root.resolve("empty"))),
        )
    ,
    test("a compiled class is the entry in the jar"):
      ZIO.scoped:
        for
          root   <- temp
          source <- ZIO.attempt {
            val path = root.resolve("Hello.scala")
            Files.writeString(
              path,
              "object Hello:\n  def greet: String = \"hi\"\n",
            )
            path
          }
          ask      <- request(root, source)
          compiled <- JvmCompile.compile(ask)
          packed   <- ClassJar.write(compiled.classes, root.resolve("hello.jar"))
          names    <- ZIO.attempt {
            val zip = new ZipFile(packed.path.toFile)
            try zip.entries.asScala.map(_.getName).toList
            finally zip.close()
          }
        yield assertTrue(
          compiled.classFiles >= 1,
          names.contains("Hello.class"),
          names.contains("Hello$.class"),
          names.contains("Hello.tasty"),
          packed.entries == names.length,
        ),
  )

  private def request(
      root: Path,
      source: Path,
  ): zio.IO[GraphCompileError, CompileRequest] =
    GraphCompile.jdkHere.map: jdk =>
      CompileRequest(
        CompileToolchain(
          histos.ScalaVersion("3.9.0"),
          histos.Platform.Jvm,
          jdk,
          ScalacFlags.empty,
        ),
        List(source),
        Nil,
        root.resolve("classes"),
        root.resolve("analysis"),
        Nil,
        Nil,
      )

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-jar"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths
            .sortBy(_.getNameCount)
            .reverse
            .foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = suite("package")(keys, jar).provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
    histos.ToolFixture.jvm(histos.ScalaVersion("3.9.0"))
  ) @@ TestAspect.withLiveSystem
end JarSpec
