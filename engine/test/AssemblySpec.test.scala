package histos.pack

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}
import zio.test.*

object AssemblySpec extends ZIOSpecDefault:
  private val main = MainClass.trusted("demo.Entry")
  private val utf8 = StandardCharsets.UTF_8

  private val merged = suite("assembly")(
    test("application classes win and service files are concatenated"):
      ZIO.scoped:
        for
          root <- temp
          classes <- ZIO.attempt(writeClasses(
            root,
            List(
              "demo/A.class" -> Array[Byte](1),
              "demo/Entry.class" -> Array[Byte](7),
              "META-INF/services/demo.Spi" -> "zero\n".getBytes(utf8),
            ),
          ))
          first = root.resolve("first.jar")
          second = root.resolve("second.jar")
          _ <- ZIO.attempt {
                 jar(
                   first,
                   List(
                     "demo/A.class" -> Array[Byte](2),
                     "demo/B.class" -> Array[Byte](3),
                     "META-INF/A.SF" -> Array[Byte](9),
                     "META-INF/A.RSA" -> Array[Byte](9),
                     "META-INF/services/demo.Spi" -> "one\r\ntwo\n".getBytes(utf8),
                   ),
                 )
                 jar(
                   second,
                   List(
                     "demo/B.class" -> Array[Byte](4),
                     "demo/C.class" -> Array[Byte](5),
                     "module-info.class" -> Array[Byte](8),
                     "META-INF/versions/9/module-info.class" -> Array[Byte](8),
                     "META-INF/versions/21/demo/N.class" -> Array[Byte](6),
                     "META-INF/INDEX.LIST" -> "Index".getBytes(utf8),
                     "META-INF/services/demo.Spi" -> "one\nthree\n".getBytes(utf8),
                   ),
                 )
               }
          packed <- Assembly.write(classes, List(first, second), main, root.resolve("out").resolve("app.jar"))
          names <- ZIO.attempt(entries(packed.path))
          body <- ZIO.attempt(texts(packed.path))
        yield assertTrue(
          body.get("demo/A.class").contains(List(1)),
          body.get("demo/B.class").contains(List(3)),
          body.get("demo/C.class").contains(List(5)),
          body.get("demo/Entry.class").contains(List(7)),
          body.get("META-INF/versions/21/demo/N.class").contains(List(6)),
          text(body, "META-INF/services/demo.Spi").contains("zero\none\ntwo\nthree\n"),
          text(body, "META-INF/MANIFEST.MF").exists(manifest =>
            manifest.contains("Main-Class: demo.Entry") &&
              manifest.contains("Enable-Native-Access: ALL-UNNAMED") &&
              manifest.contains("Multi-Release: true"),
          ),
          !names.contains("module-info.class"),
          !names.contains("META-INF/versions/9/module-info.class"),
          !names.contains("META-INF/A.SF"),
          !names.contains("META-INF/A.RSA"),
          !names.contains("META-INF/INDEX.LIST"),
          packed.entries == names.size,
        )
    ,
    test("a missing main is refused"):
      ZIO.scoped:
        for
          root <- temp
          classes <- ZIO.attempt(writeClasses(root, List("demo/A.class" -> Array[Byte](1))))
          missing <- Assembly.write(classes, Nil, main, root.resolve("app.jar")).either
        yield assertTrue(missing == Left(AssemblyError.MissingMain(main)))
    ,
    test("an empty classes directory is refused"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(Files.createDirectories(root.resolve("empty")))
          empty <- Assembly.write(root.resolve("empty"), Nil, main, root.resolve("app.jar")).either
        yield assertTrue(empty == Left(AssemblyError.Empty(root.resolve("empty"))))
    ,
    test("a file is not a classes directory"):
      ZIO.scoped:
        for
          root <- temp
          file = root.resolve("classes")
          _ <- ZIO.attempt(Files.write(file, Array[Byte](1)))
          refused <- Assembly.write(file, Nil, main, root.resolve("app.jar")).either
        yield assertTrue(refused == Left(AssemblyError.NotADirectory(file)))
    ,
    test("a file that is not a jar is refused"):
      ZIO.scoped:
        for
          root <- temp
          classes <- ZIO.attempt(writeClasses(root, List("demo/Entry.class" -> Array[Byte](1))))
          notes = root.resolve("notes.txt")
          _ <- ZIO.attempt(Files.writeString(notes, "not a jar"))
          refused <- Assembly.write(classes, List(notes), main, root.resolve("app.jar")).either
        yield assertTrue(refused == Left(AssemblyError.NotAJar(notes)))
    ,
    test("a jar entry that escapes is refused"):
      ZIO.scoped:
        for
          root <- temp
          classes <- ZIO.attempt(writeClasses(root, List("demo/Entry.class" -> Array[Byte](1))))
          hostile = root.resolve("hostile.jar")
          _ <- ZIO.attempt(jar(hostile, List("../Evil.class" -> Array[Byte](1))))
          refused <- Assembly.write(classes, List(hostile), main, root.resolve("app.jar")).either
        yield assertTrue(refused == Left(AssemblyError.Escaped("../Evil.class")))
    ,
    test("the destination is not inside the classes directory"):
      ZIO.scoped:
        for
          root <- temp
          classes <- ZIO.attempt(writeClasses(root, List("demo/Entry.class" -> Array[Byte](1))))
          destination = classes.resolve("app.jar")
          refused <- Assembly.write(classes, Nil, main, destination).either
        yield assertTrue(refused == Left(AssemblyError.DestinationInside(destination)))
    ,
    test("the destination is not one of the jars"):
      ZIO.scoped:
        for
          root <- temp
          classes <- ZIO.attempt(writeClasses(root, List("demo/Entry.class" -> Array[Byte](1))))
          destination = root.resolve("app.jar")
          _ <- ZIO.attempt(jar(destination, List("demo/Other.class" -> Array[Byte](2))))
          refused <- Assembly.write(classes, List(destination), main, destination).either
        yield assertTrue(refused == Left(AssemblyError.Overlap(destination)))
    ,
  )

  private def writeClasses(root: Path, files: List[(String, Array[Byte])]): Path =
    val dir = root.resolve("classes")
    files.foreach: (name, body) =>
      val path = dir.resolve(name)
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.write(path, body)
    dir

  private def jar(path: Path, files: List[(String, Array[Byte])]): Unit =
    Option(path.getParent).foreach(parent => Files.createDirectories(parent))
    val out = new ZipOutputStream(Files.newOutputStream(path))
    try
      files.foreach: (name, body) =>
        out.putNextEntry(new ZipEntry(name))
        out.write(body)
        out.closeEntry()
    finally out.close()

  private def entries(path: Path): Set[String] =
    val zip = new ZipFile(path.toFile)
    try zip.entries.asScala.map(_.getName).toSet
    finally zip.close()

  private def texts(path: Path): Map[String, List[Int]] =
    val zip = new ZipFile(path.toFile)
    try
      zip.entries.asScala.map { entry =>
        val in = zip.getInputStream(entry)
        val bytes =
          try in.readAllBytes().toList.map(_ & 0xff)
          finally in.close()
        entry.getName -> bytes
      }.toMap
    finally zip.close()

  private def text(body: Map[String, List[Int]], name: String): Option[String] =
    body.get(name).map: bytes =>
      val raw = new Array[Byte](bytes.length)
      bytes.zipWithIndex.foreach: (value, index) =>
        raw(index) = value.toByte
      new String(raw, utf8)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-assembly"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = merged
