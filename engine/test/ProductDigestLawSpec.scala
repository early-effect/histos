package histos.zinc

import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Line numbers and jar timestamps are not part of a product digest. Only the checkout root is removed from a path. */
object ProductDigestLawSpec extends ZIOSpecDefault:
  def spec = suite("product digest laws")(
    test("a foreign path stays, two jar timestamps are one digest, and bytes that are not a class are refused"):
      Workspace
        .work("histos-product-digest")
        .flatMap: dir =>
          val bytes     = classBytes
          val classes   = dir.resolve("classes")
          val classFile = classes.resolve("demo").resolve("Plug.class")
          val early     = dir.resolve("early.jar")
          val later     = dir.resolve("later.jar")
          val broken    = dir.resolve("broken.jar")
          for
            cleaned <- ZIO
              .fromEither(ProductDigest.normalize(bytes, dir))
              .catchAll(error => ZIO.dieMessage(error.message))
            again <- ZIO
              .fromEither(ProductDigest.normalize(cleaned, dir))
              .catchAll(error => ZIO.dieMessage(error.message))
            refused    <- ZIO.succeed(ProductDigest.normalize(Array[Byte](1, 2, 3), dir))
            _          <- write(classFile, bytes)
            _          <- jar(early, "demo/Plug.class", bytes, 0L)
            _          <- jar(later, "demo/Plug.class", bytes, 5_000L)
            _          <- write(broken, Array[Byte](1, 2, 3))
            fromDir    <- ZIO.succeed(ProductDigest.ofDirectory(classes, dir))
            fromEarly  <- ZIO.succeed(ProductDigest.ofJar(early, dir))
            fromLater  <- ZIO.succeed(ProductDigest.ofJar(later, dir))
            fromBroken <- ZIO.succeed(ProductDigest.ofJar(broken, dir))
            missing    <- ZIO.succeed(ProductDigest.ofDirectory(dir.resolve("absent"), dir))
            written    <- ZIO.succeed(ProductDigest.write(dir.resolve("out"), classes, dir))
            read       <- ZIO.succeed(ProductDigest.read(dir.resolve("out")))
            text = new String(cleaned, StandardCharsets.ISO_8859_1)
          yield assertTrue(
            again.sameElements(cleaned),
            text.contains("/tmp/proj/Hello.scala"),
            refused == Left(ClassBytesError.Unreadable),
            ClassBytesError.Unreadable.message == "class bytes are not a class file",
            fromDir == fromEarly,
            fromEarly == fromLater,
            fromDir.exists(_.value.nonEmpty),
            fromBroken == Left(CompileError.Io(IoFailure(broken, IoFault.Read))),
            missing == Left(CompileError.Io(IoFailure(dir.resolve("absent"), IoFault.Stat))),
            written == read,
            written.exists(digest => fromDir.contains(digest)),
          )
    ,
    test("two digests that differ only in a rooted string constant differ"):
      Workspace
        .work("histos-product-path")
        .flatMap: dir =>
          val left     = dir.resolve("v1")
          val right    = dir.resolve("v2")
          val here     = dir.resolve("here")
          val there    = dir.resolve("there")
          val hereAbs  = here.toAbsolutePath.normalize.toString
          val thereAbs = there.toAbsolutePath.normalize.toString
          val url      = "https://example.com/v1/users"
          for
            _      <- write(left.resolve("demo").resolve("Plug.class"), constantClass("/api/v1/users"))
            _      <- write(right.resolve("demo").resolve("Plug.class"), constantClass("/api/v2/users"))
            _      <- write(here.resolve("demo").resolve("Plug.class"), constantClass(s"$hereAbs/src/Hello.scala"))
            _      <- write(there.resolve("demo").resolve("Plug.class"), constantClass(s"$thereAbs/src/Hello.scala"))
            v1     <- ZIO.succeed(ProductDigest.ofDirectory(left, dir))
            v2     <- ZIO.succeed(ProductDigest.ofDirectory(right, dir))
            rooted <- ZIO.succeed(
              ProductDigest.ofDirectory(left, Option(left.toAbsolutePath.getRoot).getOrElse(left))
            )
            fromHere  <- ZIO.succeed(ProductDigest.ofDirectory(here, here))
            fromThere <- ZIO.succeed(ProductDigest.ofDirectory(there, there))
            urlBytes  <- ZIO
              .fromEither(ProductDigest.normalize(constantClass(url), dir))
              .catchAll(error => ZIO.dieMessage(error.message))
            kept <- ZIO
              .fromEither(ProductDigest.positionFree(constantClass("/api/v1/users")))
              .catchAll(error => ZIO.dieMessage(error.message))
          yield assertTrue(
            v1 != v2,
            v1.exists(_.value.nonEmpty),
            v2.exists(_.value.nonEmpty),
            rooted == v1,
            fromHere == fromThere,
            fromHere.exists(_.value.nonEmpty),
            new String(urlBytes, StandardCharsets.ISO_8859_1).contains(url),
            new String(kept, StandardCharsets.ISO_8859_1).contains("/api/v1/users"),
          ),
  )

  private def classBytes: Array[Byte] =
    constantClass("/tmp/proj/Hello.scala")

  private def constantClass(path: String): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "demo/Plug", null, "java/lang/Object", null)
    val note = writer.visitAnnotation("Ldemo/Note;", true)
    note.visit("path", path)
    note.visit("word", "héllo")
    note.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def write(path: Path, bytes: Array[Byte]): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.write(path, bytes)
      ()
    }.orDie

  private def jar(path: Path, name: String, bytes: Array[Byte], time: Long): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      val out = new java.util.zip.ZipOutputStream(Files.newOutputStream(path))
      try
        val entry = new java.util.zip.ZipEntry(name)
        entry.setTime(time)
        out.putNextEntry(entry)
        out.write(bytes)
        out.closeEntry()
      finally out.close()
    }.orDie
end ProductDigestLawSpec
