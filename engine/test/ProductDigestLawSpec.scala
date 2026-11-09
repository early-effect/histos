package histos.zinc

import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Line numbers, jar timestamps, and absolute paths are not part of a product digest. */
object ProductDigestLawSpec extends ZIOSpecDefault:
  def spec = suite("product digest laws")(
    test("a path in a class is a leaf, two jar timestamps are one digest, and bytes that are not a class are refused"):
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
            cleaned <- ZIO.fromEither(ProductDigest.normalize(bytes)).catchAll(error => ZIO.dieMessage(error.message))
            again   <- ZIO.fromEither(ProductDigest.normalize(cleaned)).catchAll(error => ZIO.dieMessage(error.message))
            refused <- ZIO.succeed(ProductDigest.normalize(Array[Byte](1, 2, 3)))
            _       <- write(classFile, bytes)
            _       <- jar(early, "demo/Plug.class", bytes, 0L)
            _       <- jar(later, "demo/Plug.class", bytes, 5_000L)
            _       <- write(broken, Array[Byte](1, 2, 3))
            fromDir <- ZIO.succeed(ProductDigest.ofDirectory(classes))
            fromEarly  <- ZIO.succeed(ProductDigest.ofJar(early))
            fromLater  <- ZIO.succeed(ProductDigest.ofJar(later))
            fromBroken <- ZIO.succeed(ProductDigest.ofJar(broken))
            missing    <- ZIO.succeed(ProductDigest.ofDirectory(dir.resolve("absent")))
            written    <- ZIO.succeed(ProductDigest.write(dir.resolve("out"), classes))
            read       <- ZIO.succeed(ProductDigest.read(dir.resolve("out")))
            text = new String(cleaned, StandardCharsets.ISO_8859_1)
          yield assertTrue(
            again.sameElements(cleaned),
            !text.contains("/tmp/proj/"),
            text.contains("Hello.scala"),
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
  )

  private def classBytes: Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "demo/Plug", null, "java/lang/Object", null)
    val note = writer.visitAnnotation("Ldemo/Note;", true)
    note.visit("path", "/tmp/proj/Hello.scala")
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
