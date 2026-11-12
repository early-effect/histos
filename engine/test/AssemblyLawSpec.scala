package histos.pack

import histos.jdk.Toolchain
import histos.plugin.ApiDigest

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import org.objectweb.asm.{ClassWriter, Opcodes}
import org.objectweb.asm.tree.ClassNode

import zio.ZIO
import zio.test.*

object AssemblyLawSpec extends ZIOSpecDefault:
  private val mainName = "demo.Main"
  private val entry    = "demo/Main.class"

  def spec = suite("assembly laws")(
    test("the manifest and the main class are the ones given, and a signature is left out"):
      MainClass.from(mainName) match
        case Left(_)     => ZIO.succeed(assertTrue(false))
        case Right(main) =>
          ZIO.acquireRelease(directory("histos-assembly"))(delete).flatMap { root =>
            val classes = root.resolve("classes")
            val jar     = root.resolve("lib.jar")
            val dest    = root.resolve("out.jar")
            val note    = root.resolve("note.txt")
            for
              _ <- ZIO.attemptBlocking {
                writeClass(classes, "demo/Main", "own")
                Files.writeString(note, "hi")
                dependency(jar)
              }.orDie
              assembled <- Assembly.write(List(classes), List(jar), main, dest, List(AssemblyFile("note.txt", note)))
              manifest  <- zipBytes(dest, "META-INF/MANIFEST.MF")
              mainClass <- zipBytes(dest, entry)
              names     <- zipNames(dest)
              service   <- zipBytes(dest, "META-INF/services/demo.Service")
              resource  <- zipBytes(dest, "note.txt")
            yield assertTrue(
              assembled.path == dest,
              manifest.exists(bytes => new String(bytes, StandardCharsets.UTF_8) == manifestOf(main)),
              mainClass.exists(bytes => marks(bytes).contains("own") && !marks(bytes).contains("other")),
              !names.contains("META-INF/DEMO.SF"),
              !names.contains("module-info.class"),
              service.exists(bytes => new String(bytes, StandardCharsets.UTF_8).contains("demo.Impl")),
              resource.exists(bytes => new String(bytes, StandardCharsets.UTF_8) == "hi"),
            )
          }
    ,
    test("a missing main, an empty classes directory, and a destination inside it are typed errors"):
      MainClass.from(mainName) match
        case Left(_)     => ZIO.succeed(assertTrue(false))
        case Right(main) =>
          ZIO.acquireRelease(directory("histos-assembly-miss"))(delete).flatMap { root =>
            val classes = root.resolve("classes")
            val empty   = root.resolve("empty")
            val outside = root.resolve("out.jar")
            val inside  = classes.resolve("out.jar")
            for
              _ <- ZIO.attemptBlocking {
                Files.createDirectories(empty)
                writeClass(classes, "demo/Other", "own")
              }.orDie
              missing <- Assembly.write(List(classes), Nil, main, outside).either
              none    <- Assembly.write(Nil, Nil, main, outside).either
              vacant  <- Assembly.write(List(empty), Nil, main, outside).either
              nested  <- Assembly.write(List(classes), Nil, main, inside).either
            yield assertTrue(
              missing == Left(AssemblyError.MissingMain(main)),
              none == Left(AssemblyError.NoClasses),
              vacant == Left(AssemblyError.Empty(empty)),
              nested == Left(AssemblyError.DestinationInside(inside)),
            )
          }
    ,
    test("an escaped resource is refused, and a resource that repeats a class is a duplicate"):
      MainClass.from(mainName) match
        case Left(_)     => ZIO.succeed(assertTrue(false))
        case Right(main) =>
          ZIO.acquireRelease(directory("histos-assembly-resource"))(delete).flatMap { root =>
            val classes = root.resolve("classes")
            val dest    = root.resolve("out.jar")
            val note    = root.resolve("note.txt")
            for
              _ <- ZIO.attemptBlocking {
                writeClass(classes, "demo/Main", "own")
                Files.writeString(note, "hi")
              }.orDie
              escaped <- Assembly.write(List(classes), Nil, main, dest, List(AssemblyFile("..", note))).either
              again   <- Assembly.write(List(classes), Nil, main, dest, List(AssemblyFile(entry, note))).either
              absent  <- Assembly
                .write(List(classes), Nil, main, dest, List(AssemblyFile("note.txt", root.resolve("gone"))))
                .either
            yield assertTrue(
              escaped == Left(AssemblyError.Escaped("..")),
              again == Left(AssemblyError.Duplicate(entry)),
              absent == Left(AssemblyError.NotAFile(root.resolve("gone"))),
            )
          }
    ,
    test("replacing drops the class-directory file and stores the replacement"):
      MainClass.from(mainName) match
        case Left(_)     => ZIO.succeed(assertTrue(false))
        case Right(main) =>
          ZIO.acquireRelease(directory("histos-assembly-replace"))(delete).flatMap { root =>
            val classes = root.resolve("classes")
            val dest    = root.resolve("out.jar")
            val stamp   = root.resolve("api.version")
            val version = classes.resolve(ApiDigest.resource)
            for
              _ <- ZIO.attemptBlocking {
                writeClass(classes, "demo/Main", "own")
                Option(version.getParent).foreach(Files.createDirectories(_))
                Files.writeString(version, "planted\n")
                Files.writeString(stamp, "host\n")
              }.orDie
              _ <- Assembly
                .write(List(classes), Nil, main, dest, replacing = List(AssemblyFile(ApiDigest.resource, stamp)))
              found    <- zipBytes(dest, ApiDigest.resource)
              manifest <- zipBytes(dest, "META-INF/MANIFEST.MF")
              stored = found.map(bytes => new String(bytes, StandardCharsets.UTF_8)).getOrElse("")
              header = manifest.map(bytes => new String(bytes, StandardCharsets.UTF_8)).getOrElse("")
            yield assertTrue(
              stored == "host\n",
              header == manifestOf(main),
            )
          },
  )

  private def manifestOf(main: MainClass): String =
    "Manifest-Version: 1.0\r\n" +
      s"Main-Class: ${main.binary}\r\n" +
      s"${Toolchain.NativeAccessManifest}\r\n" +
      "Multi-Release: true\r\n\r\n"

  private def writeClass(dir: Path, internal: String, mark: String): Unit =
    val path = dir.resolve(internal + ".class")
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, classBytes(internal, mark))

  private def classBytes(internal: String, mark: String): Array[Byte] =
    val writer = new ClassWriter(0)
    emit(writer, internal, mark)
    writer.toByteArray

  private def emit(writer: ClassWriter, internal: String, mark: String): Unit =
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, internal, null, "java/lang/Object", null)
    val field = writer.visitField(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL,
      "mark",
      "Ljava/lang/String;",
      null,
      mark,
    )
    field.visitEnd()
    val init = writer.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
    init.visitCode()
    init.visitVarInsn(Opcodes.ALOAD, 0)
    init.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    init.visitInsn(Opcodes.RETURN)
    init.visitMaxs(1, 1)
    init.visitEnd()
    writer.visitEnd()

  private def marks(bytes: Array[Byte]): List[String] =
    val node = new ClassNode()
    new org.objectweb.asm.ClassReader(bytes).accept(node, 0)
    node.fields.asScala.toList.flatMap(field => Option(field.value).collect { case text: String => text })

  private def dependency(path: Path): Unit =
    Option(path.getParent).foreach(Files.createDirectories(_))
    val stream = new ZipOutputStream(Files.newOutputStream(path))
    try
      put(stream, entry, classBytes("demo/Main", "other"))
      put(stream, "META-INF/DEMO.SF", Array[Byte](1))
      put(stream, "module-info.class", Array[Byte](2))
      put(stream, "META-INF/services/demo.Service", "demo.Impl\n".getBytes(StandardCharsets.UTF_8))
    finally stream.close()

  private def put(stream: ZipOutputStream, name: String, bytes: Array[Byte]): Unit =
    stream.putNextEntry(new ZipEntry(name))
    stream.write(bytes)
    stream.closeEntry()

  private def zipBytes(path: Path, name: String): ZIO[Any, Nothing, Option[Array[Byte]]] =
    ZIO.attemptBlocking {
      val zip = new ZipFile(path.toFile)
      try
        Option(zip.getEntry(name)).map { item =>
          val in = zip.getInputStream(item)
          try in.readAllBytes
          finally in.close()
        }
      finally zip.close()
    }.orDie

  private def zipNames(path: Path): ZIO[Any, Nothing, Set[String]] =
    ZIO.attemptBlocking {
      val zip = new ZipFile(path.toFile)
      try zip.entries.asScala.map(_.getName).toSet
      finally zip.close()
    }.orDie

  private def directory(prefix: String): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking(Files.createTempDirectory(prefix)).orDie

  private def delete(root: Path): zio.UIO[Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.orDie
end AssemblyLawSpec
