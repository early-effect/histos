package histos.program

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.{Scope, ZIO}
import zio.test.*

object EntryPointsSpec extends ZIOSpecDefault:
  def spec = suite("entry points")(
    test("a public static main is reported, and a class without one is skipped"):
      ZIO.scoped:
        temp.flatMap: root =>
          val demo = root.resolve("demo")
          ZIO.attempt {
            Files.createDirectories(demo)
            Files.write(demo.resolve("Check.class"), bytecode("demo/Check", main = true))
            Files.write(demo.resolve("Other.class"), bytecode("demo/Other", main = false))
          } *> EntryPoints.mains(root).map(names => assertTrue(names == List("demo.Check")))
  )

  private def bytecode(internal: String, main: Boolean): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, internal, null, "java/lang/Object", null)
    if main then
      val method = writer.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
      method.visitInsn(Opcodes.RETURN)
      method.visitMaxs(0, 1)
      method.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-mains"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore
