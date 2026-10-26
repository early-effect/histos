package histos.native

import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

import org.objectweb.asm.{ClassWriter, Opcodes}

import histos.workspace.Workspace

import zio.{Scope, ZIO}
import zio.test.*

object ClassShapesSpec extends ZIOSpecDefault:
  def spec = suite("class shapes")(
    test("a directory of classes returns both shapes sorted by name"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _     <- write(classes, "demo/Zebra", "demo/Animal", List("demo/Named"))
          _     <- write(classes, "demo/Alpha", "demo/Base", Nil)
          found <- ClassShapes.read(List(classes)).either
        yield assertTrue(
          found == Right(
            List(
              ClassShapes.Shape("demo.Alpha", false, List("demo.Base")),
              ClassShapes.Shape("demo.Zebra", false, List("demo.Animal", "demo.Named")),
            )
          )
        )
    ,
    test("a short class file is a class file error"):
      ZIO.scoped:
        for
          root <- temp
          classes   = root.resolve("classes")
          directory = classes.resolve("demo")
          short     = directory.resolve("Short.class")
          _ <- ZIO.attempt:
            Files.createDirectories(directory)
            Files.write(short, Array[Byte](1, 2, 3, 4))
            ()
          found <- ClassShapes.read(List(classes)).either
        yield assertTrue(found == Left(ClassShapeError.ClassFile(short.toString)))
    ,
    test("a missing directory is empty"):
      ZIO.scoped:
        for
          root <- temp
          missing = root.resolve("absent")
          found <- ClassShapes.read(List(missing)).either
        yield assertTrue(found == Right(Nil))
    ,
    test("a jar returns the class shape"):
      ZIO.scoped:
        for
          root <- temp
          jar = root.resolve("one.jar")
          _     <- writeJar(jar, "demo/Only", "demo/Parent")
          found <- ClassShapes.read(List(jar)).either
        yield assertTrue(
          found == Right(List(ClassShapes.Shape("demo.Only", false, List("demo.Parent"))))
        ),
  )

  private def write(
      root: Path,
      internal: String,
      parent: String,
      interfaces: List[String],
  ): ZIO[Any, Throwable, Unit] =
    val relative  = internal + ".class"
    val file      = relative.split("/").foldLeft(root)(_.resolve(_))
    val directory = relative.split("/").toList.dropRight(1).foldLeft(root)(_.resolve(_))
    ZIO.attempt:
      Files.createDirectories(directory)
      Files.write(file, classBytes(internal, parent, interfaces))
      ()

  private def writeJar(path: Path, internal: String, parent: String): ZIO[Any, Throwable, Unit] =
    ZIO.scoped:
      for
        output <- ZIO.acquireRelease(
          ZIO.attempt(new JarOutputStream(Files.newOutputStream(path)))
        )(opened => ZIO.attempt(opened.close()).ignore)
        _ <- ZIO.attempt:
          output.putNextEntry(new JarEntry(internal + ".class"))
          output.write(classBytes(internal, parent, Nil))
          output.closeEntry()
      yield ()

  private def classBytes(internal: String, parent: String, interfaces: List[String]): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER,
      internal,
      null,
      parent,
      interfaces.toArray[String],
    )
    writer.visitEnd()
    writer.toByteArray

  private def temp: ZIO[Scope, Throwable, Path] =
    Workspace.work("histos-shapes").mapError(failure => new java.io.IOException(failure.message))
end ClassShapesSpec
