package histos.native

import histos.pack.ClassJar
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A class directory and a jar of it name the same shape. A missing path adds nothing. A broken class is refused. */
object ClassShapeLawSpec extends ZIOSpecDefault:
  def spec = suite("class shape laws")(
    test("shapes from a directory match shapes from its jar, and a broken class is refused"):
      Workspace
        .work("histos-class-shapes")
        .flatMap: dir =>
          val classes = dir.resolve("classes")
          val jar     = dir.resolve("shapes.jar")
          val note    = dir.resolve("note.txt")
          val broken  = dir.resolve("broken").resolve("Broken.class")
          val missing = dir.resolve("absent")
          val badJar  = dir.resolve("bad.jar")
          val shapes  = List(
            ClassShapes.Shape("law.Suite", false, List("law.Base", "java.io.Serializable")),
            ClassShapes.Shape("law.Suite$", true, Nil),
          )
          for
            _        <- emit(classes, "law/Suite", "law/Base", List("java/io/Serializable"))
            _        <- emit(classes, "law/Suite$", "java/lang/Object", Nil)
            _        <- emit(classes, "law/Suite$$anon$1", "java/lang/Object", Nil)
            _        <- Workspace.writeString(note, "note\n")
            _        <- Workspace.createDirectories(dir.resolve("broken"))
            _        <- ZIO.attemptBlocking(Files.write(broken, Array[Byte](1, 2, 3)))
            _        <- ZIO.attemptBlocking(Files.write(badJar, Array[Byte](1, 2, 3)))
            packed   <- ClassJar.write(classes, jar)
            fromDir  <- ClassShapes.read(List(classes))
            fromJar  <- ClassShapes.read(List(jar))
            none     <- ClassShapes.read(List(missing, note))
            failed   <- ClassShapes.read(List(dir.resolve("broken"))).either
            unopened <- ClassShapes.read(List(badJar)).either
          yield assertTrue(
            packed.entries == 3,
            fromDir == shapes,
            fromJar == shapes,
            none == Nil,
            failed == Left(ClassShapeError.ClassFile(broken.toString)),
            unopened == Left(ClassShapeError.Read(badJar)),
          )
  )

  /** One empty public class named `name`, with that superclass and those interfaces. */
  private def emit(classes: Path, name: String, parent: String, interfaces: List[String]) =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, name, null, parent, interfaces.toArray)
    writer.visitEnd()
    val file = classes.resolve(s"$name.class")
    Workspace.createDirectories(Option(file.getParent).getOrElse(classes)) *>
      ZIO.attemptBlocking(Files.write(file, writer.toByteArray)).unit
end ClassShapeLawSpec
