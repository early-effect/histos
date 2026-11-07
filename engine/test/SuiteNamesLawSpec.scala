package histos.testrun

import histos.native.{ClassShapeError, ClassShapes}
import histos.workspace.Workspace

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.ZIO
import zio.test.*

/** A suite is a class that reaches a fingerprint. An anonymous class, a subprocess class, and a class that does not
  * reach the mark are not.
  */
object SuiteNamesLawSpec extends ZIOSpecDefault:
  def spec = suite("suite names")(
    test("an empty directory has no suites, only sjsir is unseen, and a jar of sjsir is still no suites"):
      ZIO.scoped {
        Workspace.work("histos-suite-names-empty").flatMap { root =>
          val empty   = root.resolve("empty")
          val scripts = root.resolve("scripts")
          val packed  = root.resolve("scripts.jar")
          for
            _       <- ZIO.attemptBlocking(Files.createDirectories(empty)).orDie
            _       <- writeBytes(scripts.resolve("demo").resolve("App.sjsir"), Array[Byte](1))
            _       <- jar(packed, "demo/App.sjsir" -> Array[Byte](1))
            none    <- SuiteNames.discover(empty, Nil, "fw.Marker")
            unseen  <- SuiteNames.discover(scripts, Nil, "fw.Marker")
            archive <- SuiteNames.discover(packed, Nil, "fw.Marker")
            missing <- SuiteNames.discover(root.resolve("absent"), Nil, "fw.Marker")
          yield assertTrue(
            none == SuiteNames.Found.Suites(Nil),
            unseen == SuiteNames.Found.Unseen,
            archive == SuiteNames.Found.Suites(Nil),
            missing == SuiteNames.Found.Suites(Nil),
          )
        }
      }
    ,
    test("a class that reaches the fingerprint is a suite, and an anonymous or subprocess class is not"):
      ZIO.scoped {
        Workspace.work("histos-suite-names").flatMap { root =>
          val tests = root.resolve("tests")
          val lib   = root.resolve("lib.jar")
          for
            _ <- writeClass(tests.resolve("demo/HelloSpec$.class"), "demo/HelloSpec$", "demo/Mid")
            _ <- writeClass(tests.resolve("demo/Plain.class"), "demo/Plain", "demo/Parent")
            _ <- writeClass(tests.resolve("demo/HelloSpec$$anon.class"), "demo/HelloSpec$$anon", "demo/Parent")
            _ <- writeClass(tests.resolve("subprocess/Hidden.class"), "subprocess/Hidden", "demo/Parent")
            _ <- writeClass(tests.resolve("demo/Other.class"), "demo/Other", "java/lang/Object")
            _ <- writeClass(tests.resolve("Bad.class"), bytes = Array[Byte](1, 2, 3))
            entries = List(
              "demo/Parent.class" -> classBytes("demo/Parent", "java/lang/Object"),
              "demo/Mid.class"    -> classBytes("demo/Mid", "demo/Parent"),
              "fw/Marker.class"   -> classBytes("fw/Marker", "java/lang/Object"),
              "demo/Finger.class" -> fingerprint("demo/Finger", "demo.Parent", module = true),
              "demo/PlainMark.class" -> fingerprint("demo/PlainMark", "demo.Parent", module = false),
              "demo/Odd.class"    -> odd("demo/Odd"),
            )
            _       <- jar(lib, entries*)
            broken  <- SuiteNames.discover(tests, List(lib), "fw.Marker").either
            _       <- ZIO.attemptBlocking(Files.delete(tests.resolve("Bad.class"))).orDie
            found   <- SuiteNames.discover(tests, List(lib), "fw.Marker")
          yield assertTrue(
            broken match
              case Left(ClassShapeError.ClassFile(label)) => label.endsWith("Bad.class")
              case _                                       => false
            ,
            found == SuiteNames.Found.Suites(List("demo.HelloSpec", "demo.Plain")),
          )
        }
      }
    ,
    test("a cycle is not a reach, and dollars or a subprocess prefix are dropped before the mark"):
      val parent = ClassShapes.Shape("demo.Parent", false, Nil)
      val loop   = ClassShapes.Shape("demo.Loop", false, List("demo.Loop"))
      val anon   = ClassShapes.Shape("demo.A$$anon", false, List("demo.Parent"))
      val hidden = ClassShapes.Shape("subprocess.Hidden", false, List("demo.Parent"))
      val hit    = ClassShapes.Shape("demo.Hit", true, List("demo.Parent"))
      val world  = List(parent, loop, anon, hidden, hit).map(shape => shape.name -> shape).toMap
      val marks  = List(SuiteNames.Mark("demo.Parent", module = true), SuiteNames.Mark("demo.Parent", module = false))
      assertTrue(
        SuiteNames.choose(List(loop, anon, hidden, hit, parent), world, marks) == List("demo.Hit", "demo.Parent"),
        SuiteNames.choose(List(hit), world, List(SuiteNames.Mark("demo.Parent", module = false))) == Nil,
      )
  )

  private def writeClass(path: Path, name: String = "", superName: String = "", bytes: Array[Byte] = Array.empty) =
    val body = if bytes.nonEmpty then bytes else classBytes(name, superName)
    writeBytes(path, body)

  private def classBytes(name: String, superName: String): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, name, null, superName, null)
    writer.visitEnd()
    writer.toByteArray

  private def fingerprint(name: String, superclass: String, module: Boolean): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER,
      name,
      null,
      "java/lang/Object",
      Array("sbt/testing/SubclassFingerprint"),
    )
    val text = writer.visitMethod(Opcodes.ACC_PUBLIC, "superclassName", "()Ljava/lang/String;", null, null)
    text.visitCode()
    text.visitLdcInsn(superclass)
    text.visitInsn(Opcodes.ARETURN)
    text.visitMaxs(1, 1)
    text.visitEnd()
    val flag = writer.visitMethod(Opcodes.ACC_PUBLIC, "isModule", "()Z", null, null)
    flag.visitCode()
    flag.visitInsn(if module then Opcodes.ICONST_1 else Opcodes.ICONST_0)
    flag.visitInsn(Opcodes.IRETURN)
    flag.visitMaxs(1, 1)
    flag.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  /** A fingerprint whose superclass constant is not text, so the mark is dropped. */
  private def odd(name: String): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER,
      name,
      null,
      "java/lang/Object",
      Array("sbt/testing/SubclassFingerprint"),
    )
    val text = writer.visitMethod(Opcodes.ACC_PUBLIC, "superclassName", "()Ljava/lang/String;", null, null)
    text.visitCode()
    text.visitLdcInsn(Integer.valueOf(1))
    text.visitInsn(Opcodes.ACONST_NULL)
    text.visitInsn(Opcodes.ARETURN)
    text.visitMaxs(1, 1)
    text.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def writeBytes(path: Path, bytes: Array[Byte]) =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
      }
      .orDie

  private def jar(path: Path, entries: (String, Array[Byte])*) =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new ZipOutputStream(Files.newOutputStream(path))
        try
          entries.foreach { (name, bytes) =>
            out.putNextEntry(new ZipEntry(name))
            out.write(bytes)
            out.closeEntry()
          }
        finally out.close()
      }
      .orDie
end SuiteNamesLawSpec
