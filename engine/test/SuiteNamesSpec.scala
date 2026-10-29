package histos.testrun

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import org.objectweb.asm.{ClassWriter, Opcodes}

import histos.native.ClassShapeError
import histos.workspace.Workspace

import zio.{Scope, ZIO}
import zio.test.*

object SuiteNamesSpec extends histos.SuiteClock:
  private val framework = "zio.test.sbt.ZTestFramework"

  def spec = suite("suite names")(
    test("an unreadable class file fails and the other suite is not returned"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          libs    = root.resolve("libs")
          _ <- write(classes, "demo/Keep$", "zio/test/ZIOSpecAbstract")
          _ <- write(classes, "demo/Drop$", "demo/Hidden")
          hidden = libs.resolve("demo/Hidden.class")
          _     <- write(libs, "demo/Hidden", "java/lang/Object")
          _     <- deny(hidden)
          found <- SuiteNames.discover(classes, List(libs), framework).either
        yield assertTrue(found == Left(ClassShapeError.Read(hidden)))
    ,
    test("a directory that cannot be walked is a read error"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          libs    = root.resolve("libs")
          _ <- write(classes, "demo/Keep$", "zio/test/ZIOSpecAbstract")
          _ <- write(libs, "zio/test/sbt/ZTestFramework", "java/lang/Object")
          blocked = libs.resolve("blocked")
          _     <- ZIO.attempt(Files.createDirectories(blocked))
          _     <- deny(blocked)
          found <- SuiteNames.discover(classes, List(libs), framework).either
        yield assertTrue(found == Left(ClassShapeError.Read(libs)))
    ,
    test("a missing class on the classpath is a miss and discovery succeeds"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          libs    = root.resolve("libs")
          _     <- write(classes, "demo/Keep$", "zio/test/ZIOSpecAbstract")
          _     <- write(classes, "demo/Drop$", "demo/Absent")
          _     <- write(libs, "other/Note", "java/lang/Object")
          found <- SuiteNames.discover(classes, List(libs), framework).either
        yield assertTrue(found == Right(SuiteNames.Found.Suites(List("demo.Keep"))))
    ,
    test("a short class file is a class file error"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          libs    = root.resolve("libs")
          _ <- write(classes, "demo/Keep$", "zio/test/ZIOSpecAbstract")
          _ <- write(classes, "demo/Drop$", "demo/Hidden")
          hidden = libs.resolve("demo/Hidden.class")
          _ <- ZIO.attempt:
            Files.createDirectories(hidden.getParent)
            Files.write(hidden, Array[Byte](1, 2, 3, 4))
          found <- SuiteNames.discover(classes, List(libs), framework).either
        yield assertTrue(found == Left(ClassShapeError.ClassFile(hidden.toString)))
    ,
    test("a jar that cannot be opened is a read error"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          jar     = root.resolve("broken.jar")
          _     <- write(classes, "demo/Keep$", "zio/test/ZIOSpecAbstract")
          _     <- ZIO.attempt(Files.write(jar, Array[Byte](1, 2, 3, 4)))
          found <- SuiteNames.discover(classes, List(jar), framework).either
        yield assertTrue(found == Left(ClassShapeError.Read(jar))),
  )

  private def write(root: Path, internal: String, parent: String): ZIO[Any, Throwable, Unit] =
    ZIO.attempt:
      val path = root.resolve(internal + ".class")
      Files.createDirectories(path.getParent)
      Files.write(path, classBytes(internal, parent))
      ()

  private def classBytes(internal: String, parent: String): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, internal, null, parent, null)
    writer.visitEnd()
    writer.toByteArray

  private def deny(path: Path): ZIO[Scope, Throwable, Unit] =
    ZIO
      .acquireRelease(
        ZIO.attempt(Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("---------")))
      )(_ =>
        ZIO
          .attempt(Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwx------")))
          .ignore
      )
      .unit

  private def temp: ZIO[Scope, Throwable, Path] =
    Workspace.work("histos-suites").mapError(failure => new java.io.IOException(failure.message))
end SuiteNamesSpec
