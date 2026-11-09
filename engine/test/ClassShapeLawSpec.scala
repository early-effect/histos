package histos.native

import histos.pack.ClassJar
import histos.workspace.Workspace

import java.nio.file.{Files, Path, StandardCopyOption}

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A class directory and a jar of it name the same shape. A missing path adds nothing. A broken class is refused. */
object ClassShapeLawSpec extends ZIOSpecDefault:
  def spec = suite("class shape laws")(
    test("Module from a directory matches Module from a jar, and a broken class is refused"):
      Workspace
        .work("histos-class-shapes")
        .flatMap: dir =>
          val source = repository
            .resolve(".histos")
            .resolve("classes")
            .resolve("api")
            .resolve("3.10.0")
            .resolve("jvm")
            .resolve("histos")
            .resolve("Module.class")
          val classes = dir.resolve("classes")
          val copied  = classes.resolve("histos").resolve("Module.class")
          val jar     = dir.resolve("shapes.jar")
          val note    = dir.resolve("note.txt")
          val broken  = dir.resolve("broken").resolve("Broken.class")
          val missing = dir.resolve("absent")
          val badJar  = dir.resolve("bad.jar")
          for
            there <- ZIO.attempt(Files.isRegularFile(source)).orDie
            _     <- ZIO
              .when(there)(ZIO.unit)
              .flatMap: _ =>
                if there then ZIO.unit else ZIO.dieMessage(s"could not read $source")
            _ <- (Workspace.createDirectories(parentOf(copied)) *> ZIO
              .attemptBlocking(
                Files.copy(source, copied, StandardCopyOption.REPLACE_EXISTING)
              )
              .unit).catchAll(error => ZIO.dieMessage(error.toString))
            _ <- Workspace.writeString(note, "note\n").catchAll(error => ZIO.dieMessage(error.message))
            _ <- (Workspace.createDirectories(parentOf(broken)) *> ZIO
              .attemptBlocking(
                Files.write(broken, Array[Byte](1, 2, 3))
              )
              .unit).catchAll(error => ZIO.dieMessage(error.toString))
            _        <- ZIO.attemptBlocking(Files.write(badJar, Array[Byte](1, 2, 3))).orDie
            packed   <- ClassJar.write(classes, jar)
            fromDir  <- ClassShapes.read(List(classes))
            fromJar  <- ClassShapes.read(List(jar))
            none     <- ClassShapes.read(List(missing, note))
            failed   <- ClassShapes.read(List(parentOf(broken))).either
            unopened <- ClassShapes.read(List(badJar)).either
          yield
            val named = fromDir.filter(_.name == "histos.Module")
            assertTrue(
              packed.entries == 1,
              named.map(shape => (shape.name, shape.module)) == List(("histos.Module", false)),
              fromJar.filter(_.name == "histos.Module").map(shape => (shape.name, shape.module, shape.parents)) ==
                named.map(shape => (shape.name, shape.module, shape.parents)),
              none == Nil,
              failed == Left(ClassShapeError.ClassFile(broken.toString)),
              unopened == Left(ClassShapeError.Read(badJar)),
            )
  )

  private def repository: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end ClassShapeLawSpec
