package histos.doctor

import histos.*
import histos.version.{BumpPart, ShipNumber}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

import org.objectweb.asm.{ClassWriter, Opcodes}
import zio.{IO, ZIO, ZLayer}
import zio.test.*

object CompatibilitySpec extends ZIOSpecDefault:
  private val previous = ShipNumber.trusted(0, 15, 0)

  def spec = suite("compatibility evidence")(
    test("baseline extraction rejects escaping classfile paths") {
      ZIO.scoped:
        for
          root   <- Workspace.work("escape")
          old    <- products(root.resolve("old"), method = true)
          jar    <- baselineJar(root.resolve("escape.jar"), old, "../Escape.class")
          now    <- products(root.resolve("now"), method = true)
          result <- ZIO
            .serviceWithZIO[CompatibilityAnalyzer](_.analyze(CompatibilityInput(Platform.Jvm, jar, now)).either)
            .provideLayer(CompatibilityAnalyzer.jvm)
        yield assertTrue(result == Left(CompatibilityError.InvalidBaselineEntry(jar, "../Escape.class")))
    },
    test("TASTy entries obey the same traversal and embedded-NUL jail") {
      ZIO.scoped:
        for
          root    <- Workspace.work("tasty-jail")
          old     <- products(root.resolve("old"), method = true)
          now     <- products(root.resolve("now"), method = true)
          results <- ZIO.foreach(List("../Escape.tasty", "fixture/Invalid\u0000.tasty").zipWithIndex) {
            (entry, index) =>
              for
                jar    <- baselineJar(root.resolve(s"malignant-$index.jar"), old, entry)
                result <- ZIO
                  .serviceWithZIO[CompatibilityAnalyzer](_.analyze(CompatibilityInput(Platform.Jvm, jar, now)).either)
                  .provideLayer(CompatibilityAnalyzer.jvm)
              yield result == Left(CompatibilityError.InvalidBaselineEntry(jar, entry))
          }
        yield assertTrue(results == List(true, true))
    },
    test("embedded-NUL classfile entries return the exact typed analyzer error") {
      ZIO.scoped:
        for
          root <- Workspace.work("nul-analyzer")
          old  <- products(root.resolve("old"), method = true)
          entry = "fixture/Invalid\u0000.class"
          jar    <- baselineJar(root.resolve("nul.jar"), old, entry)
          now    <- products(root.resolve("now"), method = true)
          result <- ZIO
            .serviceWithZIO[CompatibilityAnalyzer](_.analyze(CompatibilityInput(Platform.Jvm, jar, now)).either)
            .provideLayer(CompatibilityAnalyzer.jvm)
        yield assertTrue(result == Left(CompatibilityError.InvalidBaselineEntry(jar, entry)))
    },
    test("the JVM layer explicitly rejects JS and Native") {
      val errors = ZIO.foreach(List(Platform.Js, Platform.Native)): backend =>
        ZIO
          .serviceWithZIO[CompatibilityAnalyzer](
            _.analyze(CompatibilityInput(backend, Path.of("old"), Path.of("new"))).either
          )
          .map(_ == Left(CompatibilityError.UnsupportedBackend(backend)))
      val checked = errors.provideLayer(CompatibilityAnalyzer.jvm)
      checked.map(results => assertTrue(results == List(true, true)))
    },
    test("MiMa missing-class problems are typed and retain stable major-bump policy") {
      ZIO.scoped:
        for
          root   <- Workspace.work("missing-class")
          old    <- products(root.resolve("old"), method = true)
          now    <- Workspace.createDirectories(root.resolve("now"))
          result <- ZIO
            .serviceWithZIO[CompatibilityAnalyzer](_.analyze(CompatibilityInput(Platform.Jvm, old, now)))
            .provideLayer(CompatibilityAnalyzer.jvm)
        yield assertTrue(
          BinaryCheck.difference(ShipNumber.trusted(1, 0, 0), result) == BinaryCheck.Difference
            .Break(BumpPart.Major, 1),
          BinaryCheck.difference(previous, result) == BinaryCheck.Difference.Break(BumpPart.Minor, 1),
        )
    },
  )

  private def baselineJar(path: Path, source: Path, entry: String): IO[IoFailure, Path] =
    ZIO.scoped:
      ZIO
        .acquireRelease(
          ZIO
            .attemptBlocking(new JarOutputStream(Files.newOutputStream(path)))
            .mapError(_ => IoFailure(path, IoFault.Write))
        )(jar => ZIO.attemptBlocking(jar.close()).ignore)
        .flatMap: jar =>
          ZIO
            .attemptBlocking:
              jar.putNextEntry(new JarEntry(entry))
              jar.write(Files.readAllBytes(source.resolve("fixture/Api.class")))
              jar.closeEntry()
              path
            .mapError(_ => IoFailure(path, IoFault.Write))

  private def products(path: Path, method: Boolean, name: String = "fixture/Api"): IO[IoFailure, Path] =
    ZIO
      .attemptBlocking:
        Files.createDirectories(path.resolve("fixture"))
        val writer = new ClassWriter(0)
        writer.visit(
          Opcodes.V1_8,
          Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
          name,
          "Ljava/lang/Object;",
          "java/lang/Object",
          Array.empty[String],
        )
        if method then
          writer
            .visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT, "kept", "()I", "()I", Array.empty[String])
            .visitEnd()
        writer.visitEnd()
        Files.write(path.resolve(s"$name.class"), writer.toByteArray)
        path
      .mapError(_ => IoFailure(path, IoFault.Write))
end CompatibilitySpec
