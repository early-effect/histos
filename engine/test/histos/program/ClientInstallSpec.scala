package histos.program

import histos.jdk.Sha256
import histos.native.{ClangTarget, ClangText, ClangVersion, NativeKey}
import histos.pack.{MainClass, MainClassError}
import histos.publish.DistLayout
import histos.suite.HistosSpec
import histos.{ClangMajor, NativeVersion, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.ZIO
import zio.test.*

object ClientInstallSpec extends HistosSpec:
  private enum KeyInputs:
    case Clang(error: ClangText)
    case Main(error: MainClassError)

  def spec = suite("an installed client")(
    test("the manifest names the coordinate and the kit") {
      val text = DistLayout.manifestJson(
        "0.3.2",
        DistLayout.MavenPin("rocks.earlyeffect", "histos", "0.3.2"),
        Some("0.3.2-client-kit.zip" -> ("b" * 64)),
      )
      assertTrue(
        text.contains(""""group""""),
        text.contains(""""artifact""""),
        text.contains(""""revision""""),
        text.contains(""""clientKit""""),
        !text.contains(""""jar""""),
        !text.contains(""""jarSha256""""),
        !text.contains("linux-x86_64"),
        !text.contains("macos-aarch64"),
        !text.contains(""""clients""""),
      )
    },
    test("optimize changes the link key") {
      val version = ScalaVersion("3.10.0")
      val target  = ClangTarget("arm64-apple-darwin24.0.0")
      val differ  =
        for
          clang <- ClangVersion.from("21.1.8").left.map(KeyInputs.Clang(_))
          main  <- MainClass.from("histos.client.Main").left.map(KeyInputs.Main(_))
        yield
          val on = NativeKey.of(
            version,
            clang,
            target,
            main,
            List.empty[Sha256],
            List.empty[Sha256],
            optimize = true,
          )
          val off = NativeKey.of(
            version,
            clang,
            target,
            main,
            List.empty[Sha256],
            List.empty[Sha256],
            optimize = false,
          )
          on != off
      assertTrue(differ == Right(true))
    },
    test("a kit round-trips one classes directory") {
      ZIO.scoped {
        ZIO
          .acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-kit")).orDie)(dir =>
            ZIO.attemptBlocking(remove(dir)).orDie
          )
          .flatMap { root =>
            val classes = root.resolve("classes")
            val bytes   = Array[Byte](1, 2, 3)
            for
              _ <- ZIO.attemptBlocking {
                Files.createDirectories(classes)
                Files.write(classes.resolve("Demo.class"), bytes)
                ()
              }.orDie
              main <- ZIO
                .fromEither(MainClass.from("histos.client.Main"))
                .mapError(error => ClientKit.Failure.Corrupt(error.message))
              written <- ClientKit.write(
                root.resolve(ClientKit.FileName),
                ClientKit(main, ClangMajor(21), ScalaVersion("3.10.0"), NativeVersion("0.5.13"), List(classes)),
              )
              read <- ClientKit.read(written, root.resolve("out"))
              file <- read.classpath match
                case dir :: Nil => ZIO.attemptBlocking(Files.readAllBytes(dir.resolve("Demo.class"))).orDie
                case _          => ZIO.fail(ClientKit.Failure.Corrupt("classpath is not one directory"))
            yield assertTrue(
              read.main == main,
              read.clang == ClangMajor(21),
              read.scala == ScalaVersion("3.10.0"),
              read.native == NativeVersion("0.5.13"),
              java.util.Arrays.equals(file, bytes),
            )
          }
      }
    },
  )

  private def remove(root: Path): Unit =
    if Files.exists(root) then
      val walk  = Files.walk(root)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
end ClientInstallSpec
