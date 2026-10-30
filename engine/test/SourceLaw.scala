package histos.zinc

import histos.jdk.JdkRuntime
import histos.toolchain.BuildCompiler
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.{Platform, ScalaVersion}

import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.ZIO
import zio.test.{assertTrue, suite, test}

/** Source layout, classpath bans, and loader leases. No scalac and no child.
  */
object SourceLaw:
  private val scala213 = ScalaVersion("2.13.18")

  def files =
    suite("layout")(series, shared, packaged)

  def seen =
    test("The engine classpath cannot see the Scala 2 compiler."):
      val loader = Option(classOf[SourceLaw.type].getClassLoader).getOrElse(ClassLoader.getPlatformClassLoader)
      assertTrue(!ClassMarker.NscGlobal.visibleTo(loader))

  def lease =
    test("Loader leases close when later construction fails."):
      Workspace
        .work("histos-sources")
        .flatMap: root =>
          val jar    = root.resolve("lease.jar")
          val closed = new AtomicBoolean(false)
          writeZip(jar).flatMap: _ =>
            ZIO
              .scoped:
                CompilerPool
                  .acquireLoader(new Marked(jar.toUri.toURL, closed))
                  .flatMap: _ =>
                    ZIO.fail(CompileError.NoSources)
              .either
              .map: failed =>
                assertTrue(failed == Left(CompileError.NoSources), closed.get)

  def banned =
    test("A plugin marker is banned in a jar and in a classes directory."):
      BuildCompiler.version.flatMap: version =>
        GraphCompile.jdkHere.flatMap: jdk =>
          Workspace
            .work("histos-sources")
            .flatMap: root =>
              val classes = root.resolve("classes")
              val jar     = root.resolve("plugin.jar")
              val marker  = ClassMarker.PluginEntry.under(classes)
              for
                _   <- Workspace.writeString(marker, "")
                _   <- writeZip(jar, ClassMarker.PluginEntry.entry)
                dir <- stage(version, jdk, classes, root.resolve("Dir.scala")).either
                zip <- stage(version, jdk, jar, root.resolve("Jar.scala")).either
              yield (dir, zip) match
                case (
                      Left(CompileError.Banned(dirPath, ClasspathBan.Histos)),
                      Left(CompileError.Banned(jarPath, ClasspathBan.Histos)),
                    ) =>
                  assertTrue(dirPath == classes, jarPath == jar)
                case (dirGot, jarGot) =>
                  assertTrue(shown(dirGot).isEmpty, shown(jarGot).isEmpty)

  private def series =
    test("Scala 2.13 keeps its directories and drops Scala 3 and other platforms."):
      Workspace
        .work("histos-sources")
        .flatMap: root =>
          val kept   = "src/main/scala-2.13/Kept.scala"
          val here   = "src/main/jvm/Here.scala"
          val scala3 = "src/main/scala-3/Gone.scala"
          val js     = "src/main/js/Js.scala"
          val native = "src/main/native/Native.scala"
          for
            _   <- write(root, kept)
            _   <- write(root, here)
            _   <- write(root, scala3)
            _   <- write(root, js)
            _   <- write(root, native)
            got <- Sources.select(root, scala213, Platform.Jvm)
          yield assertTrue(names(root, got) == List(here, kept))

  private def shared =
    test("A package named `native` is shared, and `src/main` filters still apply."):
      Workspace
        .work("histos-sources")
        .flatMap: root =>
          val common = "src/main/scala/native/Shared.scala"
          val only   = "src/main/jvm/Only.scala"
          val js     = "src/main/js/JsOnly.scala"
          for
            _      <- write(root, common)
            _      <- write(root, only)
            _      <- write(root, js)
            jvm    <- Sources.select(root, scala213, Platform.Jvm)
            jsGot  <- Sources.select(root, scala213, Platform.Js)
            native <- Sources.select(root, scala213, Platform.Native)
          yield assertTrue(
            names(root, jvm) == List(only, common),
            names(root, jsGot) == List(js, common),
            names(root, native) == List(common),
          )

  private def packaged =
    test("A sources jar entry is the package path."):
      Workspace
        .work("histos-sources")
        .flatMap: root =>
          for
            _   <- write(root, "src/main/jvm/scala/demo/Only.scala")
            _   <- write(root, "src/main/scala/demo/App.scala")
            _   <- write(root, "src/main/scala/native/Shared.scala")
            got <- Sources.packaged(root, scala213, Platform.Jvm)
          yield assertTrue(got.map(_.entry) == List("demo/Only.scala", "demo/App.scala", "native/Shared.scala"))

  private def stage(version: ScalaVersion, jdk: JdkRuntime, banned: Path, source: Path) =
    JvmCompile.stage(
      CompileRequest(
        CompileToolchain(version, Platform.Jvm, jdk, histos.ScalacFlags.empty),
        List(source),
        List(banned),
        Path.of("out"),
        Path.of("analysis"),
        Nil,
        Nil,
      ),
      Nil,
      Nil,
      _ => (),
      None,
    )

  private def shown(got: Either[CompileError, CompileCall]): String =
    got.fold(_.message, _ => "compiled")

  private def names(root: Path, paths: List[Path]): List[String] =
    paths.map(path => root.relativize(path).iterator.asScala.mkString("/"))

  private def write(root: Path, relative: String) =
    Workspace.writeString(root.resolve(relative), "object Kept\n")

  private def writeZip(path: Path, entry: String = "notes.txt") =
    ZIO
      .attemptBlocking:
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new ZipOutputStream(Files.newOutputStream(path))
        try
          out.putNextEntry(new ZipEntry(entry))
          out.write(Array.emptyByteArray)
          out.closeEntry()
        finally out.close()
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit

  /** Records `close` so a failed scope can show the lease was released. */
  private final class Marked(url: URL, closed: AtomicBoolean)
      extends URLClassLoader(Array(url), ClassLoader.getPlatformClassLoader):
    override def close(): Unit =
      closed.set(true)
      super.close()
end SourceLaw
